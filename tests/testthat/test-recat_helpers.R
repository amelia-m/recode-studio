# Run with: testthat::test_dir("tests/testthat")
#
# Tests for the recategorization core in R/recat_helpers.R.
# The central invariant: eval(parse(generate_recat_R(...))) must produce the
# SAME output column as apply_recat() for every rule set, so the exported
# script and the in-app preview can never disagree.

source(file.path("..", "..", "R", "recat_helpers.R"))

# --- Fixtures / helpers ------------------------------------------------------

# Build a one-row-per-rule tibble matching the empty_recat_tibble() schema from
# human-friendly vector inputs. vars / include_terms / exclude_terms are passed
# as character vectors and joined here.
mk_rule <- function(out_col, category, vars,
                    include_terms = character(0),
                    exclude_terms = character(0),
                    match_type = "literal", priority = 100L,
                    sibling_pattern = NA_character_, notes = NA_character_) {
  vars_j <- recat_join(vars)
  inc_j  <- recat_join(include_terms)
  exc_j  <- recat_join(exclude_terms)
  tibble::tibble(
    recat_id        = recat_rule_id(out_col, category, vars_j, inc_j, exc_j,
                                    match_type),
    out_col         = out_col,
    category        = category,
    vars            = vars_j,
    sibling_pattern = sibling_pattern,
    include_terms   = inc_j,
    exclude_terms   = exc_j,
    match_type      = match_type,
    priority        = as.integer(priority),
    notes           = notes,
    author          = NA_character_,
    created_at      = "2026-07-09T00:00:00",
    source_dataset  = "example_messy.csv"
  )
}

# Eval the generated R script against `df` in a clean environment and return the
# resulting data frame. Columns are resolved at generation time against
# `data_cols`, so pass the frame's own names for parity with apply_recat().
#
# The eval(parse()) here is deliberate and test-only: the whole point of these
# tests is that the script generate_recat_R() emits really does produce the same
# column apply_recat() does. The input is not user data — it is code this test
# just generated from its own fixtures — and it runs in a throwaway env.
eval_generated <- function(df, rules, data_cols = names(df)) {
  code <- generate_recat_R(rules, dataset_id = "example_messy",
                           data_cols = data_cols)
  env <- new.env(parent = globalenv())
  env$df <- df
  suppressWarnings(suppressMessages(eval(parse(text = code), envir = env)))
  env$df
}

# The core agreement assertion: apply_recat() and the generated-code path must
# yield identical out_col vectors. Returns the applied vector for extra checks.
expect_recat_agree <- function(df, rules, out_col, data_cols = names(df)) {
  applied <- apply_recat(df, rules)$df
  genned  <- eval_generated(df, rules, data_cols = data_cols)
  testthat::expect_true(out_col %in% names(applied))
  testthat::expect_true(out_col %in% names(genned))
  testthat::expect_identical(genned[[out_col]], applied[[out_col]])
  applied[[out_col]]
}

# A small café-orders-shaped frame, same shape as inst/extdata/example_messy.csv.
demo_df <- function() {
  tibble::tibble(
    order_id = 1:6,
    drink1   = c("cappuccino", "espresso", "flat white",
                 "decaf latte", "cold brew", NA),
    drink2   = c("latte", NA, "cortado", NA, "americano", "macchiato")
  )
}


# --- recat_split / recat_join round-trip -------------------------------------

test_that("recat_split trims, drops blanks, and handles NA / empty", {
  expect_equal(recat_split("a; b ;c"), c("a", "b", "c"))
  expect_equal(recat_split("  solo  "), "solo")
  expect_equal(recat_split("a;;b"), c("a", "b"))
  expect_equal(recat_split(NA_character_), character(0))
  expect_equal(recat_split(""), character(0))
  expect_equal(recat_split(character(0)), character(0))
})

test_that("recat_join joins, drops empties, and NA-encodes empty input", {
  expect_equal(recat_join(c("a", "b")), "a;b")
  expect_equal(recat_join(c("a", "", "b")), "a;b")
  expect_identical(recat_join(character(0)), NA_character_)
})

test_that("recat_join / recat_split are inverse for clean vectors", {
  v <- c("espresso", "macchiato", "flat white")
  expect_equal(recat_split(recat_join(v)), v)
})


# --- Schema / constants ------------------------------------------------------

test_that("empty_recat_tibble has the expected schema", {
  x <- empty_recat_tibble()
  expect_named(x, c(
    "recat_id", "out_col", "category", "vars", "sibling_pattern",
    "include_terms", "exclude_terms", "match_type", "priority",
    "notes", "author", "created_at", "source_dataset"
  ))
  expect_equal(nrow(x), 0)
  expect_type(x$priority, "integer")
})

test_that("recat constants are single-sourced", {
  expect_equal(RECAT_MATCH_TYPES, c("literal", "regex"))
  expect_equal(RECAT_LIST_SEP, ";")
})

test_that("recat_rule_id is stable and content-sensitive", {
  a <- recat_rule_id("oc", "cat", "v1;v2", "t1", "e1", "literal")
  b <- recat_rule_id("oc", "cat", "v1;v2", "t1", "e1", "literal")
  c <- recat_rule_id("oc", "cat", "v1;v2", "t1", "e1", "regex")
  expect_identical(a, b)
  expect_false(a == c)
  expect_equal(nchar(a), 12L)
})


# --- Single rule (literal) ---------------------------------------------------

test_that("single literal rule matches ANY include term across vars", {
  df <- demo_df()
  rules <- mk_rule("drink_family", "Espresso-based",
                   vars = c("drink1", "drink2"),
                   include_terms = c("espresso", "cortado", "americano"),
                   match_type = "literal", priority = 10L)
  out <- expect_recat_agree(df, rules, "drink_family")
  # row2 espresso, row3 cortado, row5 americano.
  expect_equal(out, c(NA, "Espresso-based", "Espresso-based", NA,
                      "Espresso-based", NA))
})

test_that("literal matching is case-insensitive", {
  df <- tibble::tibble(x = c("Flat WHITE", "cold brew"))
  rules <- mk_rule("cat", "Milk-based", vars = "x",
                   include_terms = "flat white")
  out <- expect_recat_agree(df, rules, "cat")
  expect_equal(out, c("Milk-based", NA))
})


# --- Regex vs literal --------------------------------------------------------

test_that("regex rule matches an alternation pattern that literal would not", {
  df <- tibble::tibble(
    drink1 = c("capuccino", "cappucino", "esspresso", "moccha", "flatwhite"))
  rx <- mk_rule("drink_family", "Milk-based", vars = "drink1",
                include_terms = "ca[bp]+u?c+ino|latt|moc+[ah]+|flat ?white",
                match_type = "regex")
  out <- expect_recat_agree(df, rx, "drink_family")
  expect_equal(out, c("Milk-based", "Milk-based", NA, "Milk-based",
                      "Milk-based"))

  # The same string used literally is a fixed substring and matches nothing.
  lit <- mk_rule("drink_family", "Milk-based", vars = "drink1",
                 include_terms = "ca[bp]+u?c+ino|latt|moc+[ah]+|flat ?white",
                 match_type = "literal")
  out_lit <- expect_recat_agree(df, lit, "drink_family")
  expect_true(all(is.na(out_lit)))
})

test_that("an invalid regex yields no matches instead of erroring", {
  df <- tibble::tibble(x = c("latte", "mocha"))
  rules <- mk_rule("cat", "Broken", vars = "x",
                   include_terms = "latte(", match_type = "regex")
  expect_silent(hits <- recat_rule_hits(df, rules[1, ]))
  expect_equal(hits, c(FALSE, FALSE))
})


# --- Exclude terms (AND-NOT) -------------------------------------------------

test_that("exclude terms suppress a row even when include matches (AND-NOT)", {
  df <- tibble::tibble(review = c(
    "Coffee was cold and service slow.",
    "Loved the cold brew on a hot day.",
    "Great espresso with a smooth finish."))
  rules <- mk_rule("review_flag", "Needs follow-up", vars = "review",
                   include_terms = c("slow", "cold"),
                   exclude_terms = "cold brew",
                   match_type = "literal", priority = 10L)
  out <- expect_recat_agree(df, rules, "review_flag")
  # row2 contains "cold" but is vetoed by the "cold brew" exclude term.
  expect_equal(out, c("Needs follow-up", NA, NA))
})

test_that("exclude term is checked across ALL vars, not just the include hit", {
  df <- tibble::tibble(
    drink1 = c("latte", "latte"),
    notes  = c("extra hot", "decaf soy")
  )
  rules <- mk_rule("cat", "Milk-based", vars = c("drink1", "notes"),
                   include_terms = "latte",
                   exclude_terms = "decaf")
  out <- expect_recat_agree(df, rules, "cat")
  # row2 has the exclude term in a DIFFERENT column -> still excluded.
  expect_equal(out, c("Milk-based", NA))
})


# --- "any var matches" -------------------------------------------------------

test_that("a term present in only the second var still matches (OR across vars)", {
  df <- tibble::tibble(
    drink1 = c("cold brew", "cold brew"),
    drink2 = c("espresso", "latte")
  )
  rules <- mk_rule("cat", "Espresso-based", vars = c("drink1", "drink2"),
                   include_terms = "espresso")
  expect_equal(recat_rule_hits(df, rules[1, ]), c(TRUE, FALSE))
  out <- expect_recat_agree(df, rules, "cat")
  expect_equal(out, c("Espresso-based", NA))
})

test_that("a rule with zero include terms matches nothing (blanket guard)", {
  df <- demo_df()
  rules <- mk_rule("cat", "Everything", vars = c("drink1", "drink2"))
  expect_true(all(!recat_rule_hits(df, rules[1, ])))
  out <- expect_recat_agree(df, rules, "cat")
  expect_true(all(is.na(out)))
})

test_that("a rule selecting no existing column matches nothing", {
  df <- demo_df()
  rules <- mk_rule("cat", "Ghost", vars = "not_a_column",
                   include_terms = "latte")
  expect_equal(recat_rule_hits(df, rules[1, ]), rep(FALSE, nrow(df)))
})


# --- sibling_pattern column resolution ---------------------------------------

test_that("sibling_pattern expands to the whole column family", {
  df <- demo_df()
  rules <- mk_rule("drink_family", "Espresso-based", vars = character(0),
                   sibling_pattern = "^drink[0-9]+$",
                   include_terms = c("espresso", "macchiato"))
  # drink1 (row2) and drink2 (row6) both contribute.
  expect_equal(recat_rule_hits(df, rules[1, ]),
               c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE))
  out <- expect_recat_agree(df, rules, "drink_family")
  expect_equal(out, c(NA, "Espresso-based", NA, NA, NA, "Espresso-based"))
})


# --- Per-column breakdown ----------------------------------------------------

test_that("recat_rule_hits_by_col reports per-column include/exclude/kept", {
  df <- tibble::tibble(
    drink1 = c("latte", "cold brew", "cold brew"),
    drink2 = c("espresso", "latte", "cortado"),
    notes  = c("decaf soy", "extra hot", "extra hot")
  )
  rules <- mk_rule("cat", "Milk-based", vars = c("drink1", "drink2", "notes"),
                   include_terms = "latte", exclude_terms = "decaf")
  by <- recat_rule_hits_by_col(df, rules[1, ])
  expect_equal(by$column, c("drink1", "drink2", "notes"))
  expect_equal(by$include_hits, c(1L, 1L, 0L))
  expect_equal(by$exclude_hits, c(0L, 0L, 1L))
  # row1 is vetoed rule-wide by notes; drink2's row2 latte survives.
  expect_equal(by$kept, c(0L, 1L, 0L))
})

test_that("recat_rule_hits_by_col is empty for a rule with no columns/terms", {
  df <- demo_df()
  no_cols <- mk_rule("cat", "X", vars = "nope", include_terms = "latte")
  expect_equal(nrow(recat_rule_hits_by_col(df, no_cols[1, ])), 0L)
  no_terms <- mk_rule("cat", "X", vars = "drink1")
  expect_equal(nrow(recat_rule_hits_by_col(df, no_terms[1, ])), 0L)
})


# --- Multiple rules, same out_col, first-match-wins by priority --------------

test_that("overlapping rules on one out_col resolve by priority (first match wins)", {
  # A row satisfying BOTH the Espresso rule and the Milk rule.
  df <- tibble::tibble(x = c("espresso macchiato latte", "latte", "cold brew"))

  espresso_first <- dplyr::bind_rows(
    mk_rule("drink_family", "Espresso-based", vars = "x",
            include_terms = "espresso", priority = 10L),
    mk_rule("drink_family", "Milk-based", vars = "x",
            include_terms = "latte", priority = 20L)
  )
  out1 <- expect_recat_agree(df, espresso_first, "drink_family")
  expect_equal(out1, c("Espresso-based", "Milk-based", NA))

  # Flip the priorities: now Milk-based wins the overlapping row.
  milk_first <- dplyr::bind_rows(
    mk_rule("drink_family", "Espresso-based", vars = "x",
            include_terms = "espresso", priority = 20L),
    mk_rule("drink_family", "Milk-based", vars = "x",
            include_terms = "latte", priority = 10L)
  )
  out2 <- expect_recat_agree(df, milk_first, "drink_family")
  expect_equal(out2, c("Milk-based", "Milk-based", NA))
})

test_that("NA priority sorts last", {
  df <- tibble::tibble(x = c("espresso latte"))
  rules <- dplyr::bind_rows(
    mk_rule("drink_family", "Milk-based", vars = "x",
            include_terms = "latte", priority = NA_integer_),
    mk_rule("drink_family", "Espresso-based", vars = "x",
            include_terms = "espresso", priority = 50L)
  )
  out <- expect_recat_agree(df, rules, "drink_family")
  expect_equal(out, "Espresso-based")
})

test_that("summary reflects first-match-wins (later rule matches fewer rows)", {
  df <- tibble::tibble(x = c("espresso latte", "latte only", "cold brew"))
  rules <- dplyr::bind_rows(
    mk_rule("drink_family", "Espresso-based", vars = "x",
            include_terms = "espresso", priority = 10L),
    mk_rule("drink_family", "Milk-based", vars = "x",
            include_terms = "latte", priority = 20L)
  )
  res <- apply_recat(df, rules)
  esp  <- res$summary$rows_matched[res$summary$category == "Espresso-based"]
  milk <- res$summary$rows_matched[res$summary$category == "Milk-based"]
  expect_equal(esp, 1L)
  expect_equal(milk, 1L)   # row1 already taken by the Espresso rule
  expect_named(res$summary,
               c("recat_id", "out_col", "category", "rows_matched"))
})


# --- Pre-existing out_col value preserved ------------------------------------

test_that("a pre-existing non-NA out_col value is never overwritten", {
  df <- demo_df()
  df$drink_family <- c("Manual override", NA, NA, NA, NA, NA)
  rules <- mk_rule("drink_family", "Milk-based",
                   vars = c("drink1", "drink2"),
                   include_terms = c("cappuccino", "latte"),
                   priority = 10L)
  out <- expect_recat_agree(df, rules, "drink_family")
  # row1 would match but keeps its manual value; row4 ("decaf latte") matches.
  expect_equal(out, c("Manual override", NA, NA, "Milk-based", NA, NA))
})


# --- Unmatched -> NA ---------------------------------------------------------

test_that("rows matching nothing are left NA in the new column", {
  df <- tibble::tibble(x = c("cold brew", "iced tea", "hot chocolate"))
  rules <- mk_rule("cat", "Espresso-based", vars = "x",
                   include_terms = "espresso")
  out <- expect_recat_agree(df, rules, "cat")
  expect_true(all(is.na(out)))
})


# --- Non-syntactic column names ----------------------------------------------

test_that("a non-syntactic out_col with spaces generates parseable, correct code", {
  df <- demo_df()
  oc <- "drink family"
  rules <- mk_rule(oc, "Espresso-based",
                   vars = c("drink1", "drink2"),
                   include_terms = c("espresso", "cortado"),
                   exclude_terms = "flat white",
                   priority = 10L)
  code <- generate_recat_R(rules, dataset_id = "example_messy",
                           data_cols = names(df))
  expect_silent(parse(text = code))
  out <- expect_recat_agree(df, rules, oc)
  # row2 espresso; row3 has cortado but "flat white" in drink1 vetoes it.
  expect_equal(out, c(NA, "Espresso-based", NA, NA, NA, NA))
})

test_that("a non-syntactic SOURCE column generates parseable, correct code", {
  df <- tibble::tibble(`drink 1` = c("latte", "cold brew"))
  rules <- mk_rule("cat", "Milk-based", vars = "drink 1",
                   include_terms = "latte")
  code <- generate_recat_R(rules, dataset_id = "example_messy",
                           data_cols = names(df))
  expect_silent(parse(text = code))
  out <- expect_recat_agree(df, rules, "cat")
  expect_equal(out, c("Milk-based", NA))
})


# --- Multiple out_cols in one rule set ---------------------------------------

test_that("independent out_cols are each derived correctly in one pass", {
  df <- demo_df()
  rules <- dplyr::bind_rows(
    mk_rule("drink_family", "Espresso-based",
            vars = c("drink1", "drink2"),
            include_terms = c("espresso", "cortado", "americano")),
    mk_rule("is_decaf", "yes", vars = c("drink1", "drink2"),
            include_terms = "decaf")
  )
  a <- expect_recat_agree(df, rules, "drink_family")
  b <- expect_recat_agree(df, rules, "is_decaf")
  expect_equal(a, c(NA, "Espresso-based", "Espresso-based", NA,
                    "Espresso-based", NA))
  expect_equal(b, c(NA, NA, NA, "yes", NA, NA))
})


# --- NA source values --------------------------------------------------------

test_that("NA source cells never match and never error", {
  df <- tibble::tibble(x = c(NA_character_, "latte", NA_character_))
  rules <- mk_rule("cat", "Milk-based", vars = "x", include_terms = "latte")
  out <- expect_recat_agree(df, rules, "cat")
  expect_equal(out, c(NA, "Milk-based", NA))
})


# --- Empty rule set ----------------------------------------------------------

test_that("an empty rule set is a no-op and generates a runnable stub", {
  df <- demo_df()
  res <- apply_recat(df, empty_recat_tibble())
  expect_identical(res$df, df)
  expect_equal(nrow(res$summary), 0)

  code <- generate_recat_R(empty_recat_tibble(), dataset_id = "example_messy")
  expect_silent(parse(text = code))
  env <- new.env(parent = globalenv())
  env$df <- df
  suppressWarnings(suppressMessages(eval(parse(text = code), envir = env)))
  expect_identical(env$df, df)
})


# --- recat_rule_condition_R --------------------------------------------------

test_that("recat_rule_condition_R degrades to FALSE without cols or terms", {
  r <- mk_rule("cat", "X", vars = "a", include_terms = "t")[1, ]
  expect_equal(recat_rule_condition_R(r, character(0)), "FALSE")
  r2 <- mk_rule("cat", "X", vars = "a")[1, ]
  expect_equal(recat_rule_condition_R(r2, "a"), "FALSE")
})

test_that("recat_rule_condition_R emits fixed() for literal, regex() for regex", {
  lit <- mk_rule("cat", "X", vars = "a", include_terms = "Latte")[1, ]
  code_lit <- recat_rule_condition_R(lit, "a")
  expect_true(grepl("fixed(", code_lit, fixed = TRUE))
  expect_true(grepl('"latte"', code_lit, fixed = TRUE))   # lower-cased

  rx <- mk_rule("cat", "X", vars = "a", include_terms = "latt|moc",
                match_type = "regex")[1, ]
  code_rx <- recat_rule_condition_R(rx, "a")
  expect_true(grepl("ignore_case = TRUE", code_rx, fixed = TRUE))
})


# --- Generated script shape --------------------------------------------------

test_that("generated script uses case_when (never the deprecated case_match)", {
  rules <- mk_rule("drink_family", "Milk-based", vars = "drink1",
                   include_terms = "latte")
  code <- generate_recat_R(rules, dataset_id = "example_messy",
                           data_cols = c("drink1"))
  expect_true(grepl("case_when(", code, fixed = TRUE))
  expect_false(grepl("case_match(", code, fixed = TRUE))
  expect_true(grepl("dataset: example_messy", code, fixed = TRUE))
})

test_that("rule notes are carried into the generated script as comments", {
  rules <- mk_rule("drink_family", "Milk-based", vars = "drink1",
                   include_terms = "latte",
                   notes = "milk-forward drinks")
  code <- generate_recat_R(rules, dataset_id = "example_messy",
                           data_cols = "drink1")
  expect_true(grepl("# milk-forward drinks", code, fixed = TRUE))
  expect_silent(parse(text = code))
})

test_that("without data_cols, generation falls back to the rule's own vars", {
  rules <- mk_rule("drink_family", "Milk-based",
                   vars = c("drink1", "drink2"), include_terms = "latte")
  code <- generate_recat_R(rules, dataset_id = "example_messy")
  expect_silent(parse(text = code))
  expect_true(grepl("drink1", code, fixed = TRUE))
  expect_true(grepl("drink2", code, fixed = TRUE))
})


# --- read_recat / write_recat round-trip -------------------------------------

test_that("write_recat -> read_recat round-trips a rule set exactly", {
  rules <- dplyr::bind_rows(
    mk_rule("drink_family", "Espresso-based",
            vars = c("drink1", "drink2"),
            include_terms = c("espresso", "macchiato", "cortado"),
            exclude_terms = "decaf",
            match_type = "literal", priority = 10L,
            notes = "primary espresso rule"),
    mk_rule("drink_family", "Milk-based",
            vars = "drink1",
            include_terms = "ca[bp]+u?c+ino|latt|moc+[ah]+|flat ?white",
            match_type = "regex", priority = 20L)
  )
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  write_recat(rules, path)
  back <- read_recat(path)

  strip <- function(x) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    attr(x, "spec") <- NULL
    attr(x, "problems") <- NULL
    x
  }
  expect_equal(strip(back), strip(rules))
  expect_type(back$priority, "integer")
})

test_that("read_recat on a missing path returns the empty schema", {
  x <- read_recat(tempfile(fileext = ".csv"))
  expect_equal(nrow(x), 0)
  expect_named(x, names(empty_recat_tibble()))
})


# --- End-to-end against the bundled example dataset --------------------------

test_that("the bundled worked examples behave as documented on example_messy.csv", {
  path <- file.path("..", "..", "inst", "extdata", "example_messy.csv")
  skip_if_not(file.exists(path), "bundled example dataset not found")
  df <- readr::read_csv(path, show_col_types = FALSE)

  rules <- dplyr::bind_rows(
    # Worked example 1: espresso-forward drinks across the drink1/drink2 family.
    mk_rule("drink_family", "Espresso-based",
            vars = c("drink1", "drink2"),
            include_terms = c("espresso", "expresso", "esspresso",
                              "macchiato", "machiato", "macchiatto",
                              "americano", "americanno", "cortado"),
            match_type = "literal", priority = 10L),
    # Worked example 2: milk-forward drinks, one regex over every variant.
    mk_rule("drink_family", "Milk-based",
            vars = c("drink1", "drink2"),
            include_terms = "ca[bp]+u?c+ino|latt|moc+[ah]+|flat ?white",
            match_type = "regex", priority = 20L),
    # Worked example 3: reviews needing follow-up, with the "cold brew" veto.
    mk_rule("review_flag", "Needs follow-up", vars = "review",
            include_terms = c("slow", "lukewarm", "burnt", "watery", "wrong",
                              "disappointing", "overly sweet", "too loud",
                              "cold"),
            exclude_terms = "cold brew",
            match_type = "literal", priority = 10L)
  )

  fam  <- expect_recat_agree(df, rules, "drink_family")
  flag <- expect_recat_agree(df, rules, "review_flag")

  # Both categories are actually used, and every row lands somewhere.
  expect_true(all(c("Espresso-based", "Milk-based") %in% fam))
  expect_equal(sum(is.na(fam)), 0L)

  # The exclude term rescues the happy "Loved the cold brew..." review (row 11),
  # while genuine complaints are still flagged.
  cold_brew_row <- grep("Loved the cold brew", df$review)
  expect_length(cold_brew_row, 1)
  expect_true(is.na(flag[cold_brew_row]))
  expect_equal(flag[grep("Slow service this afternoon", df$review)],
               "Needs follow-up")
  expect_true(sum(!is.na(flag)) >= 8)
})
