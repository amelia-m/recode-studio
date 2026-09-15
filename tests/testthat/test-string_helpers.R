# Run with: testthat::test_dir("tests/testthat")

source(file.path("..", "..", "R", "string_helpers.R"))

test_that("empty_recodes_tibble has expected schema", {
  x <- empty_recodes_tibble()
  expect_named(x, c(
    "rule_id", "variable", "apply_to_siblings", "sibling_pattern",
    "match_type", "old_value", "new_value", "action", "notes",
    "author", "created_at", "updated_at", "source_dataset"
  ))
  expect_equal(nrow(x), 0)
})

test_that("normalize_value applies match_type strategy", {
  expect_equal(normalize_value("  Foo  Bar  ", "trimmed_ci"), "foo bar")
  expect_equal(normalize_value("Foo Bar", "exact_ci"),        "foo bar")
  expect_equal(normalize_value("Foo Bar", "exact"),           "Foo Bar")
})

test_that("cluster_strings groups a variant family", {
  values <- c("cappuccino", "capuccino", "cappucino",
              "capccino", "flat white")
  out <- cluster_strings(values, threshold = 0.85, algorithm = "jw")
  expect_equal(nrow(out), 5)
  variants <- c("cappuccino", "capuccino", "cappucino", "capccino")
  expect_length(unique(out$cluster_id[out$value %in% variants]), 1L)
  cap  <- out$cluster_id[out$value == "cappuccino"]
  flat <- out$cluster_id[out$value == "flat white"]
  expect_false(cap == flat)
})

test_that("cluster_strings handles empty input", {
  out <- cluster_strings(character(0))
  expect_equal(nrow(out), 0)
  expect_named(out, c("value", "n", "cluster_id", "is_rare"))
})

test_that("normalize_for_cluster applies the selected steps", {
  expect_equal(normalize_for_cluster("Foo, BAR", c("lower","punct","squish")),
               "foo bar")
  expect_equal(normalize_for_cluster("latte latte", "dedupe_tokens"),
               "latte")
  expect_equal(normalize_for_cluster("oat milk latte", "sort_tokens"),
               "latte milk oat")
  expect_equal(normalize_for_cluster("Foo Bar", character(0)), "Foo Bar")
})

test_that("cosine metric clusters reordered multi-word values", {
  values <- c("oat milk vanilla latte", "vanilla latte with oat milk",
              "flat white")
  out <- cluster_strings(values, algorithm = "cosine", q = 2, threshold = 0.6,
                         normalize = c("lower", "punct", "squish"))
  reordered <- c("oat milk vanilla latte", "vanilla latte with oat milk")
  expect_length(unique(out$cluster_id[out$value %in% reordered]), 1L)
  expect_false(out$cluster_id[out$value == "flat white"] ==
               out$cluster_id[out$value == "oat milk vanilla latte"])
})

test_that("sort_tokens normalization groups order-permuted values exactly", {
  values <- c("a b c", "c b a", "x y z")
  out <- cluster_strings(values, algorithm = "lv", threshold = 0.99,
                         normalize = "sort_tokens")
  expect_equal(out$cluster_id[out$value == "a b c"],
               out$cluster_id[out$value == "c b a"])
})

test_that("metaphone clusters phonetically-equal drink names", {
  testthat::skip_if_not_installed("phonics")
  values <- c("espresso", "esspresso", "latte")
  out <- cluster_strings(values, algorithm = "metaphone")
  expect_equal(out$cluster_id[out$value == "espresso"],
               out$cluster_id[out$value == "esspresso"])
  expect_false(out$cluster_id[out$value == "latte"] ==
               out$cluster_id[out$value == "espresso"])
})

test_that("CLUSTER_ALGORITHMS / NORMALIZERS constants are exported", {
  expect_true(all(c("jw","osa","lv","lcs","cosine","jaccard","soundex",
                    "metaphone") %in% CLUSTER_ALGORITHMS))
  expect_true(all(c("lower","punct","squish","dedupe_tokens","sort_tokens")
                  %in% CLUSTER_NORMALIZERS))
})

test_that("write_recodes -> read_recodes preserves NA encoding", {
  rules <- tibble::tibble(
    rule_id = c("a", "b"), variable = c("drink1", "city"),
    apply_to_siblings = c(TRUE, FALSE),
    sibling_pattern = c("^drink[0-9]+$", NA_character_),
    match_type = c("trimmed_ci", "trimmed_ci"),
    old_value = c("capuccino", "uptwon"),
    new_value = c("cappuccino", NA_character_),
    action = c("recode", "delete"),
    notes = c("typo", NA_character_), author = c("t", "t"),
    created_at = c("2026-05-27T10:00:00", "2026-05-27T10:00:00"),
    updated_at = c("2026-05-27T10:00:00", "2026-05-27T10:00:00"),
    source_dataset = c("example_messy.csv", "example_messy.csv")
  )
  tmp <- tempfile(fileext = ".csv"); on.exit(unlink(tmp), add = TRUE)
  write_recodes(rules, tmp)
  rt <- read_recodes(tmp)
  expect_equal(nrow(rt), 2)
  expect_true(is.na(rt$new_value[2]))
  expect_equal(rt$apply_to_siblings, rules$apply_to_siblings)
})

test_that("recode_rule_id is stable", {
  a <- recode_rule_id("drink1", "trimmed_ci", "capuccino")
  b <- recode_rule_id("drink1", "trimmed_ci", "capuccino")
  c <- recode_rule_id("drink1", "trimmed_ci", "cappuccino")
  expect_equal(a, b)
  expect_false(a == c)
  expect_equal(nchar(a), 12L)
})

test_that("validate_recodes flags duplicate key, rule chain, blank new_value", {
  rules <- tibble::tibble(
    rule_id = c("r1","r2","r3","r4","r5"),
    variable = rep("drink1", 5),
    apply_to_siblings = rep(FALSE, 5), sibling_pattern = rep(NA_character_, 5),
    match_type = rep("trimmed_ci", 5),
    old_value = c("a","a","b","x","c"),
    new_value = c("b","b","c","","a"),
    action = rep("recode", 5),
    notes = rep(NA_character_, 5), author = rep("t", 5),
    created_at = rep("", 5), updated_at = rep("", 5),
    source_dataset = rep("d", 5)
  )
  issues <- validate_recodes(rules)
  expect_true(nrow(issues$duplicate_keys) >= 1)
  expect_true(nrow(issues$blank_new_value) >= 1)
  expect_true(nrow(issues$rule_chains) >= 1)
})

test_that("apply_recodes expands to sibling columns when requested", {
  df <- tibble::tibble(
    drink1 = c("capuccino", "x"),
    drink2 = c("capuccino", NA_character_),
    other  = c("capuccino", "z")
  )
  rules <- tibble::tibble(
    rule_id = "r1", variable = "drink1",
    apply_to_siblings = TRUE, sibling_pattern = "^drink[0-9]+$",
    match_type = "trimmed_ci", old_value = "capuccino",
    new_value = "cappuccino", action = "recode",
    notes = NA_character_, author = "t",
    created_at = "", updated_at = "", source_dataset = "d"
  )
  res <- apply_recodes(df, rules)
  expect_equal(res$df$drink1[1], "cappuccino")
  expect_equal(res$df$drink2[1], "cappuccino")
  expect_equal(res$df$other[1], "capuccino")  # untouched
  expect_equal(res$summary$cells_changed, 2L)
})

test_that("apply_recodes delete sets NA", {
  df <- tibble::tibble(x = c("foo", "bar"))
  rules <- tibble::tibble(
    rule_id = "r1", variable = "x", apply_to_siblings = FALSE,
    sibling_pattern = NA_character_, match_type = "trimmed_ci",
    old_value = "foo", new_value = NA_character_, action = "delete",
    notes = NA_character_, author = "t",
    created_at = "", updated_at = "", source_dataset = "d"
  )
  res <- apply_recodes(df, rules)
  expect_true(is.na(res$df$x[1]))
  expect_equal(res$df$x[2], "bar")
})

test_that("generate_recode_R parses and contains case_when", {
  rules <- tibble::tibble(
    rule_id = c("r1","r2"), variable = c("drink1","drink1"),
    apply_to_siblings = c(FALSE, FALSE), sibling_pattern = c(NA_character_, NA_character_),
    match_type = c("trimmed_ci","trimmed_ci"),
    old_value = c("capuccino","gone"),
    new_value = c("cappuccino", NA_character_),
    action = c("recode","delete"),
    notes = c("typo","x"), author = c("t","t"),
    created_at = c("",""), updated_at = c("",""),
    source_dataset = c("d","d")
  )
  code <- generate_recode_R(rules, dataset_id = "example_messy")
  expect_silent(parse(text = code))
  expect_true(grepl("case_when", code))
  expect_true(grepl("NA_character_", code))
})

test_that("generate_recode_R handles values with quotes and backslashes", {
  rules <- tibble::tibble(
    rule_id = "r1", variable = "x",
    apply_to_siblings = FALSE, sibling_pattern = NA_character_,
    match_type = "trimmed_ci",
    old_value = 'it\'s "quoted"', new_value = "clean\\value",
    action = "recode", notes = "multi\nline note", author = "t",
    created_at = "", updated_at = "", source_dataset = "d"
  )
  code <- generate_recode_R(rules, "test_dataset")
  expect_silent(parse(text = code))
})

test_that("validate_recodes flags invalid match_type / action enums", {
  rules <- tibble::tibble(
    rule_id = c("r1","r2","r3"), variable = rep("x", 3),
    apply_to_siblings = rep(FALSE, 3), sibling_pattern = rep(NA_character_, 3),
    match_type = c("trimmed_ci", "regx", "exact"),   # r2 bad
    old_value = c("a","b","c"), new_value = c("A","B","C"),
    action = c("recode", "recode", "destroy"),       # r3 bad
    notes = rep(NA_character_, 3), author = rep("t", 3),
    created_at = rep("", 3), updated_at = rep("", 3), source_dataset = rep("d", 3)
  )
  issues <- validate_recodes(rules)
  expect_equal(nrow(issues$invalid_enum), 2L)
  expect_setequal(issues$invalid_enum$rule_id, c("r2", "r3"))
})

test_that("enum constants are exported and complete", {
  expect_setequal(RECODE_MATCH_TYPES, c("exact","exact_ci","trimmed_ci","regex"))
  expect_setequal(RECODE_ACTIONS, c("recode","delete"))
})

test_that("apply_recodes with match_type = regex matches by pattern", {
  df <- tibble::tibble(
    drink = c("double espresso", "espresso macchiato", "flat white", NA_character_)
  )
  rules <- tibble::tibble(
    rule_id = "r1", variable = "drink", apply_to_siblings = FALSE,
    sibling_pattern = NA_character_, match_type = "regex",
    old_value = "espresso", new_value = "ESPRESSO",   # unanchored substring
    action = "recode", notes = NA_character_, author = "t",
    created_at = "", updated_at = "", source_dataset = "d"
  )
  res <- apply_recodes(df, rules)
  expect_equal(res$df$drink[1], "ESPRESSO")   # "double espresso" matched
  expect_equal(res$df$drink[2], "ESPRESSO")   # "espresso macchiato" matched
  expect_equal(res$df$drink[3], "flat white")  # untouched
  expect_true(is.na(res$df$drink[4]))         # NA untouched
  expect_equal(res$summary$cells_changed, 2L)
})

test_that("regex delete sets matching cells to NA", {
  df <- tibble::tibble(x = c("test123", "keep", "test999"))
  rules <- tibble::tibble(
    rule_id = "r1", variable = "x", apply_to_siblings = FALSE,
    sibling_pattern = NA_character_, match_type = "regex",
    old_value = "^test[0-9]+$", new_value = NA_character_,
    action = "delete", notes = NA_character_, author = "t",
    created_at = "", updated_at = "", source_dataset = "d"
  )
  res <- apply_recodes(df, rules)
  expect_true(is.na(res$df$x[1]))
  expect_equal(res$df$x[2], "keep")
  expect_true(is.na(res$df$x[3]))
})

test_that("validate_recodes flags an invalid regex pattern", {
  rules <- tibble::tibble(
    rule_id = c("r1", "r2"), variable = c("x", "x"),
    apply_to_siblings = c(FALSE, FALSE), sibling_pattern = c(NA_character_, NA_character_),
    match_type = c("regex", "regex"),
    old_value = c("valid[0-9]+", "broken[unclosed"),  # second is invalid
    new_value = c("a", "b"), action = c("recode", "recode"),
    notes = c(NA_character_, NA_character_), author = c("t", "t"),
    created_at = c("", ""), updated_at = c("", ""), source_dataset = c("d", "d")
  )
  issues <- validate_recodes(rules)
  expect_equal(nrow(issues$invalid_regex), 1L)
  expect_equal(issues$invalid_regex$old_value, "broken[unclosed")
})

test_that("generate_recode_R emits str_detect for regex rules", {
  rules <- tibble::tibble(
    rule_id = "r1", variable = "drink", apply_to_siblings = FALSE,
    sibling_pattern = NA_character_, match_type = "regex",
    old_value = "espresso", new_value = "espresso drink",
    action = "recode", notes = NA_character_, author = "t",
    created_at = "", updated_at = "", source_dataset = "d"
  )
  code <- generate_recode_R(rules, "d")
  expect_silent(parse(text = code))
  expect_true(grepl("str_detect", code))
  expect_false(grepl("== \"espresso\"", code))
})

test_that("generate_recode_R sibling rule uses across()", {
  rules <- tibble::tibble(
    rule_id = "r1", variable = "drink1",
    apply_to_siblings = TRUE, sibling_pattern = "^drink[0-9]+$",
    match_type = "trimmed_ci", old_value = "capuccino",
    new_value = "cappuccino", action = "recode",
    notes = NA_character_, author = "t",
    created_at = "", updated_at = "", source_dataset = "d"
  )
  code <- generate_recode_R(rules, "example_messy")
  expect_silent(parse(text = code))
  expect_true(grepl("across", code))
  expect_true(grepl("matches", code))
})


# =============================================================================
# Correctness regressions: single-pass apply, normalized codegen literals,
# NA-targeting rules, and one case_when block per target pattern.
# =============================================================================

mk_rule <- function(variable, match_type, old_value, new_value,
                    action = "recode", siblings = FALSE,
                    pattern = NA_character_, notes = NA_character_) {
  tibble::tibble(
    rule_id           = recode_rule_id(variable, match_type, old_value),
    variable          = variable,
    apply_to_siblings = siblings,
    sibling_pattern   = pattern,
    match_type        = match_type,
    old_value         = old_value,
    new_value         = new_value,
    action            = action,
    notes             = notes,
    author            = "test",
    created_at        = "2026-08-13T00:00:00",
    updated_at        = "2026-08-13T00:00:00",
    source_dataset    = "synthetic"
  )
}

# Synthetic frame. One column per thing under test:
#   race        - exact, exact_ci, regex, and the CASCADE pair
#   drink1/2    - sibling expansion (other must stay untouched)
#   status      - a rule targeting the MISSING cells (`<NA>` sentinel)
#   notes       - free text rewritten by a plain rule
synthetic_df <- function() {
  tibble::tibble(
    id     = paste0("p", 1:6),
    race   = c("asain", "Asian", "wht", "BLK", "Nat Am", NA),
    drink1 = c("cap", "latte", "cap", "mocha", NA, "cap"),
    drink2 = c("cap", NA, "americano", "cap", "latte", NA),
    other  = rep("cap", 6),
    status = c("open", NA, "closed", NA, "open", NA),
    notes  = c("cap with oat milk", "mocha no whip", "latte",
               "cap extra hot", "americano", "unknown")
  )
}

synthetic_rules <- function() {
  dplyr::bind_rows(
    # CASCADE PAIR: rule 1's replacement is rule 2's old_value. Single-pass
    # means "asain" -> "Asian" and the pre-existing "Asian" -> "Asian or PI",
    # and rule 1's output is NOT pushed through rule 2.
    mk_rule("race", "trimmed_ci", "asain", "Asian"),
    mk_rule("race", "trimmed_ci", "Asian", "Asian or PI"),
    mk_rule("race", "exact",      "wht",   "White"),
    mk_rule("race", "exact_ci",   "blk",   "Black"),   # hits "BLK"
    mk_rule("race", "regex",      "^Nat",  "Native"),
    mk_rule("drink1", "trimmed_ci", "cap", "cappuccino",
            siblings = TRUE, pattern = "^drink[0-9]+$"),
    mk_rule("status", "exact", NA_character_, "unrecorded"),
    mk_rule("notes", "trimmed_ci", "cap with oat milk", "oat milk cappuccino")
  )
}

# The eval() here runs code this test just produced from its own in-test rule
# table (no external or user input) — that IS the thing under test.
eval_generated_recodes <- function(df, rules, dataset_id = "synthetic") {
  code <- generate_recode_R(rules, dataset_id = dataset_id)
  env  <- new.env(parent = globalenv())
  env$df <- df
  suppressWarnings(suppressMessages(eval(parse(text = code), envir = env)))
  env$df
}


# --- Defect 1: apply_recodes is single pass, first match wins ----------------

test_that("a rule's replacement is never fed into another rule (no cascade)", {
  # Symptom: with `asain -> Asian` and `Asian -> Asian or PI` in one rule set,
  # every Asian row came out "Asian or PI" and no cell held "Asian".
  df <- tibble::tibble(race = c("asain", "Asian", "White", "asain"))
  rules <- dplyr::bind_rows(
    mk_rule("race", "trimmed_ci", "asain", "Asian"),
    mk_rule("race", "trimmed_ci", "Asian", "Asian or PI"))

  got <- apply_recodes(df, rules)$df$race
  expect_identical(got, c("Asian", "Asian or PI", "White", "Asian"))

  # ...and the answer must not depend on the row order of the rule table.
  rev <- apply_recodes(df, rules[c(2, 1), ])$df$race
  expect_identical(rev, got)
})

test_that("apply_recodes is independent of rule row order", {
  df    <- synthetic_df()
  rules <- synthetic_rules()
  a <- apply_recodes(df, rules)$df
  b <- apply_recodes(df, rules[rev(seq_len(nrow(rules))), ])$df
  expect_identical(as.data.frame(a), as.data.frame(b))
})

test_that("the synthetic rule set produces exactly the expected frame", {
  res <- apply_recodes(synthetic_df(), synthetic_rules())
  expect_identical(res$df$race,
                   c("Asian", "Asian or PI", "White", "Black", "Native", NA))
  expect_identical(res$df$drink1,
                   c("cappuccino", "latte", "cappuccino", "mocha",
                     NA, "cappuccino"))
  expect_identical(res$df$drink2,
                   c("cappuccino", NA, "americano", "cappuccino",
                     "latte", NA))
  expect_identical(res$df$other, rep("cap", 6))     # not a sibling column
  expect_identical(res$df$status,
                   c("open", "unrecorded", "closed", "unrecorded", "open",
                     "unrecorded"))
  expect_identical(res$df$notes[1], "oat milk cappuccino")
})

test_that("apply_recodes summary separates matched / changed / shadowed", {
  df <- tibble::tibble(x = c("White", "blk"))
  res <- apply_recodes(df, mk_rule("x", "exact", "White", "White"))  # no-op
  expect_equal(res$summary$cells_matched, 1L)
  expect_equal(res$summary$cells_changed, 0L)
  expect_equal(res$summary$cells_shadowed, 0L)
  expect_named(res$summary,
               c("rule_id", "cells_changed", "cells_matched", "cells_shadowed"))
})


# --- Defect 4 + the agreement guard -----------------------------------------

test_that("generate_recode_R agrees with apply_recodes cell for cell", {
  df      <- synthetic_df()
  rules   <- synthetic_rules()
  applied <- apply_recodes(df, rules)$df
  genned  <- eval_generated_recodes(df, rules)
  for (col in names(df)) {
    expect_identical(genned[[col]], applied[[col]],
                     info = sprintf("column %s", col))
  }
})

test_that("mixed match_types on one column emit a single case_when block", {
  # Pre-fix, grouping by (effective_pattern, match_type) emitted the `exact`
  # block first and the `trimmed_ci` block second, so the trimmed block
  # re-recoded what the exact block wrote and the exact rule was discarded --
  # and the script disagreed with apply_recodes().
  df <- tibble::tibble(city = c("Uptown", "uptown "))
  rules <- dplyr::bind_rows(
    mk_rule("city", "trimmed_ci", "uptown", "Uptown"),
    mk_rule("city", "exact",      "Uptown", "UPTOWN"))

  code <- generate_recode_R(rules, "d")
  expect_silent(parse(text = code))
  expect_equal(lengths(regmatches(code, gregexpr("case_when", code)))[[1]], 1L)

  applied <- apply_recodes(df, rules)
  expect_identical(applied$df$city, c("Uptown", "Uptown"))
  expect_identical(eval_generated_recodes(df, rules)$city, c("Uptown", "Uptown"))
  # The exact rule matched the original "Uptown" but the trimmed rule claimed it.
  expect_equal(applied$summary$cells_shadowed, c(0L, 1L))
})

test_that("blocks are emitted in first-appearance order of the pattern", {
  rules <- dplyr::bind_rows(
    mk_rule("zeta",  "exact", "a", "A"),
    mk_rule("alpha", "exact", "b", "B"))
  code <- generate_recode_R(rules, "d")
  expect_lt(regexpr("mutate(zeta", code, fixed = TRUE)[[1]],
            regexpr("mutate(alpha", code, fixed = TRUE)[[1]])
})

test_that("a column reached by two patterns gets a WARNING comment", {
  rules <- dplyr::bind_rows(
    mk_rule("drink1", "exact", "a", "A"),
    mk_rule("drink1", "exact", "b", "B",
            siblings = TRUE, pattern = "^drink[0-9]+$"))
  code <- generate_recode_R(rules, "d")
  expect_true(grepl("# WARNING: these columns are targeted by more than one",
                    code, fixed = TRUE))
  expect_true(grepl("#   drink1", code, fixed = TRUE))
})

test_that("a single-pattern rule set gets no overlap warning", {
  expect_false(grepl("# WARNING", generate_recode_R(synthetic_rules(), "d"),
                     fixed = TRUE))
})


# --- Defect 2: the generated script normalizes BOTH sides -------------------

test_that("case-insensitive rules fire in the generated script too", {
  # Pre-fix the script emitted `tolower(x) == "WHT"`, which can never be TRUE,
  # so exact_ci / trimmed_ci rules with any capital letter did nothing in the
  # exported script while working in the app.
  df <- tibble::tibble(x = c("WHT", "wht", " Wht "))
  rules <- dplyr::bind_rows(
    mk_rule("x", "exact_ci",   "WHT",   "White"),
    mk_rule("x", "trimmed_ci", " Wht ", "White"))

  code <- generate_recode_R(rules, "d")
  expect_silent(parse(text = code))
  expect_true(grepl('tolower(x) == "wht"', code, fixed = TRUE))
  expect_true(grepl('str_squish(tolower(x)) == "wht"', code, fixed = TRUE))
  expect_false(grepl('"WHT"', code, fixed = TRUE))
  expect_false(grepl('" Wht "', code, fixed = TRUE))

  expect_identical(eval_generated_recodes(df, rules)$x, rep("White", 3))
  expect_identical(apply_recodes(df, rules)$df$x, rep("White", 3))
})

test_that("exact rules keep the literal verbatim", {
  code <- generate_recode_R(mk_rule("x", "exact", "WHT", "White"), "d")
  expect_true(grepl('x == "WHT"', code, fixed = TRUE))
})


# --- Defect 3: NA-targeting rules -------------------------------------------

test_that(".rule_hits returns a plain logical for an NA old_value", {
  hits <- .rule_hits(c("open", NA, "closed"), NA_character_, "exact")
  expect_identical(hits, c(FALSE, TRUE, FALSE))
  expect_false(anyNA(hits))
})

test_that(".rule_hits matches nothing for an unusable match_type", {
  expect_identical(.rule_hits(c("a", "b"), "a", NA_character_),
                   c(FALSE, FALSE))
  expect_identical(.rule_hits(c("a", "b"), "a", "regx"), c(FALSE, FALSE))
  expect_identical(.rule_hits(character(0), "a", "exact"), logical(0))
})

test_that("a rule targeting NA rewrites the missing cells instead of erroring", {
  df    <- tibble::tibble(status = c("open", NA, "closed", NA))
  rules <- mk_rule("status", "exact", NA_character_, "unrecorded")

  expect_no_error(res <- apply_recodes(df, rules))
  expect_identical(res$df$status,
                   c("open", "unrecorded", "closed", "unrecorded"))
  expect_equal(res$summary$cells_changed, 2L)

  expect_no_error(issues <- validate_recodes(rules, df))
  expect_equal(nrow(issues$stale), 0L)   # it hits the NA cells, so not stale
})

test_that("codegen emits is.na() for an NA-targeting rule", {
  code <- generate_recode_R(
    mk_rule("status", "exact", NA_character_, "unrecorded"), "d")
  expect_silent(parse(text = code))
  expect_true(grepl("is.na(status)", code, fixed = TRUE))
  expect_false(grepl("NA_character_ ~", code, fixed = TRUE))

  df <- tibble::tibble(status = c("open", NA))
  expect_identical(eval_generated_recodes(
    df, mk_rule("status", "exact", NA_character_, "unrecorded"))$status,
    c("open", "unrecorded"))
})

test_that("an NA-targeting rule survives the recodes CSV round trip", {
  tmp <- tempfile(fileext = ".csv"); on.exit(unlink(tmp), add = TRUE)
  rules <- mk_rule("status", "exact", NA_character_, "unrecorded")
  write_recodes(rules, tmp)
  back <- read_recodes(tmp)
  expect_true(is.na(back$old_value))
  expect_identical(back$rule_id, rules$rule_id)
  expect_identical(apply_recodes(tibble::tibble(status = c("open", NA)),
                                 back)$df$status,
                   c("open", "unrecorded"))
})

test_that("an unusable match_type emits a FALSE arm rather than a live one", {
  code <- generate_recode_R(mk_rule("x", "regx", "a", "A"), "d")
  expect_silent(parse(text = code))
  expect_true(grepl("FALSE ~ \"A\"", code, fixed = TRUE))
})

test_that(".fingerprint_key standardizes word order and whitespace/punctuation", {
  expect_equal(.fingerprint_key("Smith, John"), "john smith")
  expect_equal(.fingerprint_key("John   Smith"), "john smith")
  expect_equal(.fingerprint_key("  Oat  Milk, No Sugar! "), "milk no oat sugar")
  expect_equal(.fingerprint_key("No Sugar, Oat Milk"), "milk no oat sugar")
  expect_equal(.fingerprint_key(""), "")
  expect_equal(.fingerprint_key(NA_character_), "")
})

test_that(".ngram_fingerprint_key n-grams the whole string, whitespace stripped", {
  # Whitespace and punctuation are removed BEFORE gramming, so n-grams span
  # what used to be word boundaries. This is what distinguishes the key from
  # .fingerprint_key() instead of duplicating it.
  expect_equal(.ngram_fingerprint_key("new york", n = 2),
               .ngram_fingerprint_key("newyork", n = 2))
  expect_equal(.ngram_fingerprint_key("Wal Mart", n = 2),
               .ngram_fingerprint_key("WalMart", n = 2))
  expect_equal(.ngram_fingerprint_key("e-mail", n = 2),
               .ngram_fingerprint_key("email", n = 2))

  # Grams are deduplicated and sorted: "abab" -> {ab, ba, ab} -> "ab" + "ba".
  expect_equal(.ngram_fingerprint_key("abab", n = 2), "abba")

  # A string shorter than n falls back to the cleaned string itself.
  expect_equal(.ngram_fingerprint_key("a", n = 2), "a")

  # At n = 1 the key is the distinct-character set, so it is invariant to
  # transposition and duplication (OpenRefine's Krzysztof example).
  expect_equal(.ngram_fingerprint_key("Krzysztof", n = 1),
               .ngram_fingerprint_key("Kryzysztof", n = 1))

  expect_equal(.ngram_fingerprint_key(""), "")
  expect_equal(.ngram_fingerprint_key(NA_character_), "")
  expect_equal(.ngram_fingerprint_key("!!!"), "")
})

test_that("cluster_strings with fingerprint algorithm groups word order swaps", {
  values <- c("Oat Milk No Sugar", "no sugar, oat milk", "Regular Milk", "Latte")
  out <- cluster_strings(values, algorithm = "fingerprint")
  expect_equal(nrow(out), 4)
  c1 <- out$cluster_id[out$value == "Oat Milk No Sugar"]
  c2 <- out$cluster_id[out$value == "no sugar, oat milk"]
  c3 <- out$cluster_id[out$value == "Regular Milk"]
  expect_equal(c1, c2)
  expect_false(c1 == c3)
})

test_that("cluster_strings with ngram_fingerprint groups compound-word variants", {
  # The case key collision CANNOT reach: the token sets differ, so only a
  # whole-string n-gram key collapses these.
  values <- c("Wal Mart", "WalMart", "espresso")
  out <- cluster_strings(values, algorithm = "ngram_fingerprint", q = 2)
  expect_equal(nrow(out), 3)
  c1 <- out$cluster_id[out$value == "Wal Mart"]
  c2 <- out$cluster_id[out$value == "WalMart"]
  c3 <- out$cluster_id[out$value == "espresso"]
  expect_equal(c1, c2)
  expect_false(c1 == c3)
})

test_that("cluster_strings with ngram_fingerprint groups transpositions at q = 1", {
  values <- c("Krzysztof", "Kryzysztof", "espresso")
  out <- cluster_strings(values, algorithm = "ngram_fingerprint", q = 1)
  c1 <- out$cluster_id[out$value == "Krzysztof"]
  c2 <- out$cluster_id[out$value == "Kryzysztof"]
  c3 <- out$cluster_id[out$value == "espresso"]
  expect_equal(c1, c2)
  expect_false(c1 == c3)
})

test_that("ngram_fingerprint and fingerprint cover complementary cases", {
  # Documents the deliberate trade, so a future "simplification" back to
  # per-token n-grams (which would make this algorithm a fuzzier restatement
  # of key collision) fails loudly here.
  swap <- c("oat milk", "milk oat")

  fp <- cluster_strings(swap, algorithm = "fingerprint")
  expect_equal(fp$cluster_id[fp$value == "oat milk"],
               fp$cluster_id[fp$value == "milk oat"])

  ng <- cluster_strings(swap, algorithm = "ngram_fingerprint", q = 2)
  expect_false(ng$cluster_id[ng$value == "oat milk"] ==
               ng$cluster_id[ng$value == "milk oat"])
})

test_that("match_taxonomy matches exact and fuzzy target standards", {
  values <- c("FW", "Capuccino", "Americanno", "Unknown Drink")
  targets <- c("Cappuccino", "Americano", "Flat White", "Macchiato")

  res <- match_taxonomy(values, targets, method = "jw", threshold = 0.70)
  expect_equal(nrow(res), 4)
  expect_named(res, c("value", "n", "matched_target", "similarity", "is_matched", "status"))

  # "Capuccino" should match "Cappuccino"
  cap_row <- res[res$value == "Capuccino", ]
  expect_true(cap_row$is_matched)
  expect_equal(cap_row$matched_target, "Cappuccino")
  expect_gte(cap_row$similarity, 0.85)

  # "Americanno" should match "Americano"
  ame_row <- res[res$value == "Americanno", ]
  expect_true(ame_row$is_matched)
  expect_equal(ame_row$matched_target, "Americano")

  # "Unknown Drink" should be below threshold
  unk_row <- res[res$value == "Unknown Drink", ]
  expect_false(unk_row$is_matched)
  expect_true(is.na(unk_row$matched_target))
})

test_that("match_taxonomy handles empty inputs and empty targets gracefully", {
  expect_equal(nrow(match_taxonomy(character(0), c("A", "B"))), 0)

  res_notarget <- match_taxonomy(c("a", "b"), character(0))
  expect_equal(nrow(res_notarget), 2)
  expect_false(any(res_notarget$is_matched))
  expect_equal(res_notarget$status, c("no_taxonomy_targets", "no_taxonomy_targets"))
})


# --- Schema-conforming recodes CSV read / write ------------------------------

# Write raw CSV lines to a temp file and return its path.
write_csv_lines <- function(lines) {
  tmp <- tempfile(fileext = ".csv")
  writeLines(lines, tmp)
  tmp
}

schema_cols <- function() names(empty_recodes_tibble())

test_that("a recodes CSV missing sibling columns reads to the full schema", {
  tmp <- write_csv_lines(c(
    "rule_id,variable,match_type,old_value,new_value,action,notes,author,created_at,updated_at,source_dataset",
    "r1,drink1,trimmed_ci,capuccino,cappuccino,recode,typo,t,2026-01-01,2026-01-01,cafe",
    "r2,city,exact,<NA>,unknown,recode,<NA>,t,2026-01-01,2026-01-01,cafe"
  ))
  on.exit(unlink(tmp), add = TRUE)

  expect_no_warning(rules <- read_recodes(tmp))
  expect_named(rules, schema_cols())
  expect_type(rules$apply_to_siblings, "logical")
  expect_true(all(is.na(rules$apply_to_siblings)))
  expect_true(all(is.na(rules$sibling_pattern)))
  expect_true(is.na(rules$old_value[2]))

  df <- tibble::tibble(drink1 = c("Capuccino ", "latte"), city = c("Uptown", NA))
  expect_no_warning(code <- generate_recode_R(rules, "cafe"))
  expect_silent(parse(text = code))
  expect_no_warning(res <- apply_recodes(df, rules))
  expect_identical(res$df$drink1, c("cappuccino", "latte"))
  expect_identical(res$df$city, c("Uptown", "unknown"))
})

test_that("read_recodes puts schema columns in schema order", {
  tmp <- write_csv_lines(c(
    "source_dataset,new_value,old_value,variable,rule_id,match_type,action",
    "cafe,latte,late,drink1,r1,exact,recode"
  ))
  on.exit(unlink(tmp), add = TRUE)
  rules <- read_recodes(tmp)
  expect_named(rules, schema_cols())
  expect_identical(rules$old_value, "late")
  expect_identical(rules$source_dataset, "cafe")
})

test_that("read_recodes coerces apply_to_siblings text to logical", {
  tmp <- write_csv_lines(c(
    "rule_id,variable,apply_to_siblings",
    "a,drink1,TRUE", "b,drink1,true", "c,drink1,T", "d,drink1,1",
    "e,drink1,FALSE", "f,drink1,false", "g,drink1,0", "h,drink1,",
    "i,drink1,maybe"
  ))
  on.exit(unlink(tmp), add = TRUE)
  rules <- read_recodes(tmp)
  expect_identical(rules$apply_to_siblings,
                   c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA, NA))
})

test_that("read_recodes keeps unknown extra columns after the schema", {
  tmp <- write_csv_lines(c(
    "reviewer,rule_id,variable,match_type,old_value,new_value,action,ticket",
    "sam,r1,drink1,exact,late,latte,recode,T-12"
  ))
  on.exit(unlink(tmp), add = TRUE)
  rules <- read_recodes(tmp)
  expect_named(rules, c(schema_cols(), "reviewer", "ticket"))
  expect_identical(rules$reviewer, "sam")
  expect_identical(rules$ticket, "T-12")

  # ...but write_recodes writes exactly the schema.
  out <- tempfile(fileext = ".csv"); on.exit(unlink(out), add = TRUE)
  write_recodes(rules, out)
  expect_named(read_recodes(out), schema_cols())
  expect_identical(
    strsplit(readLines(out, n = 1), ",", fixed = TRUE)[[1]], schema_cols())
})

test_that("write_recodes conforms a partial rule set before writing", {
  partial <- tibble::tibble(
    variable = "drink1", rule_id = "r1", match_type = "exact",
    old_value = "late", new_value = "latte", action = "recode",
    apply_to_siblings = "1"
  )
  out <- tempfile(fileext = ".csv"); on.exit(unlink(out), add = TRUE)
  write_recodes(partial, out)
  rt <- read_recodes(out)
  expect_named(rt, schema_cols())
  expect_identical(rt$apply_to_siblings, TRUE)
  expect_identical(rt$new_value, "latte")
})

test_that("read_recodes(conform = FALSE) is the unconformed raw read", {
  # Full-schema file: typed columns, same as a direct read with the schema spec.
  rules <- mk_rule("drink1", "trimmed_ci", "capuccino", NA_character_,
                   siblings = TRUE, pattern = "^drink[0-9]+$")
  full <- tempfile(fileext = ".csv"); on.exit(unlink(full), add = TRUE)
  write_recodes(rules, full)
  raw <- read_recodes(full, conform = FALSE)
  expected <- readr::read_csv(
    full, na = "<NA>", show_col_types = FALSE,
    col_types = readr::cols(
      rule_id = readr::col_character(), variable = readr::col_character(),
      apply_to_siblings = readr::col_logical(),
      sibling_pattern = readr::col_character(),
      match_type = readr::col_character(), old_value = readr::col_character(),
      new_value = readr::col_character(), action = readr::col_character(),
      notes = readr::col_character(), author = readr::col_character(),
      created_at = readr::col_character(), updated_at = readr::col_character(),
      source_dataset = readr::col_character()))
  # Compare columns; readr attaches spec/problems attributes to each read.
  expect_identical(lapply(raw, identity), lapply(expected, identity))

  # Missing-column file: columns stay missing, readr still warns, no reorder.
  partial <- write_csv_lines(c(
    "variable,rule_id,match_type,old_value,new_value,action,reviewer",
    "drink1,r1,exact,late,latte,recode,sam"
  ))
  on.exit(unlink(partial), add = TRUE)
  expect_warning(raw2 <- read_recodes(partial, conform = FALSE))
  expect_named(raw2, c("variable", "rule_id", "match_type", "old_value",
                       "new_value", "action", "reviewer"))

  # The import module's path: raw read, then conform.
  expect_named(.conform_recodes_schema(raw2), c(schema_cols(), "reviewer"))
})

test_that("the <NA> round trip is the same with and without conform", {
  rules <- dplyr::bind_rows(
    mk_rule("drink1", "trimmed_ci", "capuccino", "cappuccino"),
    mk_rule("city", "exact", NA_character_, "unknown"),
    mk_rule("notes", "exact", "n/a", NA_character_, action = "delete"),
    mk_rule("review", "exact", "meh", "")
  )
  tmp <- tempfile(fileext = ".csv"); on.exit(unlink(tmp), add = TRUE)
  write_recodes(rules, tmp)
  conformed <- read_recodes(tmp)
  raw       <- read_recodes(tmp, conform = FALSE)
  expect_identical(lapply(conformed, identity), lapply(raw, identity))
  expect_true(is.na(conformed$old_value[2]))
  expect_true(is.na(conformed$new_value[3]))
  expect_true(is.na(conformed$notes[1]))
  expect_equal(conformed$apply_to_siblings, rules$apply_to_siblings)
})

test_that("read_recodes on a missing path returns the empty schema either way", {
  nope <- file.path(tempdir(), "definitely-not-here-recodes.csv")
  expect_identical(read_recodes(nope), empty_recodes_tibble())
  expect_identical(read_recodes(nope, conform = FALSE), empty_recodes_tibble())
})


# --- merge_recode_rules ------------------------------------------------------

test_that("merge_recode_rules replaces a colliding rule in place", {
  base <- dplyr::bind_rows(
    mk_rule("drink1", "exact", "late", "latte"),
    mk_rule("drink1", "exact", "expresso", "espresso"),
    mk_rule("city", "exact", "uptown", "Uptown")
  )
  edited <- base[2, ]
  edited$notes <- "common misspelling"
  out <- merge_recode_rules(base, edited)
  expect_identical(out$rule_id, base$rule_id)
  expect_identical(out$notes, c(NA, "common misspelling", NA))
})

test_that("merge_recode_rules keeps precedence where drop-and-append would not", {
  df <- tibble::tibble(drink1 = c("latte", "oat latte", "mocha"))
  # Both rules match "latte"; the first one listed claims it.
  base <- dplyr::bind_rows(
    mk_rule("drink1", "trimmed_ci", "latte", "Latte"),
    mk_rule("drink1", "regex", "latte", "Latte (other)")
  )
  expect_identical(apply_recodes(df, base)$df$drink1,
                   c("Latte", "Latte (other)", "mocha"))

  edited <- base[1, ]
  edited$notes <- "house style"

  merged <- merge_recode_rules(base, edited)
  expect_identical(apply_recodes(df, merged)$df$drink1,
                   c("Latte", "Latte (other)", "mocha"))

  # The old drop-then-append behaviour moves the edited rule last, and a
  # notes-only edit changes the output.
  dropped <- dplyr::bind_rows(base[base$rule_id != edited$rule_id, ], edited)
  expect_identical(apply_recodes(df, dropped)$df$drink1,
                   c("Latte (other)", "Latte (other)", "mocha"))
})

test_that("merge_recode_rules appends new rule ids in overlay order", {
  base <- mk_rule("drink1", "exact", "late", "latte")
  overlay <- dplyr::bind_rows(
    mk_rule("city", "exact", "uptown", "Uptown"),
    base,
    mk_rule("drink2", "exact", "moca", "mocha")
  )
  out <- merge_recode_rules(base, overlay)
  expect_identical(out$rule_id,
                   c(base$rule_id, overlay$rule_id[1], overlay$rule_id[3]))
})

test_that("merge_recode_rules handles empty and NULL inputs", {
  r <- mk_rule("drink1", "exact", "late", "latte")
  expect_identical(merge_recode_rules(r, empty_recodes_tibble()), r)
  expect_identical(merge_recode_rules(empty_recodes_tibble(), r), r)
  expect_identical(merge_recode_rules(NULL, r), r)
  expect_identical(merge_recode_rules(r, NULL), r)
  expect_identical(merge_recode_rules(NULL, NULL), empty_recodes_tibble())
})


# --- Coverage: match_type truth table, rare flag, stale, regex normalize -----

test_that(".rule_hits truth table across every match_type and an NA target", {
  x <- c("Yes", "yes", " yes ", "yesterday", NA)
  hit <- function(mt, old) .rule_hits(x, old, mt)

  expect_identical(hit("exact",      "yes"),  c(FALSE, TRUE,  FALSE, FALSE, FALSE))
  expect_identical(hit("exact",      "YES"),  c(FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(hit("exact_ci",   "YES"),  c(TRUE,  TRUE,  FALSE, FALSE, FALSE))
  expect_identical(hit("trimmed_ci", "yes"),  c(TRUE,  TRUE,  TRUE,  FALSE, FALSE))
  # regex is unanchored and case-sensitive, and never matches a missing cell.
  expect_identical(hit("regex",      "^yes"), c(FALSE, TRUE,  FALSE, TRUE,  FALSE))
  expect_identical(hit("regex",      "yes"),  c(FALSE, TRUE,  TRUE,  TRUE,  FALSE))
  # An NA old_value targets the missing cells whatever the match_type.
  for (mt in RECODE_MATCH_TYPES) {
    expect_identical(hit(mt, NA_character_), c(FALSE, FALSE, FALSE, FALSE, TRUE))
  }
})

test_that("cluster_strings flags a low-share cluster member as rare, not singletons", {
  out <- cluster_strings(c("cappuccino", "capuccino", "espresso"),
                         frequencies = c(99L, 1L, 1L),
                         threshold = 0.85, algorithm = "jw")
  cap <- out$cluster_id[out$value == "cappuccino"]
  expect_equal(out$cluster_id[out$value == "capuccino"], cap)
  expect_false(out$cluster_id[out$value == "espresso"] == cap)

  expect_true(out$is_rare[out$value == "capuccino"])    # 1 of 100 in cluster
  expect_false(out$is_rare[out$value == "cappuccino"])  # the dominant spelling
  expect_false(out$is_rare[out$value == "espresso"])    # singleton, never rare
})

test_that("validate_recodes flags a rule matching nothing as stale", {
  df <- tibble::tibble(drink1 = c("latte", "mocha"))
  rules <- dplyr::bind_rows(
    mk_rule("drink1", "exact", "latte", "Latte"),       # matches
    mk_rule("drink1", "exact", "frappe", "Frappe"),     # value not present
    mk_rule("drink9", "exact", "latte", "Latte")        # column not present
  )
  issues <- validate_recodes(rules, df)
  expect_identical(issues$stale$rule_id, rules$rule_id[2:3])
})

test_that("normalize_value leaves values untouched for regex", {
  x <- c(" Oat  Latte ", "^Mocha$", NA)
  expect_identical(normalize_value(x, "regex"), x)
})

