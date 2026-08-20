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
  values <- c("asphyxiation", "ascphyxiation", "ashphxiation",
              "asphyziation", "gunshot wound")
  out <- cluster_strings(values, threshold = 0.85, algorithm = "jw")
  expect_equal(nrow(out), 5)
  variants <- c("asphyxiation", "ascphyxiation", "ashphxiation", "asphyziation")
  expect_length(unique(out$cluster_id[out$value %in% variants]), 1L)
  asph <- out$cluster_id[out$value == "asphyxiation"]
  gun  <- out$cluster_id[out$value == "gunshot wound"]
  expect_false(asph == gun)
})

test_that("cluster_strings handles empty input", {
  out <- cluster_strings(character(0))
  expect_equal(nrow(out), 0)
  expect_named(out, c("value", "n", "cluster_id", "is_rare"))
})

test_that("normalize_for_cluster applies the selected steps", {
  expect_equal(normalize_for_cluster("Foo, BAR", c("lower","punct","squish")),
               "foo bar")
  expect_equal(normalize_for_cluster("asphyxia asphyxia", "dedupe_tokens"),
               "asphyxia")
  expect_equal(normalize_for_cluster("head and torso", "sort_tokens"),
               "and head torso")
  expect_equal(normalize_for_cluster("Foo Bar", character(0)), "Foo Bar")
})

test_that("cosine metric clusters reordered multi-word values", {
  values <- c("head and torso injuries", "injuries of torso and head",
              "gunshot wound")
  out <- cluster_strings(values, algorithm = "cosine", q = 2, threshold = 0.6,
                         normalize = c("lower", "punct", "squish"))
  reordered <- c("head and torso injuries", "injuries of torso and head")
  expect_length(unique(out$cluster_id[out$value %in% reordered]), 1L)
  expect_false(out$cluster_id[out$value == "gunshot wound"] ==
               out$cluster_id[out$value == "head and torso injuries"])
})

test_that("sort_tokens normalization groups order-permuted values exactly", {
  values <- c("a b c", "c b a", "x y z")
  out <- cluster_strings(values, algorithm = "lv", threshold = 0.99,
                         normalize = "sort_tokens")
  expect_equal(out$cluster_id[out$value == "a b c"],
               out$cluster_id[out$value == "c b a"])
})

test_that("metaphone clusters phonetically-equal medical terms", {
  testthat::skip_if_not_installed("phonics")
  values <- c("asphyxiation", "asfixiation", "gunshot")
  out <- cluster_strings(values, algorithm = "metaphone")
  expect_equal(out$cluster_id[out$value == "asphyxiation"],
               out$cluster_id[out$value == "asfixiation"])
  expect_false(out$cluster_id[out$value == "gunshot"] ==
               out$cluster_id[out$value == "asphyxiation"])
})

test_that("CLUSTER_ALGORITHMS / NORMALIZERS constants are exported", {
  expect_true(all(c("jw","osa","lv","lcs","cosine","jaccard","soundex",
                    "metaphone") %in% CLUSTER_ALGORITHMS))
  expect_true(all(c("lower","punct","squish","dedupe_tokens","sort_tokens")
                  %in% CLUSTER_NORMALIZERS))
})

test_that("write_recodes -> read_recodes preserves NA encoding", {
  rules <- tibble::tibble(
    rule_id = c("a", "b"), variable = c("cause1", "city"),
    apply_to_siblings = c(TRUE, FALSE),
    sibling_pattern = c("^cause[0-9]+$", NA_character_),
    match_type = c("trimmed_ci", "trimmed_ci"),
    old_value = c("ascphyxiation", "linclon"),
    new_value = c("asphyxiation", NA_character_),
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
  a <- recode_rule_id("cause1", "trimmed_ci", "ascphyxiation")
  b <- recode_rule_id("cause1", "trimmed_ci", "ascphyxiation")
  c <- recode_rule_id("cause1", "trimmed_ci", "asphyxiation")
  expect_equal(a, b)
  expect_false(a == c)
  expect_equal(nchar(a), 12L)
})

test_that("validate_recodes flags duplicate key, rule chain, blank new_value", {
  rules <- tibble::tibble(
    rule_id = c("r1","r2","r3","r4","r5"),
    variable = rep("cause1", 5),
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
    cause1 = c("ascphyxiation", "x"),
    cause2 = c("ascphyxiation", NA_character_),
    other  = c("ascphyxiation", "z")
  )
  rules <- tibble::tibble(
    rule_id = "r1", variable = "cause1",
    apply_to_siblings = TRUE, sibling_pattern = "^cause[0-9]+$",
    match_type = "trimmed_ci", old_value = "ascphyxiation",
    new_value = "asphyxiation", action = "recode",
    notes = NA_character_, author = "t",
    created_at = "", updated_at = "", source_dataset = "d"
  )
  res <- apply_recodes(df, rules)
  expect_equal(res$df$cause1[1], "asphyxiation")
  expect_equal(res$df$cause2[1], "asphyxiation")
  expect_equal(res$df$other[1], "ascphyxiation")  # untouched
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
    rule_id = c("r1","r2"), variable = c("cause1","cause1"),
    apply_to_siblings = c(FALSE, FALSE), sibling_pattern = c(NA_character_, NA_character_),
    match_type = c("trimmed_ci","trimmed_ci"),
    old_value = c("ascphyxiation","gone"),
    new_value = c("asphyxiation", NA_character_),
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
    cause = c("acute asphyxia", "asphyxiation", "gunshot wound", NA_character_)
  )
  rules <- tibble::tibble(
    rule_id = "r1", variable = "cause", apply_to_siblings = FALSE,
    sibling_pattern = NA_character_, match_type = "regex",
    old_value = "asphyx", new_value = "ASPHYXIA",   # unanchored substring
    action = "recode", notes = NA_character_, author = "t",
    created_at = "", updated_at = "", source_dataset = "d"
  )
  res <- apply_recodes(df, rules)
  expect_equal(res$df$cause[1], "ASPHYXIA")   # "acute asphyxia" matched
  expect_equal(res$df$cause[2], "ASPHYXIA")   # "asphyxiation" matched
  expect_equal(res$df$cause[3], "gunshot wound")  # untouched
  expect_true(is.na(res$df$cause[4]))         # NA untouched
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
    rule_id = "r1", variable = "cause", apply_to_siblings = FALSE,
    sibling_pattern = NA_character_, match_type = "regex",
    old_value = "asphyx", new_value = "asphyxiation",
    action = "recode", notes = NA_character_, author = "t",
    created_at = "", updated_at = "", source_dataset = "d"
  )
  code <- generate_recode_R(rules, "d")
  expect_silent(parse(text = code))
  expect_true(grepl("str_detect", code))
  expect_false(grepl("== \"asphyx\"", code))
})

test_that("generate_recode_R sibling rule uses across()", {
  rules <- tibble::tibble(
    rule_id = "r1", variable = "cause1",
    apply_to_siblings = TRUE, sibling_pattern = "^cause[0-9]+$",
    match_type = "trimmed_ci", old_value = "ascphyxiation",
    new_value = "asphyxiation", action = "recode",
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
#   cause1/2    - sibling expansion (other must stay untouched)
#   status      - a rule targeting the MISSING cells (`<NA>` sentinel)
#   notes       - free text rewritten by a plain rule
synthetic_df <- function() {
  tibble::tibble(
    id     = paste0("p", 1:6),
    race   = c("asain", "Asian", "wht", "BLK", "Nat Am", NA),
    cause1 = c("gsw", "hanging", "gsw", "overdose", NA, "gsw"),
    cause2 = c("gsw", NA, "drowning", "gsw", "hanging", NA),
    other  = rep("gsw", 6),
    status = c("open", NA, "closed", NA, "open", NA),
    notes  = c("self inflicted gsw", "overdose of pills", "hanging",
               "gsw to the head", "drowning", "unknown")
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
    mk_rule("cause1", "trimmed_ci", "gsw", "gunshot wound",
            siblings = TRUE, pattern = "^cause[0-9]+$"),
    mk_rule("status", "exact", NA_character_, "unrecorded"),
    mk_rule("notes", "trimmed_ci", "self inflicted gsw", "firearm injury")
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
  expect_identical(res$df$cause1,
                   c("gunshot wound", "hanging", "gunshot wound", "overdose",
                     NA, "gunshot wound"))
  expect_identical(res$df$cause2,
                   c("gunshot wound", NA, "drowning", "gunshot wound",
                     "hanging", NA))
  expect_identical(res$df$other, rep("gsw", 6))     # not a sibling column
  expect_identical(res$df$status,
                   c("open", "unrecorded", "closed", "unrecorded", "open",
                     "unrecorded"))
  expect_identical(res$df$notes[1], "firearm injury")
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
  df <- tibble::tibble(city = c("Omaha", "omaha "))
  rules <- dplyr::bind_rows(
    mk_rule("city", "trimmed_ci", "omaha", "Omaha"),
    mk_rule("city", "exact",      "Omaha", "OMAHA"))

  code <- generate_recode_R(rules, "d")
  expect_silent(parse(text = code))
  expect_equal(lengths(regmatches(code, gregexpr("case_when", code)))[[1]], 1L)

  applied <- apply_recodes(df, rules)
  expect_identical(applied$df$city, c("Omaha", "Omaha"))
  expect_identical(eval_generated_recodes(df, rules)$city, c("Omaha", "Omaha"))
  # The exact rule matched the original "Omaha" but the trimmed rule claimed it.
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
    mk_rule("cause1", "exact", "a", "A"),
    mk_rule("cause1", "exact", "b", "B",
            siblings = TRUE, pattern = "^cause[0-9]+$"))
  code <- generate_recode_R(rules, "d")
  expect_true(grepl("# WARNING: these columns are targeted by more than one",
                    code, fixed = TRUE))
  expect_true(grepl("#   cause1", code, fixed = TRUE))
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
