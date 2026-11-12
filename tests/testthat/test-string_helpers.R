# Run with: testthat::test_dir("tests/testthat")

source(file.path("..", "..", "R", "string_helpers.R"))

test_that("empty_recodes_tibble has expected schema", {
  x <- empty_recodes_tibble()
  expect_named(x, c(
    "rule_id", "variable", "apply_to_siblings", "sibling_pattern",
    "match_type", "old_value", "new_value", "action", "notes",
    "author", "created_at", "updated_at", "source_dataset", "norm_spec"
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

test_that("generate_recode_R emits grepl() for regex rules (the app's engine)", {
  rules <- tibble::tibble(
    rule_id = "r1", variable = "drink", apply_to_siblings = FALSE,
    sibling_pattern = NA_character_, match_type = "regex",
    old_value = "espresso", new_value = "espresso drink",
    action = "recode", notes = NA_character_, author = "t",
    created_at = "", updated_at = "", source_dataset = "d"
  )
  code <- generate_recode_R(rules, "d")
  expect_silent(parse(text = code))
  expect_true(grepl("grepl(\"espresso\", drink)", code, fixed = TRUE))
  expect_false(grepl("str_detect", code, fixed = TRUE))
  expect_false(grepl("== \"espresso\"", code))
  # A pattern the app's engine rejects is an arm that matches nothing.
  rules$old_value <- "\\p{Lu}"
  code <- generate_recode_R(rules, "d")
  expect_true(grepl("rep_len(FALSE, length(drink)) ~", code, fixed = TRUE))
  expect_equal(nrow(validate_recodes(rules)$invalid_regex), 1L)
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
# table (no external or user input); that IS the thing under test.
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

# Column names that are not plain identifiers. A plain rule must reach its
# named column and nothing else in BOTH engines:
#   q1.a / q1_a  - the `.` used to be an unescaped wildcard in the script, so a
#                  rule on q1.a also rewrote q1_a
#   price ($)    - `(`, `)` and `$` used to make the script's pattern wrong
#   my col       - a space; also carries empty and whitespace-only cells
tricky_df <- function(df = synthetic_df()) {
  df$`q1.a`      <- c("yes", "no", "yes", "YES", NA, "yes")
  df$q1_a        <- c("yes", "yes", "no", "yes", "yes", NA)
  df$`price ($)` <- c("free", "2", "free ", "", "FREE", NA)
  df$`my col`    <- c("hot", "iced", "", "  ", "hot", NA)
  df
}

tricky_rules <- function() {
  dplyr::bind_rows(
    mk_rule("q1.a",      "exact",      "yes", "Y"),
    mk_rule("price ($)", "trimmed_ci", "free", "0"),
    mk_rule("my col",    "exact",      "hot", "Hot"),
    # Empty old_value: trimmed_ci also claims the whitespace-only cell.
    mk_rule("my col",    "trimmed_ci", "",    "unknown")
  )
}

# Rules the two regex engines (base TRE in the app, ICU in stringr) disagree
# on, plus unusable sibling patterns. The script used to emit str_detect()
# (ICU): `\p{Lu}` (TRE rejects it) then matched in the script only, and
# `a{,2}` (ICU rejects it) crashed the script. Sibling rules with a pattern
# that does not compile used to throw everywhere; a blank pattern reached
# every column in the app and crashed the script's matches("").
engine_df <- function(df = tricky_df()) {
  df$code  <- c("Abc", "abc", "ABC", "x1", NA, "a")
  df$code2 <- c("b", "a", NA, "aa", "", "c")
  df$fam1  <- c("v", "w", "v", NA, "v", "w")
  df$fam2  <- c("v", "v", "w", "w", NA, "v")
  df
}

engine_rules <- function() {
  dplyr::bind_rows(
    mk_rule("code",  "regex", "\\p{Lu}",       "ICU-only"),  # TRE: invalid
    mk_rule("code",  "regex", "[a",            "broken"),    # invalid in both
    mk_rule("code",  "regex", "^[[:upper:]]",  "UP"),
    mk_rule("code2", "regex", "a{,2}",         "TRE-only"),  # ICU: invalid
    mk_rule("fam1",  "exact", "v", "V", siblings = TRUE, pattern = "fam["),
    mk_rule("fam1",  "exact", "w", "W", siblings = TRUE, pattern = "  ")
  )
}

test_that("generate_recode_R agrees with apply_recodes cell for cell", {
  df      <- engine_df(tricky_df())
  rules   <- dplyr::bind_rows(synthetic_rules(), tricky_rules(),
                              engine_rules())
  applied <- apply_recodes(df, rules)$df
  genned  <- eval_generated_recodes(df, rules)
  expect_identical(names(genned), names(applied))
  for (col in names(df)) {
    expect_identical(genned[[col]], applied[[col]],
                     info = sprintf("column %s", col))
  }
  # The tricky columns came out as intended, not merely equally wrong.
  expect_identical(applied$`q1.a`, c("Y", "no", "Y", "YES", NA, "Y"))
  expect_identical(applied$q1_a, df$q1_a)                 # untouched
  expect_identical(applied$`price ($)`, c("0", "2", "0", "", "0", NA))
  expect_identical(applied$`my col`,
                   c("Hot", "iced", "unknown", "unknown", "Hot", NA))
  # One regex engine on both sides; unusable patterns match nothing.
  expect_identical(applied$code, c("UP", "abc", "UP", "x1", NA, "a"))
  expect_identical(applied$code2,
                   c("TRE-only", "TRE-only", NA, "TRE-only", "TRE-only",
                     "TRE-only"))
  # The invalid sibling rule is skipped; the blank-pattern rule is plain.
  expect_identical(applied$fam1, c("v", "W", "v", NA, "v", "W"))
  expect_identical(applied$fam2, df$fam2)
  # Plain rules are single-column mutate()s on a backticked name, never a
  # matches() pattern.
  code <- generate_recode_R(rules, "synthetic")
  expect_true(grepl("mutate(`q1.a` = case_when(", code, fixed = TRUE) ||
              grepl("mutate(q1.a = case_when(", code, fixed = TRUE))
  expect_true(grepl("mutate(`price ($)` = case_when(", code, fixed = TRUE))
  expect_true(grepl("mutate(`my col` = case_when(", code, fixed = TRUE))
  expect_false(grepl("matches(\"^q1", code, fixed = TRUE))
  expect_false(grepl("matches(\"^price", code, fixed = TRUE))
  expect_false(grepl("# WARNING", code, fixed = TRUE))
})

test_that("a plain rule's pattern is its escaped column name", {
  # Before the fix: `^q1.a$` grouped and warned as if it reached q1_a too.
  rules <- dplyr::bind_rows(
    mk_rule("q1.a", "exact", "yes", "Y"),
    mk_rule("q1_a", "exact", "yes", "Y"))
  code <- generate_recode_R(rules, "d")
  expect_false(grepl("# WARNING", code, fixed = TRUE))
  expect_equal(lengths(regmatches(code, gregexpr("case_when", code)))[[1]], 2L)

  # Every metacharacter a name can carry, one rule each: the script parses,
  # runs, and each rule reaches only its own column.
  nms <- c("a.b", "a_b", "w{2}", "x[1]", "a|b", "c^d", "e+f", "g*h", "i?j",
           "back\\slash", "tick`name", "if")
  df  <- tibble::as_tibble(stats::setNames(
    lapply(nms, function(n) c("v", "w")), nms), .name_repair = "minimal")
  rules <- dplyr::bind_rows(lapply(nms[nms != "a_b"], function(n)
    mk_rule(n, "exact", "v", paste0("V:", n))))
  code <- generate_recode_R(rules, "d")
  expect_silent(parse(text = code))
  applied <- apply_recodes(df, rules)$df
  genned  <- eval_generated_recodes(df, rules)
  for (n in nms) {
    expect_identical(genned[[n]], applied[[n]], info = n)
  }
  expect_identical(applied$a_b, c("v", "w"))
  expect_identical(applied$`w{2}`, c("V:w{2}", "w"))
})

test_that("a sibling rule plus a plain rule on a family member still warns", {
  rules <- dplyr::bind_rows(
    mk_rule("q1.a", "exact", "a", "A"),
    mk_rule("q1.a", "exact", "b", "B", siblings = TRUE,
            pattern = "^q1[._]a$"))
  code <- generate_recode_R(rules, "d")
  expect_true(grepl("#   q1.a", code, fixed = TRUE))
})

# The same guard with column normalizations in front of the recodes. The app
# runs apply_normalizations() then apply_recodes(); the script runs the
# normalization blocks then the recode blocks. They must agree cell for cell,
# including a sibling-family normalization and a non-syntactic column name
# (which the script has to backtick).
mk_norm_rule <- function(variable, ops, siblings = FALSE,
                         pattern = NA_character_) {
  tibble::tibble(
    norm_id           = normalization_rule_id(variable,
                                              if (siblings) pattern
                                              else NA_character_),
    variable          = variable,
    apply_to_siblings = siblings,
    sibling_pattern   = if (siblings) pattern else NA_character_,
    ops               = normalize_ops_join(ops),
    notes             = NA_character_,
    author            = "test",
    created_at        = "2026-08-13T00:00:00",
    updated_at        = "2026-08-13T00:00:00",
    source_dataset    = "synthetic"
  )
}

synthetic_norm_df <- function() {
  df <- synthetic_df()
  df$drink1[2] <- "  LATTE "
  df$drink2[1] <- "Cap"
  df$`drink notes` <- c("  extra   hot ", "OAT milk", NA, "", "no whip", "x")
  # A column the family pattern must NOT reach: capitalized differently, so a
  # case-insensitive matches() in the script would wrongly include it.
  df$Drink3 <- c("CAP", "cap", NA, "cap", "Cap", "cap")
  df
}

synthetic_norms <- function() {
  dplyr::bind_rows(
    mk_norm_rule("drink1", c("squish", "lowercase"), siblings = TRUE,
                 pattern = "^drink[0-9]+$"),
    mk_norm_rule("drink notes", c("titlecase", "squish")),
    mk_norm_rule("status", "uppercase")
  )
}

synthetic_norm_rules <- function() {
  dplyr::bind_rows(
    synthetic_rules(),
    # Written against the NORMALIZED values.
    mk_rule("drink notes", "exact", "Oat Milk", "oat milk"),
    mk_rule("status", "exact", "OPEN", "Open"),
    mk_rule("Drink3", "exact", "cap", "cappuccino")
  )
}

eval_generated_with_norms <- function(df, rules, norms) {
  code <- generate_recode_R(rules, dataset_id = "synthetic",
                            normalizations = norms)
  env  <- new.env(parent = globalenv())
  env$df <- df
  suppressWarnings(suppressMessages(eval(parse(text = code), envir = env)))
  env$df
}

test_that("generated script agrees with normalize-then-recode cell for cell", {
  # Plus the metacharacter columns, each normalized, with recodes written
  # against the normalized values. q1_a must be reached by neither step.
  # Plus the engine-divergent and unusable-pattern rules, and a
  # normalization whose sibling pattern does not compile (skipped by both).
  df    <- engine_df(tricky_df(synthetic_norm_df()))
  rules <- dplyr::bind_rows(
    synthetic_norm_rules(),
    mk_rule("q1.a",      "exact", "YES",  "Y"),
    mk_rule("price ($)", "exact", "FREE", "0"),
    mk_rule("my col",    "exact", "",     "unknown"),
    engine_rules())
  norms <- dplyr::bind_rows(
    synthetic_norms(),
    mk_norm_rule("q1.a",      "uppercase"),
    mk_norm_rule("price ($)", c("squish", "uppercase")),
    mk_norm_rule("my col",    "squish"),
    mk_norm_rule("fam1",      "uppercase", siblings = TRUE,
                 pattern = "fam[0-9"))
  applied <- apply_recodes(apply_normalizations(df, norms)$df, rules)$df
  genned  <- eval_generated_with_norms(df, rules, norms)
  expect_identical(names(genned), names(applied))
  for (col in names(df)) {
    expect_identical(genned[[col]], applied[[col]],
                     info = sprintf("column %s", col))
  }
  # The normalizations really ran, in both engines, before the recodes.
  expect_identical(applied$`drink notes`[1:2], c("Extra Hot", "oat milk"))
  expect_identical(applied$drink1[2], "latte")
  expect_identical(applied$status[c(1, 3)], c("Open", "CLOSED"))
  # The family pattern is case-sensitive in both engines.
  expect_identical(applied$Drink3[1], "CAP")
  expect_identical(applied$Drink3[2], "cappuccino")
  # And the non-syntactic name is backticked in the script.
  code <- generate_recode_R(rules, "synthetic", normalizations = norms)
  expect_true(grepl("`drink notes` = str_to_title(str_squish(`drink notes`))",
                    code, fixed = TRUE))
  expect_true(grepl("`price ($)` = toupper(str_squish(`price ($)`))",
                    code, fixed = TRUE))
  # The metacharacter columns: normalized, then recoded, and nothing else.
  expect_identical(applied$`q1.a`, c("Y", "NO", "Y", "Y", NA, "Y"))
  expect_identical(applied$q1_a, df$q1_a)
  expect_identical(applied$`price ($)`, c("0", "2", "0", "", "0", NA))
  # "  " squishes to "", so the exact "" rule claims both blank cells.
  expect_identical(applied$`my col`,
                   c("hot", "iced", "unknown", "unknown", "hot", NA))
  # The bad-pattern normalization was skipped in both engines.
  expect_identical(applied$fam2, df$fam2)
  expect_true(grepl("# SKIPPED: the normalization", code, fixed = TRUE))
  expect_lt(regexpr("Column normalizations", code, fixed = TRUE)[1],
            regexpr("case_when(", code, fixed = TRUE)[1])
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
  expect_true(grepl("rep_len(FALSE, length(x)) ~ \"A\"", code, fixed = TRUE))
  # As the ONLY arm it must still run: a scalar FALSE made case_when() fail
  # to recycle `.default`.
  df <- tibble::tibble(x = c("a", "b"))
  expect_identical(eval_generated_recodes(df, mk_rule("x", "regx", "a", "A"))$x,
                   c("a", "b"))
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
      source_dataset = readr::col_character(),
      norm_spec = readr::col_character()))
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


# =============================================================================
# October review: codegen against the data's own columns, merge by key,
# clustering cost, and the awkward-input agreement cases.
# =============================================================================

# Like eval_generated_recodes(), but able to pass the data's column names.
# The eval() runs code this test just generated from its own rule table.
eval_gen <- function(df, rules, data_cols = NULL) {
  code <- generate_recode_R(rules, "d", data_cols = data_cols)
  env  <- new.env(parent = globalenv())
  env$df <- df
  suppressWarnings(suppressMessages(eval(parse(text = code), envir = env)))
  env$df
}


# --- engine 1: the overlap warning must know the data's columns --------------

test_that("two sibling patterns sharing an unnamed column warn (engine 1)", {
  # Neither rule NAMES drink2, but both patterns reach it, and they chain:
  # "cap" -> "latte" then "latte" -> "mocha".
  rules <- dplyr::bind_rows(
    mk_rule("drink1", "exact", "cap",   "latte", siblings = TRUE,
            pattern = "^drink[12]$"),
    mk_rule("drink3", "exact", "latte", "mocha", siblings = TRUE,
            pattern = "^drink[23]$"))
  df <- tibble::tibble(drink1 = "cap", drink2 = "cap", drink3 = "americano")

  # The two engines really do disagree on the shared column: the app is
  # single pass, the script runs the two blocks in sequence.
  expect_identical(apply_recodes(df, rules)$df$drink2, "latte")
  expect_identical(eval_gen(df, rules)$drink2, "mocha")

  # Told the data's columns, the script says so instead of staying silent.
  warned <- generate_recode_R(rules, "d", data_cols = names(df))
  expect_match(warned, "# WARNING")
  expect_match(warned, "drink2")

  # Without data_cols, unchanged: the sweep can only see the rules' names.
  expect_false(grepl("# WARNING", generate_recode_R(rules, "d"), fixed = TRUE))
})

test_that("a column reached by one pattern is not reported as overlapping", {
  rules <- mk_rule("drink1", "exact", "cap", "cappuccino", siblings = TRUE,
                   pattern = "^drink[0-9]+$")
  df <- tibble::tibble(drink1 = "cap", drink2 = "cap", other = "cap")
  expect_false(grepl("# WARNING",
                     generate_recode_R(rules, "d", data_cols = names(df)),
                     fixed = TRUE))
})


# --- engine 7: the overlap sweep must not throw ------------------------------

test_that("a column name too long for TRE does not break codegen (engine 7)", {
  # base grepl() THROWS on a pattern of roughly 2,600 characters or more, and
  # a plain rule's pattern is its column name, escaped and anchored.
  long <- paste(rep("a", 5000), collapse = "")
  expect_false(regex_compiles(paste0("^", long, "$")))   # TRE really refuses
  # grepl() warns as well as throwing, hence the suppression.
  expect_error(suppressWarnings(grepl(paste0("^", long, "$"), "probe")))

  expect_no_error(generate_recode_R(mk_rule(long, "exact", "x", "Y"), "d"))
  # And when the long name is merely one of the loaded columns.
  expect_no_error(generate_recode_R(
    mk_rule("kept", "exact", "x", "Y"), "d", data_cols = c("kept", long)))
  expect_false(.pattern_hits_name(paste0("^", long, "$"), long))
})


# --- engine 3: a rule for a column the data does not have --------------------

test_that("a rule for an absent column is skipped in codegen (engine 3)", {
  rules <- mk_rule("gone", "exact", "cap", "cappuccino")
  df    <- tibble::tibble(kept = "cap")

  # The app resolves it to no column and leaves the data alone.
  expect_length(.recode_cols(df, rules), 0L)

  # Without data_cols the script names a column that is not there and dies.
  expect_error(eval_gen(df, rules), "gone")

  # Told the columns, it says it skipped the rule, and then agrees.
  code <- generate_recode_R(rules, "d", data_cols = names(df))
  expect_match(code, "# SKIPPED")
  expect_match(code, "That column is not in")
  expect_match(code, rules$rule_id)
  expect_identical(eval_gen(df, rules, data_cols = names(df)),
                   apply_recodes(df, rules)$df)
})

test_that("a sibling pattern matching no column is skipped too", {
  rules <- mk_rule("drink1", "exact", "cap", "cappuccino", siblings = TRUE,
                   pattern = "^nothing[0-9]$")
  df    <- tibble::tibble(kept = "cap")
  code  <- generate_recode_R(rules, "d", data_cols = names(df))
  expect_match(code, "No column in")
  expect_identical(eval_gen(df, rules, data_cols = names(df)),
                   apply_recodes(df, rules)$df)
})


# --- engine 2: merge_recode_rules collides on the KEY -----------------------

test_that("a stale id no longer hides a new rule for the same key (engine 2)", {
  stale <- mk_rule("city", "exact", "lincon", "OLD")
  stale$rule_id <- "deadbeefcafe"        # e.g. saved before a key edit
  fresh <- mk_rule("city", "exact", "lincon", "NEW")

  merged <- merge_recode_rules(stale, fresh)
  expect_equal(nrow(merged), 1L)
  expect_identical(merged$new_value, "NEW")
  # The winner carries the id its own key says it must carry.
  expect_identical(merged$rule_id, recode_key_id(merged))

  # The point of all of it: the new rule actually runs.
  expect_identical(apply_recodes(tibble::tibble(city = "lincon"),
                                 merged)$df$city, "NEW")
})

test_that("merging by key keeps a colliding rule where it stood", {
  base <- dplyr::bind_rows(mk_rule("city", "exact", "a", "A"),
                           mk_rule("city", "exact", "b", "B"),
                           mk_rule("city", "exact", "z", "Z"))
  out  <- merge_recode_rules(base, mk_rule("city", "exact", "b", "B2"))
  expect_identical(out$old_value, c("a", "b", "z"))
  expect_identical(out$new_value, c("A", "B2", "Z"))
})

test_that("merge_recode_rules de-duplicates when base is empty", {
  # The old nrow(base) == 0 early return handed the overlay back untouched,
  # so two rows for one key both survived and the second could never run.
  dup <- dplyr::bind_rows(mk_rule("city", "exact", "a", "V1"),
                          mk_rule("city", "exact", "a", "V2"))
  out <- merge_recode_rules(empty_recodes_tibble(), dup)
  expect_equal(nrow(out), 1L)
  expect_identical(out$new_value, "V2")
})

test_that("merging does not restamp norm_spec", {
  base <- mk_rule("city", "exact", "a", "A")
  base$norm_spec <- "city=squish"
  over <- mk_rule("city", "exact", "a", "A2")
  over$norm_spec <- "city=titlecase"
  expect_identical(merge_recode_rules(base, over)$norm_spec, "city=titlecase")
  # A rule with a different key still appends rather than replacing.
  expect_equal(nrow(merge_recode_rules(base, mk_rule("city", "exact", "b",
                                                     "B"))), 2L)
  # NA old_value is a key like any other.
  expect_equal(nrow(merge_recode_rules(
    mk_rule("c", "exact", NA_character_, "A"),
    mk_rule("c", "exact", NA_character_, "B"))), 1L)
})


# --- clustering cost: self-comparison dropped, output unchanged --------------

test_that("clustering does not compare a value with itself", {
  # Rebuilds the old two-argument distance step and requires the SAME
  # partition, for an edit-distance metric and a q-gram metric.
  old_partition <- function(keys, algorithm, threshold = 0.8, q = 2) {
    sim <- if (algorithm %in% c("jw", "cosine", "jaccard")) {
      1 - stringdist::stringdistmatrix(keys, keys, method = algorithm, q = q)
    } else {
      d    <- stringdist::stringdistmatrix(keys, keys, method = algorithm)
      lens <- outer(nchar(keys), nchar(keys), pmax)
      lens[lens == 0] <- 1L
      1 - d / lens
    }
    adj <- sim >= threshold
    diag(adj) <- FALSE
    as.integer(igraph::components(igraph::graph_from_adjacency_matrix(
      adj, mode = "undirected", diag = FALSE))$membership)
  }
  groups <- function(x) sort(vapply(split(seq_along(x), x),
                                    function(i) paste(i, collapse = "-"),
                                    character(1)))
  sets <- list(c("cappuccino", "capuccino", "cappucino", "espresso",
                 "expresso", "latte", "late", "mocha"),
               c("latte", "late"), "latte", c("cap", "cap", "cap"),
               c("alpha", "bravo", "charlie"))
  for (keys in sets) {
    for (a in c("osa", "cosine")) {
      expect_identical(
        groups(cluster_strings(keys, algorithm = a, threshold = 0.8,
                               q = 2)$cluster_id),
        groups(old_partition(keys, a)),
        info = paste(a, paste(keys, collapse = ",")))
    }
  }
})


# --- tests 4: the .rule_hits() NA invariant ---------------------------------

test_that(".rule_hits always returns a plain logical with no NA (tests 4)", {
  # CLAUDE.md: a single NA here makes the caller's `if (any(hit))` throw
  # "missing value where TRUE/FALSE needed" and aborts Apply & Export.
  values <- list(
    c("latte", NA, "", "   ", "LATTE"),
    c(NA_character_, NA_character_),
    character(0),
    c("café", intToUtf8(0x202e), "<NA>", "a\nb", "\\", "\"'"),
    NA_character_
  )
  olds <- list("latte", NA_character_, "", "   ", "^lat", "<NA>", "[",
               "café", "a\nb")
  types <- c(RECODE_MATCH_TYPES, NA_character_, "nonsense", "")

  bad_type <- character(0)
  bad_na   <- character(0)
  bad_len  <- character(0)
  bad_attr <- character(0)
  for (vi in seq_along(values)) for (o in olds) for (mt in types) {
    v   <- values[[vi]]
    hit <- .rule_hits(v, o, mt)
    label <- sprintf("values[[%d]] / old=%s / type=%s", vi,
                     if (is.na(o)) "NA" else sprintf("'%s'", o),
                     if (is.na(mt)) "NA" else sprintf("'%s'", mt))
    if (!is.logical(hit) || is.object(hit)) bad_type <- c(bad_type, label)
    if (anyNA(hit))                         bad_na   <- c(bad_na, label)
    if (length(hit) != length(v))           bad_len  <- c(bad_len, label)
    if (!is.null(attributes(hit)))          bad_attr <- c(bad_attr, label)
  }
  expect_identical(bad_type, character(0))
  expect_identical(bad_na,   character(0))
  expect_identical(bad_len,  character(0))
  expect_identical(bad_attr, character(0))
})


# --- tests 1 and 6: awkward input, app and script agreement -----------------

test_that("an embedded newline, a very long value and RTL text agree", {
  nl <- "oat milk\nextra hot"
  expect_identical(eval_gen(tibble::tibble(v = c(nl, "other")),
                            mk_rule("v", "exact", nl, "FIXED")),
                   apply_recodes(tibble::tibble(v = c(nl, "other")),
                                 mk_rule("v", "exact", nl, "FIXED"))$df)

  long <- paste(rep("a", 20000), collapse = "")
  dfl  <- tibble::tibble(v = c(long, "other"))
  expect_identical(eval_gen(dfl, mk_rule("v", "exact", long, "FIXED")),
                   apply_recodes(dfl, mk_rule("v", "exact", long,
                                              "FIXED"))$df)

  # Arabic, built at runtime so this file holds no right-to-left run.
  ar  <- intToUtf8(c(0x0645, 0x0631, 0x062d, 0x0628, 0x0627))
  dfa <- tibble::tibble(v = c(ar, "other"))
  expect_identical(eval_gen(dfa, mk_rule("v", "exact", ar, "FIXED")),
                   apply_recodes(dfa, mk_rule("v", "exact", ar, "FIXED"))$df)
})

test_that("a bidi control character does not break the script (security F3)", {
  # R's parser refuses these raw, even inside a string literal, and
  # deparse() does not escape them. Built at runtime: a raw one in this file
  # would stop the file itself from parsing.
  for (cp in c(0x202a, 0x202e, 0x2066, 0x2069)) {
    val <- paste0("oat", intToUtf8(cp), "milk")
    df  <- tibble::tibble(v = c(val, "other"))
    rules <- mk_rule("v", "exact", val, "FIXED")

    code <- generate_recode_R(rules, "d")
    expect_false(grepl(intToUtf8(cp), code, fixed = TRUE))  # escaped
    expect_match(code, sprintf("\\\\u%04x", cp))
    expect_no_error(parse(text = code))
    # And it still means the same string, so the recode fires.
    expect_identical(eval_gen(df, rules), apply_recodes(df, rules)$df)
    expect_identical(eval_gen(df, rules)$v, c("FIXED", "other"))
  }
  # A value with no bidi character is emitted exactly as deparse() had it.
  plain <- "quotes \" backslash \\ café"
  expect_identical(.deparse_literal(plain), deparse(plain))
})

test_that("a regex rule with a capture group replaces the whole cell", {
  rules <- mk_rule("v", "regex", "^(lat)te([0-9]+)$", "FIXED")
  df    <- tibble::tibble(v = c("latte12", "mocha"))
  expect_identical(apply_recodes(df, rules)$df$v, c("FIXED", "mocha"))
  expect_identical(eval_gen(df, rules), apply_recodes(df, rules)$df)
  # Documented: no backreference substitution, the cell is replaced whole.
  expect_false(grepl("\\1", generate_recode_R(rules, "d"), fixed = TRUE))
})

test_that("duplicate column names: the app takes the first, the script refuses", {
  # KNOWN DEFECT, pinned not fixed. read_dataset() repairs duplicate names,
  # so this is reachable through the API only. The app recodes the FIRST
  # column of that name and says nothing; the generated script will not run
  # on such a frame at all, so the two do NOT agree.
  df <- data.frame(a = c("cap", "latte"), a = c("cap", "mocha"),
                   check.names = FALSE, stringsAsFactors = FALSE)
  expect_identical(names(df), c("a", "a"))
  rules <- mk_rule("a", "exact", "cap", "FIXED")

  got <- apply_recodes(df, rules)$df
  expect_identical(got[[1]], c("FIXED", "latte"))
  expect_identical(got[[2]], c("cap", "mocha"))     # second column untouched

  expect_error(eval_gen(df, rules), "duplicate names")
  # Nothing reports it either.
  expect_equal(nrow(validate_recodes(rules, df)$missing_column), 0L)
})

test_that("a literal <NA> old_value is destroyed by the CSV round trip", {
  # KNOWN DEFECT, pinned not fixed (engine 4 / security F11): `<NA>` is the
  # deliberate, documented sentinel for a missing value, so a value whose
  # text really is "<NA>" cannot be told apart from one. Changing the
  # sentinel costs compatibility with existing rule files and is the owner's
  # decision.
  rules <- mk_rule("v", "exact", "<NA>", "FIXED")
  path  <- withr::local_tempfile(fileext = ".csv")
  write_recodes(rules, path)
  back <- read_recodes(path)

  expect_true(is.na(back$old_value))                # was the literal "<NA>"
  expect_identical(back$new_value, "FIXED")         # other fields survive

  # Before the round trip the rule matches the literal text; after it, the
  # rule targets missing cells instead, which is a different rule.
  df <- tibble::tibble(v = c("<NA>", NA))
  expect_identical(apply_recodes(df, rules)$df$v, c("FIXED", NA))
  expect_identical(apply_recodes(df, back)$df$v, c("<NA>", "FIXED"))
})
