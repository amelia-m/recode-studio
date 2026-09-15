# Run with: testthat::test_dir("tests/testthat")
#
# Tests for the rule checks in R/rule_warnings.R: recode rules that match
# nothing, and recat rules whose include terms the recodes rewrote.

source(file.path("..", "..", "R", "string_helpers.R"))
source(file.path("..", "..", "R", "recat_helpers.R"))
source(file.path("..", "..", "R", "rule_warnings.R"))

# --- Fixtures ----------------------------------------------------------------

rw_recode <- function(variable, match_type, old_value, new_value,
                      action = "recode", siblings = FALSE,
                      pattern = NA_character_) {
  tibble::tibble(
    rule_id           = recode_rule_id(variable, match_type, old_value),
    variable          = variable,
    apply_to_siblings = siblings,
    sibling_pattern   = pattern,
    match_type        = match_type,
    old_value         = old_value,
    new_value         = new_value,
    action            = action,
    notes             = NA_character_,
    author            = "test",
    created_at        = "2026-01-01T00:00:00",
    updated_at        = "2026-01-01T00:00:00",
    source_dataset    = "cafe"
  )
}

rw_recat <- function(out_col, category, vars, include_terms,
                     exclude_terms = character(0), match_type = "literal",
                     priority = 100L) {
  vars_j <- recat_join(vars)
  inc_j  <- recat_join(include_terms)
  exc_j  <- recat_join(exclude_terms)
  tibble::tibble(
    recat_id        = recat_rule_id(out_col, category, vars_j, inc_j, exc_j,
                                    match_type),
    out_col         = out_col,
    category        = category,
    vars            = vars_j,
    sibling_pattern = NA_character_,
    include_terms   = inc_j,
    exclude_terms   = exc_j,
    match_type      = match_type,
    priority        = as.integer(priority),
    notes           = NA_character_,
    author          = NA_character_,
    created_at      = "2026-01-01T00:00:00",
    source_dataset  = "cafe"
  )
}

cafe_df <- function() {
  tibble::tibble(
    order_id = 1:5,
    drink1   = c("capuccino", "cappuccino", "latte", "mocha", NA),
    drink2   = c(NA, "capuccino", NA, "espresso", "latte"),
    notes    = c("extra hot", "oat milk", "no foam", "decaf", "to go")
  )
}

expect_warning_schema <- function(w) {
  expect_named(w, names(empty_rule_warnings()))
  expect_type(w$n_before, "integer")
  expect_type(w$n_after, "integer")
}


# --- empty_rule_warnings -----------------------------------------------------

test_that("empty_rule_warnings has the documented typed schema", {
  w <- empty_rule_warnings()
  expect_equal(nrow(w), 0L)
  expect_named(w, c("kind", "id", "target", "message", "n_before", "n_after"))
  expect_type(w$kind, "character")
  expect_type(w$id, "character")
  expect_type(w$target, "character")
  expect_type(w$message, "character")
  expect_warning_schema(w)
})


# --- warn_recodes_no_match ---------------------------------------------------

test_that("a recode rule matching no cell is reported", {
  rules <- dplyr::bind_rows(
    rw_recode("drink1", "trimmed_ci", "capuccino", "cappuccino"),  # matches
    rw_recode("drink1", "exact", "frappe", "frappuccino")          # does not
  )
  w <- warn_recodes_no_match(cafe_df(), rules)
  expect_warning_schema(w)
  expect_equal(nrow(w), 1L)
  expect_identical(w$kind, "recode_no_match")
  expect_identical(w$id, rules$rule_id[2])
  expect_identical(w$target, "drink1")
  expect_identical(w$n_before, 0L)
  expect_true(is.na(w$n_after))
  expect_match(w$message, "frappe", fixed = TRUE)
  expect_match(w$message, "no cell matched")
})

test_that("a recode rule on a column missing from the data says so", {
  rules <- rw_recode("drink9", "exact", "latte", "Latte")
  w <- warn_recodes_no_match(cafe_df(), rules)
  expect_equal(nrow(w), 1L)
  expect_match(w$message, "not in this data")
})

test_that("an NA-targeting recode is described in plain words", {
  df <- tibble::tibble(city = c("Uptown", "Downtown"))
  w <- warn_recodes_no_match(df, rw_recode("city", "exact", NA_character_, "unknown"))
  expect_equal(nrow(w), 1L)
  expect_match(w$message, "blank (missing) cell", fixed = TRUE)
})

test_that("a shadowed rule still counts as matching, a sibling rule resolves", {
  rules <- dplyr::bind_rows(
    rw_recode("drink1", "trimmed_ci", "capuccino", "cappuccino"),
    rw_recode("drink1", "regex", "^capu", "cappuccino (other)"),  # shadowed
    rw_recode("drink1", "exact", "espresso", "Espresso",
              siblings = TRUE, pattern = "^drink[0-9]+$")         # hits drink2
  )
  expect_equal(nrow(warn_recodes_no_match(cafe_df(), rules)), 0L)
})

test_that("no rules, no warnings", {
  w <- warn_recodes_no_match(cafe_df(), empty_recodes_tibble())
  expect_equal(nrow(w), 0L)
  expect_warning_schema(w)
})


# --- warn_recat_terms_recoded_away -------------------------------------------

test_that("a recat rule that loses rows after recoding is reported", {
  raw <- cafe_df()
  recoded <- apply_recodes(
    raw, rw_recode("drink1", "trimmed_ci", "capuccino", "cappuccino",
                   siblings = TRUE, pattern = "^drink[0-9]+$"))$df
  recat <- dplyr::bind_rows(
    rw_recat("typo_flag", "Misspelled", c("drink1", "drink2"), "capuccino"),
    rw_recat("milk", "Milk-based", c("drink1", "drink2"), "latte")
  )
  w <- warn_recat_terms_recoded_away(raw, recoded, recat)
  expect_warning_schema(w)
  expect_equal(nrow(w), 1L)
  expect_identical(w$kind, "recat_terms_recoded_away")
  expect_identical(w$id, recat$recat_id[1])
  expect_identical(w$target, "typo_flag = Misspelled")
  expect_identical(w$n_before, 2L)
  expect_identical(w$n_after, 0L)
  expect_match(w$message, "before recategorization")
})

test_that("recat warnings are empty when nothing changed or no rules", {
  raw <- cafe_df()
  recat <- rw_recat("milk", "Milk-based", "drink1", "latte")
  expect_equal(nrow(warn_recat_terms_recoded_away(raw, raw, recat)), 0L)
  expect_equal(
    nrow(warn_recat_terms_recoded_away(raw, raw, empty_recat_tibble())), 0L)
})

test_that("a recode that ADDS recat matches is not a warning", {
  raw <- cafe_df()
  recoded <- apply_recodes(
    raw, rw_recode("drink1", "exact", "mocha", "mocha latte"))$df
  recat <- rw_recat("milk", "Milk-based", c("drink1", "drink2"), "latte")
  expect_equal(nrow(warn_recat_terms_recoded_away(raw, recoded, recat)), 0L)
})


# --- collect_rule_warnings ---------------------------------------------------

test_that("end to end: a recode rewrites a recat include term", {
  df <- cafe_df()
  rules <- dplyr::bind_rows(
    # Fixes the typo in both drink columns...
    rw_recode("drink1", "trimmed_ci", "capuccino", "cappuccino",
              siblings = TRUE, pattern = "^drink[0-9]+$"),
    # ...and a stale rule that matches nothing.
    rw_recode("notes", "exact", "iced", "over ice")
  )
  # ...while the recat rule was written against the misspelling.
  recat <- dplyr::bind_rows(
    rw_recat("coffee_type", "Cappuccino", c("drink1", "drink2"), "capuccino"),
    rw_recat("milk", "Milk-based", c("drink1", "drink2"), "latte")
  )

  w <- collect_rule_warnings(df, rules, recat)
  expect_warning_schema(w)
  expect_identical(w$kind, c("recode_no_match", "recat_terms_recoded_away"))
  expect_identical(w$id, c(rules$rule_id[2], recat$recat_id[1]))
  expect_identical(w$n_before, c(0L, 2L))
  expect_identical(w$n_after, c(NA_integer_, 0L))

  # The warning describes what export will really do: recat runs on the
  # recoded frame and the Cappuccino rule assigns nothing.
  out <- apply_recat(apply_recodes(df, rules)$df, recat)$df
  expect_true(all(is.na(out$coffee_type)))
})

test_that("collect_rule_warnings defaults to no recat rules", {
  w <- collect_rule_warnings(cafe_df(),
                             rw_recode("drink1", "exact", "frappe", "frappuccino"))
  expect_identical(w$kind, "recode_no_match")
})

test_that("a clean rule set produces no warnings", {
  df <- cafe_df()
  rules <- rw_recode("drink1", "trimmed_ci", "capuccino", "cappuccino")
  recat <- rw_recat("milk", "Milk-based", c("drink1", "drink2"), "latte")
  w <- collect_rule_warnings(df, rules, recat)
  expect_equal(nrow(w), 0L)
  expect_warning_schema(w)
  expect_equal(nrow(collect_rule_warnings(df, empty_recodes_tibble())), 0L)
})
