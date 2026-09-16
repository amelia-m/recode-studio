# Run with: testthat::test_dir("tests/testthat")
#
# EMPTY values ("") as a recode target.
#
# Browse values used to drop "", so a rule could not recode empty cells (to
# "Unknown", or to missing with action delete). It now lists them, labelled
# "(empty)" on screen, and the rule it mints has old_value "". These tests pin
# how every engine treats such a rule: matching per match type, apply and
# script agreement, validation, and the recodes CSV round trip.
#
# Note: read_dataset() reads an empty CSV cell (and a quoted blank) as
# MISSING, so "" normally reaches the app only from data built another way,
# or from an Excel file. The rules must still behave.

source(file.path("..", "..", "R", "ui_helpers.R"))
source(file.path("..", "..", "R", "string_helpers.R"))
source(file.path("..", "..", "R", "recat_helpers.R"))
source(file.path("..", "..", "R", "rule_proxies.R"))
source(file.path("..", "..", "R", "mod_value_table.R"))

.rule <- function(variable, match_type, old_value, new_value,
                  action = "recode") {
  tibble::tibble(
    rule_id           = recode_rule_id(variable, match_type, old_value),
    variable          = variable,
    apply_to_siblings = FALSE,
    sibling_pattern   = NA_character_,
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

.eval_script <- function(df, rules, norms = NULL) {
  code <- generate_recode_R(rules, "cafe", normalizations = norms)
  env  <- new.env(parent = globalenv())
  env$df <- df
  # The eval() runs code this test just generated from its own rule table.
  suppressWarnings(suppressMessages(eval(parse(text = code), envir = env)))
  env$df
}

.vals <- c("", "  ", NA, "latte", " latte ")


# --- Matching -------------------------------------------------------------------

test_that(".rule_hits truth table for an empty old_value", {
  expect_identical(.rule_hits(.vals, "", "exact"),
                   c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(.rule_hits(.vals, "", "exact_ci"),
                   c(TRUE, FALSE, FALSE, FALSE, FALSE))
  # Whitespace-only squishes to "", so trimmed_ci claims it too.
  expect_identical(.rule_hits(.vals, "", "trimmed_ci"),
                   c(TRUE, TRUE, FALSE, FALSE, FALSE))
  # And a whitespace-only old_value is the same trimmed_ci target.
  expect_identical(.rule_hits(.vals, "   ", "trimmed_ci"),
                   c(TRUE, TRUE, FALSE, FALSE, FALSE))
  # An EMPTY regex matches nothing (grepl("") would match every cell).
  expect_identical(.rule_hits(.vals, "", "regex"), rep(FALSE, 5))
  # `^$` is how a regex targets empty cells.
  expect_identical(.rule_hits(.vals, "^$", "regex"),
                   c(TRUE, FALSE, FALSE, FALSE, FALSE))
  for (mt in RECODE_MATCH_TYPES) {
    expect_false(anyNA(.rule_hits(.vals, "", mt)), info = mt)
  }
})

test_that("empty-value rules agree between apply_recodes and the script, per match type", {
  df <- tibble::tibble(
    ex   = .vals, ci = .vals, tr = .vals, rx_empty = .vals, rx_anchor = .vals,
    del  = .vals)
  rules <- dplyr::bind_rows(
    .rule("ex",        "exact",      "", "Unknown"),
    .rule("ci",        "exact_ci",   "", "Unknown"),
    .rule("tr",        "trimmed_ci", "", "Unknown"),
    .rule("rx_empty",  "regex",      "", "Unknown"),
    .rule("rx_anchor", "regex",      "^$", "Unknown"),
    .rule("del",       "trimmed_ci", "", NA_character_, action = "delete"))
  code <- generate_recode_R(rules, "cafe")
  expect_silent(parse(text = code))
  expect_true(grepl('ex == "" ~ "Unknown"', code, fixed = TRUE))
  expect_true(grepl('tolower(ci) == "" ~ "Unknown"', code, fixed = TRUE))
  expect_true(grepl('str_squish(tolower(tr)) == "" ~ "Unknown"', code,
                    fixed = TRUE))
  expect_true(grepl('rep_len(FALSE, length(rx_empty)) ~ "Unknown"', code,
                    fixed = TRUE))
  expect_false(grepl('str_detect(rx_empty, "")', code, fixed = TRUE))

  res     <- apply_recodes(df, rules)
  applied <- res$df
  genned  <- .eval_script(df, rules)
  for (col in names(df)) {
    expect_identical(genned[[col]], applied[[col]], info = col)
  }
  expect_identical(applied$ex, c("Unknown", "  ", NA, "latte", " latte "))
  expect_identical(applied$ci, c("Unknown", "  ", NA, "latte", " latte "))
  expect_identical(applied$tr, c("Unknown", "Unknown", NA, "latte", " latte "))
  expect_identical(applied$rx_empty, .vals)
  expect_identical(applied$rx_anchor, c("Unknown", "  ", NA, "latte", " latte "))
  expect_identical(applied$del, c(NA, NA, NA, "latte", " latte "))
  expect_identical(res$summary$cells_changed, c(1L, 1L, 2L, 0L, 1L, 2L))
})

test_that("an empty-value rule after a squish normalization catches blank cells", {
  df    <- tibble::tibble(city = c("", "   ", "Portland", NA))
  norms <- tibble::tibble(
    norm_id = normalization_rule_id("city"), variable = "city",
    apply_to_siblings = FALSE, sibling_pattern = NA_character_,
    ops = "squish", notes = NA_character_, author = "test",
    created_at = "x", updated_at = "x", source_dataset = "cafe")
  rules <- .rule("city", "exact", "", "Unknown")
  applied <- apply_recodes(apply_normalizations(df, norms)$df, rules)$df
  expect_identical(applied$city, c("Unknown", "Unknown", "Portland", NA))
  expect_identical(.eval_script(df, rules, norms)$city, applied$city)
})


# --- Validation -------------------------------------------------------------------

test_that("validate_recodes on empty-value rules", {
  df <- tibble::tibble(x = c("  ", "a"))
  rules <- dplyr::bind_rows(
    .rule("x", "trimmed_ci", "", "Unknown"),
    .rule("x", "exact",      "", "Unknown"),
    .rule("x", "regex",      "", "Unknown"),
    .rule("x", "exact_ci",   "a", ""))
  v <- validate_recodes(rules, df)
  # The empty regex is reported, a working pattern is not.
  expect_identical(v$invalid_regex$rule_id, rules$rule_id[3])
  # trimmed_ci "" hits the whitespace-only cell, so it is not stale; the exact
  # "" rule and the empty regex hit nothing.
  expect_setequal(v$stale$rule_id, rules$rule_id[2:3])
  # A recode TO "" is still reported as a blank new value.
  expect_identical(v$blank_new_value$rule_id, rules$rule_id[4])
})


# --- CSV round trip -----------------------------------------------------------------

test_that('"" stays "" and NA stays <NA> through the recodes CSV', {
  rules <- dplyr::bind_rows(
    .rule("x", "trimmed_ci", "", "Unknown"),
    .rule("x", "exact", NA_character_, ""),
    .rule("x", "exact", "", NA_character_, action = "delete"))
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  write_recodes(rules, tmp)
  txt <- readLines(tmp)
  expect_true(any(grepl(",trimmed_ci,,Unknown,", txt, fixed = TRUE)))
  expect_true(any(grepl(",exact,<NA>,,recode,", txt, fixed = TRUE)))
  back <- read_recodes(tmp)
  expect_identical(back$old_value, c("", NA, ""))
  expect_identical(back$new_value, c("Unknown", "", NA))
  expect_identical(back$rule_id, rules$rule_id)
  # The two exact rules have different keys ("" versus NA).
  expect_false(identical(back$rule_id[2], back$rule_id[3]))
  df <- tibble::tibble(x = c("", " ", NA, "a"))
  expect_identical(apply_recodes(df, back)$df, apply_recodes(df, rules)$df)
})


# --- Browse values -------------------------------------------------------------------

test_that("value_frequency_table keeps the empty value and drops NA", {
  df <- tibble::tibble(x = c("latte", "", "", NA, "latte", "latte"))
  out <- value_frequency_table(df, "x")
  expect_identical(out$value, c("latte", ""))
  expect_identical(out$n, c(3L, 2L))
  expect_null(value_frequency_table(tibble::tibble(x = c(NA, NA)), "x"))
  expect_null(value_frequency_table(df, "missing_col"))
  expect_null(value_frequency_table(NULL, "x"))
  expect_null(value_frequency_table(df, NULL))
  # A value squished to "" by a normalization is listed as "".
  norms <- tibble::tibble(
    norm_id = normalization_rule_id("x"), variable = "x",
    apply_to_siblings = FALSE, sibling_pattern = NA_character_,
    ops = "squish", notes = NA_character_, author = "test",
    created_at = "x", updated_at = "x", source_dataset = "cafe")
  dn <- apply_normalizations(tibble::tibble(x = c("   ", "a", NA)), norms)$df
  expect_setequal(value_frequency_table(dn, "x")$value, c("", "a"))
})

test_that("the empty-value label is constant, escaped markup keyed on ''", {
  defs <- dt_empty_value_defs(targets = 0)
  expect_length(defs, 1L)
  expect_identical(defs[[1]]$targets, 0)
  js <- as.character(defs[[1]]$render)
  expect_true(grepl("type === 'display' && data === ''", js, fixed = TRUE))
  expect_true(grepl("rs-empty-value", js, fixed = TRUE))
  expect_true(grepl(EMPTY_VALUE_LABEL, js, fixed = TRUE))
  # The truncation renderer handles every other value.
  expect_true(grepl("rs-dt-trunc", js, fixed = TRUE))
  expect_false(grepl("rs-dt-trunc",
                     as.character(dt_empty_value_defs(1, truncate = FALSE)[[1]]$render),
                     fixed = TRUE))
  # The tooltip explains the label, so a real "(empty)" value (shown as plain
  # text, not in the outlined italic label) is distinguishable.
  expect_match(EMPTY_VALUE_TITLE, "holds no text", fixed = TRUE)
  # Neither table unescapes its dataset columns.
  for (f in c("mod_value_table.R", "mod_recode_editor.R")) {
    src <- readLines(file.path("..", "..", "R", f), encoding = "UTF-8")
    expect_false(any(grepl("escape\\s*=\\s*FALSE", src)), info = f)
  }
})

test_that("Browse values lists '' and mints a rule with old_value ''", {
  rv <- shiny::reactiveValues(rules = empty_recodes_tibble())
  proxy <- make_rules_proxy(rv)
  df <- tibble::tibble(city = c("Portland", "", "", NA, "Seattle"))
  st <- shiny::reactiveValues(data = df, meta = NULL, dataset_name = "cafe.csv",
                              source_meta = NULL)
  shiny::testServer(mod_value_table_server,
    args = list(shared_state    = st,
                selected_var_r  = shiny::reactive("city"),
                unique_values_r = shiny::reactive(value_frequency_table(df, "city")),
                rules_proxy     = proxy), {
      tbl <- table_data()
      expect_true("" %in% tbl$value)
      expect_false(anyNA(tbl$value))
      row <- which(tbl$value == "")
      session$setInputs(tbl_rows_selected = row, recode_sel = 1)
      r <- shiny::isolate(rv$rules)
      expect_equal(nrow(r), 1L)
      expect_identical(r$old_value, "")
      expect_identical(r$match_type, "trimmed_ci")
      expect_identical(r$rule_id, recode_rule_id("city", "trimmed_ci", ""))
      # Recoding it and applying it rewrites exactly the empty cells.
      r$new_value <- "Unknown"
      expect_identical(apply_recodes(df, r)$df$city,
                       c("Portland", "Unknown", "Unknown", NA, "Seattle"))
    })
})
