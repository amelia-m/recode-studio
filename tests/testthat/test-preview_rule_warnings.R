# Run with: testthat::test_dir("tests/testthat")
#
# "Rules that did nothing" panel on Preview & export (normalizations included): rule_warnings_ui()
# renders collect_rule_warnings() output, and the module server wires it to
# the loaded data, the recode rules and the recat rules.

source(file.path("..", "..", "R", "ui_helpers.R"))
source(file.path("..", "..", "R", "string_helpers.R"))
source(file.path("..", "..", "R", "recat_helpers.R"))
source(file.path("..", "..", "R", "rule_warnings.R"))
source(file.path("..", "..", "R", "export_bundle.R"))
source(file.path("..", "..", "R", "mod_preview_export.R"))

.html <- function(x) as.character(htmltools::renderTags(x)$html)

.rule <- function(variable, old_value, new_value) {
  tibble::tibble(
    rule_id           = recode_rule_id(variable, "trimmed_ci", old_value),
    variable          = variable,
    apply_to_siblings = FALSE,
    sibling_pattern   = NA_character_,
    match_type        = "trimmed_ci",
    old_value         = old_value,
    new_value         = new_value,
    action            = "recode",
    notes             = NA_character_,
    author            = "test",
    created_at        = "2026-01-01T00:00:00",
    updated_at        = "2026-01-01T00:00:00",
    source_dataset    = "cafe"
  )
}

.recat <- function(include) {
  tibble::tibble(
    recat_id        = recat_rule_id("drink_family", "Espresso-based", "drink1",
                                    include, "", "literal"),
    out_col         = "drink_family",
    category        = "Espresso-based",
    vars            = "drink1",
    sibling_pattern = NA_character_,
    include_terms   = include,
    exclude_terms   = NA_character_,
    match_type      = "literal",
    priority        = 10L,
    notes           = NA_character_,
    author          = NA_character_,
    created_at      = "2026-01-01T00:00:00",
    source_dataset  = "cafe"
  )
}

.df <- data.frame(drink1 = c("expresso", "latte", "expresso"),
                  stringsAsFactors = FALSE)

.proxy <- function(rules) list(get = function() rules)

# --- Formatter ---------------------------------------------------------------

test_that("nothing to check renders nothing", {
  expect_null(rule_warnings_ui(NULL))
})

test_that("no warnings renders a quiet all-clear, not a warning box", {
  h <- .html(rule_warnings_ui(empty_rule_warnings()))
  expect_match(h, "No problems found", fixed = TRUE)
  expect_false(grepl("alert-warning", h, fixed = TRUE))
})

test_that("each warning is listed in plain language", {
  w <- collect_rule_warnings(
    .df,
    dplyr::bind_rows(.rule("drink1", "mocha", "Mocha"),
                     .rule("drink1", "expresso", "espresso")),
    .recat("expresso"))
  expect_identical(w$kind, c("recode_no_match", "recat_terms_recoded_away"))
  h <- .html(rule_warnings_ui(w))
  expect_match(h, "alert-warning", fixed = TRUE)
  expect_match(h, "2 rules did less than expected.", fixed = TRUE)
  expect_equal(lengths(regmatches(h, gregexpr("<li>", h))), 2L)
  expect_match(h, "Recodes are applied first", fixed = TRUE)
})

test_that("a single recode warning uses the singular and no recat note", {
  w <- collect_rule_warnings(.df, .rule("drink1", "mocha", "Mocha"))
  h <- .html(rule_warnings_ui(w))
  expect_match(h, "1 rule did less than expected.", fixed = TRUE)
  expect_false(grepl("Recodes are applied first", h, fixed = TRUE))
})

test_that("warning text from the data is escaped", {
  w <- collect_rule_warnings(.df, .rule("drink1", "<b>x</b>", "y"))
  h <- .html(rule_warnings_ui(w))
  expect_false(grepl("<b>x</b>", h, fixed = TRUE))
  expect_match(h, "&lt;b&gt;x&lt;/b&gt;", fixed = TRUE)
})

# --- Server wiring -----------------------------------------------------------

test_that("no data loaded: no panel and no error", {
  st <- shiny::reactiveValues(data = NULL, meta = NULL, dataset_name = NULL)
  shiny::testServer(mod_preview_export_server,
    args = list(shared_state = st,
                rules_proxy  = .proxy(.rule("drink1", "mocha", "Mocha")),
                recat_proxy  = .proxy(.recat("expresso"))), {
      expect_null(rule_warnings_r())
      expect_silent(output$rule_warnings)
    })
})

test_that("server reports both kinds, matching collect_rule_warnings()", {
  st <- shiny::reactiveValues(data = .df, meta = NULL, dataset_name = "cafe.csv")
  rules <- dplyr::bind_rows(.rule("drink1", "mocha", "Mocha"),
                            .rule("drink1", "expresso", "espresso"))
  recat <- .recat("expresso")
  shiny::testServer(mod_preview_export_server,
    args = list(shared_state = st, rules_proxy = .proxy(rules),
                recat_proxy = .proxy(recat)), {
      expect_identical(rule_warnings_r(),
                       collect_rule_warnings(.df, rules, recat))
      expect_match(output$rule_warnings$html, "2 rules did less than expected.",
                   fixed = TRUE)
    })
})

test_that("server without a recat proxy still checks the recodes", {
  st <- shiny::reactiveValues(data = .df, meta = NULL, dataset_name = "cafe.csv")
  shiny::testServer(mod_preview_export_server,
    args = list(shared_state = st,
                rules_proxy = .proxy(.rule("drink1", "latte", "Latte"))), {
      expect_equal(nrow(rule_warnings_r()), 0L)
      expect_match(output$rule_warnings$html, "No problems found", fixed = TRUE)
    })
})


# --- Normalizations in the panel ---------------------------------------------

.norm <- function(variable, ops) {
  tibble::tibble(
    norm_id = normalization_rule_id(variable, NA_character_),
    variable = variable, apply_to_siblings = FALSE,
    sibling_pattern = NA_character_, ops = ops, notes = NA_character_,
    author = "test", created_at = "2026-01-01T00:00:00",
    updated_at = "2026-01-01T00:00:00", source_dataset = "cafe")
}

test_that("drift warnings carry the normalization order note", {
  # exact, so the case change is a real WARNING: trimmed_ci ignores case, and
  # its drift is only a note (which carries no normalization-order hint).
  r <- .rule("drink1", "expresso", "espresso")
  r$match_type <- "exact"
  r$rule_id <- recode_rule_id("drink1", "exact", "expresso")
  r$norm_spec <- ""
  w <- collect_rule_warnings(.df, r, normalizations = .norm("drink1", "uppercase"))
  expect_true("norm_spec_drift" %in% w$kind)
  expect_identical(w$severity[w$kind == "norm_spec_drift"], "warning")
  h <- .html(rule_warnings_ui(w))
  expect_match(h, "Normalizations are applied before the recodes", fixed = TRUE)
})

test_that("server includes normalization warnings, matching collect_rule_warnings()", {
  st <- shiny::reactiveValues(data = .df, meta = NULL, dataset_name = "cafe.csv")
  rules <- .rule("drink1", "latte", "Latte")
  rules$norm_spec <- ""
  norms <- dplyr::bind_rows(.norm("drink1", "titlecase"), .norm("gone", "trim"))
  shiny::testServer(mod_preview_export_server,
    args = list(shared_state = st, rules_proxy = .proxy(rules),
                norms_proxy = .proxy(norms)), {
      w <- rule_warnings_r()
      expect_identical(w, collect_rule_warnings(.df, rules, empty_recat_tibble(),
                                                norms))
      expect_identical(w$kind, c("normalize_no_target", "norm_spec_drift"))
    })
})

test_that("normalizations alone are enough to show the panel", {
  st <- shiny::reactiveValues(data = .df, meta = NULL, dataset_name = "cafe.csv")
  shiny::testServer(mod_preview_export_server,
    args = list(shared_state = st, rules_proxy = .proxy(empty_recodes_tibble()),
                norms_proxy = .proxy(.norm("drink1", "uppercase"))), {
      expect_equal(nrow(rule_warnings_r()), 0L)
      expect_match(output$rule_warnings$html, "No problems found", fixed = TRUE)
    })
})


# --- Notes (severity "note") vs warnings --------------------------------------
#
# Stamp-only findings the rule's match type already ignores are notes: they
# cannot change the result, so they are not counted as rules that did less
# than expected, and they sit in a quiet collapsed block of their own.

.w <- function(kind, message, severity) {
  tibble::tibble(kind = kind, id = "r1", target = "drink1", message = message,
                 n_before = NA_integer_, n_after = NA_integer_,
                 severity = severity)
}

test_that("notes alone keep the all-clear line and add a collapsed notes block", {
  w <- .w("norm_spec_drift", "Stamp differs but trimmed_ci ignores it.", "note")
  h <- .html(rule_warnings_ui(w))
  expect_match(h, "No problems found", fixed = TRUE)
  expect_false(grepl("alert-warning", h, fixed = TRUE))
  expect_false(grepl("did less than expected", h, fixed = TRUE))
  expect_match(h, "rs-rule-notes", fixed = TRUE)
  expect_match(h, "1 note: stamp differences that cannot change the result",
               fixed = TRUE)
  expect_match(h, "Stamp differs but trimmed_ci ignores it.", fixed = TRUE)
  # Collapsed: a <details> without the open attribute.
  expect_match(h, "<details class=\"rs-guide-note\">", fixed = TRUE)
})

test_that("warnings are counted without the notes, which are listed apart", {
  w <- dplyr::bind_rows(
    .w("recode_no_match", "Rule A did nothing.", "warning"),
    .w("norm_spec_drift", "Note one.", "note"),
    .w("norm_spec_unstamped", "Note two.", "note"))
  h <- .html(rule_warnings_ui(w))
  expect_match(h, "1 rule did less than expected.", fixed = TRUE)
  expect_match(h, "2 notes: stamp differences", fixed = TRUE)
  expect_false(grepl("No problems found", h, fixed = TRUE))
  # The yellow box holds the one warning only.
  box <- regmatches(h, regexpr("alert-warning.*?</div>", h))
  expect_match(box, "Rule A did nothing.", fixed = TRUE)
  expect_false(grepl("Note one.", box, fixed = TRUE))
  # The normalization-order hint belongs to drift WARNINGS; a note is not one.
  expect_false(grepl("Normalizations are applied before the recodes", h,
                     fixed = TRUE))
})

test_that("a tibble without severity, or an unknown severity, counts as warnings", {
  w <- .w("recode_no_match", "Old-style row.", "warning")
  w$severity <- NULL
  h <- .html(rule_warnings_ui(w))
  expect_match(h, "1 rule did less than expected.", fixed = TRUE)
  w2 <- .w("recode_no_match", "Odd severity.", "something-new")
  expect_match(.html(rule_warnings_ui(w2)), "1 rule did less than expected.",
               fixed = TRUE)
})

test_that("note text is escaped", {
  h <- .html(rule_warnings_ui(.w("norm_spec_drift", "<b>x</b>", "note")))
  expect_false(grepl("<b>x</b>", h, fixed = TRUE))
  expect_match(h, "&lt;b&gt;x&lt;/b&gt;", fixed = TRUE)
})

test_that("a real neutral stamp change reaches the panel as a note", {
  r <- .rule("drink1", "latte", "Latte")
  r$norm_spec <- ""
  w <- collect_rule_warnings(.df, r, normalizations = .norm("drink1", "uppercase"))
  expect_identical(w$kind, "norm_spec_drift")
  expect_identical(w$severity, "note")
  h <- .html(rule_warnings_ui(w))
  expect_match(h, "No problems found", fixed = TRUE)
  expect_match(h, "1 note:", fixed = TRUE)
})
