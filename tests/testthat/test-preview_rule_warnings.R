# Run with: testthat::test_dir("tests/testthat")
#
# "Rules that did nothing" panel on Preview & export: rule_warnings_ui()
# renders collect_rule_warnings() output, and the module server wires it to
# the loaded data, the recode rules and the recat rules.

source(file.path("..", "..", "R", "ui_helpers.R"))
source(file.path("..", "..", "R", "string_helpers.R"))
source(file.path("..", "..", "R", "recat_helpers.R"))
source(file.path("..", "..", "R", "rule_warnings.R"))
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
