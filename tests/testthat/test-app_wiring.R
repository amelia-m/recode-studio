# Run with: testthat::test_dir("tests/testthat")
#
# Rule store proxies (R/rule_proxies.R), as built by app.R's server().
#
# add() must replace a colliding rule IN PLACE. Both rule sets are
# first-match-wins by row order, so the older drop-and-append add() could move
# an edited rule (even a notes-only edit) behind another rule matching the
# same cell, and silently change the output.

source(file.path("..", "..", "R", "string_helpers.R"))
source(file.path("..", "..", "R", "recat_helpers.R"))
source(file.path("..", "..", "R", "rule_proxies.R"))

# --- Fixtures ----------------------------------------------------------------

.rule <- function(variable, match_type, old_value, new_value,
                  notes = NA_character_) {
  tibble::tibble(
    rule_id           = recode_rule_id(variable, match_type, old_value),
    variable          = variable,
    apply_to_siblings = FALSE,
    sibling_pattern   = NA_character_,
    match_type        = match_type,
    old_value         = old_value,
    new_value         = new_value,
    action            = "recode",
    notes             = notes,
    author            = "test",
    created_at        = "2026-01-01T00:00:00",
    updated_at        = "2026-01-01T00:00:00",
    source_dataset    = "cafe"
  )
}

.recat <- function(category, include, priority = 10L, notes = NA_character_) {
  tibble::tibble(
    recat_id        = recat_rule_id("drink_family", category, "drink1",
                                    include, "", "literal"),
    out_col         = "drink_family",
    category        = category,
    vars            = "drink1",
    sibling_pattern = NA_character_,
    include_terms   = include,
    exclude_terms   = NA_character_,
    match_type      = "literal",
    priority        = priority,
    notes           = notes,
    author          = NA_character_,
    created_at      = "2026-01-01T00:00:00",
    source_dataset  = "cafe"
  )
}

.df <- data.frame(drink1 = c("latte", "Latte", "mocha"),
                  stringsAsFactors = FALSE)

# --- Recode proxy ------------------------------------------------------------

test_that("rules_proxy$add() keeps an edited rule in its row", {
  rv <- shiny::reactiveValues(rules = empty_recodes_tibble(),
                              recat = empty_recat_tibble())
  p  <- make_rules_proxy(rv)
  a  <- .rule("drink1", "exact", "latte", "Latte")
  b  <- .rule("drink1", "trimmed_ci", "latte", "LATTE")
  c3 <- .rule("drink1", "exact", "mocha", "Mocha")
  p$add(dplyr::bind_rows(a, b, c3))
  ids_before <- shiny::isolate(p$get())$rule_id

  a_edit <- a
  a_edit$notes <- "notes-only edit"
  p$add(a_edit)

  after <- shiny::isolate(p$get())
  expect_identical(after$rule_id, ids_before)
  expect_identical(after$notes[1], "notes-only edit")
  expect_equal(nrow(after), 3L)
})

test_that("a notes-only edit does not change apply_recodes() output", {
  rv <- shiny::reactiveValues(rules = empty_recodes_tibble())
  p  <- make_rules_proxy(rv)
  # Both rules hit "latte"; the exact rule is first, so it claims the cell.
  a <- .rule("drink1", "exact", "latte", "Latte")
  b <- .rule("drink1", "trimmed_ci", "latte", "LATTE")
  p$add(dplyr::bind_rows(a, b))
  before <- apply_recodes(.df, shiny::isolate(p$get()))$df

  a$notes <- "just a note"
  p$add(a)
  after <- apply_recodes(.df, shiny::isolate(p$get()))$df

  expect_identical(after$drink1, before$drink1)
  expect_identical(after$drink1[1], "Latte")

  # The bug this guards against: drop-and-append puts the exact rule last,
  # and the trimmed_ci rule then claims the cell.
  moved <- dplyr::bind_rows(b, a)
  expect_identical(apply_recodes(.df, moved)$df$drink1[1], "LATTE")
})

test_that("rules_proxy$add() appends new ids and conforms partial rows", {
  rv <- shiny::reactiveValues(rules = empty_recodes_tibble())
  p  <- make_rules_proxy(rv)
  a  <- .rule("drink1", "exact", "latte", "Latte")
  p$add(a)
  partial <- .rule("drink1", "exact", "mocha", "Mocha")
  partial$source_dataset <- NULL
  p$add(partial)
  out <- shiny::isolate(p$get())
  expect_equal(nrow(out), 2L)
  expect_identical(names(out), names(empty_recodes_tibble()))
  expect_true(is.na(out$source_dataset[2]))
})

test_that("rules_proxy$set() replaces the store", {
  rv <- shiny::reactiveValues(rules = empty_recodes_tibble())
  p  <- make_rules_proxy(rv)
  p$add(.rule("drink1", "exact", "latte", "Latte"))
  p$set(empty_recodes_tibble())
  expect_equal(nrow(shiny::isolate(p$get())), 0L)
})

# --- Recat proxy -------------------------------------------------------------

test_that("recat_proxy$add() replaces in place and appends new ids", {
  rv <- shiny::reactiveValues(recat = empty_recat_tibble())
  p  <- make_recat_proxy(rv)
  e  <- .recat("Espresso-based", "espresso")
  m  <- .recat("Milk-based", "latte")
  p$add(dplyr::bind_rows(e, m))

  e2 <- e
  e2$notes <- "edited"
  p$add(e2)
  out <- shiny::isolate(p$get())
  expect_identical(out$category, c("Espresso-based", "Milk-based"))
  expect_identical(out$notes[1], "edited")

  p$add(.recat("Other", "tea"))
  expect_identical(shiny::isolate(p$get())$category,
                   c("Espresso-based", "Milk-based", "Other"))
})

test_that("merge_recat_rules() handles empty and NULL inputs", {
  e <- .recat("Espresso-based", "espresso")
  expect_identical(merge_recat_rules(NULL, e)$recat_id, e$recat_id)
  expect_identical(merge_recat_rules(e, NULL)$recat_id, e$recat_id)
  expect_equal(nrow(merge_recat_rules(empty_recat_tibble(),
                                      empty_recat_tibble())), 0L)
})
