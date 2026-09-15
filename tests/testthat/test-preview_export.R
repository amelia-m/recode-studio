# Run with: testthat::test_dir("tests/testthat")
#
# Preview & export diff table: preview_diff_table() joins each rule to its
# apply_recodes() counts. The live failure it guards: with rules in session but
# no data loaded, the summary is an empty tibble, the old lookup produced NULL,
# the count column vanished, and DT::formatStyle() on it threw. The table must
# always carry all three count columns, NA when there is nothing to count.

source(file.path("..", "..", "R", "ui_helpers.R"))
source(file.path("..", "..", "R", "string_helpers.R"))
source(file.path("..", "..", "R", "mod_preview_export.R"))

.rules <- function() {
  tibble::tibble(
    rule_id           = c("r1", "r2", "r3"),
    variable          = c("drink1", "drink1", "city"),
    apply_to_siblings = c(FALSE, FALSE, FALSE),
    sibling_pattern   = NA_character_,
    match_type        = "trimmed_ci",
    old_value         = c("latte", "Latte", "springfeld"),
    new_value         = c("Latte", "Latte", "Springfield"),
    action            = "recode"
  )
}

.summ <- tibble::tibble(
  rule_id        = c("r2", "r1"),
  cells_matched  = c(6L, 12L),
  cells_changed  = c(4L, 10L),
  cells_shadowed = c(2L, 0L)
)

.count_cols <- c("cells_matched", "cells_changed", "cells_shadowed")

test_that("counts are joined by rule_id, not by row order", {
  out <- preview_diff_table(.rules(), .summ)
  expect_equal(out$cells_matched,  c(12L, 6L, NA_integer_))
  expect_equal(out$cells_changed,  c(10L, 4L, NA_integer_))
  expect_equal(out$cells_shadowed, c(0L,  2L, NA_integer_))
})

test_that("an empty summary yields all-NA counts rather than failing", {
  # The no-data-loaded path: diff_r() hands back tibble::tibble().
  for (s in list(tibble::tibble(), .summ[0, ], NULL)) {
    out <- preview_diff_table(.rules(), s)
    expect_equal(nrow(out), 3L)
    expect_true(all(.count_cols %in% names(out)))
    for (nm in .count_cols) expect_true(all(is.na(out[[nm]])))
  }
})

test_that("a summary missing a column degrades to NA for that column only", {
  out <- preview_diff_table(.rules(), .summ[, c("rule_id", "cells_matched",
                                                 "cells_changed")])
  expect_true(all(is.na(out$cells_shadowed)))
  expect_equal(out$cells_matched, c(12L, 6L, NA_integer_))
})

test_that("the table carries the display columns in order", {
  out <- preview_diff_table(.rules(), .summ)
  expect_named(out, c("variable", "old_value", "new_value", "action",
                      "apply_to_siblings", "sibling_pattern", .count_cols))
})

test_that("the styled datatable builds with no data loaded", {
  df <- preview_diff_table(.rules(), tibble::tibble())
  expect_no_error(
    DT::datatable(df, rownames = FALSE,
                  options = list(columnDefs = dt_truncate_defs())) |>
      DT::formatStyle("action",
        backgroundColor = DT::styleEqual(c("delete", "recode"),
                                         c("#fce4e4", "#e6f4ea"))) |>
      DT::formatStyle("cells_changed",
        backgroundColor = DT::styleEqual(c(0L), c("#eeeeee")))
  )
})

test_that("counts agree with apply_recodes() on real data", {
  d <- tibble::tibble(drink1 = c("latte", "LATTE ", "mocha"))
  r <- empty_recodes_tibble() |>
    dplyr::bind_rows(tibble::tibble(
      rule_id = "a", variable = "drink1", apply_to_siblings = FALSE,
      match_type = "trimmed_ci", old_value = "latte", new_value = "Latte",
      action = "recode"))
  res <- apply_recodes(d, r)
  out <- preview_diff_table(r, res$summary)
  expect_equal(out$cells_matched, 2L)
  expect_equal(out$cells_changed, 2L)
  expect_equal(out$cells_shadowed, 0L)
})

test_that("the UI builds", {
  expect_no_error(mod_preview_export_ui("pe"))
})
