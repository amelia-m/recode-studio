# Run with: testthat::test_dir("tests/testthat")
#
# Import guard: .check_incoming_recodes() on hand-built frames, so these tests
# do not depend on read_recodes(). Current upstream rule files name the
# provenance column `source_quarter` and add a `norm_spec` column; both are
# known divergences and must be accepted. Anything else off-schema is refused.

source(file.path("..", "..", "R", "ui_helpers.R"))
source(file.path("..", "..", "R", "string_helpers.R"))
source(file.path("..", "..", "R", "mod_import_recodes.R"))

.one_rule <- function() {
  tibble::tibble(
    rule_id = "abc123def456", variable = "city", apply_to_siblings = FALSE,
    sibling_pattern = NA_character_, match_type = "trimmed_ci",
    old_value = "springfeld", new_value = "Springfield", action = "recode",
    notes = NA_character_, author = "someone",
    created_at = "2026-01-01T00:00:00", updated_at = "2026-01-01T00:00:00",
    source_dataset = "example"
  )
}
.schema <- names(empty_recodes_tibble())

test_that("a schema-exact frame is accepted unchanged", {
  chk <- .check_incoming_recodes(.one_rule())
  expect_true(chk$ok)
  expect_false(chk$renamed)
  expect_length(chk$dropped, 0)
  expect_named(chk$df, .schema)
  expect_null(.import_adjust_msg(chk))
})

test_that("a norm_spec-only extra is accepted and dropped", {
  inc <- .one_rule()
  inc$norm_spec <- "squish"
  chk <- .check_incoming_recodes(inc)
  expect_true(chk$ok)
  expect_equal(chk$dropped, "norm_spec")
  expect_named(chk$df, .schema)
  expect_match(.import_adjust_msg(chk), "norm_spec")
})

test_that("source_quarter plus norm_spec is accepted, renamed and dropped", {
  inc <- .one_rule()
  names(inc)[names(inc) == "source_dataset"] <- "source_quarter"
  inc$norm_spec <- NA_character_
  inc <- inc[, rev(names(inc))]  # column order must not matter
  chk <- .check_incoming_recodes(inc)
  expect_true(chk$ok)
  expect_true(chk$renamed)
  expect_equal(chk$dropped, "norm_spec")
  expect_named(chk$df, .schema)
  expect_equal(chk$df$source_dataset, "example")
  msg <- .import_adjust_msg(chk)
  expect_match(msg, "source_quarter")
  expect_match(msg, "norm_spec")
})

test_that("an unknown extra column is refused", {
  inc <- .one_rule()
  inc$reviewer <- "x"
  inc$norm_spec <- "squish"
  chk <- .check_incoming_recodes(inc)
  expect_false(chk$ok)
  expect_null(chk$df)
  expect_equal(chk$extra, "reviewer")
  expect_match(.schema_error_msg(chk), "unexpected column\\(s\\): reviewer")
})

test_that("a missing required column is refused", {
  inc <- .one_rule()
  inc$action <- NULL
  chk <- .check_incoming_recodes(inc)
  expect_false(chk$ok)
  expect_equal(chk$missing, "action")
  expect_match(.schema_error_msg(chk), "missing required column\\(s\\): action")
})

test_that("both source_dataset and source_quarter are refused", {
  inc <- .one_rule()
  inc$source_quarter <- "other"
  chk <- .check_incoming_recodes(inc)
  expect_false(chk$ok)
  expect_false(chk$renamed)
  expect_equal(chk$extra, "source_quarter")
})

test_that("a NULL read is refused without error", {
  chk <- .check_incoming_recodes(NULL)
  expect_false(chk$ok)
})

test_that("the UI builds", {
  expect_no_error(mod_import_recodes_ui("imp"))
})
