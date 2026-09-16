# Run with: testthat::test_dir("tests/testthat")
#
# Recodes table edits (R/mod_recode_editor.R).
#
# `rule_id` hashes (variable, match_type, old_value). An edit to old_value or
# match_type used to keep the OLD id, so a rule minted later for the original
# value collided with the edited rule and the proxy's in-place merge silently
# overwrote it. A key edit now recomputes the id, and an edit that would give
# a rule the id of ANOTHER rule is refused with the table left unchanged.

source(file.path("..", "..", "R", "ui_helpers.R"))
source(file.path("..", "..", "R", "string_helpers.R"))
source(file.path("..", "..", "R", "recat_helpers.R"))
source(file.path("..", "..", "R", "rule_proxies.R"))
source(file.path("..", "..", "R", "mod_value_table.R"))
source(file.path("..", "..", "R", "mod_recode_editor.R"))

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
    source_dataset    = "cafe",
    norm_spec         = "city=squish"
  )
}

.rules <- function() {
  dplyr::bind_rows(
    .rule("city",   "trimmed_ci", "portand", "Portland"),
    .rule("city",   "trimmed_ci", "seatle",  "Seattle"),
    .rule("drink1", "exact",      "expresso", "espresso"))
}

# 0-based DT column index of an editor column.
.col <- function(name) match(name, .RECODE_EDITOR_COLS) - 1L

# Run `code` against the editor module with a real rules proxy. Notifications
# are captured instead of shown.
.with_editor <- function(rules, code) {
  rv    <- shiny::reactiveValues(rules = rules)
  norms <- shiny::reactiveVal(empty_normalizations_tibble())
  proxy <- make_rules_proxy(rv, norms_r = function() norms(),
                            data_cols_r = function() c("city", "drink1"))
  notes <- new.env()
  notes$msgs  <- character(0)
  notes$types <- character(0)
  testthat::local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      notes$msgs  <- c(notes$msgs, paste(as.character(ui), collapse = ""))
      notes$types <- c(notes$types, type)
      invisible("id")
    },
    .package = "shiny")
  st <- shiny::reactiveValues(data = NULL, meta = NULL, dataset_name = "cafe",
                              source_meta = NULL)
  shiny::testServer(mod_recode_editor_server,
    args = list(shared_state = st, rules_proxy = proxy), {
      code(session, rv, proxy, notes)
    })
}

.edit <- function(session, row, col, value) {
  session$setInputs(tbl_cell_edit = list(row = row, col = .col(col),
                                         value = value))
}


# --- The pure edit ---------------------------------------------------------------

test_that("apply_recode_edit recomputes the id on key edits only", {
  r <- .rules()
  res <- apply_recode_edit(r, 1, "old_value", "portlnd", now = "T1")
  expect_identical(res$status, "ok")
  expect_identical(res$rules$rule_id[1],
                   recode_rule_id("city", "trimmed_ci", "portlnd"))
  expect_identical(res$rules$updated_at[1], "T1")
  expect_identical(res$rules$rule_id[2:3], r$rule_id[2:3])

  res <- apply_recode_edit(r, 3, "match_type", "exact_ci", now = "T2")
  expect_identical(res$rules$rule_id[3],
                   recode_rule_id("drink1", "exact_ci", "expresso"))

  res <- apply_recode_edit(r, 2, "new_value", "SEATTLE", now = "T3")
  expect_identical(res$rules$rule_id, r$rule_id)
  expect_identical(res$rules$new_value[2], "SEATTLE")

  # Editing a key cell to the value it already holds changes no id.
  res <- apply_recode_edit(r, 1, "old_value", "portand", now = "T4")
  expect_identical(res$status, "ok")
  expect_identical(res$rules$rule_id, r$rule_id)
})

test_that("apply_recode_edit refuses a key collision and names the other rule", {
  r <- .rules()
  res <- apply_recode_edit(r, 1, "old_value", "seatle")
  expect_identical(res$status, "refused")
  expect_identical(res$rules, r)
  expect_match(res$message, "rule 2", fixed = TRUE)
  expect_match(res$message, r$rule_id[2], fixed = TRUE)
  expect_match(res$message, "'seatle'", fixed = TRUE)
  # Empty and missing values are named plainly.
  r2 <- dplyr::bind_rows(r, .rule("city", "trimmed_ci", "", "Unknown"))
  res <- apply_recode_edit(r2, 1, "old_value", "   ")
  expect_identical(res$status, "ok")   # "   " is a different key from ""
  res <- apply_recode_edit(r2, 1, "old_value", "")
  expect_identical(res$status, "refused")
  expect_match(res$message, "(empty cells)", fixed = TRUE)
})

test_that("apply_recode_edit rejects bad enums and ignores locked columns", {
  r <- .rules()
  expect_identical(apply_recode_edit(r, 1, "match_type", "fuzzy")$status, "refused")
  expect_identical(apply_recode_edit(r, 1, "action", "drop")$status, "refused")
  expect_identical(apply_recode_edit(r, 1, "variable", "town")$status, "ignored")
  expect_identical(apply_recode_edit(r, 1, "rule_id", "x")$status, "ignored")
  expect_identical(apply_recode_edit(r, 9, "notes", "x")$status, "ignored")
  res <- apply_recode_edit(r, 1, "apply_to_siblings", "true")
  expect_true(res$rules$apply_to_siblings[1])
  expect_identical(res$rules$rule_id, r$rule_id)
})


# --- The module ------------------------------------------------------------------

test_that("an old_value edit in the table recomputes the rule id", {
  .with_editor(.rules(), function(session, rv, proxy, notes) {
    .edit(session, 1, "old_value", "portlnd")
    r <- shiny::isolate(rv$rules)
    expect_identical(r$old_value[1], "portlnd")
    expect_identical(r$rule_id[1],
                     recode_rule_id("city", "trimmed_ci", "portlnd"))
    expect_false(identical(r$updated_at[1], "2026-01-01T00:00:00"))
    # set(), not add(): the stamp is kept, not recomputed (the stub
    # normalizations are empty, which would have stamped "").
    expect_identical(r$norm_spec, rep("city=squish", 3))
    expect_length(notes$msgs, 0L)
  })
})

test_that("a match_type edit in the table recomputes the rule id", {
  .with_editor(.rules(), function(session, rv, proxy, notes) {
    .edit(session, 3, "match_type", "exact_ci")
    r <- shiny::isolate(rv$rules)
    expect_identical(r$rule_id[3],
                     recode_rule_id("drink1", "exact_ci", "expresso"))
  })
})

test_that("a colliding edit is refused with an error and the table unchanged", {
  before <- .rules()
  .with_editor(before, function(session, rv, proxy, notes) {
    .edit(session, 1, "old_value", "seatle")
    expect_identical(shiny::isolate(rv$rules), before)
    expect_identical(notes$types, "error")
    expect_match(notes$msgs, "rule 2", fixed = TRUE)
    expect_match(notes$msgs, before$rule_id[2], fixed = TRUE)
  })
})

test_that("a later rule for the ORIGINAL value no longer overwrites the edited one", {
  .with_editor(.rules(), function(session, rv, proxy, notes) {
    .edit(session, 1, "old_value", "portlnd")
    # Browse values later mints a rule for the original "portand".
    proxy$add(.rule("city", "trimmed_ci", "portand", "PORTLAND"))
    r <- shiny::isolate(rv$rules)
    expect_equal(nrow(r), 4L)
    expect_identical(r$old_value, c("portlnd", "seatle", "expresso", "portand"))
    expect_identical(r$new_value[1], "Portland")   # the edited rule survives
    expect_false(anyDuplicated(r$rule_id) > 0)
  })
})

test_that("a notes-only edit keeps the id and the position", {
  before <- .rules()
  .with_editor(before, function(session, rv, proxy, notes) {
    .edit(session, 2, "notes", "typo seen in March")
    r <- shiny::isolate(rv$rules)
    expect_identical(r$rule_id, before$rule_id)
    expect_identical(r$old_value, before$old_value)
    expect_identical(r$notes[2], "typo seen in March")
    expect_false(identical(r$updated_at[2], before$updated_at[2]))
    expect_identical(r$updated_at[c(1, 3)], before$updated_at[c(1, 3)])
  })
})

test_that("an invalid enum edit is refused with an error", {
  before <- .rules()
  .with_editor(before, function(session, rv, proxy, notes) {
    .edit(session, 1, "match_type", "fuzzy")
    expect_identical(shiny::isolate(rv$rules), before)
    expect_identical(notes$types, "error")
    expect_match(notes$msgs, "Invalid match_type", fixed = TRUE)
  })
})
