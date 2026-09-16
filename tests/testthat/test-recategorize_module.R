# Run with: testthat::test_dir("tests/testthat")
#
# Reactive wiring of the Recategorize tab (R/mod_recategorize.R):
#
#   * the match-count preview is computed on the RECODED frame, because the
#     exported recat script runs after the recode script, and it says which
#     frame it counted on;
#   * the source-variable picker is seeded from the Variable tab's selection
#     only while it is empty, and never overwrites a deliberate selection.
#
# The recode basis is built exactly the way app.R builds `recode_basis_r`:
# list(df, n_rules), with the session's recodes applied by apply_recodes().

source(file.path("..", "..", "R", "ui_helpers.R"))
source(file.path("..", "..", "R", "string_helpers.R"))
source(file.path("..", "..", "R", "data_loader.R"))
source(file.path("..", "..", "R", "recat_helpers.R"))
source(file.path("..", "..", "R", "mod_recategorize.R"))


# --- Fixtures ----------------------------------------------------------------

.orders <- data.frame(
  order_id = c("O1", "O2", "O3", "O4"),
  drink1   = c("expresso", "latte", "expresso", "espresso"),
  drink2   = c("mocha", NA, "latte", NA),
  review   = c("great", "slow service", "fine", "burnt"),
  stringsAsFactors = FALSE)

.meta <- tibble::tibble(
  column                 = c("order_id", "drink1", "drink2", "review"),
  is_free_text_candidate = c(FALSE, TRUE, TRUE, TRUE))

.state <- function() {
  shiny::reactiveValues(data = .orders, meta = .meta,
                        dataset_name = "orders.csv")
}

.recat_proxy <- function() {
  e <- new.env(parent = emptyenv())
  e$rules <- empty_recat_tibble()
  list(
    get = function() e$rules,
    add = function(new_rules) e$rules <- dplyr::bind_rows(e$rules, new_rules),
    set = function(new_rules) e$rules <- new_rules)
}

# One recode rule rewriting the misspelling the recat rule below looks for.
.recode_rules <- function() {
  now <- "2026-01-01T00:00:00"
  dplyr::bind_rows(empty_recodes_tibble(), tibble::tibble(
    rule_id           = recode_rule_id("drink1", "trimmed_ci", "expresso"),
    variable          = "drink1",
    apply_to_siblings = FALSE,
    sibling_pattern   = NA_character_,
    match_type        = "trimmed_ci",
    old_value         = "expresso",
    new_value         = "espresso",
    action            = "recode",
    notes             = NA_character_,
    author            = NA_character_,
    created_at        = now,
    updated_at        = now,
    source_dataset    = "orders.csv"))
}

# Mirrors recode_basis_r in app.R with no normalizations (the fields it adds,
# n_norms and df_before_recodes, are optional to the module).
.basis <- function(d, r) {
  shiny::reactive({
    if (nrow(r) == 0) return(list(df = d, n_rules = 0L))
    list(df = apply_recodes(d, r)$df, n_rules = nrow(r))
  })
}

.preview <- function(session, include) {
  session$setInputs(
    vars = "drink1", out_col = "drink_family", category = "Espresso-based",
    match_type = "literal", include_terms = include, exclude_terms = "",
    priority = 10, notes = "")
  session$flushReact()
  session$setInputs(preview = 1)
}

.text <- function(out) {
  html <- if (is.list(out) && !is.null(out$html)) out$html else out
  gsub("\\s+", " ", paste(as.character(html), collapse = " "))
}


# --- Preview basis -----------------------------------------------------------

test_that("the fixture recode really removes the misspelling", {
  # Guard for the tests below: if this stops holding they test nothing.
  recoded <- apply_recodes(.orders, .recode_rules())$df
  expect_equal(sum(.orders$drink1 == "expresso"), 2L)
  expect_equal(sum(recoded$drink1 == "expresso"), 0L)
})

test_that("preview counts rows AFTER the recodes, and says so", {
  shiny::testServer(
    mod_recategorize_server,
    args = list(shared_state   = .state(),
                recat_proxy    = .recat_proxy(),
                recode_basis_r = .basis(.orders, .recode_rules())), {
    .preview(session, "espresso")
    txt <- .text(output$preview_out)
    # Raw data has one "espresso"; the recode turns both "expresso" rows into
    # it, so the export matches three.
    expect_match(txt, "3 rows match", fixed = TRUE)
    expect_match(txt, "Counted AFTER applying 1 recode rule,", fixed = TRUE)
    expect_false(grepl("before the recodes but", txt, fixed = TRUE))
  })
})

test_that("preview reports the shortfall when a recode rewrote an include term away", {
  shiny::testServer(
    mod_recategorize_server,
    args = list(shared_state   = .state(),
                recat_proxy    = .recat_proxy(),
                recode_basis_r = .basis(.orders, .recode_rules())), {
    .preview(session, "expresso")
    txt <- .text(output$preview_out)
    # The number shown is what the export will produce...
    expect_match(txt, "0 rows match", fixed = TRUE)
    # ...and the raw count it would have reported is called out, not hidden.
    expect_match(txt, "matches 2 rows before the recodes but 0 after",
                 fixed = TRUE)
  })
})

test_that("with no recode rules the preview says the count is on the raw data", {
  shiny::testServer(
    mod_recategorize_server,
    args = list(shared_state   = .state(),
                recat_proxy    = .recat_proxy(),
                recode_basis_r = .basis(.orders, empty_recodes_tibble())), {
    .preview(session, "expresso")
    txt <- .text(output$preview_out)
    expect_match(txt, "2 rows match", fixed = TRUE)
    expect_match(txt, "no recode rules yet, so this is the raw data",
                 fixed = TRUE)
    expect_false(grepl("before the recodes but", txt, fixed = TRUE))
  })
})

test_that("with no recode basis at all the preview falls back to raw and says so", {
  shiny::testServer(
    mod_recategorize_server,
    args = list(shared_state = .state(), recat_proxy = .recat_proxy()), {
    .preview(session, "expresso")
    txt <- .text(output$preview_out)
    expect_match(txt, "2 rows match", fixed = TRUE)
    expect_match(txt, "Counted on the RAW data", fixed = TRUE)
  })
})


# --- Stale preview -----------------------------------------------------------

.stale_note <- "The form changed since this preview. Click Preview again."

test_that("editing a term after a preview marks the preview out of date", {
  shiny::testServer(
    mod_recategorize_server,
    args = list(shared_state   = .state(),
                recat_proxy    = .recat_proxy(),
                recode_basis_r = .basis(.orders, .recode_rules())), {
    # Before any preview there is nothing to be stale.
    session$flushReact()
    expect_null(output$preview_out)

    .preview(session, "espresso")
    fresh <- .text(output$preview_out)
    expect_match(fresh, "3 rows match", fixed = TRUE)
    expect_false(grepl(.stale_note, fresh, fixed = TRUE))
    expect_false(grepl("rs-preview-stale", fresh, fixed = TRUE))

    session$setInputs(include_terms = "latte")
    stale <- .text(output$preview_out)
    expect_match(stale, .stale_note, fixed = TRUE)
    expect_match(stale, "rs-preview-stale", fixed = TRUE)
    # Not recomputed on the edit: the old numbers stay, greyed, until Preview.
    expect_match(stale, "3 rows match", fixed = TRUE)

    session$setInputs(preview = 2)
    again <- .text(output$preview_out)
    expect_false(grepl(.stale_note, again, fixed = TRUE))
    expect_false(grepl("rs-preview-stale", again, fixed = TRUE))
    expect_match(again, "1 rows match", fixed = TRUE)
  })
})

test_that("every field the rule is built from marks the preview stale", {
  changes <- list(
    vars          = c("drink1", "drink2"),
    out_col       = "family",
    category      = "Espresso drinks",
    match_type    = "regex",
    exclude_terms = "decaf",
    priority      = 20)
  for (field in names(changes)) {
    shiny::testServer(
      mod_recategorize_server,
      args = list(shared_state   = .state(),
                  recat_proxy    = .recat_proxy(),
                  recode_basis_r = .basis(.orders, .recode_rules())), {
      .preview(session, "espresso")
      expect_false(grepl(.stale_note, .text(output$preview_out), fixed = TRUE),
                   info = field)
      do.call(session$setInputs, changes[field])
      expect_match(.text(output$preview_out), .stale_note, fixed = TRUE,
                   info = field)
    })
  }
})

test_that("edits that cannot change the result leave the preview current", {
  shiny::testServer(
    mod_recategorize_server,
    args = list(shared_state   = .state(),
                recat_proxy    = .recat_proxy(),
                recode_basis_r = .basis(.orders, .recode_rules())), {
    .preview(session, "espresso")
    # Notes, a trailing blank line, surrounding spaces.
    session$setInputs(notes = "just a note")
    session$setInputs(include_terms = "espresso\n\n  ")
    session$setInputs(category = "  Espresso-based ")
    expect_false(grepl(.stale_note, .text(output$preview_out), fixed = TRUE))

    # Undoing a real change brings the form back to what was previewed.
    session$setInputs(include_terms = "latte")
    expect_match(.text(output$preview_out), .stale_note, fixed = TRUE)
    session$setInputs(include_terms = "espresso")
    expect_false(grepl(.stale_note, .text(output$preview_out), fixed = TRUE))
  })
})

test_that("the no-valid-columns preview goes stale too, and clears on Preview", {
  shiny::testServer(
    mod_recategorize_server,
    args = list(shared_state   = .state(),
                recat_proxy    = .recat_proxy(),
                recode_basis_r = .basis(.orders, .recode_rules())), {
    session$setInputs(
      vars = "not_a_column", out_col = "drink_family",
      category = "Espresso-based", match_type = "literal",
      include_terms = "espresso", exclude_terms = "", priority = 10,
      notes = "")
    session$setInputs(preview = 1)
    expect_match(.text(output$preview_out),
                 "No valid source variables selected.", fixed = TRUE)

    session$setInputs(vars = "drink1")
    expect_match(.text(output$preview_out), .stale_note, fixed = TRUE)

    session$setInputs(preview = 2)
    txt <- .text(output$preview_out)
    expect_false(grepl(.stale_note, txt, fixed = TRUE))
    expect_match(txt, "3 rows match", fixed = TRUE)
  })
})


# --- Source-variable seeding -------------------------------------------------

# MockShinySession drops update*Input messages, so record what the picker
# observer asks the client to select instead.
.record_selectize <- function(env = parent.frame()) {
  calls <- new.env(parent = emptyenv())
  calls$selected <- list()
  testthat::local_mocked_bindings(
    updateSelectizeInput = function(session, inputId, label = NULL,
                                    choices = NULL, selected = NULL, ...) {
      if (identical(inputId, "vars")) {
        calls$selected[[length(calls$selected) + 1L]] <-
          if (is.null(selected)) character(0) else selected
      }
      invisible(NULL)
    },
    .package = "shiny", .env = env)
  calls
}
.last_selected <- function(calls) calls$selected[[length(calls$selected)]]

test_that("the vars picker seeds from the Variable tab's selection when empty", {
  calls <- .record_selectize()
  seed  <- shiny::reactiveVal("review")
  shiny::testServer(
    mod_recategorize_server,
    args = list(shared_state = .state(), recat_proxy = .recat_proxy(),
                seed_var_r = seed), {
    session$flushReact()
    expect_gt(length(calls$selected), 0)
    expect_equal(.last_selected(calls), "review")
    # Free-text candidates are offered first.
    expect_equal(var_choices(), c("drink1", "drink2", "review", "order_id"))
  })
})

test_that("the vars picker never overwrites a deliberate multi-select", {
  calls <- .record_selectize()
  seed  <- shiny::reactiveVal("review")
  shiny::testServer(
    mod_recategorize_server,
    args = list(shared_state = .state(), recat_proxy = .recat_proxy(),
                seed_var_r = seed), {
    session$flushReact()
    session$setInputs(vars = c("drink1", "drink2"))
    n_before <- length(calls$selected)

    seed("order_id")          # a different variable is picked elsewhere
    session$flushReact()

    # The observer read the seed on its first (empty) run, so it re-runs on
    # the change; what it sends must still be the user's set.
    expect_gt(length(calls$selected), n_before)
    expect_setequal(.last_selected(calls), c("drink1", "drink2"))
    expect_setequal(input$vars, c("drink1", "drink2"))
    expect_false(any(vapply(calls$selected[-seq_len(n_before)],
                            function(s) "order_id" %in% s, logical(1))))
  })
})

test_that("a seed that is not a column of the data is ignored", {
  calls <- .record_selectize()
  shiny::testServer(
    mod_recategorize_server,
    args = list(shared_state = .state(), recat_proxy = .recat_proxy(),
                seed_var_r = shiny::reactiveVal("not_a_column")), {
    session$flushReact()
    expect_length(.last_selected(calls), 0)
  })
})

test_that("the vars picker works with no seed at all", {
  calls <- .record_selectize()
  shiny::testServer(
    mod_recategorize_server,
    args = list(shared_state = .state(), recat_proxy = .recat_proxy()), {
    session$flushReact()
    expect_gt(length(calls$selected), 0)
    expect_length(.last_selected(calls), 0)
    expect_length(var_choices(), 4)
  })
})


# --- Rules table empty state -------------------------------------------------

test_that("an empty rule list is plain text, and the table appears once rules exist", {
  store <- shiny::reactiveVal(empty_recat_tibble())
  reactive_proxy <- list(get = function() store(),
                         add = function(r) store(dplyr::bind_rows(store(), r)),
                         set = function(r) store(r))
  shiny::testServer(
    mod_recategorize_server,
    args = list(shared_state = .state(), recat_proxy = reactive_proxy), {
    session$flushReact()
    empty <- .text(output$rules_tbl_ui)
    expect_match(empty, "No recategorization rules yet.", fixed = TRUE)
    # No one-cell DT with a literal "message" header.
    expect_false(grepl("datatables", empty, fixed = TRUE))

    session$setInputs(
      vars = "drink1", out_col = "drink_family", category = "Espresso-based",
      match_type = "literal", include_terms = "espresso", exclude_terms = "",
      priority = 10, notes = "")
    session$setInputs(add_rule = 1)
    expect_equal(nrow(store()), 1L)

    filled <- .text(output$rules_tbl_ui)
    expect_false(grepl("No recategorization rules yet.", filled, fixed = TRUE))
    expect_match(filled, session$ns("rules_tbl"), fixed = TRUE)
  })
})

test_that("the Recategorize UI builds", {
  ui <- as.character(mod_recategorize_ui("recat"))
  expect_true(grepl('id="recat-rules_tbl_ui"', ui, fixed = TRUE))
})
