# Run with: testthat::test_dir("tests/testthat")
#
# Shared variable selection (R/mod_variable_picker.R): the tab-2 table and the
# "Recoding:" banner both sync to one `current_var`, and neither pushes a value
# the other already shows (which would make them ping-pong).

source(file.path("..", "..", "R", "ui_helpers.R"))
source(file.path("..", "..", "R", "data_loader.R"))
source(file.path("..", "..", "R", "mod_variable_picker.R"))

# --- Fixtures ----------------------------------------------------------------

.meta <- tibble::tibble(
  column                 = c("order_id", "drink1", "drink2", "city", "size"),
  group                  = c("order", "(none)", "(none)", "(none)", "(none)"),
  text_kind              = c("-", "short", "short", "short", "-"),
  n_unique               = c(30L, 12L, 11L, 14L, 3L),
  na_share               = c(0, 0, .2, 0, 0),
  median_len             = c(3, 7, 7, 8, 5),
  median_words           = c(1, 1, 1, 1, 1),
  is_free_text_candidate = c(FALSE, TRUE, TRUE, TRUE, FALSE)
)

.state <- function(meta = .meta) {
  shiny::reactiveValues(data = NULL, meta = meta, dataset_name = "cafe.csv")
}

# Records every call to shiny::updateSelectizeInput() instead of sending it.
.record_selectize_updates <- function(env = parent.frame()) {
  calls <- new.env()
  calls$n <- 0L
  calls$last <- NULL
  calls$all <- list()
  testthat::local_mocked_bindings(
    updateSelectizeInput = function(session, inputId, ...) {
      calls$n <- calls$n + 1L
      calls$last <- list(inputId = inputId, ...)
      calls$all[[length(calls$all) + 1L]] <- calls$last
      invisible(NULL)
    },
    .package = "shiny", .env = env)
  calls
}

# --- Pure helpers ------------------------------------------------------------

test_that("variable_choices() lists free-text candidates first, grouped", {
  ch <- variable_choices(.meta)
  expect_identical(names(ch), c("Free-text candidates", "Other columns"))
  expect_identical(variable_choice_values(ch),
                   c("drink1", "drink2", "city", "order_id", "size"))
  # Labels carry column_group() only when the column has a group.
  expect_identical(names(ch[["Other columns"]]), c("order_id (order)", "size"))
  expect_identical(names(ch[["Free-text candidates"]]),
                   c("drink1", "drink2", "city"))
})

test_that("variable_choices() is empty without metadata", {
  expect_length(variable_choices(NULL), 0)
  expect_length(variable_choice_values(variable_choices(NULL)), 0)
})

# --- Table picker ------------------------------------------------------------

test_that("a table selection is returned as the variable name", {
  shiny::testServer(mod_variable_picker_server,
    args = list(shared_state = .state()), {
      session$setInputs(free_text_only = TRUE, tbl_rows_selected = 2L)
      expect_identical(session$getReturned()(), "drink2")
      session$setInputs(tbl_rows_selected = NULL)
      expect_null(session$getReturned()())
    })
})

test_that("an external variable equal to the selection is not re-pushed", {
  ext <- shiny::reactiveVal(NULL)
  shiny::testServer(mod_variable_picker_server,
    args = list(shared_state = .state(), external_var_r = ext), {
      session$setInputs(free_text_only = TRUE, tbl_rows_selected = 1L)
      ext("drink1")
      session$flushReact()
      expect_null(pending_sel())

      ext("city")
      session$flushReact()
      expect_identical(pending_sel(), "city")

      # The browser confirms the row: the pending request clears.
      session$setInputs(tbl_rows_selected = 3L)
      expect_null(pending_sel())
      expect_identical(session$getReturned()(), "city")
    })
})

test_that("an external variable hidden by the free-text filter lifts it", {
  ext <- shiny::reactiveVal(NULL)
  shiny::testServer(mod_variable_picker_server,
    args = list(shared_state = .state(), external_var_r = ext), {
      session$setInputs(free_text_only = TRUE, tbl_rows_selected = 1L)
      ext("size")
      session$flushReact()
      expect_identical(pending_sel(), "size")
      # updateCheckboxInput only messages the browser; mimic its reply.
      session$setInputs(free_text_only = FALSE)
      expect_identical(var_tbl()$variable,
                       c("order_id", "drink1", "drink2", "city", "size"))
    })
})

# --- Banner ------------------------------------------------------------------

test_that("banner starts on the first free-text candidate and loads choices", {
  calls <- .record_selectize_updates()
  cv <- shiny::reactiveVal(NULL)
  shiny::testServer(mod_variable_banner_server,
    args = list(shared_state = .state(), current_var = cv), {
      session$flushReact()
      expect_identical(cv(), "drink1")
      expect_true(calls$n >= 1L)
      with_choices <- Filter(function(cl) !is.null(cl$choices), calls$all)
      expect_length(with_choices, 1L)
      expect_identical(variable_choice_values(with_choices[[1]]$choices),
                       c("drink1", "drink2", "city", "order_id", "size"))
      expect_identical(with_choices[[1]]$selected, "drink1")
    })
})

test_that("choosing in the banner updates current_var", {
  .record_selectize_updates()
  cv <- shiny::reactiveVal(NULL)
  shiny::testServer(mod_variable_banner_server,
    args = list(shared_state = .state(), current_var = cv), {
      session$setInputs(var = "city")
      expect_identical(cv(), "city")
      # selectize sends "" while it initialises: ignored.
      session$setInputs(var = "")
      expect_identical(cv(), "city")
    })
})

test_that("banner does not push a value it already shows", {
  calls <- .record_selectize_updates()
  cv <- shiny::reactiveVal(NULL)
  shiny::testServer(mod_variable_banner_server,
    args = list(shared_state = .state(), current_var = cv), {
      session$setInputs(var = "drink1")
      session$flushReact()
      n0 <- calls$n

      # Same value again, from either side: no update is sent.
      session$setInputs(var = "drink1")
      cv("drink1")
      session$flushReact()
      expect_identical(calls$n, n0)

      # A different value from elsewhere (tab 2): exactly one update.
      cv("drink2")
      session$flushReact()
      expect_identical(calls$n, n0 + 1L)
      expect_identical(calls$last$selected, "drink2")

      # The browser echoes it back: nothing more is sent.
      session$setInputs(var = "drink2")
      expect_identical(calls$n, n0 + 1L)
      expect_identical(cv(), "drink2")
    })
})

# --- Full wiring, as app.R calls it -----------------------------------------

test_that("table and banner both drive the shared selection", {
  .record_selectize_updates()
  st <- .state()
  shiny::testServer(function(input, output, session) {
    selected <- variable_selection_server(st, picker_id = "picker",
                                          banner_id = "banner")
  }, {
    session$flushReact()
    expect_identical(selected(), "drink1")   # default on data load

    # Tab-2 table selection -> shared selection.
    session$setInputs(`picker-free_text_only` = TRUE,
                      `picker-tbl_rows_selected` = 3L)
    expect_identical(selected(), "city")

    # Banner -> shared selection.
    session$setInputs(`banner-var` = "drink2")
    expect_identical(selected(), "drink2")
  })
})
