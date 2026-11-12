# =============================================================================
# Variable picker + "Recoding:" banner
# =============================================================================
# Two controls choose the variable every per-variable tab works on:
#
#   * the DT table on tab 2 (filterable, sortable, shows column metadata);
#   * the selectize in the "Recoding:" banner above the tabs, always visible,
#     so the variable can be changed from any tab.
#
# Neither control owns the selection. server() in app.R holds a single
# `current_var` reactiveVal, and both controls sync to it:
#
#   table row click  -> picker's returned reactive -> current_var -> banner
#   banner choice    -> current_var -> picker's external_var_r -> table row
#
# Every hop is guarded with "only if it differs", so the two controls cannot
# ping-pong an identical value back and forth.
# =============================================================================

# --- Pure helpers ------------------------------------------------------------

#' Choices for the banner's variable selectize.
#'
#' Free-text candidates first (the columns most likely to need recoding), then
#' every other column, as two option groups. Each label carries the column's
#' group from `column_group()` when it has one, e.g. `"drink_size (drink)"`.
#'
#' @param meta column metadata from `build_meta()`, or NULL.
#' @return a named list of named character vectors (option groups), or an
#'   empty character vector when there is no metadata.
variable_choices <- function(meta) {
  if (is.null(meta) || nrow(meta) == 0) return(character(0))
  label <- function(cols) {
    if (length(cols) == 0) return(character(0))
    grp <- column_group(cols)
    stats::setNames(cols, ifelse(grp == "(none)", cols,
                                 sprintf("%s (%s)", cols, grp)))
  }
  if ("is_free_text_candidate" %in% names(meta)) {
    ft  <- !is.na(meta$is_free_text_candidate) & meta$is_free_text_candidate
    out <- list(`Free-text candidates` = label(meta$column[ft]),
                `Other columns`        = label(meta$column[!ft]))
    out[vapply(out, length, integer(1)) > 0]
  } else {
    list(`Columns` = label(meta$column))
  }
}

#' Flatten `variable_choices()` output to the plain column names, in order.
variable_choice_values <- function(choices) {
  unlist(lapply(choices, unname), use.names = FALSE) %||% character(0)
}

# TRUE for a usable single variable name (selectize sends "" while starting).
.is_var_name <- function(v) {
  !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v)
}


# --- Tab 2: table picker -------------------------------------------------------

mod_variable_picker_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = FALSE,
    fill        = FALSE,
    card_header_help(
      "Choose a variable",
      shiny::p("Pick the column you want to clean. Every tab after this one ",
               "works on the column selected here."),
      shiny::p("\"Free-text candidates\" hides IDs, dates, numbers and short ",
               "fixed lists, which rarely need recoding. Untick it to see ",
               "every column. You can also switch columns at any time from ",
               "the \"Recoding:\" bar at the top of the page.")
    ),
    bslib::card_body(
      fillable = FALSE,
      shiny::checkboxInput(
        ns("free_text_only"),
        label = "Free-text candidates only (recommended)",
        value = TRUE
      ),
      shiny::tags$small(shiny::tags$em(
        "Click a row to select. Sort with the headers; filter with the search box above each column."
      )),
      shiny::br(), shiny::br(),
      # Plain text, not a one-column datatable (see empty_state()).
      shiny::uiOutput(ns("tbl_empty")),
      DT::DTOutput(ns("tbl"))
    )
  )
}

#' Variable picker table server.
#'
#' @param shared_state reactiveValues(data, meta, dataset_name). Read only.
#' @param external_var_r optional reactive giving the variable chosen elsewhere
#'   (the app's `current_var`). When it changes to a variable other than the
#'   one selected in the table, the table selects that row. The request is
#'   held as pending until the table confirms it, because the table may not
#'   have rendered yet (tab 2 is hidden until visited, and a DT proxy command
#'   sent to a table that is not drawn does nothing).
#' @return reactive giving the variable name selected in the table, or NULL.
mod_variable_picker_server <- function(id, shared_state, external_var_r = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    var_tbl <- shiny::reactive({
      meta <- shared_state$meta
      if (is.null(meta) || nrow(meta) == 0) return(NULL)
      m <- meta
      if (isTRUE(input$free_text_only)) {
        m <- m[m$is_free_text_candidate, , drop = FALSE]
      }
      tibble::tibble(
        variable   = m$column,
        group      = m$group,
        kind       = m$text_kind,        # short / long / -
        n_unique   = m$n_unique,
        na_share   = round(m$na_share, 3),
        median_len = m$median_len,
        words      = m$median_words,
        free_text  = m$is_free_text_candidate
      )
    })

    external_now <- function() {
      if (is.null(external_var_r)) return(NULL)
      shiny::isolate(tryCatch(external_var_r(), error = function(e) NULL))
    }

    # Two different empty states, which the old single message confused: with
    # no dataset there is nothing to pick from, but with a dataset loaded an
    # empty table means the free-text filter hid every column, and telling the
    # user to go back to the Data tab sent them the wrong way.
    output$tbl_empty <- shiny::renderUI({
      meta <- shared_state$meta
      if (is.null(meta) || nrow(meta) == 0) {
        return(empty_state("No dataset loaded. Load one on the Data tab."))
      }
      tb <- var_tbl()
      if (!is.null(tb) && nrow(tb) > 0) return(NULL)
      empty_state(paste0(
        "No free-text candidates in this dataset. Untick \"Free-text ",
        "candidates only\" above to see all ", nrow(meta), " columns."))
    })

    output$tbl <- DT::renderDT({
      df <- var_tbl()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      # Draw with the shared selection already applied, so a table that is
      # rendered late (or re-rendered by the filter checkbox) shows the
      # variable the rest of the app is working on.
      v   <- external_now()
      idx <- if (.is_var_name(v)) which(df$variable == v) else integer(0)
      DT::datatable(
        df,
        rownames  = FALSE,
        selection = list(mode = "single",
                         selected = if (length(idx) > 0) idx[1] else NULL),
        filter    = "top",
        options   = list(pageLength = 25, autoWidth = FALSE,
                         order = list(list(0, "asc")))
      )
    })

    # Selected variable. The table is read with isolate(): between the
    # filter changing var_tbl() and the browser redrawing, the selected row
    # index still refers to the OLD table, and reading it against the new one
    # would report the wrong column. The browser always resends the selection
    # after a redraw, which is what re-runs this.
    selected_r <- shiny::reactive({
      r  <- input$tbl_rows_selected
      df <- shiny::isolate(var_tbl())
      if (is.null(df) || length(r) == 0 || r[1] > nrow(df)) return(NULL)
      df$variable[r[1]]
    })

    # A variable requested from outside, waiting for the table to show it.
    pending_sel <- shiny::reactiveVal(NULL)

    shiny::observe({
      pv <- pending_sel()
      df <- var_tbl()
      input$tbl_rows_current   # retry after every redraw of the table
      if (is.null(pv) || is.null(df)) return()
      if (identical(selected_r(), pv)) {
        pending_sel(NULL)
        return()
      }
      idx <- which(df$variable == pv)
      if (length(idx) == 0) return()   # table not showing it (yet)
      DT::dataTableProxy("tbl", session = session) |>
        DT::selectRows(idx[1])
    })

    if (!is.null(external_var_r)) {
      shiny::observeEvent(external_var_r(), {
        v <- external_var_r()
        if (!.is_var_name(v)) return()
        # Already selected here: nothing to push. This guard is what stops
        # the table and the banner re-sending the same value to each other.
        if (identical(shiny::isolate(selected_r()), v)) {
          pending_sel(NULL)
          return()
        }
        df <- shiny::isolate(var_tbl())
        meta_cols <- shiny::isolate(shared_state$meta$column)
        # Chosen in the banner but hidden by the free-text filter: show every
        # column so the row can be selected.
        if (!is.null(df) && !(v %in% df$variable) && v %in% meta_cols &&
            isTRUE(shiny::isolate(input$free_text_only))) {
          shiny::updateCheckboxInput(session, "free_text_only", value = FALSE)
        }
        pending_sel(v)
      }, ignoreNULL = TRUE)
    }

    selected_r
  })
}


# --- Banner: change the variable from any tab --------------------------------

mod_variable_banner_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "alert alert-info d-flex flex-wrap align-items-center gap-2 rs-var-banner",
    style = "padding: .35em .8em; margin: .6em 0;",
    shiny::tags$style(shiny::HTML(
      ".rs-var-banner .form-group { margin-bottom: 0; }")),
    shiny::tags$label(`for` = ns("var"), class = "mb-0",
                      shiny::strong("Recoding:")),
    # Rendered statically, not from renderUI: an update sent to an input that
    # does not exist in the browser yet is dropped, not queued, so a picker
    # built inside renderUI can open with no choices.
    shiny::div(
      style = "min-width: 220px; flex: 0 1 340px;",
      shiny::selectizeInput(ns("var"), NULL, choices = NULL, width = "100%",
                            options = list(
                              placeholder = "Load data on tab 1, then pick a variable"))
    ),
    shiny::uiOutput(ns("note"), inline = TRUE),
    help_btn(
      title = "Recoding",
      shiny::p("The column that the Normalize, Browse values, Clusters, ",
               "Spellcheck and Text analysis tabs are working on. Change it here from any ",
               "tab, or pick a row on tab 2; the two stay in step."),
      shiny::p("Rules you have already made are kept when you switch.")
    )
  )
}

#' Banner selectize server.
#'
#' @param shared_state reactiveValues(data, meta, dataset_name). Read only.
#' @param current_var the app's shared selection, a `shiny::reactiveVal`.
#'   This module both reads and writes it.
mod_variable_banner_server <- function(id, shared_state, current_var) {
  shiny::moduleServer(id, function(input, output, session) {

    choices_r <- shiny::reactive(variable_choices(shared_state$meta))

    # New data: refresh the choices. If the current variable is not in the
    # new data (or nothing is selected yet), start on the first free-text
    # candidate, which is what tab 2 used to auto-select.
    shiny::observe({
      ch   <- choices_r()
      vals <- variable_choice_values(ch)
      cur  <- shiny::isolate(current_var())
      if (length(vals) > 0 && !(.is_var_name(cur) && cur %in% vals)) {
        cur <- vals[1]
        current_var(cur)
      }
      shiny::updateSelectizeInput(session, "var", choices = ch,
                                  selected = if (.is_var_name(cur)) cur
                                             else character(0))
    })

    # Banner -> shared selection.
    shiny::observeEvent(input$var, {
      v <- input$var
      if (!.is_var_name(v)) return()
      if (!identical(v, current_var())) current_var(v)
    })

    # Shared selection -> banner, for changes made on tab 2.
    shiny::observeEvent(current_var(), {
      v <- current_var()
      if (!.is_var_name(v) || identical(input$var, v)) return()
      shiny::updateSelectizeInput(session, "var", selected = v)
    }, ignoreNULL = TRUE)

    output$note <- shiny::renderUI({
      v <- current_var()
      if (!.is_var_name(v)) return(NULL)
      grp <- column_group(v)
      if (grp == "(none)") return(NULL)
      shiny::tags$small(sprintf("group: %s", grp))
    })
  })
}


# --- Wiring ----------------------------------------------------------------------

#' Own the shared variable selection and connect both controls to it.
#'
#' Call from server() (not inside a module). Creates the single source of
#' truth, `current_var`, and syncs it with the tab-2 table and the banner:
#'
#'   table row  -> current_var  (only when it differs)
#'   banner     -> current_var  (only when it differs; in the banner module)
#'   current_var -> table and banner (each skips a value it already shows)
#'
#' @param shared_state reactiveValues(data, meta, dataset_name).
#' @param picker_id,banner_id module ids used in the UI.
#' @return a reactive giving the selected variable name, or NULL.
variable_selection_server <- function(shared_state,
                                      picker_id = "variable_picker",
                                      banner_id = "variable_banner") {
  current_var    <- shiny::reactiveVal(NULL)
  selected_var_r <- shiny::reactive(current_var())

  table_var_r <- mod_variable_picker_server(picker_id, shared_state,
                                            external_var_r = selected_var_r)
  shiny::observeEvent(table_var_r(), {
    v <- table_var_r()
    if (.is_var_name(v) && !identical(v, current_var())) current_var(v)
  }, ignoreNULL = TRUE)

  mod_variable_banner_server(banner_id, shared_state, current_var)

  selected_var_r
}
