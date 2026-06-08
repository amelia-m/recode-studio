# =============================================================================
# Variable picker
# =============================================================================
# DT-table picker (filterable + sortable) for the column to clean.
# Single-row selection drives every other tab.
# Exposes a reactive returning the currently-selected variable name.
# =============================================================================

mod_variable_picker_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = FALSE,
    fill        = FALSE,
    bslib::card_header("Choose a variable"),
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
      DT::DTOutput(ns("tbl"))
    )
  )
}

mod_variable_picker_server <- function(id, shared_state) {
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
        n_unique   = m$n_unique,
        na_share   = round(m$na_share, 3),
        median_len = m$median_len,
        free_text  = m$is_free_text_candidate
      )
    })

    output$tbl <- DT::renderDT({
      df <- var_tbl()
      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(
          tibble::tibble(message = "No dataset loaded. Use the Data tab."),
          rownames = FALSE, options = list(dom = "t")
        ))
      }
      DT::datatable(
        df,
        rownames  = FALSE,
        selection = list(mode = "single", selected = NULL),
        filter    = "top",
        options   = list(pageLength = 25, autoWidth = FALSE,
                         order = list(list(0, "asc")))
      )
    })

    # Default selection on first render: first row.
    shiny::observeEvent(var_tbl(), {
      df <- var_tbl()
      if (is.null(df) || nrow(df) == 0) return()
      if (length(input$tbl_rows_selected) > 0) return()
      DT::dataTableProxy(session$ns("tbl"), session = session) |>
        DT::selectRows(1L)
    }, ignoreNULL = TRUE)

    # Expose selected variable.
    shiny::reactive({
      df <- var_tbl()
      r  <- input$tbl_rows_selected
      if (is.null(df) || length(r) == 0) return(NULL)
      df$variable[r[1]]
    })
  })
}
