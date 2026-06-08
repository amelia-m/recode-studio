# =============================================================================
# Data input
# =============================================================================
# Upload a CSV / Excel file (or load the bundled example), read it, and
# publish the data + metadata into shared_state. This is the only module
# allowed to WRITE shared_state.
# =============================================================================

mod_data_input_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = FALSE,
    fill        = FALSE,
    bslib::card_header("Load data"),
    bslib::card_body(
      fillable = FALSE,
      shiny::p(
        "Upload a CSV or Excel file. Every column is read as text. ",
        "Then move to the Variable tab to pick a column to clean."
      ),
      shiny::fileInput(ns("file"), "Data file (.csv, .xlsx, .xls)",
                       accept = c(".csv", ".xlsx", ".xls"), width = "100%"),
      shiny::uiOutput(ns("sheet_ui")),
      shiny::actionButton(ns("load_example"), "Load bundled example dataset",
                          class = "btn btn-sm btn-outline-secondary"),
      shiny::hr(),
      shiny::uiOutput(ns("status"))
    )
  )
}

mod_data_input_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {

    # Excel sheet picker (only when an Excel file is uploaded)
    output$sheet_ui <- shiny::renderUI({
      f <- input$file
      if (is.null(f)) return(NULL)
      sheets <- tryCatch(dataset_sheets(f$datapath), error = function(e) character(0))
      if (length(sheets) <= 1) return(NULL)
      shiny::selectInput(session$ns("sheet"), "Sheet:", choices = sheets,
                         selected = sheets[1])
    })

    load_df <- function(path, name, sheet = 1) {
      shiny::withProgress(message = sprintf("Loading %s ...", name), value = 0.4, {
        df <- read_dataset(path, sheet = sheet)
        shiny::incProgress(0.4, detail = "building metadata ...")
        meta <- build_meta(df)
        shared_state$data         <- df
        shared_state$meta         <- meta
        shared_state$dataset_name <- name
        shiny::incProgress(0.2, detail = "done")
      })
    }

    shiny::observeEvent(input$file, {
      f <- input$file
      sheet <- if (!is.null(input$sheet)) input$sheet else 1
      tryCatch(
        load_df(f$datapath, f$name, sheet),
        error = function(e)
          shiny::showNotification(paste("Load failed:", conditionMessage(e)),
                                  type = "error")
      )
    })

    shiny::observeEvent(input$sheet, {
      f <- input$file
      if (is.null(f)) return()
      tryCatch(
        load_df(f$datapath, f$name, input$sheet),
        error = function(e)
          shiny::showNotification(paste("Load failed:", conditionMessage(e)),
                                  type = "error")
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$load_example, {
      path <- system.file("extdata", "example_messy.csv",
                          package = "recodeStudio")
      if (!nzchar(path) || !file.exists(path)) {
        # Running from source (not installed as a package): use the repo path.
        path <- file.path("inst", "extdata", "example_messy.csv")
      }
      if (!file.exists(path)) {
        shiny::showNotification("Example dataset not found.", type = "error")
        return()
      }
      load_df(path, "example_messy.csv")
    })

    output$status <- shiny::renderUI({
      d <- shared_state$data
      m <- shared_state$meta
      if (is.null(d)) return(shiny::tags$em("No dataset loaded yet."))
      shiny::tagList(
        shiny::p(shiny::strong("Loaded: "), shared_state$dataset_name),
        shiny::p(shiny::strong("Dimensions: "),
                 sprintf("%d rows x %d cols", nrow(d), ncol(d))),
        shiny::p(shiny::strong("Free-text candidates: "),
                 if (is.null(m)) "—" else as.character(sum(m$is_free_text_candidate)))
      )
    })
  })
}
