# =============================================================================
# Preview + export
# =============================================================================
# Diff view (before / after / affected cells), download buttons for the
# master CSV + recode R script, copy-to-clipboard for the R.
# =============================================================================

mod_preview_export_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = FALSE,
    fill        = FALSE,
    bslib::card_header("Preview & export"),
    bslib::card_body(
      fillable = FALSE,
      warning_banner("This app does NOT modify your data file. It exports a recode CSV and an R script you run yourself."),
      shiny::fluidRow(
        shiny::column(6, shiny::uiOutput(ns("summary"))),
        shiny::column(6, shiny::uiOutput(ns("buttons")))
      ),
      shiny::hr(),
      shiny::h5("Before / after diff"),
      DT::DTOutput(ns("diff_tbl")),
      shiny::hr(),
      shiny::h5("Generated R (read-only preview)"),
      shiny::verbatimTextOutput(ns("r_preview"))
    )
  )
}

#' @param rules_proxy list(add=, set=, get=)
mod_preview_export_server <- function(id, shared_state, rules_proxy) {
  shiny::moduleServer(id, function(input, output, session) {

    dataset_id <- shiny::reactive({
      nm <- shared_state$dataset_name %||% "dataset"
      tools::file_path_sans_ext(basename(nm))
    })

    diff_r <- shiny::reactive({
      r <- rules_proxy$get()
      d <- shared_state$data
      if (nrow(r) == 0 || is.null(d)) {
        return(list(rules = r, summary = tibble::tibble(), df_after = d))
      }
      res <- apply_recodes(d, r)
      list(rules = r, summary = res$summary, df_after = res$df)
    })

    output$summary <- shiny::renderUI({
      r <- rules_proxy$get()
      if (nrow(r) == 0) return(shiny::tags$em("No rules in session."))
      diff <- diff_r()
      total_cells <- sum(diff$summary$cells_changed, na.rm = TRUE)
      shiny::tagList(
        shiny::p(shiny::strong("Rules: "), nrow(r)),
        shiny::p(shiny::strong("Cells affected: "), total_cells),
        shiny::p(shiny::strong("Dataset: "), shared_state$dataset_name %||% "(none)")
      )
    })

    output$buttons <- shiny::renderUI({
      shiny::div(style = "text-align:right;",
        shiny::downloadButton(session$ns("dl_csv"), "Download recodes CSV",
                              class = "btn btn-sm btn-primary"),
        shiny::downloadButton(session$ns("dl_r"), "Download R script",
                              class = "btn btn-sm btn-primary"),
        shiny::actionButton(session$ns("copy_r"), "Copy R to clipboard",
                            class = "btn btn-sm btn-outline-secondary")
      )
    })

    output$diff_tbl <- DT::renderDT({
      r <- rules_proxy$get()
      if (nrow(r) == 0) {
        return(DT::datatable(tibble::tibble(message = "No rules yet."),
                             rownames = FALSE, options = list(dom = "t")))
      }
      diff <- diff_r()
      df <- r |>
        dplyr::select(variable, old_value, new_value, action,
                      apply_to_siblings, sibling_pattern) |>
        dplyr::mutate(cells_affected = diff$summary$cells_changed[
                       match(r$rule_id, diff$summary$rule_id)])
      DT::datatable(df, rownames = FALSE, options = list(pageLength = 25)) |>
        DT::formatStyle("action",
          backgroundColor = DT::styleEqual(
            c("delete", "recode"), c("#fce4e4", "#e6f4ea"))) |>
        DT::formatStyle("cells_affected",
          backgroundColor = DT::styleEqual(c(0L), c("#eeeeee")))
    })

    output$r_preview <- shiny::renderText({
      generate_recode_R(rules_proxy$get(), dataset_id = dataset_id())
    })

    output$dl_csv <- shiny::downloadHandler(
      filename = function() "recodes_master.csv",
      content  = function(file) write_recodes(rules_proxy$get(), file)
    )

    output$dl_r <- shiny::downloadHandler(
      filename = function() sprintf("recode_%s.R", dataset_id()),
      content  = function(file)
        writeLines(generate_recode_R(rules_proxy$get(), dataset_id()), file)
    )

    shiny::observeEvent(input$copy_r, {
      txt <- generate_recode_R(rules_proxy$get(), dataset_id())
      ok <- tryCatch({
        clipr::write_clip(txt, allow_non_interactive = TRUE)
        TRUE
      }, error = function(e) FALSE)
      shiny::showNotification(
        if (ok) "R copied to clipboard." else "Clipboard unavailable. Use Download instead.",
        type = if (ok) "message" else "warning"
      )
    })
  })
}
