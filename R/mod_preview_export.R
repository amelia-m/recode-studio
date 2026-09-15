# =============================================================================
# Preview + export
# =============================================================================
# Diff view (before / after / affected cells), download buttons for the
# master CSV + recode R script, copy-to-clipboard for the R.
# =============================================================================

# Per-rule count columns in the diff table, in display order, each read from
# the apply_recodes()$summary column of the same name.
.PREVIEW_COUNT_COLS <- c("cells_matched", "cells_changed", "cells_shadowed")

#' Build the before / after diff table: one row per rule plus its counts.
#'
#' Pure (no Shiny) so it can be unit tested. Counts are looked up by `rule_id`,
#' not row order. A missing or empty `summary` (no data loaded yet, e.g. rules
#' imported before a dataset), a rule with no summary row, or a summary lacking
#' a count column all yield NA for that count, never an error: the count
#' columns always exist, so the table styling keyed on them always resolves.
#'
#' Built with base subsetting on purpose. A local helper called inside a dplyr
#' verb is shadowed by any dplyr export of the same name (`pick`, `across`,
#' `where`...), which upstream hit as a render failure.
#'
#' @param rules   recode rules tibble (recodes CSV schema).
#' @param summary apply_recodes()$summary, or NULL / an empty tibble.
#' @return tibble: variable, old_value, new_value, action, apply_to_siblings,
#'   sibling_pattern, cells_matched, cells_changed, cells_shadowed.
preview_diff_table <- function(rules, summary = NULL) {
  n   <- nrow(rules)
  out <- rules[, c("variable", "old_value", "new_value", "action",
                   "apply_to_siblings", "sibling_pattern")]
  has_rows <- !is.null(summary) && nrow(summary) > 0 &&
    "rule_id" %in% names(summary)
  idx <- if (has_rows) match(rules$rule_id, summary$rule_id)
         else rep(NA_integer_, n)
  for (nm in .PREVIEW_COUNT_COLS) {
    out[[nm]] <- if (has_rows && nm %in% names(summary)) {
      as.integer(summary[[nm]][idx])
    } else {
      rep(NA_integer_, n)
    }
  }
  out
}


mod_preview_export_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = FALSE,
    fill        = FALSE,
    bslib::card_header("Preview & export"),
    bslib::card_body(
      fillable = FALSE,
      warning_banner("This app does NOT modify your data file. It exports a recode CSV and an R script you run yourself."),
      help_line(
        "Each rule reports three counts, taken against your data. ",
        shiny::strong("Matched"), " = cells the rule hit. ",
        shiny::strong("Changed"), " = of those, cells whose value actually ",
        "differs afterwards (a rule that maps a value to itself changes ",
        "nothing). ", shiny::strong("Shadowed"), " = cells the rule hit, but ",
        "an earlier rule had already claimed, because rules apply in a single ",
        "pass and the first rule to claim a cell wins. A rule that changes ",
        "nothing shows a grey changed count. Counts are blank until a dataset ",
        "is loaded."
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::uiOutput(ns("summary"))),
        shiny::column(6, shiny::uiOutput(ns("buttons")))
      ),
      shiny::hr(),
      shiny::h5("Before / after diff"),
      dt_truncate_css(),
      dt_truncate_hint(),
      shiny::uiOutput(ns("diff_empty")),
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
      if (is.null(shared_state$data)) {
        return(shiny::tagList(
          shiny::p(shiny::strong("Rules: "), nrow(r)),
          shiny::p(shiny::tags$em("Load a dataset to count the cells these rules would change."))
        ))
      }
      s <- diff$summary
      total <- function(nm) if (nm %in% names(s)) sum(s[[nm]], na.rm = TRUE) else 0L
      shiny::tagList(
        shiny::p(shiny::strong("Rules: "), nrow(r)),
        shiny::p(shiny::strong("Cells matched: "), total("cells_matched")),
        shiny::p(shiny::strong("Cells changed: "), total("cells_changed")),
        shiny::p(shiny::strong("Cells shadowed: "), total("cells_shadowed")),
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

    # Empty state as a plain note, not a one-column datatable (which would
    # show a literal "message" header with a sort arrow).
    output$diff_empty <- shiny::renderUI({
      if (nrow(rules_proxy$get()) > 0) return(NULL)
      empty_state("No rules yet.")
    })

    output$diff_tbl <- DT::renderDT({
      r <- rules_proxy$get()
      if (nrow(r) == 0) return(NULL)
      df <- preview_diff_table(r, diff_r()$summary)
      DT::datatable(
        df,
        rownames = FALSE,
        # escape stays TRUE (the default): old_value / new_value are raw
        # dataset text, and dt_truncate_defs() relies on escaped input.
        options  = list(pageLength = 25, columnDefs = dt_truncate_defs())
      ) |>
        DT::formatStyle("action",
          backgroundColor = DT::styleEqual(
            c("delete", "recode"), c("#fce4e4", "#e6f4ea"))) |>
        DT::formatStyle("cells_changed",
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
