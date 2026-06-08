# =============================================================================
# Import recodes
# =============================================================================
# Upload an externally-edited recodes_master.csv. Merge with conflict
# resolution per rule_id: keep-mine / keep-theirs / keep-newer.
# =============================================================================

mod_import_recodes_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = FALSE,
    fill        = FALSE,
    bslib::card_header("Import / merge an existing recodes CSV"),
    bslib::card_body(
      fillable = FALSE,
      shiny::fileInput(ns("file"), "Upload recodes_master.csv", accept = c(".csv")),
      shiny::radioButtons(ns("conflict"), "On rule_id collision:",
        choices = c("Keep current session"  = "mine",
                    "Take incoming file"    = "theirs",
                    "Take newer updated_at" = "newer"),
        selected = "newer", inline = TRUE),
      shiny::actionButton(ns("apply"), "Merge", class = "btn btn-primary"),
      shiny::tags$hr(),
      shiny::uiOutput(ns("preview"))
    )
  )
}

#' @param rules_proxy list(add=, set=, get=)
mod_import_recodes_server <- function(id, shared_state, rules_proxy) {
  shiny::moduleServer(id, function(input, output, session) {

    incoming_r <- shiny::reactive({
      f <- input$file
      if (is.null(f)) return(NULL)
      tryCatch(read_recodes(f$datapath), error = function(e) NULL)
    })

    output$preview <- shiny::renderUI({
      inc <- incoming_r()
      if (is.null(inc)) return(shiny::tags$em("Upload a CSV to preview."))
      cur <- rules_proxy$get()
      coll <- intersect(cur$rule_id, inc$rule_id)
      novel <- setdiff(inc$rule_id, cur$rule_id)
      shiny::tagList(
        shiny::p(shiny::strong("Incoming rules: "), nrow(inc)),
        shiny::p(shiny::strong("Novel (not in session): "), length(novel)),
        shiny::p(shiny::strong("Conflicts (rule_id collision): "), length(coll))
      )
    })

    shiny::observeEvent(input$apply, {
      inc <- incoming_r()
      if (is.null(inc)) {
        shiny::showNotification("Upload a CSV first.", type = "warning")
        return()
      }
      cur <- rules_proxy$get()
      strategy <- input$conflict

      novel_ids <- setdiff(inc$rule_id, cur$rule_id)
      coll_ids  <- intersect(cur$rule_id, inc$rule_id)

      merged <- dplyr::bind_rows(
        cur[!(cur$rule_id %in% coll_ids), ],
        inc[inc$rule_id %in% novel_ids, ]
      )

      if (length(coll_ids) > 0) {
        for (cid in coll_ids) {
          m <- cur[cur$rule_id == cid, ]
          t <- inc[inc$rule_id == cid, ]
          winner <- switch(strategy,
            mine   = m,
            theirs = t,
            newer  = if (!is.na(t$updated_at[1]) &&
                         (is.na(m$updated_at[1]) ||
                          t$updated_at[1] > m$updated_at[1])) t else m
          )
          merged <- dplyr::bind_rows(merged, winner)
        }
      }

      rules_proxy$set(merged)
      shiny::showNotification(
        sprintf("Merged. Now %d rule(s) in session.", nrow(merged)), type = "message")
    })
  })
}
