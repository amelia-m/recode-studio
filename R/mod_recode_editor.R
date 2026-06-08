# =============================================================================
# Recode editor
# =============================================================================
# Editable DT bound to the rule set. Lets the user fix new_value, set
# apply_to_siblings, change action, write notes, delete rules. "Validate"
# runs validate_recodes() and surfaces issues in a modal.
# =============================================================================

mod_recode_editor_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = FALSE,
    fill        = FALSE,
    bslib::card_header("Recode rules"),
    bslib::card_body(
      fillable = FALSE,
      shiny::fluidRow(
        shiny::column(8,
          shiny::tags$small(shiny::tags$em(
            "Edit any cell directly. Use the delete buttons to remove rules. 'Validate' surfaces conflicts."
          ))
        ),
        shiny::column(4, align = "right",
          shiny::actionButton(ns("validate"), "Validate",
                              class = "btn btn-sm btn-warning"),
          shiny::actionButton(ns("clear_all"), "Clear all",
                              class = "btn btn-sm btn-outline-danger")
        )
      ),
      shiny::hr(),
      DT::DTOutput(ns("tbl"))
    )
  )
}

#' @param rules_proxy list(add=, set=, get=)
mod_recode_editor_server <- function(id, shared_state, rules_proxy) {
  shiny::moduleServer(id, function(input, output, session) {

    table_data <- shiny::reactive({
      r <- rules_proxy$get()
      if (nrow(r) == 0) return(r)
      ns <- session$ns
      r$delete <- vapply(seq_len(nrow(r)), function(i) {
        sprintf('<button class="btn btn-xs btn-outline-danger" onclick="Shiny.setInputValue(\'%s\', \'%s\', {priority: \'event\'})">Delete</button>',
                ns("delete_row"), r$rule_id[i])
      }, character(1))
      r[, c("variable", "old_value", "new_value", "action",
            "apply_to_siblings", "sibling_pattern", "match_type",
            "notes", "rule_id", "delete")]
    })

    output$tbl <- DT::renderDT({
      df <- table_data()
      DT::datatable(
        df,
        rownames  = FALSE,
        escape    = FALSE,
        editable  = list(target = "cell",
                         disable = list(columns = c(0, 8, 9))),  # variable, rule_id, delete
        selection = "none",
        options   = list(pageLength = 25, autoWidth = FALSE)
      )
    })

    shiny::observeEvent(input$tbl_cell_edit, {
      info <- input$tbl_cell_edit
      r <- rules_proxy$get()
      if (nrow(r) == 0) return()
      col_names <- c("variable", "old_value", "new_value", "action",
                     "apply_to_siblings", "sibling_pattern", "match_type",
                     "notes", "rule_id", "delete")
      col <- col_names[info$col + 1]
      row <- info$row
      val <- info$value
      if (col %in% c("delete", "rule_id", "variable")) return()  # locked
      if (col == "apply_to_siblings") {
        val <- val %in% c("TRUE", "true", "T", "1", TRUE)
      }
      r[[col]][row] <- val
      r$updated_at[row] <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      rules_proxy$set(r)
    })

    shiny::observeEvent(input$delete_row, ignoreInit = TRUE, {
      id_to_drop <- input$delete_row
      r <- rules_proxy$get()
      rules_proxy$set(r[r$rule_id != id_to_drop, ])
      shiny::showNotification("Rule deleted.", type = "message", duration = 2)
    })

    shiny::observeEvent(input$clear_all, {
      shiny::showModal(shiny::modalDialog(
        title = "Clear all rules?",
        "This wipes the in-session rule set. Anything not yet exported is lost.",
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("clear_confirm"), "Clear",
                              class = "btn btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$clear_confirm, {
      rules_proxy$set(empty_recodes_tibble())
      shiny::removeModal()
      shiny::showNotification("Rules cleared.", type = "message")
    })

    shiny::observeEvent(input$validate, {
      r <- rules_proxy$get()
      issues <- validate_recodes(r, data = shared_state$data)

      msg <- shiny::tagList()
      if (nrow(issues$duplicate_keys) > 0) {
        msg <- shiny::tagAppendChild(msg, shiny::tagList(
          shiny::h5(badge("Duplicate keys", "red")),
          shiny::pre(paste(capture.output(print(issues$duplicate_keys)), collapse = "\n"))
        ))
      }
      if (nrow(issues$rule_chains) > 0) {
        msg <- shiny::tagAppendChild(msg, shiny::tagList(
          shiny::h5(badge("Rule chains", "yellow")),
          shiny::pre(paste(capture.output(print(issues$rule_chains)), collapse = "\n"))
        ))
      }
      if (nrow(issues$blank_new_value) > 0) {
        msg <- shiny::tagAppendChild(msg, shiny::tagList(
          shiny::h5(badge("Blank new_value with action=recode", "yellow")),
          shiny::pre(paste(capture.output(print(
            issues$blank_new_value[, c("variable", "old_value", "action")])), collapse = "\n"))
        ))
      }
      if (nrow(issues$stale) > 0) {
        msg <- shiny::tagAppendChild(msg, shiny::tagList(
          shiny::h5(badge("Stale (old_value not in current data)", "grey")),
          shiny::pre(paste(capture.output(print(issues$stale)), collapse = "\n"))
        ))
      }
      if (length(msg) == 0) {
        msg <- shiny::p(badge("All good", "green"), " — no issues detected.")
      }
      shiny::showModal(shiny::modalDialog(
        title = "Validation report", msg, size = "l", easyClose = TRUE
      ))
    })
  })
}
