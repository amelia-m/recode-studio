# =============================================================================
# Value table
# =============================================================================
# Frequency table of unique values for the selected variable. Multi-select
# rows + "Create rule for selected" wires into the rule editor.
# =============================================================================

mod_value_table_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = FALSE,
    fill        = FALSE,
    bslib::card_header("Browse values"),
    bslib::card_body(
      fillable = FALSE,
      shiny::uiOutput(ns("toolbar")),
      DT::DTOutput(ns("tbl"))
    )
  )
}

#' @param selected_var_r reactive returning the currently-selected variable
#' @param unique_values_r reactive returning tibble(value, n) for current var
#' @param rules_proxy list(add=, set=, get=) to push new rules
mod_value_table_server <- function(id, shared_state,
                                   selected_var_r,
                                   unique_values_r,
                                   rules_proxy) {
  shiny::moduleServer(id, function(input, output, session) {

    output$toolbar <- shiny::renderUI({
      v <- selected_var_r()
      if (is.null(v)) return(shiny::tags$em("Pick a variable first (Variable tab)."))
      shiny::div(
        shiny::tags$small(shiny::strong("Variable: "), v),
        shiny::actionButton(session$ns("recode_sel"),
                            "Create recode rule for selected rows",
                            class = "btn btn-sm btn-primary",
                            style = "margin-left:1em;")
      )
    })

    table_data <- shiny::reactive({
      uv <- unique_values_r()
      if (is.null(uv) || nrow(uv) == 0) {
        return(tibble::tibble(value = character(0), n = integer(0),
                              duplicate_token = logical(0)))
      }
      uv |>
        dplyr::mutate(
          duplicate_token = grepl("\\b(\\w+)\\s+\\1\\b", value)
        ) |>
        dplyr::arrange(dplyr::desc(n), value)
    })

    output$tbl <- DT::renderDT({
      df <- table_data()
      DT::datatable(
        df,
        rownames  = FALSE,
        selection = "multi",
        filter    = "top",
        options   = list(pageLength = 25, autoWidth = FALSE)
      ) |>
        DT::formatStyle(
          "duplicate_token",
          target          = "row",
          backgroundColor = DT::styleEqual(c(TRUE), c("#fff4e5"))
        )
    })

    shiny::observeEvent(input$recode_sel, {
      v <- selected_var_r()
      if (is.null(v)) return()
      rows <- input$tbl_rows_selected
      if (length(rows) == 0) {
        shiny::showNotification("Select at least one row first.", type = "warning")
        return()
      }
      df <- table_data()
      sel_values <- df$value[rows]
      now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      new_rules <- tibble::tibble(
        rule_id           = recode_rule_id(v, "trimmed_ci", sel_values),
        variable          = v,
        apply_to_siblings = FALSE,
        sibling_pattern   = NA_character_,
        match_type        = "trimmed_ci",
        old_value         = sel_values,
        new_value         = sel_values,
        action            = "recode",
        notes             = NA_character_,
        author            = Sys.info()[["user"]],
        created_at        = now,
        updated_at        = now,
        source_dataset    = shared_state$dataset_name %||% NA_character_
      )
      rules_proxy$add(new_rules)
      shiny::showNotification(
        sprintf("Added %d rule(s). Edit them on the Recodes tab.", nrow(new_rules)),
        type = "message"
      )
    })
  })
}
