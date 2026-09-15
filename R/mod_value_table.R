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
    card_header_help(
      "Browse values",
      shiny::p("Every distinct value in the selected column, with how many ",
               "rows hold it. Sort by value to spot near-duplicates next to ",
               "each other, or by count to find the odd ones out."),
      shiny::p("Click the rows you want to fix, then \"Create recode rule ",
               "for selected rows\". Each gets a rule on the Recodes tab, ",
               "where you type the value it should become. Nothing changes ",
               "in your data file.")
    ),
    bslib::card_body(
      fillable = FALSE,
      # Case-insensitive matching already exists (every rule minted here is
      # trimmed_ci) but was invisible where rules are CREATED, so it kept being
      # asked for. Keep in step with the Spellcheck tab's wording.
      help_line(shiny::tags$em(
        "Rules created here match with ", shiny::tags$code("trimmed_ci"),
        ": case and surrounding spaces are ignored, so one rule catches ",
        "\"latte\", \"Latte\" and \"LATTE \". Change that per rule on the ",
        "Recodes tab.")),
      shiny::uiOutput(ns("toolbar")),
      dt_truncate_css(),
      # Column widths must not move when the sort changes. `autoWidth = FALSE`
      # alone does not do it: the browser still lays the table out from the
      # content on the current page, so sorting by n (short values on screen)
      # gives a narrow value column and sorting by value gives a wide one.
      # table-layout:fixed makes the declared percentage widths binding. With
      # fixed layout the cell width already bounds the one-line span, so the
      # truncation's max-width cap is lifted here.
      shiny::tags$style(shiny::HTML("
        .rs-value-table { table-layout: fixed; width: 100% !important; }
        .rs-value-table td, .rs-value-table th {
          overflow-wrap: anywhere; word-break: break-word; }
        .rs-value-table td .rs-dt-trunc { max-width: none; }
      ")),
      dt_truncate_hint(),
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
        class     = "display rs-value-table",
        # escape stays TRUE (the default): value is raw dataset text, and
        # dt_truncate_defs() relies on escaped input.
        options   = list(
          pageLength = 25,
          autoWidth  = FALSE,
          # Percent widths: adapts to the window, stays put across sorts.
          columnDefs = c(
            list(list(targets = 0, width = "70%"),
                 list(targets = 1, width = "10%"),
                 list(targets = 2, width = "20%")),
            dt_truncate_defs()))
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
