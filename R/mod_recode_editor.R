# =============================================================================
# Recode editor
# =============================================================================
# Editable DT bound to the rule set. Lets the user fix new_value, set
# apply_to_siblings, change action, write notes, delete rules. "Validate"
# runs validate_recodes() and surfaces issues in a modal.
# =============================================================================

# Display column order for the editor table. The generated-markup "delete"
# column MUST stay last: the DT below unescapes exactly one column
# (`escape = -ncol(df)`), and every other column can hold raw dataset text.
.RECODE_EDITOR_COLS <- c("variable", "old_value", "new_value", "action",
                         "apply_to_siblings", "sibling_pattern", "match_type",
                         "notes", "rule_id", "delete")

# The rule-key columns an edit can change. `rule_id` hashes
# (variable, match_type, old_value); `variable` is locked in the editor.
.RECODE_KEY_COLS <- c("variable", "match_type", "old_value")

#' Apply one cell edit from the Recodes table to a rule set. Pure (no Shiny).
#'
#' Why this exists: `rule_id` is a hash of the rule key. An edit to
#' `old_value` or `match_type` used to keep the old id, so a rule minted later
#' for the ORIGINAL value collided with the edited rule, and the rules proxy's
#' in-place merge silently overwrote it. A key edit now recomputes the id. If
#' the new id already belongs to ANOTHER rule the edit is refused (two rules
#' with one key cannot both exist; the user decides which to keep), and the
#' rule set is returned unchanged.
#'
#' `norm_spec` is never touched here: it records the normalization of the
#' rule's COLUMNS when the rule was added, and editing a value or match type
#' does not change which columns the rule reaches.
#'
#' @param rules Recode rule tibble.
#' @param row   1-based row index of the edited rule.
#' @param col   Name of the edited column.
#' @param value New cell value, as DT sends it (character).
#' @param now   Timestamp string for `updated_at`.
#' @return list(status = "ok" | "refused" | "ignored", rules, message).
#'   `rules` is the input unchanged unless status is "ok".
apply_recode_edit <- function(rules, row, col, value,
                              now = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")) {
  out <- function(status, r = rules, message = NULL) {
    list(status = status, rules = r, message = message)
  }
  if (is.null(rules) || !is.data.frame(rules) || nrow(rules) == 0 ||
      length(row) != 1 || is.na(row) || row < 1 || row > nrow(rules) ||
      length(col) != 1 || is.na(col) ||
      col %in% c("delete", "rule_id", "variable") || !(col %in% names(rules))) {
    return(out("ignored"))
  }
  value <- if (length(value) == 0 || is.null(value)) NA_character_
           else as.character(value[[1]])

  if (col == "match_type" && !(value %in% RECODE_MATCH_TYPES)) {
    return(out("refused", message = sprintf(
      "Invalid match_type '%s'. Allowed: %s.",
      value, paste(RECODE_MATCH_TYPES, collapse = ", "))))
  }
  if (col == "action" && !(value %in% RECODE_ACTIONS)) {
    return(out("refused", message = sprintf(
      "Invalid action '%s'. Allowed: %s.",
      value, paste(RECODE_ACTIONS, collapse = ", "))))
  }

  r <- rules
  if (col == "apply_to_siblings") {
    r[[col]][row] <- value %in% c("TRUE", "true", "T", "1")
  } else {
    r[[col]][row] <- value
  }

  if (col %in% .RECODE_KEY_COLS) {
    new_id <- recode_rule_id(r$variable[row], r$match_type[row],
                             r$old_value[row])
    if (!identical(new_id, as.character(rules$rule_id[row]))) {
      clash <- which(as.character(rules$rule_id) == new_id &
                     seq_len(nrow(rules)) != row)
      if (length(clash) > 0) {
        k <- clash[1]
        shown <- function(x) {
          if (is.na(x)) "<NA> (missing cells)"
          else if (!nzchar(x)) "'' (empty cells)"
          else sprintf("'%s'", x)
        }
        return(out("refused", message = sprintf(paste0(
          "Not changed: rule %d (id %s) already matches %s with match_type ",
          "'%s' in column '%s'. Two rules cannot share that key. Edit or ",
          "delete rule %d first."),
          k, rules$rule_id[k], shown(rules$old_value[k]),
          rules$match_type[k], rules$variable[k], k)))
      }
      r$rule_id[row] <- new_id
    }
  }

  r$updated_at[row] <- now
  out("ok", r)
}

mod_recode_editor_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = FALSE,
    fill        = FALSE,
    card_header_help(
      "Recode rules",
      shiny::p("Every rule made on the other tabs, in one editable table. ",
               "Each rule says: where this column holds the old value, put ",
               "the new value instead (or blank it, for action \"delete\")."),
      shiny::p("Double-click a cell to edit it. Press Validate before ",
               "exporting: it flags duplicates, rules that feed into each ",
               "other, and rules whose value is no longer in the data.")
    ),
    bslib::card_body(
      fillable = FALSE,
      help_line(
        "Rules apply in a ", shiny::strong("single pass"),
        ": every rule is matched against the ", shiny::strong("original"),
        " values, and the first rule to claim a cell wins. Chained rules do ",
        "not combine: with ", shiny::tags$code("a -> b"), " and ",
        shiny::tags$code("b -> c"), ", the original ", shiny::tags$code("a"),
        " values end up as ", shiny::tags$code("b"), ", not ",
        shiny::tags$code("c"), ". Write the rule you actually mean; ",
        shiny::strong("Validate"), " flags chains so they are easy to find."
      ),
      shiny::fluidRow(
        shiny::column(8,
          shiny::tags$small(shiny::tags$em(
            "Edit any cell directly. Use the delete buttons to remove rules. 'Validate' surfaces conflicts. ",
            shiny::tags$br(),
            "match_type ∈ {exact, exact_ci, trimmed_ci, regex}; action ∈ {recode, delete}. Invalid entries are rejected."
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
      dt_empty_value_css(),
      DT::DTOutput(ns("tbl"))
    )
  )
}

#' @param rules_proxy list(add=, set=, get=)
mod_recode_editor_server <- function(id, shared_state, rules_proxy) {
  shiny::moduleServer(id, function(input, output, session) {

    table_data <- shiny::reactive({
      r <- rules_proxy$get()
      if (nrow(r) == 0) {
        # Keep the display shape stable so the escape index below is always
        # the "delete" column, even with no rules loaded.
        r$delete <- character(0)
        return(r[, .RECODE_EDITOR_COLS])
      }
      ns <- session$ns
      r$delete <- vapply(seq_len(nrow(r)), function(i) {
        sprintf('<button class="btn btn-xs btn-outline-danger" onclick="Shiny.setInputValue(\'%s\', \'%s\', {priority: \'event\'})">Delete</button>',
                ns("delete_row"), r$rule_id[i])
      }, character(1))
      r[, .RECODE_EDITOR_COLS]
    })

    output$tbl <- DT::renderDT({
      df <- table_data()
      DT::datatable(
        df,
        rownames  = FALSE,
        # SECURITY: only the last column ("delete") is generated markup. Every
        # other column can hold raw third-party dataset text (old_value /
        # new_value) or free-text notes, so it must stay HTML-escaped.
        escape    = -ncol(df),
        editable  = list(target = "cell",
                         disable = list(columns = c(0, 8, 9))),  # variable, rule_id, delete
        selection = "none",
        options   = list(
          pageLength = 25, autoWidth = FALSE,
          # Label an EMPTY old_value / new_value "(empty)", so it is not
          # mistaken for a missing one (both would otherwise be a blank
          # cell). dt_empty_value_defs() is in R/mod_value_table.R.
          columnDefs = dt_empty_value_defs(
            targets  = match(c("old_value", "new_value"),
                             .RECODE_EDITOR_COLS) - 1L,
            truncate = FALSE))
      )
    })

    # Put the browser's copy of the table back to the stored rules. A refused
    # edit leaves the rule store untouched, and assigning an identical value to
    # a reactiveValues entry does not invalidate anything, so re-setting the
    # rules would NOT redraw: the typed value would stay on screen until the
    # next sort or page. replaceData() makes the table fetch the stored rows.
    tbl_proxy <- DT::dataTableProxy("tbl", session = session)
    revert_table <- function() {
      tryCatch(
        DT::replaceData(tbl_proxy, shiny::isolate(table_data()),
                        resetPaging = FALSE, rownames = FALSE),
        error = function(e) NULL)
    }

    # Edits go through apply_recode_edit() (above), which recomputes rule_id
    # when a key column changes and refuses an edit that would collide with
    # another rule. Always set(), never add(): add() re-stamps norm_spec.
    shiny::observeEvent(input$tbl_cell_edit, {
      info <- input$tbl_cell_edit
      r <- rules_proxy$get()
      if (nrow(r) == 0) return()
      col <- .RECODE_EDITOR_COLS[as.integer(info$col) + 1]
      res <- apply_recode_edit(r, row = as.integer(info$row), col = col,
                               value = info$value)
      if (identical(res$status, "ok")) {
        rules_proxy$set(res$rules)
      } else if (identical(res$status, "refused")) {
        shiny::showNotification(res$message, type = "error", duration = 10)
        revert_table()
      }
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
      if (nrow(issues$invalid_enum) > 0) {
        msg <- shiny::tagAppendChild(msg, shiny::tagList(
          shiny::h5(badge("Invalid match_type / action", "red")),
          shiny::pre(paste(capture.output(print(issues$invalid_enum)), collapse = "\n"))
        ))
      }
      if (nrow(issues$invalid_regex) > 0) {
        msg <- shiny::tagAppendChild(msg, shiny::tagList(
          shiny::h5(badge("Invalid regex pattern", "red")),
          shiny::pre(paste(capture.output(print(issues$invalid_regex)), collapse = "\n"))
        ))
      }
      if (nrow(issues$stale) > 0) {
        msg <- shiny::tagAppendChild(msg, shiny::tagList(
          shiny::h5(badge("Stale (old_value not in current data)", "grey")),
          shiny::pre(paste(capture.output(print(issues$stale)), collapse = "\n"))
        ))
      }
      if (length(msg) == 0) {
        msg <- shiny::p(badge("All good", "green"), ": no issues detected.")
      }
      shiny::showModal(shiny::modalDialog(
        title = "Validation report", msg, size = "l", easyClose = TRUE
      ))
    })
  })
}
