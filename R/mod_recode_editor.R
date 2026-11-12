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

# What the apply_to_siblings cell accepts, after trimming, ignoring case. The
# cell is free text in the editor, and `value %in% c("TRUE", "true", "T", "1")`
# read every other entry as FALSE, so "yes" or "True " silently switched
# sibling propagation OFF while the card said invalid entries are rejected.
.RULE_FLAG_TRUE  <- c("true", "t", "1", "yes", "y")
.RULE_FLAG_FALSE <- c("false", "f", "0", "no", "n")
.RULE_FLAG_SHOWN_TRUE  <- "TRUE, true, T, 1, yes, y"
.RULE_FLAG_SHOWN_FALSE <- "FALSE, false, F, 0, no, n"

#' Read a yes/no cell entry. Pure.
#'
#' @param value the cell value as typed.
#' @return TRUE, FALSE, or NA when the entry is neither (the caller refuses it;
#'   NA is never written to the rule, which must stay a real logical).
.as_rule_flag <- function(value) {
  v <- tolower(trimws(as.character(value)))
  if (length(v) != 1L || is.na(v)) return(NA)
  if (v %in% .RULE_FLAG_TRUE)  return(TRUE)
  if (v %in% .RULE_FLAG_FALSE) return(FALSE)
  NA
}

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
  # A Spellcheck rule is an `exact` whole-value rule whose new value is the
  # old one with a single word corrected. As a regex its old value would be
  # read as a pattern (a "." or "(" in the text changes what it matches) and
  # every matching cell replaced whole, so the switch is refused.
  if (col == "match_type" && identical(value, "regex") &&
      .is_spellcheck_rule(rules$notes[row])) {
    return(out("refused", message = paste0(
      "Not changed: this rule was made by the Spellcheck tab, which corrects ",
      "one word inside a whole value, so it cannot be switched to regex. ",
      "Make a new rule on Browse values if you need a pattern.")))
  }
  if (col == "sibling_pattern") {
    # Cleared (empty or whitespace-only) is stored as NA, never "": an empty
    # sibling pattern would reach every column.
    value <- clean_sibling_pattern(value)
    if (!is.na(value) && !regex_compiles(value)) {
      return(out("refused", message = sprintf(paste0(
        "Not changed: the sibling pattern '%s' is not a valid regular ",
        "expression, so the rule would reach no column. Fix the pattern, or ",
        "clear the cell to apply the rule to its own column only."), value)))
    }
  }

  if (col == "apply_to_siblings") {
    flag <- .as_rule_flag(value)
    if (is.na(flag)) {
      return(out("refused", message = sprintf(paste0(
        "Not changed: '%s' is not a yes or no. apply_to_siblings accepts %s ",
        "for yes and %s for no (case does not matter). Sibling propagation ",
        "is unchanged."),
        value, .RULE_FLAG_SHOWN_TRUE, .RULE_FLAG_SHOWN_FALSE)))
    }
  }

  r <- rules
  if (col == "apply_to_siblings") {
    r[[col]][row] <- flag
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

# TRUE for a rule the Spellcheck tab made: its notes start with "Spellcheck"
# (current format "Spellcheck: token 'x' -> 'y'", older "Spellcheck (token
# 'x')"). The same test validate_recodes() uses for spellcheck_lost_words.
.is_spellcheck_rule <- function(notes) {
  notes <- as.character(notes)
  !is.na(notes) & grepl("^[[:space:]]*Spellcheck([^[:alnum:]_]|$)", notes)
}

# Show a rule value in the validation report: NA and "" are named, since both
# would otherwise print as nothing.
.shown_value <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x), "<NA> (missing cells)",
         ifelse(!nzchar(x), "(empty)", x))
}

# One report section: a badge with the count, an optional plain-language
# explanation, then either a list of items or the issue rows as printed text.
# Everything reaches the page as text, so htmltools escapes it.
.validation_section <- function(title, color, n, intro = NULL, items = NULL,
                                rows = NULL) {
  shiny::tagList(
    shiny::h5(badge(sprintf("%s (%d)", title, n), color)),
    if (!is.null(intro)) shiny::p(class = "small", intro),
    if (!is.null(items)) shiny::tags$ul(class = "small", items),
    if (!is.null(rows)) shiny::pre(paste(utils::capture.output(print(
      as.data.frame(rows), row.names = FALSE)), collapse = "\n"))
  )
}

.n_words <- function(n) {
  if (identical(as.integer(n), 1L)) "1 word" else paste(n, "words")
}

#' The Recodes Validate report, built from validate_recodes() output.
#'
#' Pure (tags only). Every class with rows gets a section with its count. The
#' classes added for upstream's follow-ups (invalid_sibling_pattern,
#' id_mismatch, spellcheck_lost_words) are explained in plain language. A
#' class this function does not know yet is still shown, under a title built
#' from its name (issue_class_title()), instead of being dropped or crashing
#' the report.
#'
#' @param issues named list of tibbles from validate_recodes().
#' @return a tag or tagList.
recode_validation_ui <- function(issues) {
  keep <- vapply(issues, function(x) is.data.frame(x) && nrow(x) > 0,
                 logical(1))
  issues <- issues[keep]
  if (length(issues) == 0) {
    return(shiny::p(badge("All good", "green"), ": no issues detected."))
  }
  code <- function(x) shiny::tags$code(.shown_value(x))
  one  <- function(n) n == 1

  sections <- list(
    duplicate_keys = function(x) .validation_section(
      "Duplicate keys", "red", nrow(x), rows = x),
    invalid_enum = function(x) .validation_section(
      "Invalid match_type / action", "red", nrow(x), rows = x),
    invalid_regex = function(x) .validation_section(
      "Invalid regex pattern", "red", nrow(x), rows = x),
    invalid_sibling_pattern = function(x) .validation_section(
      "Invalid sibling pattern", "red", nrow(x),
      intro = paste0(
        if (one(nrow(x))) "This sibling rule has a pattern that is"
        else sprintf("These %d sibling rules have a pattern that is", nrow(x)),
        " not a valid regular expression, so ",
        if (one(nrow(x))) "it is" else "they are",
        " skipped: nothing changes, in the app or in the exported script. Fix ",
        "the pattern in the sibling_pattern column, or clear it to apply the ",
        "rule to its own column only."),
      items = lapply(seq_len(nrow(x)), function(i) shiny::tags$li(
        code(x$variable[i]), ": pattern ", code(x$sibling_pattern[i])))),
    id_mismatch = function(x) .validation_section(
      "Rule id does not match the rule", "yellow", nrow(x),
      intro = paste0(
        if (one(nrow(x))) "This rule carries an id"
        else sprintf("These %d rules carry an id", nrow(x)),
        " that no longer matches the rule's column, match type and old value ",
        "(saved before an edit, or written by another tool). Ids are not ",
        "rewritten here. The rule still applies as shown, but a rule made ",
        "later for the same value will sit beside it instead of replacing it. ",
        "To give it the right id, delete it and make it again."),
      items = lapply(seq_len(nrow(x)), function(i) shiny::tags$li(
        code(x$variable[i]), " ", code(x$old_value[i]), " (",
        .shown_value(x$match_type[i]), "): id ", code(x$rule_id[i]),
        ", expected ", code(x$expected_id[i])))),
    spellcheck_lost_words = function(x) .validation_section(
      "Spellcheck rule may have dropped words", "yellow", nrow(x),
      intro = paste0(
        "An older version of the Spellcheck tab replaced the whole value ",
        "with the corrected word, so \"iced coffe\" became \"coffee\". ",
        if (one(nrow(x))) "This rule's new value has"
        else sprintf("These %d rules' new values have", nrow(x)),
        " fewer words than the old value. Review ",
        if (one(nrow(x))) "it" else "each one",
        ": edit new_value to keep the other words, or delete the rule and ",
        "pick the correction again on the Spellcheck tab. Nothing was ",
        "changed."),
      items = lapply(seq_len(nrow(x)), function(i) shiny::tags$li(
        code(x$variable[i]), ": ", code(x$old_value[i]), " to ",
        code(x$new_value[i]),
        sprintf(" (%s to %s)", .n_words(x$old_words[i]),
                .n_words(x$new_words[i]))))),
    blank_new_value = function(x) .validation_section(
      "Blank new_value with action=recode", "yellow", nrow(x),
      rows = x[, intersect(c("variable", "old_value", "action"), names(x)),
               drop = FALSE]),
    rule_chains = function(x) .validation_section(
      "Rule chains", "yellow", nrow(x), rows = x),
    stale = function(x) .validation_section(
      "Stale (old_value not in current data)", "grey", nrow(x), rows = x)
  )

  known <- intersect(names(sections), names(issues))
  other <- setdiff(names(issues), names(sections))
  shiny::tagList(
    lapply(known, function(nm) sections[[nm]](issues[[nm]])),
    lapply(other, function(nm) .validation_section(
      issue_class_title(nm), "grey", nrow(issues[[nm]]), rows = issues[[nm]]))
  )
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
            "match_type ∈ {exact, exact_ci, trimmed_ci, regex}; action ∈ {recode, delete}; apply_to_siblings ∈ {TRUE, FALSE} (yes / no / 1 / 0 also read). Invalid entries are rejected."
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
      # Names the row position, with the stored id only as a staleness check,
      # and escapes the whole handler (.rule_delete_button() in
      # R/ui_helpers.R). A rule id can be duplicated, blank or missing, and
      # comes from an imported CSV.
      r$delete <- vapply(seq_len(nrow(r)), function(i) {
        .rule_delete_button(ns("delete_row"), i, r$rule_id[i])
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

    # Deletes the ONE row the button names. Matching on the id string removed
    # every rule sharing a duplicated or blank id, and an NA id matched nothing
    # under `!=`, which left an all-NA row in the set and in the export.
    shiny::observeEvent(input$delete_row, ignoreInit = TRUE, {
      r <- rules_proxy$get()
      hit <- .rule_delete_target(input$delete_row, r$rule_id)
      if (!hit$ok) {
        shiny::showNotification(hit$message, type = "warning", duration = 6)
        return()
      }
      rules_proxy$set(r[-hit$row, , drop = FALSE])
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
      msg <- recode_validation_ui(issues)
      shiny::showModal(shiny::modalDialog(
        title = "Validation report", msg, size = "l", easyClose = TRUE
      ))
    })
  })
}
