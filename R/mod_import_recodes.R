# =============================================================================
# Import recodes
# =============================================================================
# Upload an externally-edited recodes_master.csv. Merge with conflict
# resolution per rule_id: keep-mine / keep-theirs / keep-newer.
#
# The incoming frame's schema is checked BEFORE any bind_rows. readr does not
# error when a col_types spec names a column the file lacks (it only warns on
# the console, which this app's user never sees), so an off-schema file would
# otherwise slip through and bind_rows would UNION the column names -- silently
# corrupting recodes_master.csv on the next export.
# =============================================================================

# Columns that current upstream rule files carry and this repo has no use for.
# They are dropped on import (and the user is told), rather than refused.
# `norm_spec` records a column-normalization step that this app does not have.
.RECODES_FOREIGN_DROP <- c("norm_spec")

#' Check an incoming recodes frame against this repo's schema.
#'
#' Tolerates two known upstream divergences:
#' - the provenance field is named `source_quarter` where this repo uses
#'   `source_dataset`; that lone difference is renamed.
#' - known-foreign columns (`.RECODES_FOREIGN_DROP`, e.g. `norm_spec`) are
#'   dropped.
#' Anything else off-schema (an unknown extra, a missing required column, or
#' both `source_dataset` and `source_quarter`) is refused.
#'
#' @param inc tibble read by read_recodes(path, conform = FALSE).
#' @return list(ok = lgl, df = tibble or NULL, renamed = lgl,
#'              dropped = chr, missing = chr, extra = chr)
.check_incoming_recodes <- function(inc) {
  expected <- names(empty_recodes_tibble())
  out <- list(ok = FALSE, df = NULL, renamed = FALSE, dropped = character(0),
              missing = character(0), extra = character(0))
  if (is.null(inc)) return(out)

  got <- names(inc)

  # Sibling-repo normalization: source_quarter -> source_dataset.
  if ("source_quarter" %in% got && !("source_dataset" %in% got)) {
    names(inc)[names(inc) == "source_quarter"] <- "source_dataset"
    got <- names(inc)
    out$renamed <- TRUE
  }

  out$missing <- setdiff(expected, got)
  out$extra   <- setdiff(got, c(expected, .RECODES_FOREIGN_DROP))
  if (length(out$missing) > 0 || length(out$extra) > 0) return(out)

  out$dropped <- intersect(got, .RECODES_FOREIGN_DROP)

  # Column ORDER may differ harmlessly; force it to the canonical order so the
  # merge can never produce a frame whose columns differ from the schema. This
  # selection is also what drops the known-foreign columns.
  out$ok <- TRUE
  out$df <- inc[, expected]
  out
}

#' User-facing note for a file .check_incoming_recodes() accepted but had to
#' adjust. NULL when nothing was renamed or dropped.
.import_adjust_msg <- function(chk) {
  bits <- character(0)
  if (isTRUE(chk$renamed)) {
    bits <- c(bits, "column 'source_quarter' was read as 'source_dataset'")
  }
  if (length(chk$dropped) > 0) {
    bits <- c(bits, sprintf(
      paste0("column(s) %s were dropped (this app has no column-normalization ",
             "step, so check any rule written against normalized values)"),
      paste0("'", chk$dropped, "'", collapse = ", ")))
  }
  if (length(bits) == 0) return(NULL)
  paste0("Upstream-format file normalized: ", paste(bits, collapse = "; "), ".")
}

#' Human-readable explanation of a failed .check_incoming_recodes().
.schema_error_msg <- function(chk) {
  bits <- character(0)
  if (length(chk$missing) > 0) {
    bits <- c(bits, sprintf("missing required column(s): %s",
                            paste(chk$missing, collapse = ", ")))
  }
  if (length(chk$extra) > 0) {
    bits <- c(bits, sprintf("unexpected column(s): %s",
                            paste(chk$extra, collapse = ", ")))
  }
  sprintf("That CSV does not match the recodes schema (%s). Nothing was merged.",
          paste(bits, collapse = "; "))
}

mod_import_recodes_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = FALSE,
    fill        = FALSE,
    card_header_help(
      "Import / merge an existing recodes CSV",
      shiny::p("Load a recodes CSV exported earlier (by you or a colleague) ",
               "and add its rules to this session, so you can pick up where ",
               "you left off."),
      shiny::p("When both have a rule for the same value, choose which one ",
               "wins: the one already here, the one in the file, or ",
               "whichever was edited most recently.")
    ),
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

    # Raw read + schema check. Returns the .check_incoming_recodes() list, or
    # NULL when nothing has been uploaded / the file would not parse at all.
    checked_r <- shiny::reactive({
      f <- input$file
      if (is.null(f)) return(NULL)
      # conform = FALSE: the raw read, so the guard sees the file's own columns.
      # Conforming happens only after the guard accepts the file.
      raw <- tryCatch(read_recodes(f$datapath, conform = FALSE),
                      error = function(e) NULL)
      if (is.null(raw)) return(NULL)
      chk <- .check_incoming_recodes(raw)
      if (chk$ok) {
        chk$df <- tryCatch(.conform_recodes_schema(chk$df),
                           error = function(e) NULL)
        if (is.null(chk$df)) return(NULL)
      }
      chk
    })

    # Schema-valid incoming rules, or NULL.
    incoming_r <- shiny::reactive({
      chk <- checked_r()
      if (is.null(chk) || !chk$ok) return(NULL)
      chk$df
    })

    # Immediate feedback on upload: normalized, refused, or unreadable.
    shiny::observeEvent(input$file, {
      chk <- checked_r()
      if (is.null(chk)) {
        shiny::showNotification(
          "Could not read that file as a recodes CSV.", type = "error")
        return()
      }
      if (!chk$ok) {
        shiny::showNotification(.schema_error_msg(chk), type = "error",
                                duration = NULL)
        return()
      }
      adjust <- .import_adjust_msg(chk)
      if (!is.null(adjust)) {
        shiny::showNotification(adjust, type = "warning", duration = 10)
      }
    })

    output$preview <- shiny::renderUI({
      chk <- checked_r()
      if (is.null(chk)) return(shiny::tags$em("Upload a CSV to preview."))
      if (!chk$ok) {
        return(shiny::div(
          class = "text-danger",
          shiny::strong("Schema mismatch — merge refused."),
          shiny::tags$br(),
          .schema_error_msg(chk)
        ))
      }
      inc <- chk$df
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
      chk <- checked_r()
      if (is.null(chk)) {
        shiny::showNotification("Upload a readable CSV first.", type = "warning")
        return()
      }
      if (!chk$ok) {
        shiny::showNotification(.schema_error_msg(chk), type = "error",
                                duration = NULL)
        return()
      }
      inc <- chk$df
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

      # Defensive: both sides are schema-shaped, so this is a no-op reorder --
      # but it guarantees the session rule set can never gain a stray column.
      merged <- merged[, names(empty_recodes_tibble())]

      rules_proxy$set(merged)
      shiny::showNotification(
        sprintf("Merged. Now %d rule(s) in session.", nrow(merged)), type = "message")
    })
  })
}
