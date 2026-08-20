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

#' Check an incoming recodes frame against this repo's schema.
#'
#' Tolerates one known upstream divergence: the upstream pipeline copy names the
#' provenance field `source_quarter` where this repo uses `source_dataset`.
#' That lone difference is renamed; anything else is refused.
#'
#' @param inc tibble read by read_recodes().
#' @return list(ok = lgl, df = tibble or NULL, renamed = lgl,
#'              missing = chr, extra = chr)
.check_incoming_recodes <- function(inc) {
  expected <- names(empty_recodes_tibble())
  out <- list(ok = FALSE, df = NULL, renamed = FALSE,
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
  out$extra   <- setdiff(got, expected)
  if (length(out$missing) > 0 || length(out$extra) > 0) return(out)

  # Column ORDER may differ harmlessly; force it to the canonical order so the
  # merge can never produce a frame whose columns differ from the schema.
  out$ok <- TRUE
  out$df <- inc[, expected]
  out
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

    # Raw read + schema check. Returns the .check_incoming_recodes() list, or
    # NULL when nothing has been uploaded / the file would not parse at all.
    checked_r <- shiny::reactive({
      f <- input$file
      if (is.null(f)) return(NULL)
      raw <- tryCatch(read_recodes(f$datapath), error = function(e) NULL)
      if (is.null(raw)) return(NULL)
      .check_incoming_recodes(raw)
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
      if (chk$renamed) {
        shiny::showNotification(
          paste0("Sibling-repo file normalized: column 'source_quarter' was ",
                 "read as 'source_dataset'."),
          type = "warning")
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
