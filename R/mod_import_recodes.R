# =============================================================================
# Import recodes
# =============================================================================
# Upload an externally-edited recodes_master.csv. Merge with conflict
# resolution per rule KEY (variable, match_type, old_value; compared with
# recode_key_id(), so a stale stored rule_id cannot hide a collision):
# keep-mine / keep-theirs / keep-newer. An incoming rule whose sibling pattern
# does not compile is accepted, and the preview says it will not apply.
#
# The same card imports a normalizations_master.csv, with the same three
# conflict choices keyed on norm_id. Normalization rules carry `updated_at`
# (set whenever the Normalize tab adds or replaces a rule), so "newer" means
# the same thing for both files, and one set of choices serves both.
#
# The incoming frame's schema is checked BEFORE any bind_rows. readr does not
# error when a col_types spec names a column the file lacks (it only warns on
# the console, which this app's user never sees), so an off-schema file would
# otherwise slip through and bind_rows would UNION the column names -- silently
# corrupting recodes_master.csv on the next export.
# =============================================================================

# Columns that upstream rule files may carry and this repo has no use for.
# They are dropped on import (and the user is told), rather than refused.
# Empty today: `norm_spec` used to be here, and is now part of the schema.
.RECODES_FOREIGN_DROP <- character(0)

# Provenance columns that an earlier tool wrote under a different name, mapped
# to this repo's name for the same field. A file carrying the old name is
# renamed on import and the user is told; the literal is the contract with
# those files, so it is defined here once and never spelled out again.
.PROVENANCE_ALIASES <- c(source_quarter = "source_dataset")

# Schema columns an incoming recodes file may lack. Their absence is not a
# "missing required column": `norm_spec` was added after files already
# existed, and an older file simply reads with an NA (unstamped) stamp.
.RECODES_OPTIONAL_COLS <- c("norm_spec")

#' Check an incoming recodes frame against this repo's schema.
#'
#' Tolerates known divergences:
#' - a provenance column written under an earlier tool's name
#'   (`.PROVENANCE_ALIASES`) is renamed to this repo's name for it.
#' - an optional column (`.RECODES_OPTIONAL_COLS`, i.e. `norm_spec`) may be
#'   absent; it is added as NA. When present it is kept.
#' - known-foreign columns (`.RECODES_FOREIGN_DROP`) are dropped.
#' Anything else off-schema is refused: an unknown extra, a missing required
#' column, or a file carrying both an alias and the name it maps to.
#'
#' @param inc tibble read by read_recodes(path, conform = FALSE).
#' @return list(ok = lgl, df = tibble or NULL, renamed = lgl,
#'              dropped = chr, added = chr, missing = chr, extra = chr)
.check_incoming_recodes <- function(inc) {
  expected <- names(empty_recodes_tibble())
  .check_incoming_schema(inc, expected, optional = .RECODES_OPTIONAL_COLS,
                         drop = .RECODES_FOREIGN_DROP)
}

#' Check an incoming normalizations frame against the normalizations schema.
#'
#' Same rules as `.check_incoming_recodes()`: a provenance alias is renamed;
#' anything else off-schema is refused.
#'
#' @param inc tibble read by read_normalizations(path, conform = FALSE).
#' @return the same list as `.check_incoming_recodes()`.
.check_incoming_normalizations <- function(inc) {
  .check_incoming_schema(inc, names(empty_normalizations_tibble()))
}

# The shared guard behind both checks.
.check_incoming_schema <- function(inc, expected, optional = character(0),
                                   drop = character(0)) {
  out <- list(ok = FALSE, df = NULL, renamed = FALSE, dropped = character(0),
              added = character(0), missing = character(0),
              extra = character(0))
  if (is.null(inc)) return(out)

  got <- names(inc)

  # An earlier tool's name for a provenance column (.PROVENANCE_ALIASES).
  # Renamed only when the file does NOT already carry this repo's name: a file
  # with both is off-schema and refused below.
  for (old in names(.PROVENANCE_ALIASES)) {
    new <- .PROVENANCE_ALIASES[[old]]
    if (old %in% got && !(new %in% got)) {
      names(inc)[names(inc) == old] <- new
      got <- names(inc)
      out$renamed <- TRUE
    }
  }

  out$missing <- setdiff(expected, c(got, optional))
  out$extra   <- setdiff(got, c(expected, drop))
  if (length(out$missing) > 0 || length(out$extra) > 0) return(out)

  out$dropped <- intersect(got, drop)
  out$added   <- setdiff(intersect(expected, optional), got)
  for (nm in out$added) inc[[nm]] <- rep(NA_character_, nrow(inc))

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
    bits <- c(bits, paste(sprintf("column '%s' was read as '%s'",
                                  names(.PROVENANCE_ALIASES),
                                  unname(.PROVENANCE_ALIASES)),
                          collapse = "; "))
  }
  if (length(chk$dropped) > 0) {
    bits <- c(bits, sprintf("column(s) %s were dropped",
                            paste0("'", chk$dropped, "'", collapse = ", ")))
  }
  if ("norm_spec" %in% chk$added) {
    bits <- c(bits, paste0(
      "the file has no 'norm_spec' column (it predates normalization stamps), ",
      "so its rules are unstamped; Preview & export flags any on a column ",
      "that is now normalized"))
  }
  if (length(bits) == 0) return(NULL)
  paste0("File adjusted on import: ", paste(bits, collapse = "; "), ".")
}

#' Human-readable explanation of a failed .check_incoming_recodes().
.schema_error_msg <- function(chk, what = "recodes") {
  bits <- character(0)
  if (length(chk$missing) > 0) {
    bits <- c(bits, sprintf("missing required column(s): %s",
                            paste(chk$missing, collapse = ", ")))
  }
  if (length(chk$extra) > 0) {
    bits <- c(bits, sprintf("unexpected column(s): %s",
                            paste(chk$extra, collapse = ", ")))
  }
  sprintf("That CSV does not match the %s schema (%s). Nothing was merged.",
          what, paste(bits, collapse = "; "))
}

# Timestamp shapes an `updated_at` cell can legitimately carry. The app writes
# the first one; the others turn up in a file edited by hand or round-tripped
# through a spreadsheet, which may drop the "T", the seconds or the time.
.STAMP_FORMATS <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d")

#' Parse an `updated_at` cell to comparable seconds. Pure.
#'
#' "Take newer" used to compare the two stamps as TEXT. That is only the same
#' as comparing times while both sides share one format: the app's "T"
#' separator sorts after a space (`"...02 23:00" < "...02T01:00"`), so a
#' spreadsheet-edited file lost every conflict on the same date whatever the
#' clock said. Vectorised; anything unparseable is NA, and the caller treats NA
#' as "not newer".
#'
#' @param x character stamps.
#' @return numeric seconds, NA where the stamp cannot be read.
.parse_stamp <- function(x) {
  x <- trimws(as.character(x))
  # Accept the ISO "T" separator and a trailing zone marker.
  x <- sub("T", " ", x, fixed = TRUE)
  x <- sub("(Z|[+-][0-9]{2}:?[0-9]{2})$", "", x)
  x <- trimws(x)
  out <- rep(NA_real_, length(x))
  for (fmt in .STAMP_FORMATS) {
    todo <- is.na(out) & !is.na(x) & nzchar(x)
    if (!any(todo)) break
    got <- suppressWarnings(
      as.numeric(as.POSIXct(x[todo], format = fmt, tz = "UTC")))
    out[todo] <- got
  }
  out
}

#' TRUE where the incoming stamp is strictly newer than the current one.
#'
#' An unparseable incoming stamp is never newer. An unparseable CURRENT stamp
#' loses to a readable incoming one, which is the behaviour "newer" already
#' had for a missing stamp: a known time beats no time.
.stamp_is_newer <- function(theirs, mine) {
  t <- .parse_stamp(theirs)
  m <- .parse_stamp(mine)
  !is.na(t) & (is.na(m) | t > m)
}

#' Merge incoming rules into the current set, resolving collisions.
#'
#' Pure. Rules only in `cur` keep their place; a colliding rule is decided by
#' `strategy` (mine / theirs / newer `updated_at`) and stays in `cur`'s
#' position; rules only in `inc` are appended in file order.
#'
#' Keeping colliding rules in place matters for recodes (first match wins by
#' row order). The previous merge appended every collision winner at the end,
#' which could reorder rules.
#'
#' What counts as a collision: by default the stored `id_col`. With `key_fn`
#' (for recodes, `recode_key_id()`), both sides are compared by the id their
#' KEY implies instead. A stored `rule_id` can be stale (saved before its
#' rule's key was edited, or written by another tool); comparing stored ids
#' then misses two rules with the same key, which both survive the merge and
#' fight over the same cells, and can pair two different rules that merely
#' share a stale id. Upstream found exactly that in its own import. The
#' winning rule keeps the id it carries (ids are never rewritten here;
#' Validate lists any that do not match).
#'
#' @param cur,inc schema-shaped rule tibbles.
#' @param id_col the id column: "rule_id" or "norm_id".
#' @param strategy "mine", "theirs" or "newer".
#' @param key_fn optional function(rules) returning one comparison key per
#'   row. NULL compares `id_col` itself.
#' @return tibble with `cur`'s columns.
merge_incoming_rules <- function(cur, inc, id_col, strategy = "newer",
                                 key_fn = NULL) {
  if (nrow(inc) == 0) return(cur)
  if (nrow(cur) == 0) return(inc[, names(cur), drop = FALSE])
  key_cur <- if (is.null(key_fn)) as.character(cur[[id_col]]) else key_fn(cur)
  key_inc <- if (is.null(key_fn)) as.character(inc[[id_col]]) else key_fn(inc)
  out <- cur
  idx <- match(key_cur, key_inc)
  for (i in which(!is.na(idx))) {
    t <- inc[idx[i], names(cur), drop = FALSE]
    m <- cur[i, , drop = FALSE]
    take_theirs <- switch(strategy,
      mine   = FALSE,
      theirs = TRUE,
      # Compared as TIMES, not as text (see .stamp_is_newer()).
      newer  = isTRUE(.stamp_is_newer(t$updated_at[1], m$updated_at[1])),
      FALSE)
    if (take_theirs) out[i, ] <- t
  }
  novel <- inc[!(key_inc %in% key_cur), names(cur), drop = FALSE]
  dplyr::bind_rows(out, novel)
}

#' What merging an incoming recodes file would do, for the import preview.
#'
#' Pure. Conflicts are found by KEY (`recode_key_id()` on both sides), the
#' same test `merge_incoming_rules()` uses for recodes, so the preview counts
#' exactly the rules the merge will treat as "in both".
#'
#' @param cur,inc recode rule tibbles (session rules, incoming rules).
#' @return list(n_incoming, n_novel, n_conflicts, n_stale_id_conflicts,
#'   n_unreadable_stamps, will_not_apply). `n_stale_id_conflicts` counts
#'   conflicts whose two stored ids differ (a stored-id check would have missed
#'   them). `n_unreadable_stamps` counts conflicts where the INCOMING
#'   `updated_at` cannot be read as a time, so "Take newer" cannot prefer it
#'   and the session's rule is kept.
#'   `will_not_apply` is a tibble (variable, old_value, sibling_pattern) of
#'   incoming sibling rules whose pattern does not compile: they are accepted
#'   but reach no column until the pattern is fixed.
import_recodes_summary <- function(cur, inc) {
  if (is.null(cur)) cur <- empty_recodes_tibble()
  key_cur <- recode_key_id(cur)
  key_inc <- recode_key_id(inc)
  in_both <- key_inc %in% key_cur
  stale <- 0L
  unreadable <- 0L
  if (any(in_both)) {
    pos   <- match(key_inc[in_both], key_cur)
    stale <- sum(as.character(inc$rule_id[in_both]) !=
                 as.character(cur$rule_id[pos]) |
                 is.na(inc$rule_id[in_both]) | is.na(cur$rule_id[pos]))
    unreadable <- sum(is.na(.parse_stamp(inc$updated_at[in_both])))
  }
  bad <- .bad_sibling_rule(inc$apply_to_siblings, inc$sibling_pattern)
  list(
    n_incoming           = nrow(inc),
    n_novel              = sum(!in_both),
    n_conflicts          = sum(in_both),
    n_stale_id_conflicts = as.integer(stale),
    n_unreadable_stamps  = as.integer(unreadable),
    will_not_apply       = tibble::tibble(
      variable        = as.character(inc$variable[bad]),
      old_value       = as.character(inc$old_value[bad]),
      sibling_pattern = as.character(inc$sibling_pattern[bad]))
  )
}

#' The recodes import preview, from import_recodes_summary(). Pure (tags).
import_recodes_preview_ui <- function(sm) {
  wna <- sm$will_not_apply
  shown <- function(x) if (is.na(x)) "<NA>" else if (!nzchar(x)) "(empty)" else x
  shiny::tagList(
    shiny::p(shiny::strong("Incoming rules: "), sm$n_incoming),
    shiny::p(shiny::strong("Novel (not in session): "), sm$n_novel),
    shiny::p(shiny::strong("Conflicts (same column, match type and old value): "),
             sm$n_conflicts),
    if (sm$n_stale_id_conflicts > 0) shiny::p(class = "small text-muted",
      sprintf(paste0("%d of these carr%s a different rule id on each side ",
                     "(an id saved before an edit, or by another tool). They ",
                     "are matched by what the rule does, not by the id."),
              sm$n_stale_id_conflicts,
              if (sm$n_stale_id_conflicts == 1) "ies" else "y")),
    if (isTRUE(sm$n_unreadable_stamps > 0)) shiny::div(
      class = "small text-muted rs-import-unreadable-stamps",
      sprintf(paste0("%d of these %s an updated_at this app cannot read as a ",
                     "time, so \"Take newer\" cannot prefer %s and the rule ",
                     "already in this session is kept. Pick \"Take incoming ",
                     "file\" to load %s anyway."),
              sm$n_unreadable_stamps,
              if (sm$n_unreadable_stamps == 1) "carries" else "carry",
              if (sm$n_unreadable_stamps == 1) "it" else "them",
              if (sm$n_unreadable_stamps == 1) "it" else "them")),
    if (nrow(wna) > 0) shiny::div(
      class = "alert alert-warning py-1 px-2 small rs-import-will-not-apply",
      shiny::strong(sprintf("%d incoming rule%s will not apply. ", nrow(wna),
                            if (nrow(wna) == 1) "" else "s")),
      if (nrow(wna) == 1) paste0(
        "Its sibling pattern is not a valid regular expression, so it reaches ",
        "no column. It is imported as it is; fix the pattern on the Recodes ",
        "tab.")
      else paste0(
        "Their sibling patterns are not valid regular expressions, so they ",
        "reach no column. They are imported as they are; fix the patterns on ",
        "the Recodes tab."),
      shiny::tags$ul(class = "mb-0", lapply(seq_len(nrow(wna)), function(i)
        shiny::tags$li(shiny::tags$code(shown(wna$variable[i])), " ",
                       shiny::tags$code(shown(wna$old_value[i])), ": pattern ",
                       shiny::tags$code(shown(wna$sibling_pattern[i]))))))
  )
}

mod_import_recodes_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = FALSE,
    fill        = FALSE,
    card_header_help(
      "Import / merge existing rule CSVs",
      shiny::p("Load a recodes CSV or a normalizations CSV exported earlier ",
               "(by you or a colleague) and add its rules to this session, so ",
               "you can pick up where you left off."),
      shiny::p("When both have a rule for the same thing, choose which one ",
               "wins: the one already here, the one in the file, or ",
               "whichever was edited most recently. The same choice applies ",
               "to both files.")
    ),
    bslib::card_body(
      fillable = FALSE,
      shiny::radioButtons(ns("conflict"), "When a rule is in both:",
        choices = c("Keep current session"  = "mine",
                    "Take incoming file"    = "theirs",
                    "Take newer updated_at" = "newer"),
        selected = "newer", inline = TRUE),
      shiny::fluidRow(
        shiny::column(6,
          shiny::fileInput(ns("file"), "Upload recodes_master.csv",
                           accept = c(".csv")),
          shiny::actionButton(ns("apply"), "Merge recodes",
                              class = "btn btn-primary"),
          shiny::tags$hr(),
          shiny::uiOutput(ns("preview"))
        ),
        shiny::column(6,
          shiny::fileInput(ns("norm_file"), "Upload normalizations_master.csv",
                           accept = c(".csv")),
          shiny::actionButton(ns("apply_norms"), "Merge normalizations",
                              class = "btn btn-primary"),
          shiny::tags$hr(),
          shiny::uiOutput(ns("norm_preview"))
        )
      )
    )
  )
}

#' @param rules_proxy list(add=, set=, get=)
#' @param norms_proxy optional list(add=, set=, get=) over the normalization
#'   rules. When NULL, a normalizations upload is refused with a message.
mod_import_recodes_server <- function(id, shared_state, rules_proxy,
                                      norms_proxy = NULL) {
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
          shiny::strong("Schema mismatch: merge refused."),
          shiny::tags$br(),
          .schema_error_msg(chk)
        ))
      }
      import_recodes_preview_ui(
        import_recodes_summary(rules_proxy$get(), chk$df))
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
      cur <- .conform_recodes_schema(rules_proxy$get())
      cur <- cur[, names(empty_recodes_tibble()), drop = FALSE]

      # Collisions by KEY, not by stored rule_id (see merge_incoming_rules()).
      merged <- merge_incoming_rules(cur, inc, "rule_id", input$conflict,
                                     key_fn = recode_key_id)

      # Defensive: both sides are schema-shaped, so this is a no-op reorder,
      # but it guarantees the session rule set can never gain a stray column.
      merged <- merged[, names(empty_recodes_tibble())]

      # set(), not add(): imported rules keep the norm_spec stamp they carry.
      rules_proxy$set(merged)
      n_bad <- sum(.bad_sibling_rule(merged$apply_to_siblings,
                                     merged$sibling_pattern))
      shiny::showNotification(
        sprintf("Merged. Now %d rule(s) in session.%s", nrow(merged),
                if (n_bad > 0) sprintf(paste0(
                  " %d will not apply until its sibling pattern is fixed ",
                  "(see Validate on the Recodes tab)."), n_bad) else ""),
        type = if (n_bad > 0) "warning" else "message")
    })

    # --- Normalizations CSV ---------------------------------------------------

    norm_checked_r <- shiny::reactive({
      f <- input$norm_file
      if (is.null(f)) return(NULL)
      raw <- tryCatch(read_normalizations(f$datapath, conform = FALSE),
                      error = function(e) NULL)
      if (is.null(raw)) return(NULL)
      chk <- .check_incoming_normalizations(raw)
      if (chk$ok) {
        chk$df <- tryCatch(.conform_normalizations_schema(chk$df),
                           error = function(e) NULL)
        if (is.null(chk$df)) return(NULL)
      }
      chk
    })

    shiny::observeEvent(input$norm_file, {
      chk <- norm_checked_r()
      if (is.null(chk)) {
        shiny::showNotification(
          "Could not read that file as a normalizations CSV.", type = "error")
        return()
      }
      if (!chk$ok) {
        shiny::showNotification(.schema_error_msg(chk, "normalizations"),
                                type = "error", duration = NULL)
        return()
      }
      adjust <- .import_adjust_msg(chk)
      if (!is.null(adjust)) {
        shiny::showNotification(adjust, type = "warning", duration = 10)
      }
    })

    output$norm_preview <- shiny::renderUI({
      chk <- norm_checked_r()
      if (is.null(chk)) return(shiny::tags$em("Upload a CSV to preview."))
      if (!chk$ok) {
        return(shiny::div(
          class = "text-danger",
          shiny::strong("Schema mismatch: merge refused."),
          shiny::tags$br(),
          .schema_error_msg(chk, "normalizations")
        ))
      }
      inc <- chk$df
      cur <- if (is.null(norms_proxy)) empty_normalizations_tibble()
             else norms_proxy$get()
      clash <- intersect(cur$norm_id, inc$norm_id)
      unreadable <- sum(is.na(.parse_stamp(
        inc$updated_at[inc$norm_id %in% clash])))
      shiny::tagList(
        shiny::p(shiny::strong("Incoming normalizations: "), nrow(inc)),
        shiny::p(shiny::strong("Novel (not in session): "),
                 length(setdiff(inc$norm_id, cur$norm_id))),
        shiny::p(shiny::strong("Conflicts (norm_id collision): "),
                 length(clash)),
        if (unreadable > 0) shiny::div(
          class = "small text-muted rs-import-unreadable-stamps",
          sprintf(paste0("%d of these %s an updated_at this app cannot read ",
                         "as a time, so \"Take newer\" keeps the rule already ",
                         "in this session."),
                  unreadable, if (unreadable == 1) "carries" else "carry"))
      )
    })

    shiny::observeEvent(input$apply_norms, {
      if (is.null(norms_proxy)) {
        shiny::showNotification("Normalizations cannot be imported here.",
                                type = "warning")
        return()
      }
      chk <- norm_checked_r()
      if (is.null(chk)) {
        shiny::showNotification("Upload a readable CSV first.", type = "warning")
        return()
      }
      if (!chk$ok) {
        shiny::showNotification(.schema_error_msg(chk, "normalizations"),
                                type = "error", duration = NULL)
        return()
      }
      cur <- .conform_normalizations_schema(norms_proxy$get())
      cur <- cur[, names(empty_normalizations_tibble()), drop = FALSE]
      merged <- merge_incoming_rules(cur, chk$df, "norm_id", input$conflict)
      merged <- merged[, names(empty_normalizations_tibble())]
      norms_proxy$set(merged)
      shiny::showNotification(
        sprintf(paste0("Merged. Now %d normalization(s) in session. Recode ",
                       "rules written under a different normalization are ",
                       "flagged on Preview & export."), nrow(merged)),
        type = "message")
    })
  })
}
