# =============================================================================
# Preview + export
# =============================================================================
# Diff view (before / after / affected cells), download buttons for the
# master CSVs + recode R script, copy-to-clipboard for the R, and a second
# card that downloads the cleaned data bundle (a zip built by
# R/export_bundle.R: cleaned CSV, rule files, one script, provenance).
#
# Everything here follows the export order: column normalizations first, then
# the recodes (on the normalized data), then recat. The normalization summary
# sits above the recode diff for that reason.
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

#' Per-rule summary of what the normalizations do to the loaded data.
#'
#' Pure (no Shiny) so it can be unit tested.
#'
#' @param normalizations normalization rules tibble.
#' @param summary apply_normalizations()$summary for those rules, or NULL / an
#'   empty tibble when no data is loaded.
#' @return tibble: target, changes, columns, cells_changed. `target` is the
#'   variable, or the sibling pattern for a family rule; `changes` is the ops
#'   in words; `columns` / `cells_changed` are NA without a summary.
normalization_summary_table <- function(normalizations, summary = NULL) {
  if (is.null(normalizations) || !is.data.frame(normalizations) ||
      nrow(normalizations) == 0) {
    return(tibble::tibble(target = character(0), changes = character(0),
                          columns = character(0), cells_changed = integer(0)))
  }
  n <- nrow(normalizations)
  family <- !is.na(normalizations$apply_to_siblings) &
    normalizations$apply_to_siblings & !is.na(normalizations$sibling_pattern)
  target <- ifelse(family, normalizations$sibling_pattern,
                   normalizations$variable)
  changes <- vapply(normalizations$ops, .ops_phrase, character(1),
                    USE.NAMES = FALSE)
  has_rows <- !is.null(summary) && nrow(summary) > 0 &&
    "norm_id" %in% names(summary)
  idx <- if (has_rows) match(normalizations$norm_id, summary$norm_id)
         else rep(NA_integer_, n)
  columns <- if (has_rows) {
    cols <- summary$columns[idx]
    ifelse(is.na(cols), NA_character_,
           gsub(NORMALIZE_LIST_SEP, ", ", cols, fixed = TRUE))
  } else rep(NA_character_, n)
  cells <- if (has_rows) as.integer(summary$cells_changed[idx])
           else rep(NA_integer_, n)
  tibble::tibble(target = as.character(target), changes = changes,
                 columns = columns, cells_changed = cells)
}

#' Rule-warning rows for recode rules whose target column is not in the data.
#'
#' Reads `validate_recodes(rules, data)$missing_column` (columns `rule_id`,
#' `variable`, `sibling_pattern`) rather than inventing its own signal, so the
#' Validate report on the Recodes tab and this panel always agree about which
#' rules are affected.
#'
#' Why it belongs here: nothing clears the rule set when a dataset is loaded,
#' which is deliberate (rules are reusable across files), so a rule for an
#' absent column is the ordinary consequence of loading a second file. The app
#' skips such a rule silently today. This REPORTS it and changes nothing: the
#' downloads keep working, and the exported script skips the same rules.
#'
#' Tolerant of a validator that does not report the class, so it is a no-op on
#' a build without it.
#'
#' @param rules recode rule tibble.
#' @param data  the loaded data frame, or NULL.
#' @return a tibble in the `empty_rule_warnings()` schema, possibly empty.
.warn_recodes_missing_column <- function(rules, data) {
  none <- empty_rule_warnings()
  if (is.null(data) || is.null(rules) || !is.data.frame(rules) ||
      nrow(rules) == 0) {
    return(none)
  }
  iss <- tryCatch(validate_recodes(rules, data = data),
                  error = function(e) NULL)
  x <- if (is.list(iss)) iss$missing_column else NULL
  if (!is.data.frame(x) || nrow(x) == 0) return(none)
  pat <- if ("sibling_pattern" %in% names(x)) as.character(x$sibling_pattern)
         else rep(NA_character_, nrow(x))
  tibble::tibble(
    kind     = rep("recode_missing_column", nrow(x)),
    id       = as.character(x$rule_id),
    target   = as.character(x$variable),
    message  = sprintf(paste0(
      "Recode rule for column '%s'%s did nothing: the loaded dataset has no ",
      "such column. Loading a dataset does not clear the rule set, so this ",
      "is expected if the rule was written for another file. The exported ",
      "script skips it too."),
      as.character(x$variable),
      ifelse(is.na(pat), "", sprintf(" (sibling pattern '%s')", pat))),
    n_before = rep(NA_integer_, nrow(x)),
    n_after  = rep(NA_integer_, nrow(x)),
    severity = rep("warning", nrow(x))
  )
}

#' Render the "rules that did nothing" panel.
#'
#' Pure (tags only, no reactivity) so it can be unit tested.
#'
#' Reads `severity` (RULE_WARNING_SEVERITIES in R/rule_warnings.R). Rows with
#' severity "warning" (or no severity at all: a tibble from before the column
#' existed) go in the yellow box and are counted as rules that did less than
#' expected. Rows with severity "note" are stamp-only differences the rule's
#' match type already ignores, so they cannot change the result: they are
#' listed in a separate, collapsed block titled with their count, and they do
#' not replace the all-clear line when there are no warnings. Any other
#' severity value is treated as a warning, the safe side.
#'
#' @param warnings a tibble in the `empty_rule_warnings()` schema (see
#'   R/rule_warnings.R), or NULL when there is nothing to check: no data
#'   loaded, or no rules of either kind in the session.
#' @return NULL when `warnings` is NULL; otherwise a tagList holding either
#'   the quiet "No problems found" line or the warning box, followed by the
#'   notes block when there are notes.
rule_warnings_ui <- function(warnings) {
  if (is.null(warnings)) return(NULL)
  sev <- if ("severity" %in% names(warnings)) as.character(warnings$severity)
         else rep("warning", nrow(warnings))
  is_note <- !is.na(sev) & sev == "note"
  notes <- warnings[is_note, , drop = FALSE]
  warnings <- warnings[!is_note, , drop = FALSE]
  n <- nrow(warnings)

  notes_ui <- if (nrow(notes) > 0) shiny::div(
    class = "rs-rule-notes mb-2",
    guide_note(
      title = sprintf("%d note%s: stamp differences that cannot change the result",
                      nrow(notes), if (nrow(notes) == 1) "" else "s"),
      shiny::tags$small(class = "d-block text-muted mb-1", paste0(
        "These rules were written under a different normalization, but their ",
        "match type already ignores the difference, so they match the same ",
        "cells either way. Listed for the record; nothing needs fixing.")),
      shiny::tags$ul(class = "mb-1 small",
                     lapply(notes$message, function(m) shiny::tags$li(m)))))

  if (n == 0) {
    return(shiny::tagList(
      # This line enumerates what WAS checked, so it goes stale the moment a
      # check is added to R/rule_warnings.R. Keep it in step with the kinds
      # collect_rule_warnings() can report.
      shiny::div(
        class = "small text-muted mb-2 rs-rule-warnings-ok",
        "No problems found: every normalization changes something, every ",
        "recode rule matches at least one cell that no earlier rule had ",
        "already claimed and was written under the current normalization, ",
        "every recode rule reaches a column that is in the data, and every ",
        "recategorization rule matches at least one row, before and after the ",
        "recodes."
      ),
      notes_ui))
  }
  n_recat <- sum(warnings$kind == "recat_terms_recoded_away")
  n_drift <- sum(warnings$kind %in% c("norm_spec_drift", "norm_spec_unstamped"))
  shiny::tagList(
    shiny::div(
      class = "alert alert-warning rs-rule-warnings",
      role  = "alert",
      shiny::strong(sprintf("%d rule%s did less than expected.",
                            n, if (n == 1) "" else "s")),
      shiny::tags$ul(
        class = "mb-1",
        lapply(warnings$message, function(m) shiny::tags$li(m))
      ),
      if (n_drift > 0) shiny::tags$small(class = "d-block", shiny::tags$em(
        "Normalizations are applied before the recodes, so recode rules must ",
        "describe the values as the normalizations leave them.")),
      if (n_recat > 0) shiny::tags$small(class = "d-block", shiny::tags$em(
        "Recodes are applied first and recategorization second, so ",
        "recategorization terms must describe the values as the recodes ",
        "leave them."))
    ),
    notes_ui)
}


mod_preview_export_ui <- function(id) {
  ns <- shiny::NS(id)
  card <- bslib::card(
    full_screen = FALSE,
    fill        = FALSE,
    card_header_help(
      "Preview & export",
      shiny::p("Check what your rules would do to the loaded data, then ",
               "download them. Nothing here changes your data file."),
      shiny::p("The recodes CSV and the normalizations CSV are the lists of ",
               "rules; keep them to reload or share them. The R script applies ",
               "the normalizations and then the recodes to the data when you ",
               "run it. A yellow box lists any rule that did nothing, was ",
               "written under a different normalization, or that the recodes ",
               "stopped from matching, so you can fix it before you export.")
    ),
    bslib::card_body(
      fillable = FALSE,
      warning_banner("This app does NOT modify your data file. It exports rule CSVs, an R script you run yourself and, if you ask for it below, a cleaned copy of the data as a separate download."),
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
      shiny::uiOutput(ns("rule_warnings")),
      shiny::hr(),
      shiny::h5("Column normalizations (applied first)"),
      help_line(
        "Each normalization runs before any recode. ",
        shiny::strong("Columns"), " = the columns it reaches in the loaded ",
        "data. ", shiny::strong("Cells changed"), " = cells whose value it ",
        "rewrote. The recode counts below are taken on the normalized values."
      ),
      shiny::uiOutput(ns("norm_empty")),
      DT::DTOutput(ns("norm_tbl")),
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
  shiny::tagList(card, shiny::br(), .bundle_card_ui(ns))
}

# The "Download cleaned data" card (R/export_bundle.R builds the zip).
.bundle_card_ui <- function(ns) {
  bslib::card(
    full_screen = FALSE,
    fill        = FALSE,
    card_header_help(
      "Download cleaned data",
      shiny::p("Downloads one zip file. Your original data file is not changed."),
      shiny::p("Inside: a cleaned copy of the data as CSV, the three rule files, ",
               "one R script that repeats the cleaning on your original file, ",
               "and a provenance record (provenance.txt to read, ",
               "provenance.json for software) saying which file was cleaned, ",
               "when, with how many rules, and any warnings."),
      shiny::p("Cleaning runs in a fixed order: normalizations, then recodes, ",
               "then recategorization.")
    ),
    bslib::card_body(
      fillable = FALSE,
      help_line(
        "The zip holds a cleaned COPY of the loaded data (missing values as ",
        "empty cells), the normalization, recode and recategorization rule ",
        "files, a script that rebuilds the copy from your original file, and ",
        "provenance.txt explaining it all."
      ),
      shiny::uiOutput(ns("bundle_summary")),
      shiny::uiOutput(ns("bundle_button"))
    )
  )
}

#' @param rules_proxy list(add=, set=, get=)
#' @param recat_proxy optional list(add=, set=, get=) over the recat rules.
#'   Used only to check recat rules for matches lost to the recodes; when NULL
#'   that check is skipped.
#' @param norms_proxy optional list(add=, set=, get=) over the normalization
#'   rules. When NULL the session is treated as having no normalizations.
mod_preview_export_server <- function(id, shared_state, rules_proxy,
                                      recat_proxy = NULL, norms_proxy = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    dataset_id <- shiny::reactive({
      nm <- shared_state$dataset_name %||% "dataset"
      tools::file_path_sans_ext(basename(nm))
    })

    norms_r <- shiny::reactive({
      n <- if (is.null(norms_proxy)) NULL else norms_proxy$get()
      if (is.null(n)) empty_normalizations_tibble() else n
    })

    # Normalize, then recode: the export order. `df_normalized` is the frame
    # the recodes see; `df_after` the frame recat would see.
    diff_r <- shiny::reactive({
      r <- rules_proxy$get()
      d <- shared_state$data
      nn <- norms_r()
      if (is.null(d)) {
        return(list(rules = r, summary = tibble::tibble(),
                    norm_summary = tibble::tibble(),
                    df_normalized = NULL, df_after = NULL))
      }
      nres <- apply_normalizations(d, nn)
      if (nrow(r) == 0) {
        return(list(rules = r, summary = tibble::tibble(),
                    norm_summary = nres$summary,
                    df_normalized = nres$df, df_after = nres$df))
      }
      res <- apply_recodes(nres$df, r)
      list(rules = r, summary = res$summary, norm_summary = nres$summary,
           df_normalized = nres$df, df_after = res$df)
    })

    output$summary <- shiny::renderUI({
      r <- rules_proxy$get()
      nn <- norms_r()
      if (nrow(r) == 0 && nrow(nn) == 0)
        return(shiny::tags$em("No rules in session."))
      diff <- diff_r()
      if (is.null(shared_state$data)) {
        return(shiny::tagList(
          shiny::p(shiny::strong("Normalizations: "), nrow(nn)),
          shiny::p(shiny::strong("Rules: "), nrow(r)),
          shiny::p(shiny::tags$em("Load a dataset to count the cells these rules would change."))
        ))
      }
      s <- diff$summary
      total <- function(nm) if (nm %in% names(s)) sum(s[[nm]], na.rm = TRUE) else 0L
      ns_sum <- diff$norm_summary
      shiny::tagList(
        shiny::p(shiny::strong("Normalizations: "), nrow(nn),
                 if (nrow(nn) > 0 && "cells_changed" %in% names(ns_sum))
                   sprintf(" (%d cells changed)",
                           as.integer(sum(ns_sum$cells_changed, na.rm = TRUE)))),
        shiny::p(shiny::strong("Rules: "), nrow(r)),
        shiny::p(shiny::strong("Cells matched: "), total("cells_matched")),
        shiny::p(shiny::strong("Cells changed: "), total("cells_changed")),
        shiny::p(shiny::strong("Cells shadowed: "), total("cells_shadowed")),
        shiny::p(shiny::strong("Dataset: "), shared_state$dataset_name %||% "(none)")
      )
    })

    # Rules that did nothing (R/rule_warnings.R). Built from diff_r(), which
    # has already applied the normalizations and recodes once, instead of
    # collect_rule_warnings(), which would apply them a second time; the result
    # is the same (a test pins that).
    rule_warnings_r <- shiny::reactive({
      d <- shared_state$data
      if (is.null(d)) return(NULL)
      r     <- rules_proxy$get()
      nn    <- norms_r()
      recat <- if (is.null(recat_proxy)) empty_recat_tibble()
               else recat_proxy$get()
      if (is.null(recat)) recat <- empty_recat_tibble()
      if (nrow(r) == 0 && nrow(recat) == 0 && nrow(nn) == 0) return(NULL)
      tryCatch({
        diff <- diff_r()
        dplyr::bind_rows(
          empty_rule_warnings(),
          .warn_normalizations(nn, diff$norm_summary),
          .warn_recodes_no_match_from_summary(diff$df_normalized, r,
                                              diff$summary),
          .warn_norm_spec_drift(r, nn, names(d)),
          .warn_recodes_missing_column(r, d),
          warn_recat_terms_recoded_away(diff$df_normalized, diff$df_after,
                                        recat)
        )},
        error = function(e) NULL
      )
    })

    output$rule_warnings <- shiny::renderUI(rule_warnings_ui(rule_warnings_r()))

    output$buttons <- shiny::renderUI({
      shiny::div(style = "text-align:right;",
        shiny::downloadButton(session$ns("dl_csv"), "Download recodes CSV",
                              class = "btn btn-sm btn-primary"),
        shiny::downloadButton(session$ns("dl_norms"),
                              "Download normalizations CSV",
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

    output$norm_empty <- shiny::renderUI({
      if (nrow(norms_r()) > 0) return(NULL)
      empty_state("No normalizations. Add them on the Normalize tab.")
    })

    output$norm_tbl <- DT::renderDT({
      nn <- norms_r()
      if (nrow(nn) == 0) return(NULL)
      df <- normalization_summary_table(nn, diff_r()$norm_summary)
      DT::datatable(
        df,
        rownames = FALSE,
        selection = "none",
        # escape stays TRUE (the default): target and columns are column
        # names from the loaded file.
        options  = list(pageLength = 10, dom = "tp",
                        columnDefs = dt_truncate_defs())
      ) |>
        DT::formatStyle("cells_changed",
          backgroundColor = DT::styleEqual(c(0L), c("#eeeeee")))
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

    # One script: normalization blocks first, then the recode blocks.
    #
    # `data_cols` matters: without it generate_recode_R() tests the overlap
    # WARNING only against columns some rule names, so two sibling patterns
    # that share a third column of the real data warn in the bundle's script
    # and not in this one, and a rule for an absent column is skipped in one
    # and not the other. The same rules must not produce two different
    # scripts. R/export_bundle.R already passes the loaded column names.
    #
    # TRANSITIONAL: the argument arrives with the engine fix for review item 5.
    # Once it is in, collapse this to a plain call passing `data_cols`.
    script_r <- function() {
      d <- shared_state$data
      args <- list(rules_proxy$get(), dataset_id = dataset_id(),
                   normalizations = norms_r())
      if ("data_cols" %in% names(formals(generate_recode_R))) {
        args$data_cols <- if (!is.null(d)) names(d) else NULL
      }
      do.call(generate_recode_R, args)
    }

    output$r_preview <- shiny::renderText(script_r())

    output$dl_csv <- shiny::downloadHandler(
      filename = function() "recodes_master.csv",
      content  = function(file) write_recodes(rules_proxy$get(), file)
    )

    output$dl_norms <- shiny::downloadHandler(
      filename = function() "normalizations_master.csv",
      content  = function(file) write_normalizations(norms_r(), file)
    )

    output$dl_r <- shiny::downloadHandler(
      filename = function() sprintf("recode_%s.R", dataset_id()),
      content  = function(file) writeLines(script_r(), file)
    )

    # --- Cleaned data bundle (R/export_bundle.R) ---------------------------

    app_version <- app_version_from_description("DESCRIPTION")

    recat_r <- shiny::reactive({
      rc <- if (is.null(recat_proxy)) NULL else recat_proxy$get()
      if (is.null(rc)) empty_recat_tibble() else rc
    })

    bundle_id <- shiny::reactive({
      sm <- shared_state$source_meta
      bundle_dataset_id(if (!is.null(sm$file_name)) sm$file_name
                        else shared_state$dataset_name %||% "dataset")
    })

    # The load-time description of the file; a minimal one (name only) when
    # the state carries none.
    source_meta_r <- function() {
      sm <- shared_state$source_meta
      if (!is.null(sm)) return(sm)
      list(file_name = shared_state$dataset_name %||% NA_character_)
    }

    # The cleaned result for the summary line. An error is returned, not
    # thrown, so the card can say what went wrong.
    bundle_preview_r <- shiny::reactive({
      d <- shared_state$data
      if (is.null(d)) return(NULL)
      tryCatch(
        build_cleaned_dataset(d, rules_proxy$get(), recat_r(), norms_r()),
        error = function(e) e)
    })

    output$bundle_summary <- shiny::renderUI({
      d <- shared_state$data
      if (is.null(d)) {
        return(empty_state(
          "No data loaded. Load a dataset on the Data tab to download a cleaned copy."))
      }
      res <- bundle_preview_r()
      if (inherits(res, "error")) {
        return(help_line(shiny::strong("Cannot build the cleaned copy: "),
                         conditionMessage(res)))
      }
      n_w <- nrow(res$warnings)
      help_line(sprintf(paste0(
        "%s rows. Columns: %d before, %d after. Rules applied: %d ",
        "normalization%s, %d recode%s, %d recategorization%s. %d warning%s."),
        format(nrow(res$df), big.mark = ","), ncol(d), ncol(res$df),
        nrow(norms_r()), if (nrow(norms_r()) == 1) "" else "s",
        nrow(rules_proxy$get()), if (nrow(rules_proxy$get()) == 1) "" else "s",
        nrow(recat_r()), if (nrow(recat_r()) == 1) "" else "s",
        n_w, if (n_w == 1) "" else "s"))
    })

    output$bundle_button <- shiny::renderUI({
      if (is.null(shared_state$data)) return(NULL)
      shiny::downloadButton(session$ns("dl_bundle"),
                            "Download cleaned data (zip)",
                            class = "btn btn-sm btn-primary")
    })

    # Builds in a fresh tempfile() directory and zips to `file`; nothing is
    # written anywhere else. Named so tests can call it directly.
    bundle_content <- function(file) {
      d <- shared_state$data
      if (is.null(d)) stop("No dataset loaded.", call. = FALSE)
      export_bundle_zip(file, d, source_meta_r(), rules_proxy$get(),
                        recat_r(), norms_r(), app_version = app_version)
    }

    output$dl_bundle <- shiny::downloadHandler(
      filename = function() {
        sprintf("%s_cleaned_bundle_%s.zip", bundle_id(),
                format(Sys.time(), "%Y%m%d-%H%M%S"))
      },
      content = bundle_content,
      contentType = "application/zip"
    )

    shiny::observeEvent(input$copy_r, {
      txt <- script_r()
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
