# =============================================================================
# Column normalizations (tab 3, Normalize)
# =============================================================================
# Case and whitespace tidying for a WHOLE COLUMN, as opposed to the
# value-to-value mappings on the Recodes tab. All the logic is pure and lives
# in R/string_helpers.R (NORMALIZE_OPS, apply_normalizations(),
# normalization_preview(), validate_normalizations(), .normalize_blocks());
# this module is UI plus wiring. The rule store is `norms_proxy`
# (make_norms_proxy() in R/rule_proxies.R).
#
# Two things this tab must never let the user get wrong:
#   - normalization is LOSSY. "McKenzie" title-cased is "Mckenzie" and there is
#     no undo, so the before / after preview is on screen BEFORE the rule is
#     added.
#   - normalizations run BEFORE the recodes. They are the first step, so the
#     preview is computed on the data as loaded, which is the frame they
#     really run against.
#
# The sibling choice is plain language first: "this column and its numbered
# siblings" builds the pattern with suggest_sibling_pattern() and names the
# columns it covers. A typed regex is an advanced override.
# =============================================================================

# Checkbox choices, built from the pure constants so the labels and the op
# tokens cannot drift apart.
normalize_op_choices <- function() {
  stats::setNames(NORMALIZE_OPS, unname(NORMALIZE_OP_LABELS[NORMALIZE_OPS]))
}

# Display columns of the rules table. The generated-markup "remove" column
# MUST stay last: the DT unescapes exactly that one column (`escape =
# -ncol(df)`), and every other column holds column names or a typed pattern.
.NORMALIZE_TABLE_COLS <- c("variable", "changes", "covers", "sibling_pattern",
                           "updated_at", "remove")

#' Plain-language issue list for validate_normalizations() output.
#'
#' Pure (tags only). NULL when there are no rules to validate; a quiet
#' all-clear line when the rules have no issues.
#'
#' @param issues named list from validate_normalizations(), or NULL.
normalization_issues_ui <- function(issues) {
  if (is.null(issues)) return(NULL)
  shown <- issues[vapply(issues, function(x) is.data.frame(x) && nrow(x) > 0,
                         logical(1))]
  if (length(shown) == 0) {
    return(shiny::div(class = "small text-muted mb-2 rs-norm-issues-ok",
                      "No problems found in the normalization rules."))
  }
  titles <- c(
    no_ops                  = "Nothing selected",
    invalid_ops             = "Unrecognised normalization",
    conflicting_case        = "Conflicting case choices",
    duplicate_targets       = "Same target twice",
    invalid_sibling_pattern = "Sibling pattern is not a valid regular expression",
    missing_column          = "Column not in the loaded data",
    overlapping_targets     = "Two rules reach one column")
  shiny::div(
    class = "alert alert-warning rs-norm-issues", role = "alert",
    shiny::strong("Check these normalization rules:"),
    shiny::tags$ul(class = "mb-0", lapply(names(shown), function(nm) {
      x <- shown[[nm]]
      var <- if ("variable" %in% names(x)) as.character(x$variable)
             else rep(NA_character_, nrow(x))
      det <- if ("detail" %in% names(x)) as.character(x$detail)
             else rep("", nrow(x))
      shiny::tags$li(
        shiny::strong(issue_class_title(nm, titles)), ": ",
        lapply(seq_len(nrow(x)), function(i) shiny::tagList(
          if (i > 1) "; ",
          if (!is.na(var[i])) shiny::tagList(shiny::tags$code(var[i]), " "),
          if (!is.na(det[i])) det[i])))
    }))
  )
}

mod_normalize_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = FALSE,
    fill        = FALSE,
    card_header_help(
      "Normalize columns",
      shiny::p("A normalization tidies how EVERY value in a column is ",
               "written (its capitalization, its spaces) without saying ",
               "which value becomes which. Use it when the values are right ",
               "but written inconsistently: ", shiny::em("latte"), ", ",
               shiny::em("LATTE"), ", ", shiny::em(" Latte "), "."),
      shiny::p("Use a recode (Browse values, Clusters, Spellcheck) when one ",
               "particular value has to become another."),
      shiny::p("Normalizations run first, then the recodes, then ",
               "recategorization. The Browse values, Clusters and Spellcheck ",
               "tabs show the normalized values. Nothing changes in your data ",
               "file: the rules are exported with the R script.")
    ),
    bslib::card_body(
      fillable = FALSE,
      help_line(
        "Pick the column in the \"Recoding:\" bar at the top, tick what to ",
        "tidy, and read the before / after list before you add the rule. ",
        "One rule per column: adding a column that already has a rule ",
        "replaces it."
      ),
      warning_banner(paste0(
        "Normalizing cannot be undone. Title Case turns McKenzie into ",
        "Mckenzie, so read the before / after list first.")),
      help_line(
        shiny::strong("Existing recode rules: "),
        "rules with match type ", shiny::tags$code("exact"), " or ",
        shiny::tags$code("regex"), " were written against the values as they ",
        "were. Once a column is normalized they may stop matching (an exact ",
        "rule for \"latte\" misses a column that is now \"Latte\"). ",
        "Preview & export lists any rule written under a different ",
        "normalization."
      ),
      help_line(
        shiny::strong("Not the same as the Clusters tab's options: "),
        "\"normalize before clustering\" only changes how values are grouped ",
        "there and never changes a value. A rule here rewrites the column."
      ),

      shiny::fluidRow(
        shiny::column(5,
          shiny::uiOutput(ns("target_line")),
          shiny::checkboxInput(ns("apply_siblings"),
            "Also apply to sibling columns", value = FALSE),
          shiny::conditionalPanel(
            condition = "input.apply_siblings === true", ns = ns,
            shiny::radioButtons(ns("sibling_mode"), label = NULL,
              choices = c(
                "This column and its numbered siblings" = "auto",
                "Columns matching a pattern I type (advanced)" = "custom"),
              selected = "auto"),
            shiny::conditionalPanel(
              condition = "input.sibling_mode === 'custom'", ns = ns,
              shiny::textInput(ns("sibling_pattern"), "Pattern (regex)",
                               value = "", width = "100%"),
              shiny::tags$small(shiny::tags$em(
                "A regular expression. Every column whose name matches gets ",
                "the same normalization, for example ",
                shiny::tags$code("^drink[0-9]+$"), "."))
            ),
            shiny::uiOutput(ns("sibling_note"))
          )),
        shiny::column(4,
          shiny::checkboxGroupInput(ns("ops"), "Normalize:",
                                    choices = normalize_op_choices())),
        shiny::column(3, style = "padding-top:1.6em;",
          shiny::actionButton(ns("add_rule"), "Add / update rule",
                              class = "btn btn-primary"),
          shiny::br(), shiny::br(),
          shiny::actionButton(ns("remove_current"),
                              "Remove rule for this column",
                              class = "btn btn-sm btn-outline-danger"))
      ),

      shiny::hr(),
      shiny::h5("Before / after preview"),
      shiny::uiOutput(ns("preview_note")),
      dt_truncate_css(),
      DT::DTOutput(ns("preview_tbl")),

      shiny::hr(),
      shiny::h5("Normalization rules in this session"),
      shiny::uiOutput(ns("validation")),
      shiny::uiOutput(ns("rules_empty")),
      DT::DTOutput(ns("rules_tbl"))
    )
  )
}

#' @param shared_state reactiveValues(data, meta, dataset_name). Read only.
#' @param selected_var_r reactive giving the column being worked on.
#' @param norms_proxy list(get=, add=, set=) over the session normalizations.
mod_normalize_server <- function(id, shared_state, selected_var_r,
                                 norms_proxy) {
  shiny::moduleServer(id, function(input, output, session) {

    data_r <- shiny::reactive(shared_state$data)

    var_r <- shiny::reactive({
      v <- selected_var_r()
      if (is.null(v) || length(v) != 1 || is.na(v) || !nzchar(v)) NULL else v
    })

    # The sibling pattern the controls describe: built in "auto" mode from the
    # column's numbered family, or typed in "custom" mode. NA when neither
    # yields one (the rule then covers the selected column alone).
    sibling_pattern_r <- shiny::reactive({
      if (!isTRUE(input$apply_siblings)) return(NA_character_)
      if (identical(input$sibling_mode %||% "auto", "custom")) {
        # A cleared (empty or whitespace-only) pattern is NO pattern, never
        # "": an empty sibling pattern would reach every column.
        return(clean_sibling_pattern(trimws(input$sibling_pattern %||% "")))
      }
      v <- var_r()
      d <- data_r()
      if (is.null(v) || is.null(d)) return(NA_character_)
      suggest_sibling_pattern(v, names(d))
    })

    # TRUE when the controls hold a sibling pattern that does not compile
    # (regex_compiles(), the test apply and codegen use). Such a rule would be
    # skipped on export, so it is refused at entry instead.
    pattern_invalid_r <- shiny::reactive({
      pat <- sibling_pattern_r()
      !is.na(pat) && !regex_compiles(pat)
    })

    # Columns the pattern reaches, or NULL for an invalid regex.
    pattern_cols_r <- shiny::reactive({
      pat <- sibling_pattern_r()
      d <- data_r()
      if (pattern_invalid_r()) return(NULL)
      if (is.na(pat) || is.null(d)) return(character(0))
      grep(pat, names(d), value = TRUE)
    })

    # What the sibling choice covers, named in plain words.
    output$sibling_note <- shiny::renderUI({
      if (!isTRUE(input$apply_siblings)) return(NULL)
      pat  <- sibling_pattern_r()
      auto <- !identical(input$sibling_mode %||% "auto", "custom")
      if (is.na(pat)) {
        return(shiny::tags$small(class = "text-muted", shiny::tags$em(
          if (auto) paste("This column has no numbered siblings, so the rule",
                          "covers it alone.")
          else "Type a pattern, or the rule covers this column alone.")))
      }
      cols <- pattern_cols_r()
      if (is.null(cols)) {
        return(shiny::tags$small(class = "text-danger",
          "That pattern is not a valid regular expression."))
      }
      shiny::tags$small(class = "text-muted", shiny::tags$em(
        sprintf("Applies to %d column%s: %s", length(cols),
                if (length(cols) == 1) "" else "s",
                if (length(cols) == 0) "(none in the loaded data)"
                else paste(cols, collapse = ", "))))
    })

    # The rule the controls describe, or NULL when it is not yet usable. Used
    # for BOTH the preview and the add button, so what is previewed is exactly
    # what gets added.
    draft_rule_r <- shiny::reactive({
      v <- var_r()
      if (is.null(v)) return(NULL)
      ops <- normalize_ops_join(input$ops)
      if (!nzchar(ops)) return(NULL)
      # An invalid typed pattern yields NO draft (not a quiet single-column
      # rule): the add button refuses it and the preview says why.
      if (pattern_invalid_r()) return(NULL)
      pattern  <- sibling_pattern_r()
      siblings <- isTRUE(input$apply_siblings) && !is.na(pattern)
      if (!siblings) pattern <- NA_character_
      now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      tibble::tibble(
        norm_id           = normalization_rule_id(v, pattern),
        variable          = v,
        apply_to_siblings = siblings,
        sibling_pattern   = pattern,
        ops               = ops,
        notes             = NA_character_,
        author            = tryCatch(Sys.info()[["user"]],
                                     error = function(e) NA_character_),
        created_at        = now,
        updated_at        = now,
        source_dataset    = shared_state$dataset_name %||% NA_character_
      )
    })

    # Switching to a column that already has a rule shows that rule's ticks,
    # so "Add / update" edits it instead of starting from blank.
    shiny::observeEvent(var_r(), {
      v <- var_r()
      cur <- norms_proxy$get()
      hit <- which(cur$variable == v)
      if (length(hit) == 0) {
        shiny::updateCheckboxGroupInput(session, "ops", selected = character(0))
        shiny::updateCheckboxInput(session, "apply_siblings", value = FALSE)
        return()
      }
      r <- cur[hit[1], ]
      shiny::updateCheckboxGroupInput(session, "ops",
                                      selected = .norm_ops_ordered(r$ops))
      fam <- !is.na(r$apply_to_siblings) && r$apply_to_siblings &&
        !is.na(r$sibling_pattern)
      shiny::updateCheckboxInput(session, "apply_siblings", value = fam)
      if (fam) {
        d <- shiny::isolate(data_r())
        auto <- !is.null(d) &&
          identical(suggest_sibling_pattern(v, names(d)), r$sibling_pattern)
        shiny::updateRadioButtons(session, "sibling_mode",
                                  selected = if (auto) "auto" else "custom")
        if (!auto) shiny::updateTextInput(session, "sibling_pattern",
                                          value = r$sibling_pattern)
      }
    }, ignoreNULL = TRUE)

    output$target_line <- shiny::renderUI({
      v <- var_r()
      shiny::div(
        class = "mb-2",
        shiny::strong("Column: "),
        if (is.null(v))
          shiny::tags$em(class = "text-muted",
                         "none selected. Pick one in the Recoding bar at the top.")
        else shiny::tags$code(v))
    })

    preview_r <- shiny::reactive({
      r <- draft_rule_r()
      d <- data_r()
      if (is.null(r) || is.null(d))
        return(tibble::tibble(column = character(0), before = character(0),
                              after = character(0), n_cells = integer(0)))
      normalization_preview(d, r)
    })

    output$preview_note <- shiny::renderUI({
      if (is.null(data_r())) {
        return(empty_state("Load a dataset on the Data tab to preview."))
      }
      if (pattern_invalid_r()) {
        return(empty_state(paste0(
          "The sibling pattern is not a valid regular expression. Fix it, or ",
          "untick \"Also apply to sibling columns\", to see what would change.")))
      }
      r <- draft_rule_r()
      if (is.null(r)) {
        return(empty_state(paste0(
          "Pick a column and tick at least one normalization to see what ",
          "would change.")))
      }
      p <- preview_r()
      n_cells <- sum(p$n_cells)
      if (nrow(p) == 0) {
        return(empty_state(paste0(
          "Nothing would change with these settings: every value is already ",
          "written that way.")))
      }
      shiny::tags$small(shiny::tags$em(sprintf(
        paste0("%d distinct value%s would be rewritten, covering %d cell%s. ",
               "Counted on the data as loaded, because normalization is the ",
               "first step."),
        nrow(p), if (nrow(p) == 1) "" else "s",
        n_cells, if (n_cells == 1) "" else "s")))
    })

    output$preview_tbl <- DT::renderDT({
      p <- preview_r()
      if (nrow(p) == 0) return(NULL)
      DT::datatable(
        p, rownames = FALSE, selection = "none",
        # escape stays TRUE (the default): before / after are raw dataset
        # text, and dt_truncate_defs() relies on escaped input.
        options = list(pageLength = 15, columnDefs = dt_truncate_defs()))
    })

    shiny::observeEvent(input$add_rule, {
      if (pattern_invalid_r()) {
        shiny::showNotification(
          sprintf(paste0("Not added: the sibling pattern \"%s\" is not a valid ",
                         "regular expression. Fix the pattern, or untick ",
                         "\"Also apply to sibling columns\"."),
                  sibling_pattern_r()),
          type = "error", duration = 8)
        return()
      }
      r <- draft_rule_r()
      if (is.null(r)) {
        shiny::showNotification(
          "Pick a column and tick at least one normalization first.",
          type = "warning")
        return()
      }
      existing <- norms_proxy$get()
      replaced <- any(existing$norm_id == r$norm_id |
                      existing$variable == r$variable)
      if (replaced) {
        old <- existing[existing$norm_id == r$norm_id |
                        existing$variable == r$variable, ][1, ]
        r$created_at <- old$created_at
      }
      norms_proxy$add(r)
      shiny::showNotification(
        sprintf("%s the normalization for %s.",
                if (replaced) "Updated" else "Added", r$variable),
        type = "message", duration = 4)
    })

    remove_ids <- function(ids) {
      cur <- norms_proxy$get()
      norms_proxy$set(cur[!(cur$norm_id %in% ids), , drop = FALSE])
    }

    shiny::observeEvent(input$remove_current, {
      v <- var_r()
      cur <- norms_proxy$get()
      ids <- if (is.null(v)) character(0) else cur$norm_id[cur$variable == v]
      if (length(ids) == 0) {
        shiny::showNotification("This column has no normalization rule.",
                                type = "warning")
        return()
      }
      remove_ids(ids)
      shiny::showNotification(sprintf("Removed the normalization for %s.", v),
                              type = "message", duration = 3)
    })

    rules_table_r <- shiny::reactive({
      r <- norms_proxy$get()
      d <- data_r()
      if (nrow(r) == 0) {
        out <- tibble::tibble(variable = character(0), changes = character(0),
                              covers = character(0),
                              sibling_pattern = character(0),
                              updated_at = character(0),
                              remove = character(0))
        return(out[, .NORMALIZE_TABLE_COLS])
      }
      ns <- session$ns
      r$changes <- vapply(r$ops, .ops_phrase, character(1), USE.NAMES = FALSE)
      r$covers <- vapply(seq_len(nrow(r)), function(i) {
        if (is.null(d)) return(NA_character_)
        cols <- tryCatch(.recode_cols(d, r[i, ]), error = function(e) character(0))
        if (length(cols) == 0) "(no column)" else paste(cols, collapse = ", ")
      }, character(1))
      # A norm_id this app minted is a 12-char hex hash, but one read from an
      # imported normalizations CSV is whatever the file said, so the button
      # names the row position and escapes the whole handler
      # (.rule_delete_button() in R/ui_helpers.R).
      r$remove <- vapply(seq_len(nrow(r)), function(i) {
        .rule_delete_button(ns("delete_row"), i, r$norm_id[i], label = "Remove")
      }, character(1))
      r[, .NORMALIZE_TABLE_COLS]
    })

    output$rules_empty <- shiny::renderUI({
      if (nrow(norms_proxy$get()) > 0) return(NULL)
      empty_state("No normalization rules yet.")
    })

    output$rules_tbl <- DT::renderDT({
      df <- rules_table_r()
      if (nrow(df) == 0) return(NULL)
      DT::datatable(
        df, rownames = FALSE, selection = "none",
        # SECURITY: only the last column ("remove") is generated markup.
        # variable / covers / sibling_pattern hold column names from the
        # loaded file and a typed pattern, so they must stay HTML-escaped.
        escape  = -ncol(df),
        options = list(pageLength = 15))
    })

    # One button, one row. `remove_ids()` above still removes every rule for a
    # column, which is what the "remove for this column" control means; a row's
    # Remove button must not reach a second rule that shares its id.
    shiny::observeEvent(input$delete_row, ignoreInit = TRUE, {
      cur <- norms_proxy$get()
      hit <- .rule_delete_target(input$delete_row, cur$norm_id)
      if (!hit$ok) {
        shiny::showNotification(hit$message, type = "warning", duration = 6)
        return()
      }
      norms_proxy$set(cur[-hit$row, , drop = FALSE])
      shiny::showNotification("Normalization removed.", type = "message",
                              duration = 2)
    })

    issues_r <- shiny::reactive({
      r <- norms_proxy$get()
      if (nrow(r) == 0) return(NULL)
      validate_normalizations(r, data = data_r())
    })

    output$validation <- shiny::renderUI(normalization_issues_ui(issues_r()))
  })
}
