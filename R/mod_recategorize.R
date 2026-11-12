# =============================================================================
# Recategorize
# =============================================================================
# Cross-variable, multi-term boolean rules that DERIVE a new output column.
# Distinct from the per-value old->new recodes on the other tabs: a recat rule
# never rewrites a source cell.
#
#   out_col       = "drink_family"
#   category      = "Espresso-based"
#   vars          = drink1 ; drink2            (or a sibling family)
#   include_terms = espresso ; macchiato       (OR)
#   exclude_terms = decaf                      (AND-NOT, rule-wide)
#
# Pure logic (match / apply / codegen) lives in R/recat_helpers.R so it is
# testable without Shiny.
# =============================================================================

# validate_recat() classes that make a rule match nothing: a rule carrying
# any of these is refused by Add instead of being stored.
# Classes that make Add refuse a rule. recat_add_refusal() names a class it
# has no bespoke wording for from .RECAT_ISSUE_TITLES, so adding one here (and
# a title below) is the whole change.
#
# PENDING review item 7 (Shiny F3): the out_col collision class that
# validate_recat() is gaining belongs in this vector and in the titles below.
# Its name was not settled when this was written, so it is not guessed here;
# until it is added, an out_col naming an existing column is still accepted.
.RECAT_REFUSE_CLASSES <- c("invalid_match_type", "invalid_regex",
                           "no_include_terms", "invalid_sibling_pattern")

# Titles for the validate_recat() classes. A class missing here still renders,
# under issue_class_title()'s fallback.
.RECAT_ISSUE_TITLES <- c(
  invalid_match_type      = "Unrecognised match type",
  invalid_regex           = "Regex term is not a valid regular expression",
  no_include_terms        = "No include terms",
  invalid_sibling_pattern = "Sibling pattern is not a valid regular expression",
  duplicate_ids           = "Same rule id twice",
  missing_columns         = "Column not in the loaded data",
  no_columns              = "No column to read")

#' Why Add must refuse a recat rule, or NULL when it may be added.
#'
#' Pure. Runs validate_recat() on the one rule and reports the classes that
#' make a rule match nothing (`.RECAT_REFUSE_CLASSES`), in plain language.
#'
#' @param rule one-row recat rule tibble.
#' @return NULL, or a single notification string naming every problem.
recat_add_refusal <- function(rule) {
  if (is.null(rule) || !is.data.frame(rule) || nrow(rule) == 0) return(NULL)
  iss <- validate_recat(rule)
  bits <- character(0)
  if (nrow(iss$invalid_match_type) > 0) {
    bits <- c(bits, sprintf("the match type '%s' is not one of %s",
                            iss$invalid_match_type$value[1],
                            paste(RECAT_MATCH_TYPES, collapse = " / ")))
  }
  if (nrow(iss$invalid_regex) > 0) {
    bits <- c(bits, sprintf("%s not a valid regular expression: %s",
                            if (nrow(iss$invalid_regex) == 1) "this term is"
                            else "these terms are",
                            paste0("'", iss$invalid_regex$value, "'",
                                   collapse = ", ")))
  }
  if (nrow(iss$no_include_terms) > 0) {
    bits <- c(bits, "it has no include terms")
  }
  if (nrow(iss$invalid_sibling_pattern) > 0) {
    bits <- c(bits, sprintf("the sibling pattern '%s' is not a valid regular expression",
                            iss$invalid_sibling_pattern$value[1]))
  }
  # The four classes above all mean "this rule would match nothing", which is
  # why the message ends the way it does. A refuse-class validate_recat()
  # gains later need not mean that (an out_col that collides with an existing
  # column is the opposite problem), so it is reported on its own terms: the
  # validator's `detail`, or failing that the class title. Adding the class to
  # .RECAT_REFUSE_CLASSES, and a title to .RECAT_ISSUE_TITLES, is then the
  # whole change needed for Add to refuse it.
  .MATCH_NOTHING <- c("invalid_match_type", "invalid_regex",
                      "no_include_terms", "invalid_sibling_pattern")
  other <- character(0)
  for (nm in setdiff(.RECAT_REFUSE_CLASSES, .MATCH_NOTHING)) {
    x <- iss[[nm]]
    if (!is.data.frame(x) || nrow(x) == 0) next
    detail <- if ("detail" %in% names(x)) as.character(x$detail[1])
              else NA_character_
    other <- c(other, if (!is.na(detail) && nzchar(detail)) detail
                      else tolower(issue_class_title(nm,
                                                     .RECAT_ISSUE_TITLES)))
  }
  if (length(bits) == 0 && length(other) == 0) return(NULL)
  msg <- "Rule not added: "
  if (length(bits) > 0) {
    msg <- paste0(msg, paste(bits, collapse = "; "),
                  ". As written it would match nothing.")
    if (length(other) > 0) msg <- paste0(msg, " Also: ")
  }
  if (length(other) > 0) {
    msg <- paste0(msg, paste(other, collapse = "; "), ".")
  }
  msg
}

#' Plain-language list of validate_recat() issues for the session's rules.
#'
#' Pure (tags only). NULL when there are no rules; a quiet all-clear line when
#' there are no issues; otherwise a warning box, one entry per class, with the
#' count. Every value is passed as text, so it is escaped.
#'
#' @param issues named list from validate_recat(), or NULL.
recat_issues_ui <- function(issues) {
  if (is.null(issues)) return(NULL)
  shown <- issues[vapply(issues, function(x) is.data.frame(x) && nrow(x) > 0,
                         logical(1))]
  if (length(shown) == 0) {
    return(shiny::div(class = "small text-muted mb-2 rs-recat-issues-ok",
                      "No problems found in the recategorization rules."))
  }
  col_or <- function(x, nm) if (nm %in% names(x)) as.character(x[[nm]])
                            else rep(NA_character_, nrow(x))
  shiny::div(
    class = "alert alert-warning rs-recat-issues", role = "alert",
    shiny::strong("Check these recategorization rules:"),
    shiny::tags$ul(class = "mb-0", lapply(names(shown), function(nm) {
      x <- shown[[nm]]
      out_col <- col_or(x, "out_col"); category <- col_or(x, "category")
      value   <- col_or(x, "value");   detail   <- col_or(x, "detail")
      shiny::tags$li(
        shiny::strong(sprintf("%s (%d)",
                              issue_class_title(nm, .RECAT_ISSUE_TITLES),
                              nrow(x))), ": ",
        lapply(seq_len(nrow(x)), function(i) shiny::tagList(
          if (i > 1) "; ",
          if (!is.na(out_col[i]) || !is.na(category[i])) shiny::tagList(
            shiny::tags$code(sprintf("%s = %s",
                                     ifelse(is.na(out_col[i]), "?", out_col[i]),
                                     ifelse(is.na(category[i]), "?", category[i]))),
            " "),
          if (!is.na(value[i])) shiny::tagList(shiny::tags$code(value[i]), " "),
          if (!is.na(detail[i])) detail[i])))
    }))
  )
}

mod_recategorize_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = FALSE,
    fill        = FALSE,
    card_header_help(
      "Recategorize",
      shiny::p("Make a NEW column that sorts rows into categories, based on ",
               "words found in one or more existing columns. Your original ",
               "columns are not changed."),
      shiny::p("Example: put \"Espresso-based\" in a drink_family column for ",
               "any row whose drink columns mention espresso or macchiato. ",
               "Open \"How recategorization works\" below for the full rules.")
    ),
    bslib::card_body(
      fillable = FALSE,

      warning_banner(paste0(
        "These rules DERIVE a new category column from cross-variable term ",
        "logic; they never overwrite your source values. As everywhere else ",
        "in this app, nothing is written to your data file: export the CSV ",
        "and the R script instead."
      )),

      # --- How it works (collapsed by default) --------------------------------
      guide_note(
        title = "How recategorization works",
        shiny::p(
          "A rule reads across one or more text variables and derives a ",
          shiny::tags$strong("new category column"),
          ": it never edits your source values. Rows that match get the ",
          "category you choose; everything else stays blank (NA)."
        ),
        shiny::tags$ul(
          class = "mb-1",
          shiny::tags$li(shiny::HTML(paste0(
            "<strong>Include (OR):</strong> a row matches when <em>any</em> ",
            "chosen variable contains <em>any</em> include term."))),
          shiny::tags$li(shiny::HTML(paste0(
            "<strong>Exclude (AND-NOT):</strong> ...but the row is dropped if ",
            "<em>any</em> chosen variable contains <em>any</em> exclude term."))),
          shiny::tags$li(shiny::HTML(paste0(
            "<strong>Literal vs Regex:</strong> literal = case-insensitive ",
            "substring match; regex = a case-insensitive pattern (alternation ",
            "<code>|</code>, anchors, groups, etc.)."))),
          shiny::tags$li(shiny::HTML(paste0(
            "<strong>Priority &amp; first-match-wins:</strong> rules sharing an ",
            "output column apply lowest-number-first; the first rule that ",
            "matches a row wins, and any value already set is kept."))),
          shiny::tags$li(shiny::HTML(paste0(
            "<strong>Recodes run first:</strong> the generated recat script is ",
            "meant to run <em>after</em> the recode script, so ",
            "<em>Preview match count</em> is computed on the recoded data too. ",
            "A term a recode has rewritten stops matching, and the preview ",
            "says so."))),
          shiny::tags$li(shiny::HTML(paste0(
            "<strong>Export:</strong> this tab writes a recat CSV and a ",
            "runnable R script. The app never touches your data file.")))
        )
      ),

      # --- Worked examples (collapsible) + load control ------------------------
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Worked examples (click to expand, or load one below)",
          icon = shiny::icon("lightbulb"),
          shiny::p(shiny::tags$small(shiny::tags$em(paste0(
            "These are written against the bundled café-orders example dataset ",
            "(Data tab → \"Load bundled example dataset\"), but the shapes ",
            "transfer to any dataset.")))),
          shiny::tags$ol(
            class = "mb-2",
            shiny::tags$li(shiny::HTML(paste0(
              "<strong>Espresso-based drinks</strong>: across the ",
              "<code>drink1</code>/<code>drink2</code> family, INCLUDE ",
              "<code>espresso</code> OR <code>macchiato</code> OR ",
              "<code>americano</code> OR <code>cortado</code> plus their common ",
              "misspellings (literal) &rarr; <code>drink_family</code> = ",
              "<code>Espresso-based</code>. Shows OR across sibling columns."))),
            shiny::tags$li(shiny::HTML(paste0(
              "<strong>Milk-based drinks</strong>: same columns, one ",
              "regex <code>ca[bp]+u?c+ino|latt|moc+[ah]+|flat ?white</code> ",
              "covering every spelling variant &rarr; ",
              "<code>drink_family</code> = <code>Milk-based</code> at priority ",
              "20. Shows regex, and first-match-wins against example 1."))),
            shiny::tags$li(shiny::HTML(paste0(
              "<strong>Reviews needing follow-up</strong>: over ",
              "<code>review</code>, INCLUDE <code>slow</code>, ",
              "<code>lukewarm</code>, <code>burnt</code>, <code>watery</code>, ",
              "<code>cold</code>&hellip; but EXCLUDE <code>cold brew</code>, so ",
              "a happy \"loved the cold brew\" review is not flagged (literal) ",
              "&rarr; <code>review_flag</code> = <code>Needs follow-up</code>. ",
              "Shows AND-NOT rescuing a false positive.")))
          ),
          shiny::tags$small(shiny::tags$em(paste0(
            "Loading an example fills the rule form below and auto-selects the ",
            "matching columns if your data has them. Adjust, then Add rule."))),
          shiny::br(), shiny::br(),
          shiny::div(
            class = "d-flex gap-2 align-items-end",
            shiny::selectInput(ns("example_pick"), "Load a worked example",
              choices = c(
                "1. Espresso-based drinks (literal, OR synonyms)" = "espresso",
                "2. Milk-based drinks (regex, lower priority)"    = "milk",
                "3. Reviews needing follow-up (literal + exclude)" = "review"),
              width = "360px"),
            shiny::actionButton(ns("load_example"), "Load example",
              class = "btn btn-outline-secondary")
          )
        )
      ),

      shiny::fluidRow(
        # --- Rule builder -----------------------------------------------------
        shiny::column(7,
          shiny::h5("Build a rule"),
          shiny::fluidRow(
            shiny::column(6,
              shiny::textInput(ns("out_col"), "Output column",
                value = "category",
                placeholder = "e.g. drink_family")),
            shiny::column(6,
              shiny::textInput(ns("category"), "Category to assign",
                value = "", placeholder = "e.g. Espresso-based"))
          ),

          shiny::selectizeInput(ns("vars"), "Source variables (any-of)",
            choices = NULL, multiple = TRUE,
            options = list(placeholder = "pick one or more columns")),
          shiny::div(
            shiny::actionLink(ns("add_siblings"),
              "Add sibling family of the first selected variable"),
            shiny::tags$small(shiny::tags$em(
              " - e.g. all drink1 / drink2 columns"))
          ),
          shiny::br(),

          shiny::radioButtons(ns("match_type"), "Term matching",
            choices = c("Literal (contains, case-insensitive)" = "literal",
                        "Regex (case-insensitive)"             = "regex"),
            selected = "literal"),

          shiny::textAreaInput(ns("include_terms"),
            "INCLUDE terms: matches if ANY appears (OR)",
            rows = 2,
            placeholder = "one term per line, e.g.\nespresso\nmacchiato"),
          shiny::textAreaInput(ns("exclude_terms"),
            "EXCLUDE terms: excluded if ANY appears (NOT)",
            rows = 2,
            placeholder = "one term per line, e.g.\ncold brew"),

          shiny::numericInput(ns("priority"), "Priority (lower wins first)",
            value = 100, min = 0, step = 10),
          shiny::textInput(ns("notes"), "Notes (optional)", value = ""),

          shiny::div(
            shiny::actionButton(ns("preview"), "Preview match count",
              class = "btn btn-outline-primary"),
            shiny::actionButton(ns("add_rule"), "Add rule",
              class = "btn btn-primary")
          ),
          shiny::br(),
          shiny::uiOutput(ns("preview_out"))
        ),

        # --- Plain-English restatement ----------------------------------------
        shiny::column(5,
          shiny::h5("How it reads"),
          shiny::uiOutput(ns("rule_sentence")),
          shiny::hr(),
          shiny::tags$small(shiny::tags$ul(
            shiny::tags$li("A row matches when ANY chosen variable contains ANY include term."),
            shiny::tags$li("...and NONE of the chosen variables contains ANY exclude term."),
            shiny::tags$li("Rules sharing an output column apply in priority order; first match wins."),
            shiny::tags$li("Unmatched rows are left NA in the new column.")
          ))
        )
      ),

      shiny::hr(),

      # --- Rule list ----------------------------------------------------------
      shiny::fluidRow(
        shiny::column(8, shiny::h5("Recategorization rules in this session")),
        shiny::column(4, align = "right",
          shiny::actionButton(ns("clear_all"), "Clear all",
            class = "btn btn-sm btn-outline-danger"))
      ),
      shiny::uiOutput(ns("rules_issues")),
      shiny::uiOutput(ns("rules_tbl_ui")),

      shiny::hr(),

      # --- Export -------------------------------------------------------------
      shiny::h5("Export"),
      shiny::div(
        shiny::downloadButton(ns("dl_csv"), "Download recat CSV",
          class = "btn btn-sm btn-primary"),
        shiny::downloadButton(ns("dl_r"), "Download recat R script",
          class = "btn btn-sm btn-primary"),
        shiny::actionButton(ns("copy_r"), "Copy R to clipboard",
          class = "btn btn-sm btn-outline-secondary")
      ),

      shiny::br(), shiny::br(),
      shiny::h6("Generated R (read-only preview)"),
      shiny::verbatimTextOutput(ns("r_preview"))
    )
  )
}

#' Recategorization module server.
#'
#' @param shared_state   reactiveValues(data, meta, dataset_name). Read only.
#' @param recat_proxy    list(get = reactive, add = fn, set = fn), the recat
#'   rule store owned by server(). Separate from the old->new `rules_proxy`.
#' @param seed_var_r optional reactive giving the single variable picked on
#'   the Variable tab (the one the other tabs work on). Used ONLY to seed this
#'   tab's source-variable picker while it is still empty, so arriving here
#'   from Clusters or Spellcheck starts on the variable you were working on. A
#'   recat rule reads across SEVERAL columns, so this tab keeps its own
#'   multi-select, and picking a different variable on the Variable tab never
#'   overwrites a selection already made here.
#' @param recode_basis_r optional reactive returning list(df, n_rules): the
#'   loaded data with the session's recodes ALREADY APPLIED. The generated
#'   recat script is meant to run after the recode script, so previewing a
#'   recat rule against raw values would report matches the export will not
#'   produce. When NULL the preview falls back to the raw frame and says so.
mod_recategorize_server <- function(id, shared_state, recat_proxy,
                                    seed_var_r = NULL, recode_basis_r = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    dataset_id <- shiny::reactive({
      nm <- shared_state$dataset_name %||% "dataset"
      tools::file_path_sans_ext(basename(nm))
    })

    # Choice universe for the "vars" picker, in display order: free-text
    # candidates first (likelier recat targets), then the rest.
    var_choices <- function() {
      meta <- shared_state$meta
      if (is.null(meta) || nrow(meta) == 0) return(character(0))
      if ("is_free_text_candidate" %in% names(meta)) {
        c(meta$column[meta$is_free_text_candidate],
          meta$column[!meta$is_free_text_candidate])
      } else {
        meta$column
      }
    }

    # --- Populate the variable picker from column metadata ------------------
    shiny::observe({
      cols <- var_choices()
      # Read the current selection via isolate() so this observer does NOT take
      # a dependency on the input it writes: it should refresh choices only
      # when the column universe changes, not re-fire on every pick (which
      # would collapse an open dropdown and reset a typed search).
      sel <- shiny::isolate(input$vars) %||% character(0)
      # Seed from the Variable tab's selection, but ONLY while nothing is
      # chosen here. A recat rule spans several columns, so silently adding or
      # replacing a deliberate selection because a different variable was
      # picked elsewhere would quietly alter what a half-written rule matches.
      if (length(sel) == 0 && !is.null(seed_var_r)) {
        v <- tryCatch(seed_var_r(), error = function(e) NULL)
        if (!is.null(v) && length(v) == 1 && !is.na(v) && v %in% cols) sel <- v
      }
      shiny::updateSelectizeInput(session, "vars", choices = cols,
                                  selected = sel)
    })

    # "Add sibling family" of the first selected var.
    shiny::observeEvent(input$add_siblings, {
      v <- input$vars
      d <- shared_state$data
      if (length(v) == 0 || is.null(d)) {
        shiny::showNotification("Select a variable first.", type = "warning")
        return()
      }
      # Built by suggest_sibling_pattern(), never typed, but checked the same
      # way as a typed pattern before it is used (clean, then compile).
      pat <- clean_sibling_pattern(suggest_sibling_pattern(v[1], names(d)))
      if (length(pat) == 1 && !is.na(pat) && !regex_compiles(pat)) {
        shiny::showNotification(
          sprintf("The sibling pattern '%s' is not a valid regular expression.",
                  pat), type = "error")
        return()
      }
      if (length(pat) != 1 || is.na(pat)) {
        shiny::showNotification(
          "No sibling family found for that variable.", type = "warning")
        return()
      }
      fam <- grep(pat, names(d), value = TRUE)
      shiny::updateSelectizeInput(session, "vars", choices = var_choices(),
                                  selected = union(v, fam))
      shiny::showNotification(
        sprintf("Added %d sibling columns (%s).", length(fam), pat),
        type = "message")
    })

    # --- Worked examples: fill the rule form on demand ----------------------
    # Written against the bundled café-orders example dataset. `var_pattern`
    # auto-selects the matching columns when the loaded data has them;
    # otherwise the user picks the source variables by hand.
    recat_examples <- list(
      espresso = list(
        out_col = "drink_family", category = "Espresso-based",
        var_pattern = "^drink[0-9]+$", match_type = "literal", priority = 10L,
        include = c("espresso", "expresso", "esspresso",
                    "macchiato", "machiato", "macchiatto",
                    "americano", "americanno", "cortado"),
        exclude = character(0),
        notes = "Espresso-forward drinks, including the common misspellings"),
      milk = list(
        out_col = "drink_family", category = "Milk-based",
        var_pattern = "^drink[0-9]+$", match_type = "regex", priority = 20L,
        include = "ca[bp]+u?c+ino|latt|moc+[ah]+|flat ?white",
        exclude = character(0),
        notes = "Milk-forward drinks; one regex covers every spelling variant"),
      review = list(
        out_col = "review_flag", category = "Needs follow-up",
        var_pattern = "^review$", match_type = "literal", priority = 10L,
        include = c("slow", "lukewarm", "burnt", "watery", "wrong",
                    "disappointing", "overly sweet", "too loud", "cold"),
        exclude = "cold brew",
        notes = paste0("Reviews naming a service or quality problem; ",
                       "'cold brew' mentions are not complaints"))
    )

    shiny::observeEvent(input$load_example, {
      ex <- recat_examples[[input$example_pick %||% ""]]
      if (is.null(ex)) return()

      shiny::updateTextInput(session, "out_col", value = ex$out_col)
      shiny::updateTextInput(session, "category", value = ex$category)
      shiny::updateRadioButtons(session, "match_type", selected = ex$match_type)
      shiny::updateNumericInput(session, "priority", value = ex$priority)
      shiny::updateTextInput(session, "notes", value = ex$notes)
      shiny::updateTextAreaInput(session, "include_terms",
        value = paste(ex$include, collapse = "\n"))
      shiny::updateTextAreaInput(session, "exclude_terms",
        value = paste(ex$exclude, collapse = "\n"))

      d <- shared_state$data
      fam <- if (!is.null(d)) {
        grep(ex$var_pattern, names(d), value = TRUE, ignore.case = TRUE)
      } else character(0)
      if (length(fam) > 0) {
        shiny::updateSelectizeInput(session, "vars", choices = var_choices(),
                                    selected = fam)
        shiny::showNotification(
          sprintf("Loaded '%s' example; selected %d matching column(s).",
                  ex$category, length(fam)),
          type = "message", duration = 3)
      } else {
        shiny::showNotification(
          sprintf(paste0("Loaded '%s' example. No columns matching '%s' in ",
                         "this dataset. Pick the source variables by hand."),
                  ex$category, ex$var_pattern),
          type = "warning", duration = 5)
      }
    })

    # --- Build a rule spec from the current inputs --------------------------
    terms_from_area <- function(x) {
      # Accept newline- or ";"-separated; store as ";"-joined.
      parts <- unlist(strsplit(x %||% "", "[\r\n;]+"))
      parts <- trimws(parts)
      parts[nzchar(parts)]
    }

    current_rule <- shiny::reactive({
      inc  <- terms_from_area(input$include_terms)
      exc  <- terms_from_area(input$exclude_terms)
      vars <- input$vars %||% character(0)
      tibble::tibble(
        recat_id        = recat_rule_id(
                            input$out_col %||% "", input$category %||% "",
                            recat_join(vars), recat_join(inc), recat_join(exc),
                            input$match_type %||% "literal"),
        out_col         = trimws(input$out_col %||% ""),
        category        = trimws(input$category %||% ""),
        vars            = recat_join(vars),
        sibling_pattern = NA_character_,
        include_terms   = recat_join(inc),
        exclude_terms   = recat_join(exc),
        match_type      = input$match_type %||% "literal",
        priority        = as.integer(input$priority %||% 100L),
        notes           = trimws(input$notes %||% ""),
        author          = NA_character_,
        created_at      = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
        source_dataset  = shared_state$dataset_name %||% NA_character_
      )
    })

    # Plain-English restatement of the rule being built.
    output$rule_sentence <- shiny::renderUI({
      inc  <- terms_from_area(input$include_terms)
      exc  <- terms_from_area(input$exclude_terms)
      vars <- input$vars %||% character(0)
      if (length(vars) == 0 || length(inc) == 0 ||
          !nzchar(input$category %||% "")) {
        return(shiny::tags$em(
          "Pick variables, include terms, and a category to see the rule."))
      }
      shiny::p(
        "If ANY of ", shiny::tags$code(paste(vars, collapse = ", ")),
        " contains ",
        shiny::strong(paste(sprintf("'%s'", inc), collapse = " OR ")),
        if (length(exc) > 0) shiny::tagList(
          " but NOT ",
          shiny::strong(paste(sprintf("'%s'", exc), collapse = " OR "))),
        " then ", shiny::tags$code(input$out_col), " = ",
        shiny::strong(sprintf("'%s'", input$category)), "."
      )
    })

    # --- Preview match count ------------------------------------------------
    # Computed on the data AFTER the recodes, because that is the frame the
    # exported recat script runs against. Previewing on raw values reports
    # rows the export then drops, silently.
    #
    # The preview is a SNAPSHOT taken when the button is clicked; recomputing
    # on every keystroke would be slow on a large frame and would count
    # half-typed terms. A snapshot left on screen after the form changes looks
    # current, though, so the form fields the rule is built from are recorded
    # with it, and while they differ the preview is greyed out under a note
    # saying so. The next Preview clears it. Parsed terms are compared, not
    # raw text, so a stray blank line does not count as a change; notes are
    # left out because they cannot change the result.
    preview_form_key <- shiny::reactive({
      list(
        vars          = input$vars %||% character(0),
        out_col       = trimws(input$out_col %||% ""),
        category      = trimws(input$category %||% ""),
        match_type    = input$match_type %||% "literal",
        include_terms = terms_from_area(input$include_terms),
        exclude_terms = terms_from_area(input$exclude_terms),
        priority      = input$priority)
    })
    preview_ui       <- shiny::reactiveVal(NULL)
    preview_form_was <- shiny::reactiveVal(NULL)

    set_preview <- function(ui) {
      preview_form_was(shiny::isolate(preview_form_key()))
      preview_ui(ui)
    }

    output$preview_out <- shiny::renderUI({
      ui <- preview_ui()
      if (is.null(ui)) return(NULL)
      if (identical(preview_form_was(), preview_form_key())) return(ui)
      shiny::div(
        class = "rs-preview-stale",
        shiny::div(
          class = "alert alert-secondary py-1 px-2 mb-2", role = "status",
          shiny::icon("clock-rotate-left"), " ",
          shiny::tags$strong("Out of date. "),
          "The form changed since this preview. Click Preview again."),
        shiny::div(style = "opacity:.45;", `aria-hidden` = "true", ui))
    })

    shiny::observeEvent(input$preview, {
      d_raw <- shared_state$data
      if (is.null(d_raw)) {
        set_preview(
          shiny::tags$em("No data loaded. Load a dataset on the Data tab."))
        return()
      }
      basis <- if (is.null(recode_basis_r)) NULL
               else tryCatch(recode_basis_r(), error = function(e) NULL)
      d <- if (is.null(basis) || is.null(basis$df)) d_raw else basis$df
      n_recodes <- if (is.null(basis)) NA_integer_ else basis$n_rules
      n_norms   <- if (is.null(basis) || is.null(basis$n_norms)) 0L
                   else basis$n_norms
      # The frame just before the recodes (normalized, when normalizations
      # exist), so a shortfall is attributable to the recodes.
      d_before <- if (is.null(basis) || is.null(basis$df_before_recodes)) d_raw
                  else basis$df_before_recodes

      r <- current_rule()[1, ]
      cols <- intersect(recat_split(r$vars), names(d))
      if (length(cols) == 0) {
        set_preview(
          shiny::div(class = "alert alert-warning",
            "No valid source variables selected."))
        return()
      }
      hit   <- recat_rule_hits(d, r)
      n_hit <- sum(hit)
      # The same rule against the untouched frame: a shortfall means the
      # recodes rewrote wording the include terms look for.
      n_hit_raw <- if (identical(d, d_before)) n_hit
                   else sum(recat_rule_hits(d_before, r))
      pct <- if (nrow(d) > 0) round(100 * n_hit / nrow(d), 1) else 0

      # EVERY selected column, not just the first: a rule over several siblings
      # reporting one total cannot show that some siblings contribute nothing,
      # or that the matches come from a column included by accident.
      by_col <- tryCatch(recat_rule_hits_by_col(d, r), error = function(e) NULL)

      # A sample of matching ROWS across all the columns. Truncated per cell:
      # these are free-text fields and one long narrative would push the rest
      # off screen.
      trunc <- function(x, n = 48) {
        x <- as.character(x)
        x[is.na(x)] <- ""
        ifelse(nchar(x) > n, paste0(substr(x, 1, n), "…"), x)
      }
      sample_rows <- if (n_hit > 0) utils::head(which(hit), 5) else integer(0)

      set_preview({
        shiny::div(class = "alert alert-info",
          shiny::p(shiny::strong(sprintf("%d rows match", n_hit)),
                   sprintf(" (%s%% of %d).", pct, nrow(d))),

          # Which frame produced that number. Never left to be assumed.
          shiny::tags$small(
            class = "d-block text-muted mb-2",
            if (is.na(n_recodes))
              paste0("Counted on the RAW data: no recode rules were available ",
                     "to apply first. Recodes run before recat, so the export ",
                     "may match fewer rows than this.")
            else if (n_recodes == 0 && n_norms > 0)
              sprintf(paste0("Counted after applying %d normalization%s, as ",
                             "the export does. There are no recode rules yet."),
                      n_norms, if (n_norms == 1) "" else "s")
            else if (n_recodes == 0)
              paste0("Counted after the recodes, as the export does. There are ",
                     "no recode rules yet, so this is the raw data.")
            else
              sprintf(paste0("Counted AFTER applying %s%d recode rule%s, which ",
                             "is the order the exported scripts use: ",
                             "normalizations, then recodes, then ",
                             "recategorization."),
                      if (n_norms > 0) sprintf("%d normalization%s and ", n_norms,
                                               if (n_norms == 1) "" else "s")
                      else "",
                      n_recodes, if (n_recodes == 1) "" else "s")),

          if (!is.na(n_recodes) && n_hit_raw > n_hit) shiny::div(
            class = "alert alert-warning py-1 px-2",
            shiny::tags$small(sprintf(
              paste0("This rule matches %d row%s before the recodes but %d ",
                     "after: the recodes rewrote wording your include terms ",
                     "look for. Restate the terms against the recoded values; ",
                     "%d is what the export will produce."),
              n_hit_raw, if (n_hit_raw == 1) "" else "s", n_hit, n_hit))),

          if (!is.null(by_col) && nrow(by_col) > 0) shiny::tagList(
            shiny::tags$small(shiny::tags$strong("Per source column")),
            shiny::tags$table(
              class = "table table-sm table-borderless mb-2",
              shiny::tags$thead(shiny::tags$tr(
                shiny::tags$th("column"), shiny::tags$th("includes"),
                shiny::tags$th("excludes"), shiny::tags$th("kept"))),
              shiny::tags$tbody(lapply(seq_len(nrow(by_col)), function(i) {
                dead <- by_col$kept[i] == 0
                shiny::tags$tr(
                  class = if (dead) "text-muted" else NULL,
                  shiny::tags$td(shiny::tags$code(by_col$column[i])),
                  shiny::tags$td(by_col$include_hits[i]),
                  shiny::tags$td(by_col$exclude_hits[i]),
                  shiny::tags$td(shiny::tags$strong(by_col$kept[i])))
              }))),
            if (any(by_col$kept == 0)) shiny::tags$small(
              class = "text-muted d-block mb-2",
              sprintf("%d selected column(s) contribute nothing to this rule.",
                      sum(by_col$kept == 0)))
          ),

          if (length(sample_rows) > 0) shiny::tagList(
            shiny::tags$small(shiny::tags$strong("Sample of matching rows")),
            shiny::tags$div(
              style = "overflow-x:auto;",
              shiny::tags$table(
                class = "table table-sm mb-0",
                shiny::tags$thead(shiny::tags$tr(
                  lapply(cols, function(cc) shiny::tags$th(
                    shiny::tags$code(cc))))),
                shiny::tags$tbody(lapply(sample_rows, function(ri) {
                  shiny::tags$tr(lapply(cols, function(cc)
                    shiny::tags$td(trunc(d[[cc]][ri]))))
                }))))
          )
        )
      })
    })

    # --- Add rule -----------------------------------------------------------
    shiny::observeEvent(input$add_rule, {
      r <- current_rule()[1, ]
      if (!nzchar(r$out_col) || !nzchar(r$category)) {
        shiny::showNotification(
          "Output column and category are required.", type = "error")
        return()
      }
      if (length(recat_split(r$vars)) == 0) {
        shiny::showNotification(
          "Select at least one source variable.", type = "error")
        return()
      }
      if (length(recat_split(r$include_terms)) == 0) {
        shiny::showNotification(
          "Add at least one INCLUDE term.", type = "error")
        return()
      }
      refusal <- recat_add_refusal(r)
      if (!is.null(refusal)) {
        shiny::showNotification(refusal, type = "error", duration = 10)
        return()
      }
      recat_proxy$add(r)
      shiny::showNotification("Rule added.", type = "message", duration = 2)
    })

    # --- Rule list table + delete -------------------------------------------
    # Empty state is plain muted text, not a one-cell DT: that rendered a
    # literal "message" column header with a sort arrow. The switch keys on
    # whether rules exist at all, so the DT output is not torn down and
    # rebuilt on every add or delete.
    # (A reactiveVal, not a reactive(): setting it to an identical value does
    # not invalidate, whereas a reactive() re-fires its dependents every time.)
    has_rules_r <- shiny::reactiveVal(FALSE)
    shiny::observe(has_rules_r(nrow(recat_proxy$get()) > 0))

    output$rules_tbl_ui <- shiny::renderUI({
      if (!has_rules_r()) {
        return(shiny::div(class = "text-muted fst-italic",
                          "No recategorization rules yet."))
      }
      DT::DTOutput(ns("rules_tbl"))
    })

    # validate_recat() over the session's rules, against the loaded data.
    recat_issues_r <- shiny::reactive({
      r <- recat_proxy$get()
      if (is.null(r) || nrow(r) == 0) return(NULL)
      validate_recat(r, data = shared_state$data)
    })
    output$rules_issues <- shiny::renderUI(recat_issues_ui(recat_issues_r()))

    output$rules_tbl <- DT::renderDT({
      r <- recat_proxy$get()
      shiny::req(nrow(r) > 0)
      show <- r
      # Row position, not the id: a recat_id read from an imported CSV can be
      # duplicated, blank or missing. .rule_delete_button() is in
      # R/ui_helpers.R and escapes the whole handler.
      show$delete <- vapply(seq_len(nrow(show)), function(i) {
        .rule_delete_button(ns("delete_row"), i, show$recat_id[i])
      }, character(1))
      show <- show[, c("out_col", "category", "vars", "include_terms",
                       "exclude_terms", "match_type", "priority", "notes",
                       "delete")]
      # Only the trailing `delete` column is generated markup; every other
      # column holds user-typed text, so it must stay escaped.
      DT::datatable(show, rownames = FALSE, escape = -ncol(show),
                    selection = "none",
                    options = list(pageLength = 15, autoWidth = FALSE))
    })

    # Deletes the ONE row the button names; see the Recodes tab for why the id
    # alone is not enough.
    shiny::observeEvent(input$delete_row, ignoreInit = TRUE, {
      r <- recat_proxy$get()
      hit <- .rule_delete_target(input$delete_row, r$recat_id)
      if (!hit$ok) {
        shiny::showNotification(hit$message, type = "warning", duration = 6)
        return()
      }
      recat_proxy$set(r[-hit$row, , drop = FALSE])
      shiny::showNotification("Rule deleted.", type = "message", duration = 2)
    })

    shiny::observeEvent(input$clear_all, {
      recat_proxy$set(empty_recat_tibble())
      shiny::showNotification("Recategorization rules cleared.",
                              type = "message")
    })

    # --- Generated R + export -----------------------------------------------
    gen_r <- shiny::reactive({
      d <- shared_state$data
      generate_recat_R(recat_proxy$get(), dataset_id = dataset_id(),
                       data_cols = if (!is.null(d)) names(d) else NULL)
    })

    output$r_preview <- shiny::renderText({ gen_r() })

    output$dl_csv <- shiny::downloadHandler(
      filename = function() sprintf("recat_%s.csv", dataset_id()),
      content  = function(file) write_recat(recat_proxy$get(), file)
    )

    output$dl_r <- shiny::downloadHandler(
      filename = function() sprintf("recat_%s.R", dataset_id()),
      content  = function(file) writeLines(gen_r(), file)
    )

    shiny::observeEvent(input$copy_r, {
      ok <- tryCatch({
        clipr::write_clip(gen_r(), allow_non_interactive = TRUE)
        TRUE
      }, error = function(e) FALSE)
      shiny::showNotification(
        if (ok) "R copied to clipboard."
        else "Clipboard unavailable. Use Download instead.",
        type = if (ok) "message" else "warning")
    })
  })
}
