# =============================================================================
# Text analysis: tools for long-text columns (sentences / paragraphs), where
# similarity clustering and spellcheck are not useful. Length distribution,
# token + n-gram frequency (with editable stopword tiers), word cloud,
# per-value language detection, and keyword-in-context search.
#
# Reads the RAW values (shared_state$data), not the normalized frame: this tab
# is descriptive and should describe the file as delivered.
# =============================================================================

# JSON-encode a value for use inside inline JavaScript. A JSON string is also
# a valid JavaScript literal, and jsonlite escapes quotes, backslashes and
# control characters. (Same approach as the Spellcheck tab; kept local so the
# two modules do not depend on each other.)
.ta_js_value <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
}

# Escape a string for use as an HTML attribute value.
.ta_attr_value <- function(x) {
  as.character(htmltools::htmlEscape(x, attribute = TRUE))
}

#' Inline "+ Stop" button for one token of the Top words table.
#'
#' The token travels as a JSON string inside the onclick handler, and the
#' whole handler is attribute-escaped, so a token can never break out of
#' either. The server re-checks the token against the current table.
.stop_btn <- function(input_id, token) {
  js <- paste0("Shiny.setInputValue(", .ta_js_value(input_id), ", ",
               .ta_js_value(token), ", {priority: 'event'})")
  sprintf(paste0('<button type="button" class="btn btn-sm ',
                 'btn-outline-secondary py-0" title="%s" onclick="%s">',
                 '+ Stop</button>'),
          .ta_attr_value(paste0("Add '", token, "' to the stopword list")),
          .ta_attr_value(js))
}

#' Language breakdown as a DT, share formatted as a percentage by DT (the
#' column stays numeric so it sorts correctly).
.lang_summary_table <- function(s) {
  DT::datatable(s, rownames = FALSE,
                colnames = c("language", "values", "share"),
                options = list(pageLength = 8, dom = "tp")) |>
    DT::formatPercentage("share", digits = 1)
}

#' Top words as a DT, optionally with a "+ Stop" button per row.
#'
#' @param tk            tibble(token, n) from top_tokens().
#' @param stop_input_id namespaced input id the buttons report to, or NULL for
#'   a plain table without buttons.
.top_words_table <- function(tk, stop_input_id = NULL) {
  df <- as.data.frame(tk)
  if (is.null(stop_input_id)) {
    return(DT::datatable(df, rownames = FALSE, colnames = c("word", "n"),
                         selection = "none",
                         options = list(pageLength = 10, dom = "tp")))
  }
  # "stop" MUST stay last: the DT below unescapes exactly one column.
  df$stop <- vapply(df$token, function(t) .stop_btn(stop_input_id, t),
                    character(1), USE.NAMES = FALSE)
  DT::datatable(
    df, rownames = FALSE, colnames = c("word", "n", ""),
    # SECURITY: only the trailing "stop" column is generated markup.
    # "token" comes from the loaded file and must stay HTML-escaped.
    escape    = -ncol(df),
    selection = "none",
    options   = list(pageLength = 10, dom = "tp",
                     columnDefs = list(list(targets = ncol(df) - 1,
                                            orderable = FALSE))))
}

#' Values not detected as English (or all values) as a DT.
#'
#' Default `escape = TRUE`: `value` is dataset text, and dt_truncate_defs()
#' relies on escaped input.
.lang_values_dt <- function(lv) {
  DT::datatable(lv, rownames = FALSE,
                options = list(pageLength = 10, dom = "tp",
                               columnDefs = dt_truncate_defs(targets = 0)))
}

#' Values to list under the language breakdown.
#'
#' @param values raw values (non-missing, non-empty).
#' @param langs  detect_languages(values), parallel to `values`.
#' @param only_non_en drop values detected as English.
#' @return tibble(value, language, n): distinct values with their row count,
#'   most frequent first. Undetected values show as "(undetected)".
lang_values_table <- function(values, langs, only_non_en = TRUE) {
  keep <- if (isTRUE(only_non_en)) is.na(langs) | langs != "en"
          else rep(TRUE, length(values))
  if (!any(keep)) {
    return(tibble::tibble(value = character(0), language = character(0),
                          n = integer(0)))
  }
  l <- langs[keep]
  l[is.na(l)] <- "(undetected)"
  tibble::tibble(value = values[keep], language = l) |>
    dplyr::count(value, language, sort = TRUE, name = "n")
}

mod_text_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = FALSE,
    fill        = FALSE,
    card_header_help(
      "Text analysis (long-text columns)",
      shiny::p("For columns holding sentences or paragraphs, where every value ",
               "is different and clustering or spellcheck are not much use. ",
               "Shows how long the entries are, which words and phrases ",
               "come up most, and which languages the entries are in."),
      shiny::p("Type a word under \"Keyword in context\" to read it inside ",
               "the entries that use it. This tab only looks at the file as ",
               "loaded; it creates no rules. The one thing it writes is the ",
               "stopword lists, when you press \"+ Stop\".")
    ),
    bslib::card_body(
      fillable = FALSE,
      dt_truncate_css(),
      shiny::uiOutput(ns("kind_note")),
      shiny::fluidRow(
        shiny::column(3,
          shiny::checkboxInput(ns("stop"), "Remove common stopwords", value = TRUE)),
        shiny::column(3,
          shiny::numericInput(ns("minchars"), "Min token length:",
            value = 3, min = 1, max = 12, step = 1)),
        shiny::column(3,
          shiny::numericInput(ns("topn"), "Show top N:",
            value = 25, min = 5, max = 200, step = 5)),
        shiny::column(3,
          shiny::numericInput(ns("ngram"), "Phrase length (n-gram):",
            value = 2, min = 2, max = 5, step = 1))
      ),
      shiny::conditionalPanel(
        condition = "input.stop", ns = ns,
        shiny::fluidRow(
          shiny::column(6,
            shiny::checkboxGroupInput(ns("sw_tiers"), "Stopword lists in use:",
              choices  = c("Base (English)" = "base",
                           "Project (shared)" = "project",
                           "User (personal)" = "user"),
              selected = STOPWORD_TIERS, inline = TRUE)),
          shiny::column(6,
            shiny::radioButtons(ns("sw_target"), "\"+ Stop\" adds the word to:",
              choices  = c("Project (shared, committed)" = "project",
                           "User (personal, gitignored)" = "user"),
              selected = "project", inline = TRUE))
        ),
        help_line(
          "Stopwords are left out of Top words, Top phrases and the word ",
          "cloud. Press \"+ Stop\" next to a word to add it to the list ",
          "chosen above; it disappears from the views straight away. Project ",
          "words live in stopwords/custom_stopwords.txt: commit that file to ",
          "share them.")
      ),
      shiny::hr(),

      shiny::h6("Length distribution"),
      shiny::uiOutput(ns("len_summary")),
      shiny::plotOutput(ns("len_plot"), height = "200px"),
      shiny::hr(),

      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::tagList(shiny::h6("Top words"),
                       shiny::uiOutput(ns("tokens_empty")),
                       DT::DTOutput(ns("tokens"))),
        shiny::tagList(shiny::h6("Top phrases"),
                       shiny::uiOutput(ns("ngrams_empty")),
                       DT::DTOutput(ns("ngrams")))
      ),
      shiny::hr(),

      shiny::h6("Interactive word cloud"),
      wordcloud2::wordcloud2Output(ns("wordcloud"), height = "320px"),
      shiny::hr(),

      shiny::h6(
        "Language",
        help_btn(
          shiny::p("Guesses the language of each entry with the cld2 ",
                   "language detector. Useful for spotting entries that are ",
                   "not in English before you read or recode them."),
          shiny::p("Entries shorter than ", LANG_MIN_CHARS, " characters are ",
                   "listed as undetected: a guess on a few characters is ",
                   "not reliable. Mixed-language entries get the main ",
                   "language only."),
          title = "Language")),
      shiny::uiOutput(ns("lang_note")),
      bslib::layout_columns(
        col_widths = c(4, 8),
        shiny::tagList(
          shiny::uiOutput(ns("lang_summary_empty")),
          DT::DTOutput(ns("lang_summary"))),
        shiny::tagList(
          shiny::checkboxInput(ns("only_non_en"),
            "Show only values not detected as English", value = TRUE),
          dt_truncate_hint(),
          shiny::uiOutput(ns("lang_values_empty")),
          DT::DTOutput(ns("lang_values")))
      ),
      shiny::hr(),

      shiny::h6("Keyword in context"),
      shiny::textInput(ns("kw"), NULL, placeholder = "type a word to see it in context"),
      shiny::uiOutput(ns("kwic_empty")),
      DT::DTOutput(ns("kwic"))
    )
  )
}

#' @param selected_var_r reactive selected variable name
#' @param stopword_dir   folder holding the stopword tier files. Tests point
#'   this at a temporary folder.
mod_text_analysis_server <- function(id, shared_state, selected_var_r,
                                     stopword_dir = STOPWORD_DIR) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Bumped after "+ Stop" writes a tier file, so every view that reads the
    # stopword lists recomputes. (Re-sending the same tier selection would not
    # invalidate anything: Shiny ignores an input update to its current value.)
    stopword_version <- shiny::reactiveVal(0L)

    stopwords_r <- shiny::reactive({
      stopword_version()
      active_stopwords(tiers = input$sw_tiers %||% character(0),
                       dir = stopword_dir)
    })

    # The full column (all rows, not unique) for the selected variable.
    col_values <- shiny::reactive({
      v <- selected_var_r(); d <- shared_state$data
      if (is.null(v) || is.null(d) || !(v %in% names(d))) return(NULL)
      x <- d[[v]]
      x[!is.na(x) & nzchar(x)]
    })

    is_long <- shiny::reactive({
      v <- selected_var_r(); m <- shared_state$meta
      if (is.null(v) || is.null(m) || !(v %in% m$column)) return(NA)
      isTRUE(m$is_long_text[m$column == v])
    })

    output$kind_note <- shiny::renderUI({
      v <- selected_var_r()
      if (is.null(v)) return(shiny::tags$em("Pick a variable (Variable tab)."))
      lg <- is_long()
      if (isTRUE(lg))
        shiny::div(class = "alert alert-success", style = "padding:.3em .8em;",
          shiny::strong(v), " looks like long text: these tools apply.")
      else
        shiny::div(class = "alert alert-secondary", style = "padding:.3em .8em;",
          shiny::strong(v), " is short text. These tools still work, but ",
          "Clusters / Spellcheck are usually the better fit for short labels.")
    })

    len_stats <- shiny::reactive({
      v <- col_values(); if (is.null(v)) return(NULL)
      text_length_stats(v)
    })

    output$len_summary <- shiny::renderUI({
      s <- len_stats(); if (is.null(s) || nrow(s) == 0) return(NULL)
      f <- function(x) sprintf("%.0f (median), %.0f max", stats::median(x), max(x))
      shiny::tagList(
        shiny::tags$small(
          shiny::strong("Chars: "), f(s$chars), " · ",
          shiny::strong("Words: "), f(s$words), " · ",
          shiny::strong("Sentences: "), f(s$sentences)))
    })

    output$len_plot <- shiny::renderPlot({
      s <- len_stats()
      shiny::validate(shiny::need(!is.null(s) && nrow(s) > 0, "No values."))
      ggplot2::ggplot(s, ggplot2::aes(words)) +
        ggplot2::geom_histogram(bins = 30, fill = "#2c3e50") +
        ggplot2::labs(x = "words per value", y = "count") +
        ggplot2::theme_minimal()
    })

    # --- Word and phrase frequency --------------------------------------------

    # One computation shared by the Top words table and the word cloud, so the
    # two always agree.
    tokens_r <- shiny::reactive({
      v <- col_values(); if (is.null(v)) return(NULL)
      top_tokens(v, n = input$topn %||% 25,
                 remove_stopwords = isTRUE(input$stop),
                 min_chars = input$minchars %||% 1,
                 stopwords = stopwords_r())
    })

    ngrams_r <- shiny::reactive({
      v <- col_values(); if (is.null(v)) return(NULL)
      top_ngrams(v, ngram = input$ngram %||% 2, n = input$topn %||% 25,
                 remove_stopwords = isTRUE(input$stop),
                 min_chars = input$minchars %||% 1,
                 stopwords = stopwords_r())
    })

    output$tokens_empty <- shiny::renderUI({
      tk <- tokens_r()
      if (is.null(tk)) return(empty_state("No values to count."))
      if (nrow(tk) == 0) return(empty_state("No words match these settings."))
      NULL
    })

    output$tokens <- DT::renderDT({
      tk <- tokens_r()
      if (is.null(tk) || nrow(tk) == 0) return(NULL)
      # "+ Stop" is offered only while stopwords are being removed: the list
      # choice it writes to is hidden otherwise.
      .top_words_table(tk, if (isTRUE(input$stop)) ns("add_stop") else NULL)
    })

    output$ngrams_empty <- shiny::renderUI({
      ng <- ngrams_r()
      if (is.null(ng)) return(empty_state("No values to count."))
      if (nrow(ng) == 0) return(empty_state("No phrases match these settings."))
      NULL
    })

    output$ngrams <- DT::renderDT({
      ng <- ngrams_r()
      if (is.null(ng) || nrow(ng) == 0) return(NULL)
      DT::datatable(ng, rownames = FALSE, colnames = c("phrase", "n"),
                    options = list(pageLength = 10, dom = "tp"))
    })

    shiny::observeEvent(input$add_stop, ignoreInit = TRUE, {
      tok <- input$add_stop
      tk  <- shiny::isolate(tokens_r())
      # Only a word currently listed in Top words can be added from here.
      if (!isTRUE(input$stop) ||
          !is.character(tok) || length(tok) != 1L || is.na(tok) ||
          is.null(tk) || !(tok %in% tk$token)) {
        return()
      }
      tier  <- input$sw_target %||% "project"
      if (!(tier %in% STOPWORD_WRITABLE_TIERS)) tier <- "project"
      label <- if (tier == "user") "User" else "Project"
      added <- tryCatch(add_stopword(tok, tier = tier, dir = stopword_dir),
                        error = function(e) e)
      if (inherits(added, "error")) {
        shiny::showNotification(
          sprintf("Could not add '%s': %s", tok, conditionMessage(added)),
          type = "error", duration = NULL)
        return()
      }
      stopword_version(stopword_version() + 1L)
      in_use <- tier %in% (input$sw_tiers %||% character(0))
      msg <- if (isTRUE(added)) {
        sprintf("Added '%s' to the %s stopword list.", tok, label)
      } else {
        sprintf("'%s' is already in the %s stopword list.", tok, label)
      }
      if (!in_use) {
        msg <- paste(msg, sprintf(
          "Tick %s under \"Stopword lists in use\" to hide it here.", label))
      }
      shiny::showNotification(msg,
        type = if (isTRUE(added) && in_use) "message" else "warning",
        duration = 5)
    })

    output$wordcloud <- wordcloud2::renderWordcloud2({
      toks <- tokens_r()
      shiny::validate(shiny::need(!is.null(toks), "No values."))
      shiny::validate(shiny::need(nrow(toks) > 0, "No tokens matching filters."))
      df_cloud <- data.frame(
        word = as.character(toks$token),
        freq = as.numeric(toks$n),
        stringsAsFactors = FALSE
      )
      wordcloud2::wordcloud2(
        data = df_cloud,
        size = 0.6,
        color = "random-dark",
        backgroundColor = "white"
      )
    })

    # --- Language -------------------------------------------------------------

    lang_available_r <- shiny::reactive(lang_detection_available())

    langs_r <- shiny::reactive({
      v <- col_values()
      if (is.null(v) || !lang_available_r()) return(NULL)
      detect_languages(v)
    })

    output$lang_note <- shiny::renderUI({
      if (!lang_available_r()) {
        return(shiny::div(
          class = "alert alert-warning", style = "padding:.3em .8em;",
          "Language detection needs the ", shiny::tags$code("cld2"),
          " package, which is not installed. Install it with ",
          shiny::tags$code('install.packages("cld2")'),
          " and restart the app."))
      }
      help_line(sprintf(paste0(
        "Values shorter than %d characters are counted as undetected rather ",
        "than guessed."), LANG_MIN_CHARS))
    })

    lang_summary_r <- shiny::reactive({
      v <- col_values(); l <- langs_r()
      if (is.null(v) || is.null(l)) return(NULL)
      summarise_languages(l)
    })

    output$lang_summary_empty <- shiny::renderUI({
      if (!lang_available_r()) return(NULL)
      s <- lang_summary_r()
      if (is.null(s) || nrow(s) == 0) return(empty_state("No values to check."))
      NULL
    })

    output$lang_summary <- DT::renderDT({
      s <- lang_summary_r()
      if (is.null(s) || nrow(s) == 0) return(NULL)
      .lang_summary_table(s)
    })

    lang_values_r <- shiny::reactive({
      v <- col_values(); l <- langs_r()
      if (is.null(v) || is.null(l)) return(NULL)
      lang_values_table(v, l, only_non_en = isTRUE(input$only_non_en))
    })

    output$lang_values_empty <- shiny::renderUI({
      if (!lang_available_r()) return(NULL)
      lv <- lang_values_r()
      if (is.null(lv)) return(empty_state("No values to check."))
      if (nrow(lv) == 0) {
        return(empty_state(if (isTRUE(input$only_non_en))
          "Every value was detected as English." else "No values to list."))
      }
      NULL
    })

    output$lang_values <- DT::renderDT({
      lv <- lang_values_r()
      if (is.null(lv) || nrow(lv) == 0) return(NULL)
      .lang_values_dt(lv)
    })

    # --- Keyword in context ---------------------------------------------------

    kwic_r <- shiny::reactive({
      v <- col_values(); if (is.null(v)) return(NULL)
      kwic(v, input$kw %||% "")
    })

    output$kwic_empty <- shiny::renderUI({
      res <- kwic_r()
      if (is.null(res)) return(empty_state("No values."))
      if (nrow(res) > 0) return(NULL)
      if (!nzchar(trimws(input$kw %||% ""))) {
        empty_state("Type a word above.")
      } else {
        empty_state("No entries contain that word.")
      }
    })

    output$kwic <- DT::renderDT({
      res <- kwic_r()
      if (is.null(res) || nrow(res) == 0) return(NULL)
      # Right-align 'before' so the keyword column lines up.
      DT::datatable(res, rownames = FALSE,
                    options = list(pageLength = 10, dom = "tp",
                                   columnDefs = list(
                                     list(targets = 0, className = "dt-right")))) |>
        DT::formatStyle("keyword", fontWeight = "bold")
    })
  })
}
