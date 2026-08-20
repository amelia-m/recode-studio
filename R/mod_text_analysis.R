# =============================================================================
# Text analysis — tools for long-text columns (sentences / paragraphs), where
# similarity clustering and spellcheck are not useful. Length distribution,
# token + n-gram frequency, and keyword-in-context search.
# =============================================================================

mod_text_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = FALSE,
    fill        = FALSE,
    bslib::card_header("Text analysis (long-text columns)"),
    bslib::card_body(
      fillable = FALSE,
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
      shiny::hr(),

      shiny::h6("Length distribution"),
      shiny::uiOutput(ns("len_summary")),
      shiny::plotOutput(ns("len_plot"), height = "200px"),
      shiny::hr(),

      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::tagList(shiny::h6("Top words"),      DT::DTOutput(ns("tokens"))),
        shiny::tagList(shiny::h6("Top phrases"),    DT::DTOutput(ns("ngrams")))
      ),
      shiny::hr(),

      shiny::h6("Interactive word cloud"),
      wordcloud2::wordcloud2Output(ns("wordcloud"), height = "320px"),
      shiny::hr(),

      shiny::h6("Keyword in context"),
      shiny::textInput(ns("kw"), NULL, placeholder = "type a word to see it in context"),
      DT::DTOutput(ns("kwic"))
    )
  )
}

#' @param selected_var_r reactive selected variable name
mod_text_analysis_server <- function(id, shared_state, selected_var_r) {
  shiny::moduleServer(id, function(input, output, session) {

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
          shiny::strong(v), " looks like long text — these tools apply.")
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

    output$tokens <- DT::renderDT({
      v <- col_values()
      shiny::validate(shiny::need(!is.null(v), "No values."))
      DT::datatable(
        top_tokens(v, n = input$topn %||% 25,
                   remove_stopwords = isTRUE(input$stop),
                   min_chars = input$minchars %||% 1),
        rownames = FALSE, options = list(pageLength = 10, dom = "tp"))
    })

    output$ngrams <- DT::renderDT({
      v <- col_values()
      shiny::validate(shiny::need(!is.null(v), "No values."))
      DT::datatable(
        top_ngrams(v, ngram = input$ngram %||% 2, n = input$topn %||% 25,
                   remove_stopwords = isTRUE(input$stop),
                   min_chars = input$minchars %||% 1),
        rownames = FALSE, options = list(pageLength = 10, dom = "tp"))
    })

    output$wordcloud <- wordcloud2::renderWordcloud2({
      v <- col_values()
      shiny::validate(shiny::need(!is.null(v) && length(v) > 0, "No values."))
      toks <- top_tokens(v, n = input$topn %||% 25,
                         remove_stopwords = isTRUE(input$stop),
                         min_chars = input$minchars %||% 1)
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

    output$kwic <- DT::renderDT({
      v <- col_values()
      shiny::validate(shiny::need(!is.null(v), "No values."))
      res <- kwic(v, input$kw %||% "")
      if (nrow(res) == 0)
        return(DT::datatable(tibble::tibble(message = "Type a word above."),
                             rownames = FALSE, options = list(dom = "t")))
      # Right-align 'before' so the keyword column lines up.
      DT::datatable(res, rownames = FALSE,
                    options = list(pageLength = 10, dom = "tp",
                                   columnDefs = list(
                                     list(targets = 0, className = "dt-right")))) |>
        DT::formatStyle("keyword", fontWeight = "bold")
    })
  })
}
