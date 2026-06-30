# =============================================================================
# Cluster view
# =============================================================================
# Similarity-grouped view of a column's unique values. Each cluster gets a
# card listing its members (with frequencies); the user picks the canonical
# target via a radio (default = most frequent) or types a custom value, then
# Apply emits sibling-aware rules recoding the rest to that target.
# =============================================================================

mod_cluster_view_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = FALSE,
    fill        = FALSE,
    bslib::card_header("Similarity clusters"),
    bslib::card_body(
      fillable = FALSE,
      shiny::fluidRow(
        shiny::column(4,
          shiny::selectInput(ns("algorithm"), "Algorithm:",
            choices = CLUSTER_ALGORITHMS, selected = "jw"),
          shiny::numericInput(ns("qgram"), "q-gram size (cosine / jaccard):",
            value = 2, min = 1, max = 5, step = 1)
        ),
        shiny::column(4,
          shiny::sliderInput(ns("threshold"), "Similarity threshold:",
            min = 0.80, max = 1.00, value = 0.92, step = 0.01),
          shiny::tags$small(shiny::tags$em(
            "Threshold is ignored by the phonetic algorithms (Soundex / Metaphone)."
          ))
        ),
        shiny::column(4,
          shiny::checkboxGroupInput(ns("normalize"),
            "Normalize before clustering:",
            choices  = CLUSTER_NORMALIZERS,
            selected = c("lower", "squish")),
          shiny::checkboxInput(ns("apply_siblings"),
            "Apply to sibling columns when recoding", value = TRUE),
          shiny::uiOutput(ns("sibling_preview"))
        )
      ),
      shiny::hr(),
      shiny::div(
        class = "alert alert-info", style = "padding:.5em .8em;",
        shiny::tags$strong("Tip: "),
        "In each cluster below, click the member you want to keep — the others ",
        "are recoded to it. The most frequent value is pre-selected; pick a ",
        "different one, or type your own target in the box."
      ),
      shiny::uiOutput(ns("clusters_ui"))
    )
  )
}

#' @param selected_var_r   reactive selected variable name
#' @param unique_values_r  reactive tibble(value, n) for current var
#' @param rules_proxy      list(add=, set=, get=)
mod_cluster_view_server <- function(id, shared_state,
                                    selected_var_r,
                                    unique_values_r,
                                    rules_proxy) {
  shiny::moduleServer(id, function(input, output, session) {

    clusters_r <- shiny::reactive({
      uv <- unique_values_r()
      if (is.null(uv) || nrow(uv) == 0) return(NULL)
      cluster_strings(
        values      = uv$value,
        frequencies = uv$n,
        threshold   = input$threshold,
        algorithm   = input$algorithm,
        q           = max(1L, as.integer(input$qgram %||% 2L)),
        normalize   = input$normalize %||% character(0)
      )
    })

    output$sibling_preview <- shiny::renderUI({
      v <- selected_var_r()
      if (!isTRUE(input$apply_siblings) || is.null(v)) return(NULL)
      cols <- names(shared_state$data)
      pat  <- suggest_sibling_pattern(v, cols)
      if (is.na(pat)) {
        return(shiny::tags$small(shiny::tags$em(
          "No sibling family detected for this column."
        )))
      }
      sibs <- grep(pat, cols, value = TRUE)
      shiny::tags$small(
        shiny::tags$em("Will apply to: "),
        paste(sibs, collapse = ", ")
      )
    })

    output$clusters_ui <- shiny::renderUI({
      v <- selected_var_r()
      if (is.null(v)) return(shiny::tags$em("Pick a variable (Variable tab)."))
      cl <- clusters_r()
      if (is.null(cl) || nrow(cl) == 0) return(shiny::tags$em("No data."))

      cluster_sizes <- cl |>
        dplyr::count(cluster_id, name = "size") |>
        dplyr::filter(size > 1) |>
        dplyr::arrange(dplyr::desc(size))

      if (nrow(cluster_sizes) == 0) {
        return(shiny::tags$em(
          "No multi-member clusters at this threshold. Try lowering it."
        ))
      }

      cards <- lapply(cluster_sizes$cluster_id, function(cid) {
        members <- cl[cl$cluster_id == cid, ] |>
          dplyr::arrange(dplyr::desc(n))
        modal <- members$value[1]   # most frequent = default suggested target

        # Radio choices = the cluster members, labelled with frequency and a
        # rare flag. Value is the raw string; default selection is the modal.
        choice_labels <- vapply(seq_len(nrow(members)), function(i)
          sprintf("%s  —  %d%s", members$value[i], members$n[i],
                  if (isTRUE(members$is_rare[i])) "  (rare)" else ""),
          character(1))
        choices <- stats::setNames(members$value, choice_labels)

        bslib::card(
          full_screen = FALSE,
          fill        = FALSE,
          bslib::card_header(
            sprintf("Cluster %d  (%d members)", cid, nrow(members))
          ),
          bslib::card_body(
            fillable = FALSE,
            shiny::radioButtons(
              session$ns(paste0("target_", cid)),
              "Recode the rest to:",
              choices  = choices,
              selected = modal),
            shiny::textInput(
              session$ns(paste0("custom_", cid)),
              label = NULL,
              placeholder = "…or type a different target value"),
            shiny::HTML(sprintf(
              '<button class="btn btn-sm btn-primary" onclick="Shiny.setInputValue(\'%s\', {cid:%d, nonce:Math.random()}, {priority:\'event\'});">Apply</button>',
              session$ns("recode_cluster_clicked"), cid))
          )
        )
      })

      do.call(shiny::tagList, cards)
    })

    # Single handler for all cluster recode buttons (avoids observer accumulation
    # from the old observe+loop pattern that grew unbounded on threshold changes).
    shiny::observeEvent(input$recode_cluster_clicked, ignoreNULL = TRUE, {
      payload <- input$recode_cluster_clicked
      .cid <- payload$cid
      v  <- selected_var_r()
      cl <- clusters_r()
      if (is.null(cl) || is.null(v)) return()
      members <- cl[cl$cluster_id == .cid, ] |> dplyr::arrange(dplyr::desc(n))

      # Target precedence: a non-empty custom value, else the selected radio,
      # else the modal. A custom target need not be one of the members (the
      # correct spelling may be absent from the data).
      custom <- input[[paste0("custom_", .cid)]]
      radio  <- input[[paste0("target_", .cid)]]
      target <- if (!is.null(custom) && nzchar(trimws(custom))) trimws(custom)
                else if (!is.null(radio) && nzchar(radio)) radio
                else members$value[1]

      to_recode <- setdiff(members$value, target)  # every member except target
      if (length(to_recode) == 0) {
        shiny::showNotification("Nothing to recode — target is the only value.",
                                type = "warning")
        return()
      }
      sib_on  <- isTRUE(input$apply_siblings)
      sib_pat <- if (sib_on) suggest_sibling_pattern(v, names(shared_state$data)) else NA_character_
      if (sib_on && is.na(sib_pat)) sib_on <- FALSE

      now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      new_rules <- tibble::tibble(
        rule_id           = recode_rule_id(v, "trimmed_ci", to_recode),
        variable          = v,
        apply_to_siblings = sib_on,
        sibling_pattern   = if (sib_on) sib_pat else NA_character_,
        match_type        = "trimmed_ci",
        old_value         = to_recode,
        new_value         = target,
        action            = "recode",
        notes             = sprintf("From cluster %d", .cid),
        author            = Sys.info()[["user"]],
        created_at        = now,
        updated_at        = now,
        source_dataset    = shared_state$dataset_name %||% NA_character_
      )
      rules_proxy$add(new_rules)
      shiny::showNotification(
        sprintf("Added %d rules from cluster %d. Edit on Recodes tab.", nrow(new_rules), .cid),
        type = "message"
      )
    })
  })
}
