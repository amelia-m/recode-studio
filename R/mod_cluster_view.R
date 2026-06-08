# =============================================================================
# Cluster view
# =============================================================================
# Similarity-grouped view of a column's unique values. Each cluster gets a
# card with members + frequencies, a modal-value suggestion, and a
# "Recode all to <modal>" button that emits sibling-aware rules.
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
            choices = c("Jaro-Winkler (default)"    = "jw",
                        "Damerau-Levenshtein (OSA)" = "osa",
                        "Soundex (phonetic)"        = "soundex"))
        ),
        shiny::column(4,
          shiny::sliderInput(ns("threshold"), "Similarity threshold:",
            min = 0.80, max = 1.00, value = 0.92, step = 0.01)
        ),
        shiny::column(4,
          shiny::checkboxInput(ns("apply_siblings"),
            "Apply to sibling columns when recoding", value = TRUE),
          shiny::uiOutput(ns("sibling_preview"))
        )
      ),
      shiny::hr(),
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
        algorithm   = input$algorithm
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
        modal <- members$value[1]
        member_lines <- lapply(seq_len(nrow(members)), function(i) {
          m <- members[i, ]
          color <- if (isTRUE(m$is_rare)) "red"
                   else if (m$value == modal) "green"
                   else "grey"
          shiny::tags$li(
            badge(sprintf("%dx", m$n), color = color),
            shiny::tags$span(" "),
            shiny::tags$code(m$value)
          )
        })
        bslib::card(
          full_screen = FALSE,
          fill        = FALSE,
          bslib::card_header(
            shiny::span(
              sprintf("Cluster %d  (%d members, modal: ", cid, nrow(members)),
              shiny::tags$code(modal),
              ")"
            )
          ),
          bslib::card_body(
            fillable = FALSE,
            shiny::tags$ul(member_lines),
            shiny::actionButton(
              session$ns(paste0("recode_", cid)),
              sprintf("Recode all to: %s", modal),
              class = "btn btn-sm btn-primary"
            )
          )
        )
      })

      do.call(shiny::tagList, cards)
    })

    # Generic observer that listens for every cluster's recode button.
    shiny::observe({
      cl <- clusters_r()
      v  <- selected_var_r()
      if (is.null(cl) || is.null(v)) return()
      cluster_ids <- unique(cl$cluster_id)
      for (cid in cluster_ids) {
        local({
          .cid <- cid
          shiny::observeEvent(input[[paste0("recode_", .cid)]],
                              ignoreInit = TRUE, once = FALSE, {
            members <- cl[cl$cluster_id == .cid, ] |>
              dplyr::arrange(dplyr::desc(n))
            modal <- members$value[1]
            to_recode <- members$value[members$value != modal]
            if (length(to_recode) == 0) {
              shiny::showNotification("Nothing to recode in this cluster.",
                                      type = "warning")
              return()
            }
            sib_on <- isTRUE(input$apply_siblings)
            sib_pat <- if (sib_on) {
              suggest_sibling_pattern(v, names(shared_state$data))
            } else NA_character_
            if (sib_on && is.na(sib_pat)) sib_on <- FALSE

            now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
            new_rules <- tibble::tibble(
              rule_id           = recode_rule_id(v, "trimmed_ci", to_recode),
              variable          = v,
              apply_to_siblings = sib_on,
              sibling_pattern   = if (sib_on) sib_pat else NA_character_,
              match_type        = "trimmed_ci",
              old_value         = to_recode,
              new_value         = modal,
              action            = "recode",
              notes             = sprintf("From cluster %d", .cid),
              author            = Sys.info()[["user"]],
              created_at        = now,
              updated_at        = now,
              source_dataset    = shared_state$dataset_name %||% NA_character_
            )
            rules_proxy$add(new_rules)
            shiny::showNotification(
              sprintf("Added %d rules from cluster %d. Edit on Recodes tab.",
                      nrow(new_rules), .cid),
              type = "message"
            )
          })
        })
      }
    })
  })
}
