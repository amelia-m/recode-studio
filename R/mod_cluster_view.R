# =============================================================================
# Cluster view
# =============================================================================
# Similarity-grouped view of a column's unique values. Each cluster gets a
# card listing its members (with frequencies); the user picks the canonical
# target via a radio (default = most frequent) or types a custom value, then
# Apply emits sibling-aware rules recoding the rest to that target.
# =============================================================================

# --- Target resolution (pure) ------------------------------------------------
# These two helpers are plain R (no Shiny) so they can be unit-tested on their
# own — see tests/testthat/test-cluster_target.R. They live here rather than in
# string_helpers.R because they describe this tab's two-choice interaction.

#' Decide what a cluster's members should be recoded to.
#'
#' The user makes two choices per cluster that can contradict each other: the
#' value to keep (radio, or a typed custom value) and the members to exclude.
#' Excluding the very value being kept is a contradiction, and the app must not
#' resolve it by guessing — that case returns `status = "conflict"` and the
#' caller refuses to emit any rules for the cluster.
#'
#' @param members  Character vector of cluster member values, most frequent
#'                 first (the fallback target when nothing at all is chosen).
#' @param radio    The keep radio's value, or NULL.
#' @param custom   The custom-target text box, or NULL. Wins over `radio`.
#' @param excluded Character vector of members ticked as excluded.
#' @return list(status, target, chosen, origin, suggestion, message) where
#'   status is "ok" (target usable), "conflict" (chosen target is also
#'   excluded) or "empty" (nothing chosen and every member excluded).
cluster_target_decision <- function(members, radio = NULL, custom = NULL,
                                    excluded = character(0)) {
  members  <- as.character(members)
  excluded <- if (is.null(excluded)) character(0) else as.character(excluded)

  usable <- function(x) {
    !is.null(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  }
  custom_t <- if (usable(custom)) trimws(custom) else NULL
  radio_t  <- if (usable(radio))  radio          else NULL

  chosen <- if (!is.null(custom_t)) custom_t else radio_t
  origin <- if (!is.null(custom_t)) "custom"
            else if (!is.null(radio_t)) "radio"
            else "none"
  remaining <- setdiff(members, excluded)   # keeps the caller's order

  out <- function(status, target, origin, suggestion = NA_character_,
                  message = NA_character_) {
    list(status     = status,
         target     = target,
         chosen     = if (is.null(chosen)) NA_character_ else chosen,
         origin     = origin,
         suggestion = suggestion,
         message    = message)
  }

  if (!is.null(chosen) && chosen %in% excluded) {
    alt <- if (length(remaining)) remaining[1] else NA_character_
    return(out(
      "conflict", NA_character_, origin, alt,
      paste0(
        sprintf('"%s" is both the value you are keeping and ticked as ', chosen),
        "excluded. Nothing was recoded, because those two choices contradict ",
        "each other. Untick Exclude for it, or choose a different value to keep",
        if (!is.na(alt)) sprintf(' (for example "%s")', alt) else "",
        ".")))
  }

  if (is.null(chosen)) {
    if (!length(remaining)) {
      return(out("empty", NA_character_, origin, NA_character_,
                 paste0("Every member of this cluster is excluded, so there ",
                        "is nothing to recode.")))
    }
    return(out("ok", remaining[1], "fallback"))
  }

  out("ok", chosen, origin)
}

#' Old->new recode pairs for a similarity cluster, honouring exclusions.
#'
#' Every member that is neither the keep target nor excluded is recoded to
#' `keep`. Excluded members produce no rule. `keep` need not be one of
#' `members` (the correct spelling may be absent from the data). If `keep` is
#' itself excluded it is defensively treated as excluded and nothing maps onto
#' it — callers should resolve the contradiction first (see
#' `cluster_target_decision()`).
#'
#' @param members  Character vector of cluster member values.
#' @param keep     Scalar target value the others recode to.
#' @param excluded Character vector of member values to drop from the recode set.
#' @return Tibble(old_value, new_value); zero rows when nothing remains.
cluster_recode_pairs <- function(members, keep, excluded = character(0)) {
  members  <- as.character(members)
  keep     <- as.character(keep)
  excluded <- if (is.null(excluded)) character(0) else as.character(excluded)
  # No valid target (missing, NA, or itself excluded) -> nothing maps onto it.
  to_recode <- if (length(keep) == 0 || is.na(keep[1]) || keep[1] %in% excluded)
                 character(0)
               else setdiff(members, union(keep[1], excluded))
  tibble::tibble(
    old_value = to_recode,
    new_value = if (length(to_recode)) rep(keep[1], length(to_recode))
                else character(0)
  )
}

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
        "different one, or type your own target in the box. Tick ",
        shiny::tags$em("Exclude"), " next to any member that does not belong: ",
        "it is dropped from the recode group and no rule is generated for it. ",
        "Excluding the value you chose to keep is a contradiction: Apply stops ",
        "and asks you which you meant, rather than picking a different target ",
        "for you."
      ),
      shiny::uiOutput(ns("conflicts")),
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

    # Standing warning for any cluster whose keep target is also ticked as
    # excluded, so the contradiction is visible before Apply is clicked (Apply
    # itself refuses). One output over all clusters rather than one per card:
    # the cards are re-rendered on every threshold change, and per-card outputs
    # would pile up with them.
    output$conflicts <- shiny::renderUI({
      cl <- clusters_r()
      if (is.null(cl) || nrow(cl) == 0) return(NULL)
      sizes <- cl |> dplyr::count(cluster_id, name = "size")
      cids  <- sort(sizes$cluster_id[sizes$size > 1])
      hits <- character(0)
      for (cid in cids) {
        excluded <- input[[paste0("exclude_", cid)]]
        if (is.null(excluded) || length(excluded) == 0) next
        members <- cl[cl$cluster_id == cid, ]
        members <- members[order(-members$n), ]
        d <- cluster_target_decision(
          members  = members$value,
          radio    = input[[paste0("target_", cid)]],
          custom   = input[[paste0("custom_", cid)]],
          excluded = excluded)
        if (identical(d$status, "ok")) next
        hits <- c(hits, sprintf("Cluster %d: %s", cid, d$message))
      }
      if (length(hits) == 0) return(NULL)
      shiny::div(
        class = "alert alert-warning", style = "padding:.5em .8em;",
        shiny::tags$strong("Sort this out before applying: "),
        shiny::tags$ul(class = "mb-0", lapply(hits, shiny::tags$li)))
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
            # Two aligned columns over the same member list (same order): pick
            # the keep target on the left, tick members to exclude on the right.
            shiny::fluidRow(
              shiny::column(7,
                shiny::radioButtons(
                  session$ns(paste0("target_", cid)),
                  "Recode the rest to:",
                  choices  = choices,
                  selected = modal)
              ),
              shiny::column(5,
                shiny::checkboxGroupInput(
                  session$ns(paste0("exclude_", cid)),
                  "Exclude from cluster:",
                  choices = choices)
              )
            ),
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

      # Members the user ticked to drop from this cluster. Excluded members get
      # no rule and are left exactly as they are.
      excluded <- input[[paste0("exclude_", .cid)]] %||% character(0)

      # Target precedence: a non-empty custom value, else the selected radio,
      # else the modal of the remaining members. A custom target need not be one
      # of the members (the correct spelling may be absent from the data).
      #
      # Excluding the chosen target is refused, not silently worked around:
      # falling through to the most frequent remaining member would emit rules
      # pointing at a value the user never picked, with nothing on screen saying
      # so. Which of the two contradictory choices was meant is theirs to say.
      decision <- cluster_target_decision(
        members  = members$value,
        radio    = input[[paste0("target_", .cid)]],
        custom   = input[[paste0("custom_", .cid)]],
        excluded = excluded)
      if (!identical(decision$status, "ok")) {
        shiny::showNotification(
          sprintf("Cluster %d: %s", .cid, decision$message),
          type = "error", duration = NULL)
        return()
      }
      target <- decision$target

      # Old->new pairs for the non-excluded, non-target members.
      pairs     <- cluster_recode_pairs(members$value, keep = target,
                                        excluded = excluded)
      to_recode <- pairs$old_value
      if (length(to_recode) == 0) {
        shiny::showNotification(
          "Nothing to recode — no non-excluded members remain besides the target.",
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
