# =============================================================================
# Cluster view & Reference Taxonomy Matcher
# =============================================================================
# Similarity-grouped view of a column's unique values and Reference Taxonomy
# matching. Supports pairwise distance metrics, linear O(N) OpenRefine key-collision
# & n-gram fingerprinting, and fuzzy matching against uploaded standard lists.
# =============================================================================

# --- Target resolution (pure) ------------------------------------------------
# These helpers are plain R (no Shiny) so they can be unit-tested on their
# own — see tests/testthat/test-cluster_target.R.

#' Decide what a cluster's members should be recoded to.
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
cluster_recode_pairs <- function(members, keep, excluded = character(0)) {
  members  <- as.character(members)
  keep     <- as.character(keep)
  excluded <- if (is.null(excluded)) character(0) else as.character(excluded)
  to_recode <- if (length(keep) == 0 || is.na(keep[1]) || keep[1] %in% excluded)
                 character(0)
               else setdiff(members, union(keep[1], excluded))
  tibble::tibble(
    old_value = to_recode,
    new_value = if (length(to_recode)) rep(keep[1], length(to_recode))
                else character(0)
  )
}

# --- Built-in standard reference taxonomies ----------------------------------
BUILTIN_TAXONOMIES <- list(
  "US 50 States & DC" = c(
    "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
    "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
    "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
    "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
    "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
    "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
    "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
    "Washington", "West Virginia", "Wisconsin", "Wyoming"
  ),
  "Standard 5-Point Likert Scale" = c(
    "Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"
  ),
  "Standard Common Departments" = c(
    "Engineering", "Sales", "Support", "Marketing", "Human Resources",
    "Finance", "Operations", "Legal", "Product Management", "Executive"
  )
)

mod_cluster_view_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::navset_card_tab(
    id = ns("cluster_subtabs"),
    
    # --- Sub-tab 1: Internal Similarity Clustering ---------------------------
    bslib::nav_panel(
      title = "Similarity Clustering",
      bslib::card_body(
        fillable = FALSE,
        shiny::fluidRow(
          shiny::column(4,
            shiny::selectInput(ns("algorithm"), "Algorithm:",
              choices = CLUSTER_ALGORITHMS, selected = "jw"),
            shiny::numericInput(ns("qgram"), "q-gram size (cosine / jaccard / n-gram):",
              value = 2, min = 1, max = 5, step = 1),
            shiny::uiOutput(ns("algo_note"))
          ),
          shiny::column(4,
            shiny::sliderInput(ns("threshold"), "Similarity threshold:",
              min = 0.70, max = 1.00, value = 0.92, step = 0.01),
            shiny::uiOutput(ns("threshold_note"))
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
        
        # Accordion: Algorithm Reference Guide & Pros/Cons
        bslib::accordion(
          id = ns("guide_accordion"),
          open = FALSE,
          bslib::accordion_panel(
            "📖 Clustering Algorithm Reference Guide & Pros/Cons",
            shiny::div(
              style = "font-size: 0.88em;",
              shiny::tags$div(
                class = "table-responsive",
                shiny::tags$table(
                  class = "table table-sm table-bordered",
                  shiny::tags$thead(
                    class = "table-light",
                    shiny::tags$tr(
                      shiny::tags$th("Algorithm"),
                      shiny::tags$th("Complexity"),
                      shiny::tags$th("Best Used For"),
                      shiny::tags$th("Pros"),
                      shiny::tags$th("Cons")
                    )
                  ),
                  shiny::tags$tbody(
                    shiny::tags$tr(
                      shiny::tags$td(shiny::tags$strong("Key collision (fingerprint)")),
                      shiny::tags$td(shiny::tags$span(class = "badge bg-success", "O(N) Linear")),
                      shiny::tags$td("Word-order swaps, whitespace runs, punctuation variations (e.g., 'Smith, John' vs 'John Smith')."),
                      shiny::tags$td("Extremely fast; zero threshold guesswork; 100% precision on word permutations."),
                      shiny::tags$td("Does not catch character-level typos ('Smtih' vs 'Smith').")
                    ),
                    shiny::tags$tr(
                      shiny::tags$td(shiny::tags$strong("N-gram fingerprint")),
                      shiny::tags$td(shiny::tags$span(class = "badge bg-success", "O(N) Linear")),
                      shiny::tags$td("Transposed characters, compound words, small spelling permutations."),
                      shiny::tags$td("Fast linear execution; catches character transpositions without quadratic distance matrices."),
                      shiny::tags$td("May over-merge very short words if q is too small.")
                    ),
                    shiny::tags$tr(
                      shiny::tags$td(shiny::tags$strong("Jaro-Winkler (Default)")),
                      shiny::tags$td(shiny::tags$span(class = "badge bg-secondary", "O(N²) Pairwise")),
                      shiny::tags$td("Short strings, person/place names, single words with typos near end (e.g. 'Nebraska' vs 'Nebrska')."),
                      shiny::tags$td("Heavily rewards matching prefixes; gold standard for entity names."),
                      shiny::tags$td("Less effective for multi-word phrases with reordered words.")
                    ),
                    shiny::tags$tr(
                      shiny::tags$td(shiny::tags$strong("Levenshtein / OSA")),
                      shiny::tags$td(shiny::tags$span(class = "badge bg-secondary", "O(N²) Pairwise")),
                      shiny::tags$td("Fixed-length codes, serial IDs, or words with insertions/deletions/transpositions."),
                      shiny::tags$td("Predictable character edit penalties; standard metric."),
                      shiny::tags$td("Favors equal-length strings; sensitive to word length disparities.")
                    ),
                    shiny::tags$tr(
                      shiny::tags$td(shiny::tags$strong("Cosine / Jaccard (q-gram)")),
                      shiny::tags$td(shiny::tags$span(class = "badge bg-secondary", "O(N²) Pairwise")),
                      shiny::tags$td("Multi-word phrases, notes, free text with overlapping word pieces."),
                      shiny::tags$td("Tolerant to word addition/removal; handles out-of-order substrings."),
                      shiny::tags$td("Can be computationally slow on large vocabularies.")
                    ),
                    shiny::tags$tr(
                      shiny::tags$td(shiny::tags$strong("Soundex / Metaphone")),
                      shiny::tags$td(shiny::tags$span(class = "badge bg-success", "O(N) Linear")),
                      shiny::tags$td("Auditory typos, transcribed call notes, phonetically identical names ('Claire' vs 'Clare')."),
                      shiny::tags$td("Groups words by pronunciation regardless of spelling differences."),
                      shiny::tags$td("English phonetic bias; merges phonetically identical but semantically distinct words.")
                    )
                  )
                )
              )
            )
          )
        ),
        shiny::br(),
        shiny::div(
          class = "alert alert-info", style = "padding:.5em .8em;",
          shiny::tags$strong("Tip: "),
          "In each cluster below, click the member you want to keep — the others ",
          "are recoded to it. The most frequent value is pre-selected; pick a ",
          "different one, or type your own target in the box. Tick ",
          shiny::tags$em("Exclude"), " next to any member that does not belong: ",
          "it is dropped from the recode group and no rule is generated for it."
        ),
        shiny::uiOutput(ns("conflicts")),
        shiny::uiOutput(ns("clusters_ui"))
      )
    ),
    
    # --- Sub-tab 2: Reference Taxonomy Matcher --------------------------------
    bslib::nav_panel(
      title = "Reference Taxonomy Matcher",
      bslib::card_body(
        fillable = FALSE,
        shiny::div(
          class = "alert alert-secondary", style = "padding:.6em 1em; margin-bottom:1em;",
          shiny::tags$strong("Reference Taxonomy Standardizer: "),
          "Match messy column values against an approved master taxonomy or standard list ",
          "(e.g., standard US State names, standard department categories, ISO codes, or custom company codebooks). ",
          "Accept fuzzy matches and create recode rules in one click."
        ),
        shiny::fluidRow(
          shiny::column(4,
            shiny::radioButtons(ns("tax_source_type"), "Taxonomy source:",
              choices = c("Built-in Standard" = "builtin",
                          "Upload File (CSV/Excel)" = "file",
                          "Paste Text List" = "paste"),
              inline = FALSE),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'builtin'", ns("tax_source_type")),
              shiny::selectInput(ns("tax_builtin"), "Select standard taxonomy:",
                choices = names(BUILTIN_TAXONOMIES), selected = "US 50 States & DC")
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'file'", ns("tax_source_type")),
              shiny::fileInput(ns("tax_file"), "Upload reference CSV or Excel:",
                accept = c(".csv", ".xlsx", ".xls")),
              shiny::uiOutput(ns("tax_col_picker"))
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'paste'", ns("tax_source_type")),
              shiny::textAreaInput(ns("tax_paste"), "Paste standard terms (one per line):",
                rows = 4, placeholder = "Standard Category A\nStandard Category B\nStandard Category C")
            )
          ),
          shiny::column(4,
            shiny::selectInput(ns("tax_method"), "Distance metric:",
              choices = c("Jaro-Winkler" = "jw",
                          "Optimal String Alignment" = "osa",
                          "Levenshtein" = "lv",
                          "Cosine (q-gram)" = "cosine",
                          "Jaccard" = "jaccard"),
              selected = "jw"),
            shiny::sliderInput(ns("tax_threshold"), "Minimum similarity cutoff:",
              min = 0.50, max = 1.00, value = 0.75, step = 0.01)
          ),
          shiny::column(4,
            shiny::checkboxInput(ns("tax_apply_siblings"),
              "Apply to sibling columns when recoding", value = TRUE),
            shiny::uiOutput(ns("tax_sibling_preview")),
            shiny::br(),
            shiny::actionButton(ns("apply_tax_rules"),
              "Create Recode Rules from Matches",
              class = "btn-success btn-sm w-100",
              icon = shiny::icon("check"))
          )
        ),
        shiny::hr(),
        shiny::uiOutput(ns("tax_summary_banner")),
        DT::dataTableOutput(ns("tax_match_table"))
      )
    )
  )
}

mod_cluster_view_server <- function(id, shared_state,
                                    selected_var_r,
                                    unique_values_r,
                                    rules_proxy) {
  shiny::moduleServer(id, function(input, output, session) {

    # --- Similarity Clustering Server Logic -----------------------------------

    output$algo_note <- shiny::renderUI({
      alg <- input$algorithm
      if (alg == "fingerprint") {
        shiny::tags$small(shiny::tags$span(
          class = "text-success fw-bold",
          "⚡ Linear O(N) key collision: groups words regardless of order or punctuation. Threshold is bypassed."
        ))
      } else if (alg == "ngram_fingerprint") {
        shiny::tags$small(shiny::tags$span(
          class = "text-success fw-bold",
          "⚡ Linear O(N) n-gram fingerprint: groups strings with shared character pieces. Threshold is bypassed."
        ))
      } else if (alg %in% c("soundex", "metaphone")) {
        shiny::tags$small(shiny::tags$span(
          class = "text-info fw-bold",
          "🗣️ Phonetic bucketing: groups words that sound identical. Threshold is bypassed."
        ))
      } else {
        shiny::tags$small(shiny::tags$span(
          class = "text-muted",
          "🔍 Pairwise similarity clustering: groups values whose similarity >= threshold."
        ))
      }
    })

    output$threshold_note <- shiny::renderUI({
      if (input$algorithm %in% c("fingerprint", "ngram_fingerprint", "soundex", "metaphone")) {
        shiny::tags$small(shiny::tags$em(
          "Threshold is bypassed by this exact-key / phonetic algorithm."
        ))
      } else {
        shiny::tags$small(shiny::tags$em(
          "Values with similarity >= threshold are clustered together."
        ))
      }
    })

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
          "No multi-member clusters at this threshold. Try lowering it or picking a different algorithm."
        ))
      }

      cards <- lapply(cluster_sizes$cluster_id, function(cid) {
        members <- cl[cl$cluster_id == cid, ] |>
          dplyr::arrange(dplyr::desc(n))
        modal <- members$value[1]

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

    shiny::observeEvent(input$recode_cluster_clicked, ignoreNULL = TRUE, {
      payload <- input$recode_cluster_clicked
      .cid <- payload$cid
      v  <- selected_var_r()
      cl <- clusters_r()
      if (is.null(cl) || is.null(v)) return()
      members <- cl[cl$cluster_id == .cid, ] |> dplyr::arrange(dplyr::desc(n))

      excluded <- input[[paste0("exclude_", .cid)]] %||% character(0)

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

    # --- Reference Taxonomy Server Logic --------------------------------------

    uploaded_tax_df <- shiny::reactive({
      req(input$tax_source_type == "file")
      f <- input$tax_file
      if (is.null(f)) return(NULL)
      ext <- tolower(tools::file_ext(f$name))
      if (ext %in% c("xlsx", "xls")) {
        readxl::read_excel(f$datapath)
      } else {
        readr::read_csv(f$datapath, show_col_types = FALSE)
      }
    })

    output$tax_col_picker <- shiny::renderUI({
      df <- uploaded_tax_df()
      if (is.null(df) || ncol(df) == 0) return(NULL)
      shiny::selectInput(session$ns("tax_col"), "Select taxonomy column:",
        choices = names(df), selected = names(df)[1])
    })

    taxonomy_targets_r <- shiny::reactive({
      stype <- input$tax_source_type %||% "builtin"
      if (stype == "builtin") {
        bname <- input$tax_builtin %||% names(BUILTIN_TAXONOMIES)[1]
        return(BUILTIN_TAXONOMIES[[bname]] %||% character(0))
      } else if (stype == "file") {
        df <- uploaded_tax_df()
        if (is.null(df)) return(character(0))
        col <- input$tax_col %||% names(df)[1]
        if (!col %in% names(df)) return(character(0))
        return(as.character(df[[col]]))
      } else if (stype == "paste") {
        txt <- input$tax_paste %||% ""
        lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
        lines <- trimws(lines)
        return(lines[nzchar(lines)])
      }
      character(0)
    })

    taxonomy_matches_r <- shiny::reactive({
      uv <- unique_values_r()
      targets <- taxonomy_targets_r()
      if (is.null(uv) || nrow(uv) == 0 || length(targets) == 0) return(NULL)
      match_taxonomy(
        values           = uv$value,
        frequencies      = uv$n,
        taxonomy_targets = targets,
        method           = input$tax_method %||% "jw",
        threshold        = input$tax_threshold %||% 0.75
      )
    })

    output$tax_sibling_preview <- shiny::renderUI({
      v <- selected_var_r()
      if (!isTRUE(input$tax_apply_siblings) || is.null(v)) return(NULL)
      cols <- names(shared_state$data)
      pat  <- suggest_sibling_pattern(v, cols)
      if (is.na(pat)) {
        return(shiny::tags$small(shiny::tags$em("No sibling family detected.")))
      }
      sibs <- grep(pat, cols, value = TRUE)
      shiny::tags$small(
        shiny::tags$em("Will apply to: "),
        paste(sibs, collapse = ", ")
      )
    })

    output$tax_summary_banner <- shiny::renderUI({
      m <- taxonomy_matches_r()
      if (is.null(m) || nrow(m) == 0) return(NULL)
      n_total   <- nrow(m)
      n_matched <- sum(m$is_matched, na.rm = TRUE)
      pct       <- round(100 * n_matched / max(1, n_total), 1)
      
      shiny::div(
        class = "alert alert-info", style = "padding:.5em .8em;",
        shiny::tags$strong("Match Summary: "),
        sprintf("Matched %d of %d unique values (%s%%) with similarity >= %.2f.",
                n_matched, n_total, pct, input$tax_threshold %||% 0.75)
      )
    })

    output$tax_match_table <- DT::renderDataTable({
      m <- taxonomy_matches_r()
      if (is.null(m) || nrow(m) == 0) {
        return(DT::datatable(
          data.frame(Message = "No taxonomy targets or dataset values available to match."),
          options = list(dom = "t")
        ))
      }

      display_df <- data.frame(
        `Original Value` = m$value,
        `Count (N)`      = m$n,
        `Matched Target` = ifelse(is.na(m$matched_target), "—", m$matched_target),
        `Similarity`     = sprintf("%.1f%%", m$similarity * 100),
        `Status`         = ifelse(m$status == "exact", "Exact Match",
                           ifelse(m$status == "fuzzy_match", "Fuzzy Match", "Below Threshold")),
        stringsAsFactors = FALSE,
        check.names      = FALSE
      )

      DT::datatable(
        display_df,
        selection = "none",
        rownames  = FALSE,
        escape    = TRUE,
        options   = list(
          pageLength = 15,
          order = list(list(3, "desc")),
          dom = "frtip"
        )
      )
    })

    shiny::observeEvent(input$apply_tax_rules, {
      v <- selected_var_r()
      m <- taxonomy_matches_r()
      if (is.null(v) || is.null(m) || nrow(m) == 0) {
        shiny::showNotification("No taxonomy matches available to apply.", type = "warning")
        return()
      }

      # Filter to matched items where original value differs from matched target
      hits <- m[m$is_matched & !is.na(m$matched_target) & m$value != m$matched_target, ]
      if (nrow(hits) == 0) {
        shiny::showNotification(
          "All matched values are already identical to the standard targets (or below threshold).",
          type = "information")
        return()
      }

      sib_on  <- isTRUE(input$tax_apply_siblings)
      sib_pat <- if (sib_on) suggest_sibling_pattern(v, names(shared_state$data)) else NA_character_
      if (sib_on && is.na(sib_pat)) sib_on <- FALSE

      now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      new_rules <- tibble::tibble(
        rule_id           = recode_rule_id(v, "trimmed_ci", hits$value),
        variable          = v,
        apply_to_siblings = sib_on,
        sibling_pattern   = if (sib_on) sib_pat else NA_character_,
        match_type        = "trimmed_ci",
        old_value         = hits$value,
        new_value         = hits$matched_target,
        action            = "recode",
        notes             = sprintf("Taxonomy match: %s (sim: %.2f)", hits$matched_target, hits$similarity),
        author            = Sys.info()[["user"]],
        created_at        = now,
        updated_at        = now,
        source_dataset    = shared_state$dataset_name %||% NA_character_
      )
      rules_proxy$add(new_rules)
      shiny::showNotification(
        sprintf("Created %d recode rules from taxonomy matches! Review on Recodes tab.", nrow(new_rules)),
        type = "message"
      )
    })

  })
}
