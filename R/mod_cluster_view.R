# =============================================================================
# Cluster view & Reference Taxonomy Matcher
# =============================================================================
# Similarity-grouped view of a column's unique values and Reference Taxonomy
# matching. Supports pairwise distance metrics, linear O(N) OpenRefine key-collision
# & n-gram fingerprinting, and fuzzy matching against uploaded standard lists.
# =============================================================================

# --- Target resolution (pure) ------------------------------------------------
# These helpers are plain R (no Shiny) so they can be unit-tested on their
# own (see tests/testthat/test-cluster_target.R).

#' Decide what a cluster's members should be recoded to.
#'
#' The user makes two choices per cluster that can contradict each other: the
#' value to keep (radio, or a typed custom value) and the members to exclude.
#' Excluding the very value being kept is a contradiction, and the app must not
#' resolve it by guessing: that case returns `status = "conflict"` and the
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
#' it; callers should resolve the contradiction first (see
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

# --- Input guard, recompute cap and card cap (pure) --------------------------

#' Drop missing, empty and whitespace-only values before clustering/matching.
#'
#' `unique_values_r()` keeps the empty string so Browse values can target it,
#' but a blank is meaningless to a similarity cluster or a taxonomy match: ""
#' and "   " would even cluster together once whitespace is squished.
#'
#' @param uv tibble(value, n) or NULL.
#' @return `uv` without those rows (NULL stays NULL).
cluster_drop_blank <- function(uv) {
  if (is.null(uv) || nrow(uv) == 0) return(uv)
  v <- as.character(uv$value)
  uv[!is.na(v) & nzchar(trimws(v)), , drop = FALSE]
}

# Algorithms that build a full N x N distance matrix. Key collision, n-gram
# fingerprint and the phonetic ones bucket on a key and scale linearly.
CLUSTER_PAIRWISE_ALGORITHMS <- c("jw", "osa", "lv", "lcs", "cosine", "jaccard")

# Most distinct values a pairwise algorithm clusters. Measured on synthetic
# 6-18 character strings (lower + squish): 1000 values take about 1 s for
# osa / lv / lcs / jw and 2-2.5 s for cosine / jaccard; 1500 take about 2 s
# and 5 s; 2000 about 3 s and 8 s. Time and memory grow with the square.
PAIRWISE_CLUSTER_CAP <- 1000L

# The three count metrics are edit distances: one pair costs the PRODUCT of
# the two lengths, so a handful of long values can cost more than a thousand
# short ones, which the value cap above cannot see. jw and the two q-gram
# metrics are not length-sensitive in the same way and are left to the value
# cap. Measured (R 4.6.1, stringdist 0.9.17) on 20 synthetic values of 3,200
# characters: osa 24.9 s, lv 15.2 s, lcs 13.3 s, against jw 0.44 s,
# cosine 0.09 s, jaccard 0.06 s.
CLUSTER_EDIT_DISTANCE_ALGORITHMS <- c("osa", "lv", "lcs")

# Most characters, summed over the values, an edit-distance algorithm will
# compare on either side.
#
# PROVISIONAL: a judgement read off the measurements, not a measured
# constant, and the owner has the last word on it. Measured elapsed time for
# osa over synthetic values (lower + squish): 40,000 characters 1.7-12.1 s
# depending on the shape, 64,000 characters 24.9 s, 128,000 characters
# 217.5 s. The documented supported workload (1,000 values of 6-18
# characters) is 6,000-18,000 characters, so 40,000 leaves it untouched and
# bites well before the range that costs minutes.
PAIRWISE_CLUSTER_CHAR_CAP <- 40000L

# Most cluster cards drawn at once. Each card is a table of inputs rebuilt by
# renderUI, so hundreds of them make the tab crawl.
MAX_CLUSTER_CARDS <- 50L

#' Is `algorithm` a quadratic pairwise one?
cluster_is_pairwise <- function(algorithm) {
  length(algorithm) == 1L && !is.na(algorithm) &&
    algorithm %in% CLUSTER_PAIRWISE_ALGORITHMS
}

#' Is `algorithm` an edit distance, whose cost grows with value LENGTH as
#' well as with the number of values?
cluster_is_edit_distance <- function(algorithm) {
  length(algorithm) == 1L && !is.na(algorithm) &&
    algorithm %in% CLUSTER_EDIT_DISTANCE_ALGORITHMS
}

#' Length of each value for the character budget, in bytes.
#'
#' `nchar()` in characters THROWS on a value that is not valid UTF-8, and this
#' app ingests arbitrary third-party files. Bytes never throw and are an upper
#' bound on characters, which is the safe direction for a cost budget.
.budget_lengths <- function(values) {
  n <- suppressWarnings(nchar(as.character(values), type = "bytes",
                              allowNA = TRUE))
  n[is.na(n)] <- 0L
  as.integer(n)
}

#' How many of `chars`, taken in order, fit inside a character budget.
#'
#' Always at least one, so there is something to compare: a single value
#' longer than the whole budget is kept. (One value on its own forms no
#' cluster, and the self-comparison that makes it expensive today is being
#' removed from `cluster_strings()` separately.)
.fit_char_budget <- function(chars, budget) {
  if (length(chars) == 0L) return(0L)
  max(1L, sum(cumsum(as.numeric(chars)) <= as.numeric(budget)))
}

#' Limit the values a pairwise algorithm clusters to the most frequent ones
#' that fit both budgets: at most `cap` values, and for an edit distance at
#' most `char_cap` characters in all.
#'
#' Linear algorithms are never capped. The character budget applies to the
#' edit distances only: their cost grows with the square of the value length,
#' so 20 values of 6,400 characters cost minutes while 1,000 values of 12
#' cost seconds, and the value count alone cannot tell them apart
#' (security F6).
#'
#' @param uv        tibble(value, n).
#' @param algorithm one of CLUSTER_ALGORITHMS.
#' @param cap       maximum distinct values for a pairwise algorithm.
#' @param char_cap  maximum characters in all for an edit-distance algorithm.
#' @return list(uv, capped, n_total, cap, kept, reason, chars_total): `uv` is
#'   the (possibly) reduced table, most frequent first when capped; `n_total`
#'   the distinct values before capping; `kept` how many are being clustered;
#'   `reason` `"values"` or `"characters"`, whichever budget bit.
cluster_cap_values <- function(uv, algorithm, cap = PAIRWISE_CLUSTER_CAP,
                               char_cap = PAIRWISE_CLUSTER_CHAR_CAP) {
  n_total <- if (is.null(uv)) 0L else nrow(uv)
  out <- list(uv = uv, capped = FALSE, n_total = n_total,
              cap = as.integer(cap), kept = n_total, reason = "values",
              chars_total = 0L)
  if (!cluster_is_pairwise(algorithm) || n_total == 0L) return(out)

  chars <- .budget_lengths(uv$value)
  out$chars_total <- as.integer(sum(chars))

  ord <- order(-uv$n, seq_len(n_total))
  n_by_value <- min(n_total, max(0L, as.integer(cap)))
  keep_n <- n_by_value
  reason <- "values"
  if (cluster_is_edit_distance(algorithm)) {
    n_by_chars <- .fit_char_budget(chars[ord][seq_len(n_by_value)], char_cap)
    if (n_by_chars < keep_n) {
      keep_n <- n_by_chars
      reason <- "characters"
    }
  }
  if (keep_n >= n_total) return(out)

  out$uv     <- uv[ord[seq_len(keep_n)], , drop = FALSE]
  out$capped <- TRUE
  out$kept   <- as.integer(keep_n)
  out$reason <- reason
  out
}

#' Which multi-member clusters get a card: the largest first, at most
#' `max_cards` of them.
#'
#' @param cl        cluster_strings() output.
#' @param max_cards maximum cards to draw.
#' @return list(ids, sizes, n_total): ids and sizes of the clusters to draw,
#'   in drawing order, and how many multi-member clusters exist in all.
cluster_cards_to_show <- function(cl, max_cards = MAX_CLUSTER_CARDS) {
  if (is.null(cl) || nrow(cl) == 0) {
    return(list(ids = integer(0), sizes = integer(0), n_total = 0L))
  }
  tab   <- table(cl$cluster_id)
  ids   <- as.integer(names(tab))
  sizes <- as.integer(tab)
  multi <- sizes > 1
  ids   <- ids[multi]
  sizes <- sizes[multi]
  # Largest first; ties keep ascending cluster id (order() is stable).
  o <- order(-sizes)
  ids   <- ids[o]
  sizes <- sizes[o]
  n_total <- length(ids)
  keep <- seq_len(min(n_total, max(0L, as.integer(max_cards))))
  list(ids = ids[keep], sizes = sizes[keep], n_total = n_total)
}

# Where to go when a cap bit. One copy, so the value cap, the length cap and
# the taxonomy matcher all point at the linear algorithms in the same words.
CLUSTER_LINEAR_HINT <- paste0(
  "To cluster them all, choose Key collision, N-gram fingerprint, Soundex or ",
  "Metaphone, which handle any number of values quickly.")

#' Limit what the Reference Taxonomy Matcher compares.
#'
#' `match_taxonomy()` builds a full values-by-targets distance matrix, and its
#' call site applied no cap of any kind, so the same length cost reachable on
#' the Clusters tab was reachable here with no bound at all: 20 values of
#' 6,400 characters against 20 reference terms of 6,400 characters took
#' 229.7 s with `osa` (0.17 s with the built-in 51-term state list, whose
#' terms are short, and 6.3 s with `jw`).
#'
#' Only the edit distances are capped, and only on characters: the number of
#' values is left alone because this tab reports on the whole column and
#' 1,000 values against 51 short terms measured 0.17 s.
#'
#' @param uv       tibble(value, n), blanks already dropped.
#' @param targets  character vector of reference terms.
#' @param method   the distance metric chosen in the UI.
#' @param char_cap maximum characters on each side.
#' @return list(uv, targets, capped, values_total, values_kept,
#'   targets_total, targets_kept).
taxonomy_cap_input <- function(uv, targets, method,
                               char_cap = PAIRWISE_CLUSTER_CHAR_CAP) {
  n_values  <- if (is.null(uv)) 0L else nrow(uv)
  n_targets <- length(targets)
  out <- list(uv = uv, targets = targets, capped = FALSE,
              values_total = n_values, values_kept = n_values,
              targets_total = n_targets, targets_kept = n_targets)
  if (!cluster_is_edit_distance(method) || n_values == 0L ||
      n_targets == 0L) {
    return(out)
  }

  ord <- order(-uv$n, seq_len(n_values))
  keep_v <- .fit_char_budget(.budget_lengths(uv$value)[ord], char_cap)
  keep_t <- .fit_char_budget(.budget_lengths(targets), char_cap)

  if (keep_v >= n_values && keep_t >= n_targets) return(out)

  if (keep_v < n_values) {
    out$uv <- uv[ord[seq_len(keep_v)], , drop = FALSE]
    out$values_kept <- as.integer(keep_v)
  }
  if (keep_t < n_targets) {
    out$targets <- targets[seq_len(keep_t)]
    out$targets_kept <- as.integer(keep_t)
  }
  out$capped <- TRUE
  out
}

#' The note shown when a pairwise algorithm was capped, or NULL.
#'
#' @param capping cluster_cap_values() result. `kept` and `reason` may be
#'   absent (an older caller): then every value up to `cap` was kept and the
#'   value cap is what bit.
cluster_cap_note <- function(capping) {
  if (is.null(capping) || !isTRUE(capping$capped)) return(NULL)
  kept  <- format(capping$kept %||% capping$cap, big.mark = ",")
  total <- format(capping$n_total, big.mark = ",")
  why <- if (identical(capping$reason, "characters")) {
    sprintf(paste0(
      "Clustering the %s most frequent of %s distinct values. This algorithm ",
      "compares every character of every value with every character of every ",
      "other, so a few very long values cost more than a thousand short ",
      "ones."), kept, total)
  } else {
    sprintf(paste0(
      "Clustering the %s most frequent of %s distinct values. This algorithm ",
      "compares every value with every other, which gets very slow past %s ",
      "values."), kept, total, kept)
  }
  help_line(paste(why, CLUSTER_LINEAR_HINT))
}

#' The note shown when the taxonomy matcher's input was reduced, or NULL.
#'
#' @param capping taxonomy_cap_input() result.
taxonomy_cap_note <- function(capping) {
  if (is.null(capping) || !isTRUE(capping$capped)) return(NULL)
  num <- function(x) format(x, big.mark = ",")
  parts <- character(0)
  if (capping$values_kept < capping$values_total) {
    parts <- c(parts, sprintf(
      "the %s most frequent of %s distinct values",
      num(capping$values_kept), num(capping$values_total)))
  }
  if (capping$targets_kept < capping$targets_total) {
    parts <- c(parts, sprintf(
      "the first %s of %s reference terms",
      num(capping$targets_kept), num(capping$targets_total)))
  }
  help_line(sprintf(paste0(
    "Matching %s. This distance metric compares every character of every ",
    "value with every character of every term, so a few very long values ",
    "cost more than a thousand short ones. For the whole list, pick a ",
    "shorter reference list or the Jaro-Winkler metric, which is not ",
    "length-sensitive in the same way."),
    paste(parts, collapse = " against ")))
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

# --- UI pieces ---------------------------------------------------------------
# Every shiny:: call below sits inside a function body, so this file still
# sources without Shiny loaded (tests/testthat/test-cluster_target.R relies on
# that).

# Both sub-tabs mint trimmed_ci rules. Users kept asking for case-insensitive
# matching that was already happening, because nothing said so where the rules
# get created.
trimmed_ci_note <- function() {
  shiny::tags$p(
    class = "text-muted small",
    shiny::tags$em(
      "Rules created here match with ", shiny::tags$code("trimmed_ci"),
      ": case and surrounding spaces are ignored. Change that per rule on the ",
      "Recodes tab."))
}

# Styles for the per-cluster member table. Emitted once in the UI, not per
# card render.
cluster_table_css <- function() {
  shiny::tags$style(shiny::HTML("
    /* Fixed layout + full width: with overflow-wrap:anywhere an auto-layout
       table shrinks the Value column to one character wide. NOT SUFFICIENT on
       its own: the table sits inside two .shiny-input-container wrappers and
       shiny.css gives those width: 300px, so width: 100% here would mean 100%
       of 300px. The wrappers carry an inline width: 100% (see
       cluster_member_table()), which no stylesheet can override. */
    .rs-cluster-table { margin-bottom: .5rem; table-layout: fixed; width: 100%; }
    .rs-cluster-table th { font-size: .8rem; text-transform: uppercase;
      letter-spacing: .03em; color: var(--bs-secondary-color, #6c757d); }
    .rs-cluster-table td { overflow-wrap: anywhere; }
    .rs-cl-keep { width: 3.25rem; }
    .rs-cl-n    { width: 2.75rem; }
    .rs-cl-excl { width: 4.5rem; }
    /* Centre the controls under their headers: the class must be on the TD
       as well as the TH, or the label centres and the input stays left. */
    .rs-cluster-table th.rs-cl-keep, .rs-cluster-table td.rs-cl-keep,
    .rs-cluster-table th.rs-cl-excl, .rs-cluster-table td.rs-cl-excl {
      text-align: center; }
    .rs-cluster-table input[type=radio],
    .rs-cluster-table input[type=checkbox] { margin: 0; }
  "))
}

#' One table per cluster: a row per member with Keep radio, n, Exclude
#' checkbox and Value.
#'
#' This replaces two side-by-side inputs over the same member list (a
#' radioButtons and a checkboxGroupInput). Those only LOOKED aligned: a value
#' long enough to wrap pushed row i of one list out of line with row i of the
#' other, so the Exclude tick beside a value could belong to a different value.
#' Each value also appeared twice, with the count glued into its label.
#'
#' HOW SHINY FINDS THESE INPUTS: the radio-group and checkbox-group bindings
#' select on the NAME attribute, `input:radio[name="<container id>"]`. So the
#' invariant is that every input's `name` equals its group container's `id`;
#' the nesting order of the two containers does not matter. Do not drop the
#' name attributes. The row inputs deliberately carry NO id of their own, which
#' stops Shiny's single-checkbox binding from claiming them. The ids are the
#' ones `radioButtons()` / `checkboxGroupInput()` used, so the conflicts output
#' and the Apply handler read exactly what they read before.
#'
#' @param members    Data frame with `value`, `n` and optionally `is_rare`,
#'                   most frequent first.
#' @param target_id  Namespaced id of the keep radio group.
#' @param exclude_id Namespaced id of the exclude checkbox group.
#' @param selected   Value whose Keep radio starts checked.
cluster_member_table <- function(members, target_id, exclude_id,
                                 selected = members$value[1]) {
  rare <- if ("is_rare" %in% names(members)) members$is_rare
          else rep(FALSE, nrow(members))
  rows <- lapply(seq_len(nrow(members)), function(i) {
    val <- members$value[i]
    shiny::tags$tr(
      shiny::tags$td(
        class = "rs-cl-keep",
        shiny::tags$input(
          type = "radio", name = target_id, value = val,
          `aria-label` = paste("Keep", val),
          checked = if (identical(val, selected)) "checked" else NULL)),
      shiny::tags$td(class = "text-end rs-cl-n", members$n[i]),
      shiny::tags$td(
        class = "rs-cl-excl",
        shiny::tags$input(
          type = "checkbox", name = exclude_id, value = val,
          `aria-label` = paste("Exclude", val))),
      # Dataset text goes in as a child, so htmltools escapes it. Never paste
      # it into an HTML string.
      shiny::tags$td(
        val,
        if (isTRUE(rare[i]))
          shiny::tags$span(class = "badge text-bg-light ms-2", "rare"))
    )
  })

  # Both wrappers exist only so the bindings find their inputs. The inline
  # width beats shiny.css's `.shiny-input-container { width: 300px }`.
  shiny::div(
    id = exclude_id,
    class = "shiny-input-checkboxgroup shiny-input-container",
    style = "width:100%",
    shiny::div(
      id = target_id,
      class = "shiny-input-radiogroup shiny-input-container",
      style = "width:100%",
      shiny::tags$table(
        class = "table table-sm align-middle rs-cluster-table",
        # Fixed-width control columns lead and Value, the only elastic
        # column, comes last, so the controls stay in one vertical line.
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(class = "rs-cl-keep", "Keep"),
          shiny::tags$th(class = "text-end rs-cl-n", "n"),
          shiny::tags$th(class = "rs-cl-excl", "Exclude"),
          shiny::tags$th("Value"))),
        shiny::tags$tbody(rows))))
}

#' Title line with a help_btn() for one Clusters sub-tab.
#'
#' Both sub-tabs sit inside one navset card whose header is the tab strip, so
#' there is no per-sub-tab card header to hang help on. This gives each its
#' own header-styled line instead.
#'
#' @param title sub-tab title, also the popover title.
#' @param ... help_btn() content.
cluster_subtab_heading <- function(title, ...) {
  shiny::div(
    class = "fw-semibold mb-2",
    shiny::span(title, help_btn(..., title = title)))
}

mod_cluster_view_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::navset_card_tab(
    id = ns("cluster_subtabs"),

    # --- Sub-tab 1: Internal Similarity Clustering ---------------------------
    bslib::nav_panel(
      title = "Similarity Clustering",
      bslib::card_body(
        fillable = FALSE,
        # The sub-tabs share one card, so each gets its own title line to hold
        # the help button that other tabs put in their card header.
        cluster_subtab_heading(
          "Similarity clusters",
          shiny::p("Groups values in the selected column that are spelled ",
                   "almost the same, so you can fix a whole group at once ",
                   "instead of one value at a time."),
          shiny::p("In each group, choose the value to keep and click Apply. ",
                   "The other values get rules on the Recodes tab that change ",
                   "them to it. Lower the similarity threshold to catch looser ",
                   "matches, or raise it if unrelated values end up together.")
        ),
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
                      shiny::tags$td("Compound words and spacing differences ('Wal Mart' vs 'WalMart', 'e-mail' vs 'email'). At q = 1, transposed characters ('Krzysztof' vs 'Kryzysztof')."),
                      shiny::tags$td("Ignores where the spaces fall, so it catches splits and joins that key collision cannot. Linear, no distance matrix."),
                      shiny::tags$td("Word-order swaps are NOT caught at q >= 2; use Key collision for those. Over-merges short values at q = 1.")
                    ),
                    shiny::tags$tr(
                      shiny::tags$td(shiny::tags$strong("Jaro-Winkler (Default)")),
                      shiny::tags$td(shiny::tags$span(class = "badge bg-secondary", "O(N²) Pairwise")),
                      shiny::tags$td("Short strings, person/place names, single words with typos near end (e.g. 'Americano' vs 'Americanno')."),
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
        trimmed_ci_note(),
        cluster_table_css(),
        shiny::div(
          class = "alert alert-info", style = "padding:.5em .8em;",
          shiny::tags$strong("Tip: "),
          "In each cluster below, click the member you want to keep; the others ",
          "are recoded to it. The most frequent value is pre-selected; pick a ",
          "different one, or type your own target in the box. Tick ",
          shiny::tags$em("Exclude"), " next to any member that does not belong: ",
          "it is dropped from the recode group and no rule is generated for it. ",
          "Excluding the value you chose to keep is a contradiction: Apply stops ",
          "and asks you which you meant, rather than picking a different target ",
          "for you."
        ),
        shiny::uiOutput(ns("conflicts")),
        shiny::uiOutput(ns("cap_note")),
        shiny::uiOutput(ns("clusters_ui"))
      )
    ),

    # --- Sub-tab 2: Reference Taxonomy Matcher --------------------------------
    bslib::nav_panel(
      title = "Reference Taxonomy Matcher",
      bslib::card_body(
        fillable = FALSE,
        cluster_subtab_heading(
          "Reference taxonomy matcher",
          shiny::p("Compares each value in the selected column with a list of ",
                   "correct values that you supply (a built-in list, an ",
                   "uploaded file, or pasted text) and shows the closest match."),
          shiny::p("Check the matches in the table, adjust the minimum ",
                   "similarity cutoff, then click \"Create Recode Rules from ",
                   "Matches\" to add rules on the Recodes tab. Values below the ",
                   "cutoff are left alone.")
        ),
        shiny::div(
          class = "alert alert-secondary", style = "padding:.6em 1em; margin-bottom:1em;",
          shiny::tags$strong("Reference Taxonomy Standardizer: "),
          "Match messy column values against an approved master taxonomy or standard list ",
          "(e.g., standard US State names, standard department categories, ISO codes, or custom company codebooks). ",
          "Accept fuzzy matches and create recode rules in one click."
        ),
        trimmed_ci_note(),
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
        shiny::uiOutput(ns("tax_cap_note")),
        shiny::uiOutput(ns("tax_summary_banner")),
        shiny::uiOutput(ns("tax_empty")),
        DT::dataTableOutput(ns("tax_match_table"))
      )
    )
  )
}

#' @param selected_var_r   reactive selected variable name
#' @param unique_values_r  reactive tibble(value, n) for current var
#' @param rules_proxy      list(add=, set=, get=)
#' @param debounce_ms      wait after the last clustering-settings change
#'                         before clustering again (a slider drag is one run).
#' @param pairwise_cap     see PAIRWISE_CLUSTER_CAP. An argument so tests can
#'                         exercise the cap without a thousand values.
#' @param pairwise_char_cap see PAIRWISE_CLUSTER_CHAR_CAP. An argument so
#'                         tests can exercise it without 40,000 characters.
#' @param max_cards        see MAX_CLUSTER_CARDS.
mod_cluster_view_server <- function(id, shared_state,
                                    selected_var_r,
                                    unique_values_r,
                                    rules_proxy,
                                    debounce_ms  = 500,
                                    pairwise_cap = PAIRWISE_CLUSTER_CAP,
                                    pairwise_char_cap =
                                      PAIRWISE_CLUSTER_CHAR_CAP,
                                    max_cards    = MAX_CLUSTER_CARDS) {
  shiny::moduleServer(id, function(input, output, session) {

    # --- Similarity Clustering Server Logic -----------------------------------

    output$algo_note <- shiny::renderUI({
      # NULL until the select input has registered, as on the first render and
      # after a re-render: `if (NULL == "...")` is "argument is of length
      # zero" and takes the note down (Shiny F13). Default to the algorithm
      # the UI selects, like every other reader in this module.
      alg <- input$algorithm %||% "jw"
      if (alg == "fingerprint") {
        shiny::tags$small(shiny::tags$span(
          class = "text-success fw-bold",
          "⚡ Linear O(N) key collision: groups words regardless of order or punctuation. Threshold is bypassed."
        ))
      } else if (alg == "ngram_fingerprint") {
        shiny::tags$small(shiny::tags$span(
          class = "text-success fw-bold",
          "⚡ Linear O(N) n-gram fingerprint: ignores where the spaces fall, so 'Wal Mart' and 'WalMart' group together. For word-order swaps use Key collision instead. Threshold is bypassed."
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
      alg <- input$algorithm %||% "jw"
      if (alg %in% c("fingerprint", "ngram_fingerprint", "soundex", "metaphone")) {
        shiny::tags$small(shiny::tags$em(
          "Threshold is bypassed by this exact-key / phonetic algorithm."
        ))
      } else {
        shiny::tags$small(shiny::tags$em(
          "Values with similarity >= threshold are clustered together."
        ))
      }
    })

    # Clustering settings, debounced: dragging the threshold slider, stepping
    # q or ticking several normalizers queues ONE recomputation once the
    # inputs have been still for `debounce_ms`, not one per intermediate
    # value. The notes above read the inputs directly and stay instant.
    settings_r <- shiny::reactive({
      alg <- input$algorithm
      q   <- suppressWarnings(as.integer(input$qgram))
      if (length(q) != 1L || is.na(q)) q <- 2L
      list(
        algorithm = alg,
        # The threshold only matters to pairwise algorithms. Blanking it for
        # the others keeps it out of the cache key, so moving the slider under
        # Key collision is a cache hit rather than a rerun.
        threshold = if (cluster_is_pairwise(alg)) input$threshold
                    else NA_real_,
        q         = max(1L, q),
        # normalize_for_cluster() applies steps in a fixed order, so sorting
        # only makes the cache key independent of tick order.
        normalize = sort(input$normalize %||% character(0)))
    })
    settings_d <- shiny::debounce(settings_r, debounce_ms)

    # What gets clustered: blanks dropped, pairwise algorithms capped.
    cluster_input_r <- shiny::reactive({
      s  <- settings_d()
      uv <- cluster_drop_blank(unique_values_r())
      if (is.null(uv) || nrow(uv) == 0 || is.null(s$algorithm)) return(NULL)
      capping <- cluster_cap_values(uv, s$algorithm, pairwise_cap,
                                    pairwise_char_cap)
      list(uv       = capping$uv,
           capping  = capping[c("capped", "n_total", "cap", "kept",
                                "reason")],
           settings = s)
    })

    # Cached on exactly what the computation reads (values, counts,
    # algorithm, threshold, q, normalizers), so returning to an earlier
    # setting is instant. Session-scoped: one user's column values never sit
    # in a cache another session could hit. bindCache() rather than a
    # hand-rolled cache around the pure call, because it slots into the
    # reactive graph as-is: clusters_r() keeps its name and its callers
    # (cards, conflicts, Apply) are untouched.
    clusters_r <- shiny::reactive({
      ci <- cluster_input_r()
      if (is.null(ci)) return(NULL)
      s <- ci$settings
      cluster_strings(
        values      = ci$uv$value,
        frequencies = ci$uv$n,
        threshold   = s$threshold,
        algorithm   = s$algorithm,
        q           = s$q,
        normalize   = s$normalize
      )
    }) |>
      shiny::bindCache(cluster_input_r(), cache = "session")

    output$cap_note <- shiny::renderUI({
      ci <- cluster_input_r()
      if (is.null(ci)) return(NULL)
      cluster_cap_note(ci$capping)
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

      shown <- cluster_cards_to_show(cl, max_cards)

      if (shown$n_total == 0) {
        return(shiny::tags$em(
          "No multi-member clusters at this threshold. Try lowering it or picking a different algorithm."
        ))
      }

      count_line <- if (length(shown$ids) < shown$n_total) {
        help_line(sprintf(
          paste0("Showing %d of %d clusters, largest first. Raise the ",
                 "similarity threshold or choose a stricter algorithm to ",
                 "narrow the list."),
          length(shown$ids), shown$n_total))
      }

      cards <- lapply(shown$ids, function(cid) {
        members <- cl[cl$cluster_id == cid, ] |>
          dplyr::arrange(dplyr::desc(n))
        modal <- members$value[1]   # most frequent = default suggested target

        bslib::card(
          full_screen = FALSE,
          fill        = FALSE,
          bslib::card_header(
            sprintf("Cluster %d  (%d members)", cid, nrow(members))
          ),
          bslib::card_body(
            fillable = FALSE,
            shiny::tags$small(class = "text-muted",
              "Keep one value; the rest are recoded to it. Tick Exclude for ",
              "any member that does not belong."),
            cluster_member_table(
              members,
              target_id  = session$ns(paste0("target_", cid)),
              exclude_id = session$ns(paste0("exclude_", cid)),
              selected   = modal),
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

      shiny::tagList(count_line, cards)
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
          "Nothing to recode: no non-excluded members remain besides the target.",
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
      shiny::req(input$tax_source_type == "file")
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

    # What the matcher compares: blanks dropped, and for an edit-distance
    # metric the two sides bounded by the character budget.
    taxonomy_input_r <- shiny::reactive({
      # A blank value has nothing to match against a standard list.
      uv <- cluster_drop_blank(unique_values_r())
      targets <- taxonomy_targets_r()
      if (is.null(uv) || nrow(uv) == 0 || length(targets) == 0) return(NULL)
      taxonomy_cap_input(uv, targets, input$tax_method %||% "jw",
                         pairwise_char_cap)
    })

    taxonomy_matches_r <- shiny::reactive({
      ti <- taxonomy_input_r()
      if (is.null(ti)) return(NULL)
      match_taxonomy(
        values           = ti$uv$value,
        frequencies      = ti$uv$n,
        taxonomy_targets = ti$targets,
        method           = input$tax_method %||% "jw",
        threshold        = input$tax_threshold %||% 0.75
      )
    })

    output$tax_cap_note <- shiny::renderUI({
      taxonomy_cap_note(taxonomy_input_r())
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

    # Shown instead of the match table while there is nothing to match; the
    # table renders NULL in the same state (see empty_state()).
    output$tax_empty <- shiny::renderUI({
      m <- taxonomy_matches_r()
      if (!is.null(m) && nrow(m) > 0) return(NULL)
      empty_state("No taxonomy targets or dataset values available to match.")
    })

    output$tax_match_table <- DT::renderDataTable({
      m <- taxonomy_matches_r()
      if (is.null(m) || nrow(m) == 0) return(NULL)

      # Similarity stays NUMERIC here and is formatted by DT at render time.
      # Pre-formatting it with sprintf("%.1f%%") makes the column a string, and
      # the default sort below then orders it lexicographically -- "9.0%" above
      # "85.0%" above "100.0%" -- putting the worst matches on top.
      display_df <- data.frame(
        `Original Value` = m$value,
        `Count (N)`      = m$n,
        `Matched Target` = ifelse(is.na(m$matched_target), "-", m$matched_target),
        `Similarity`     = m$similarity,
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
          order = list(list(3, "desc")),   # 0-based with rownames = FALSE -> Similarity
          dom = "frtip"
        )
      ) |>
        DT::formatPercentage("Similarity", digits = 1)
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
        # showNotification's type is match.arg'd against
        # c("default", "message", "warning", "error") -- anything else throws.
        shiny::showNotification(
          "All matched values are already identical to the standard targets (or below threshold).",
          type = "message")
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
