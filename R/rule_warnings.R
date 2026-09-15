# =============================================================================
# Recode Studio - Rule Warnings
# =============================================================================
#
# Pure R helpers (no Shiny). Requires string_helpers.R (apply_recodes(),
# .recode_cols_from_names()) and recat_helpers.R (recat_rule_hits(),
# empty_recat_tibble()) to be sourced first.
#
# These checks look for rules that will quietly do less than their author
# expects, and describe each one in plain language for someone who does not
# read R. They never change data or rules; they only report.
#
# ORDER OF OPERATIONS, and why the second check exists: on export, the recode
# rules run FIRST (single pass, first match wins) and the recategorization
# (recat) rules run SECOND, on the already-recoded data. A recat rule's include
# terms are therefore searched for in the RECODED values, not in the values the
# author saw when writing the rule. If a recode rewrites the wording a recat
# term looks for (for example "cappucino" -> "cappuccino" when the recat term
# is "cappucino"), that recat rule silently matches fewer rows. The check
# compares each recat rule's matches on the raw data with its matches on the
# recoded data and reports any rule that lost rows.
#
# Public functions:
#   empty_rule_warnings()                      zero-row warnings scaffold
#   warn_recodes_no_match(df, rules)           recode rules matching no cell
#   warn_recat_terms_recoded_away(df_raw, df_recoded, recat)
#                                              recat rules that lost rows to
#                                              the recodes
#   collect_rule_warnings(df, rules, recat)    both checks, recodes applied once
#
# Warning columns:
#   kind     : "recode_no_match" or "recat_terms_recoded_away"
#   id       : the rule's rule_id (recodes) or recat_id (recat)
#   target   : the recode's variable, or "out_col = category" for recat
#   message  : plain-language explanation
#   n_before : recode_no_match - cells matched (always 0);
#              recat_terms_recoded_away - rows matched on the raw data
#   n_after  : recode_no_match - NA;
#              recat_terms_recoded_away - rows matched on the recoded data
# =============================================================================

#' Zero-row tibble with the rule warnings schema.
empty_rule_warnings <- function() {
  tibble::tibble(
    kind     = character(0),
    id       = character(0),
    target   = character(0),
    message  = character(0),
    n_before = integer(0),
    n_after  = integer(0)
  )
}

# Human wording for a recode match_type.
.match_type_phrase <- function(match_type) {
  mt <- as.character(match_type)
  out <- rep("with an unrecognised match type", length(mt))
  out[!is.na(mt) & mt == "exact"]      <- "exactly"
  out[!is.na(mt) & mt == "exact_ci"]   <- "ignoring capitalization"
  out[!is.na(mt) & mt == "trimmed_ci"] <-
    "ignoring capitalization and extra spaces"
  out[!is.na(mt) & mt == "regex"]      <- "as a pattern"
  out
}

# No-match warnings from an apply_recodes() summary that is already in hand,
# so collect_rule_warnings() does not have to apply the recodes twice.
.warn_recodes_no_match_from_summary <- function(df, rules, summary) {
  if (is.null(rules) || nrow(rules) == 0 || nrow(summary) == 0) {
    return(empty_rule_warnings())
  }
  idx <- which(summary$cells_matched == 0L)
  if (length(idx) == 0) return(empty_rule_warnings())

  rows <- lapply(idx, function(i) {
    r        <- rules[i, ]
    variable <- as.character(r$variable)
    old_txt  <- if (is.na(r$old_value)) "a blank (missing) cell"
                else sprintf("\"%s\"", r$old_value)
    has_cols <- length(.recode_cols_from_names(names(df), r)) > 0
    msg <- if (!has_cols) {
      sprintf(paste0(
        "The rule that changes %s in `%s` did nothing: that column is not in ",
        "this data. Check the column name, or remove the rule if it no longer ",
        "applies."), old_txt, variable)
    } else {
      sprintf(paste0(
        "The rule that changes %s in `%s` did nothing: no cell matched it ",
        "(compared %s). The value may already have been fixed in the data, or ",
        "the rule may be spelled differently from the data."),
        old_txt, variable, .match_type_phrase(r$match_type))
    }
    tibble::tibble(
      kind     = "recode_no_match",
      id       = as.character(r$rule_id),
      target   = variable,
      message  = msg,
      n_before = 0L,
      n_after  = NA_integer_
    )
  })
  dplyr::bind_rows(rows)
}

#' Recode rules that match no cell in `df`.
#'
#' Uses `apply_recodes()`'s `cells_matched`, so a rule whose cells were all
#' claimed by an earlier rule (shadowed) still counts as matching and is not
#' reported here.
#'
#' @param df    Data frame the rules are meant for.
#' @param rules Recode rule tibble.
#' @return Tibble in the `empty_rule_warnings()` schema, one row per rule.
warn_recodes_no_match <- function(df, rules) {
  if (is.null(rules) || nrow(rules) == 0) return(empty_rule_warnings())
  summary <- apply_recodes(df, rules)$summary
  .warn_recodes_no_match_from_summary(df, rules, summary)
}

#' Recat rules that match fewer rows once the recodes have run.
#'
#' Each rule is counted on its own (`recat_rule_hits()`, ignoring priority and
#' first-match-wins between recat rules), once on `df_raw` and once on
#' `df_recoded`. A rule is reported when the recoded count is lower.
#'
#' @param df_raw     The data before recodes.
#' @param df_recoded The same data after `apply_recodes()`.
#' @param recat      Recat rule tibble.
#' @return Tibble in the `empty_rule_warnings()` schema, one row per rule.
warn_recat_terms_recoded_away <- function(df_raw, df_recoded, recat) {
  if (is.null(recat) || nrow(recat) == 0) return(empty_rule_warnings())
  if (identical(df_raw, df_recoded)) return(empty_rule_warnings())

  rows <- lapply(seq_len(nrow(recat)), function(i) {
    r    <- recat[i, ]
    pre  <- sum(recat_rule_hits(df_raw, r))
    post <- sum(recat_rule_hits(df_recoded, r))
    if (post >= pre) return(NULL)
    tibble::tibble(
      kind     = "recat_terms_recoded_away",
      id       = as.character(r$recat_id),
      target   = sprintf("%s = %s", r$out_col, r$category),
      message  = sprintf(paste0(
        "The rule that sets `%s` to \"%s\" matches %d row%s in the original ",
        "data but only %d after the recodes run. Recodes are applied before ",
        "recategorization, so a recode has changed wording this rule's ",
        "include terms look for. Update the include terms to the recoded ",
        "values."),
        r$out_col, r$category, pre, if (pre == 1) "" else "s", post),
      n_before = as.integer(pre),
      n_after  = as.integer(post)
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(empty_rule_warnings())
  dplyr::bind_rows(rows)
}

#' Run every rule check, applying the recodes once.
#'
#' @param df    The raw data.
#' @param rules Recode rule tibble.
#' @param recat Recat rule tibble (default: none).
#' @return Tibble in the `empty_rule_warnings()` schema: recode warnings first,
#'   then recat warnings.
collect_rule_warnings <- function(df, rules, recat = empty_recat_tibble()) {
  if (is.null(rules)) rules <- empty_recodes_tibble()
  if (is.null(recat)) recat <- empty_recat_tibble()
  rec <- apply_recodes(df, rules)
  dplyr::bind_rows(
    empty_rule_warnings(),
    .warn_recodes_no_match_from_summary(df, rules, rec$summary),
    warn_recat_terms_recoded_away(df, rec$df, recat)
  )
}
