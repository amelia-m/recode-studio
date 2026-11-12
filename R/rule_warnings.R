# =============================================================================
# Recode Studio - Rule Warnings
# =============================================================================
#
# Pure R helpers (no Shiny). Requires string_helpers.R (apply_recodes(),
# apply_normalizations(), recode_norm_specs(), .recode_cols_from_names()) and
# recat_helpers.R (recat_rule_hits(), empty_recat_tibble()) to be sourced
# first.
#
# These checks look for rules that will quietly do less than their author
# expects, and describe each one in plain language for someone who does not
# read R. They never change data or rules; they only report.
#
# ORDER OF OPERATIONS: column normalizations run FIRST, the recode rules run
# SECOND (single pass, first match wins) and the recategorization (recat)
# rules run THIRD, on the normalized and recoded data. Recodes are therefore
# checked against the NORMALIZED data.
#
# Why the recat check exists: recat runs after the recodes. A recat rule's include
# terms are therefore searched for in the RECODED values, not in the values the
# author saw when writing the rule. If a recode rewrites the wording a recat
# term looks for (for example "cappucino" -> "cappuccino" when the recat term
# is "cappucino"), that recat rule silently matches fewer rows. The check
# compares each recat rule's matches on the raw data with its matches on the
# recoded data and reports any rule that lost rows.
#
# Why the normalization checks exist: a recode rule is written against values
# as the normalizations leave them, and each rule is stamped with the
# normalization in force when it was made (`norm_spec`). If the normalization
# changes afterwards, the value the rule looks for may no longer exist. The
# drift check compares each stamp with the current normalization and needs no
# data at all. A normalization that changes nothing, or reaches no column, is
# reported too.
#
# Public functions:
#   empty_rule_warnings()                      zero-row warnings scaffold
#   warn_recodes_no_match(df, rules)           recode rules matching no cell
#   .warn_recodes_fully_shadowed(rules, summary)
#                                              recode rules whose every
#                                              matched cell an earlier rule
#                                              had already claimed
#   .warn_recat_no_match(recat, summary)       recat rules matching no row
#   warn_recat_terms_recoded_away(df_raw, df_recoded, recat)
#                                              recat rules that lost rows to
#                                              the recodes
#   .warn_normalizations(normalizations, summary)
#                                              normalizations that reach no
#                                              column or change nothing
#   .warn_norm_spec_drift(rules, normalizations, data_cols)
#                                              recode rules written under a
#                                              different normalization
#   collect_rule_warnings(df, rules, recat, normalizations = NULL)
#                                              every check, each step applied
#                                              once
#
# Warning columns:
#   kind     : "normalize_no_target", "normalize_no_change",
#              "recode_no_match", "recode_fully_shadowed", "norm_spec_drift",
#              "norm_spec_unstamped", "recat_terms_recoded_away" or
#              "recat_no_match"
#   id       : norm_id (normalizations), rule_id (recodes) or recat_id (recat)
#   target   : the rule's variable (or sibling pattern for a family
#              normalization), or "out_col = category" for recat
#   message  : plain-language explanation
#   n_before : recode_no_match - cells matched (always 0);
#              recode_fully_shadowed - cells matched, every one of them
#              already claimed by an earlier rule;
#              recat_no_match - rows matched (always 0);
#              recat_terms_recoded_away - rows matched before the recodes;
#              normalize_no_target / normalize_no_change - columns reached;
#              norm_spec_* - NA
#   n_after  : recat_terms_recoded_away - rows matched after the recodes;
#              normalize_no_change - cells changed (always 0);
#              everything else - NA
#   severity : "warning" (the rule may really do less than expected) or
#              "note" (a stamp-only difference the rule's match type already
#              ignores, so it cannot change what the rule matches). Only the
#              norm_spec_* kinds are ever "note"; see
#              .stamp_change_is_neutral().
# =============================================================================

#' Zero-row tibble with the rule warnings schema.
empty_rule_warnings <- function() {
  tibble::tibble(
    kind     = character(0),
    id       = character(0),
    target   = character(0),
    message  = character(0),
    n_before = integer(0),
    n_after  = integer(0),
    severity = character(0)
  )
}

# Severity values, single-sourced. "warning": the rule may really do less
# than its author expects. "note": worth knowing, but it cannot change what
# the rule does (see .stamp_change_is_neutral()).
RULE_WARNING_SEVERITIES <- c("warning", "note")

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
    bad_sib  <- isTRUE(.bad_sibling_rule(r$apply_to_siblings,
                                         r$sibling_pattern))
    msg <- if (bad_sib) {
      sprintf(paste0(
        "The rule that changes %s in `%s` did nothing: its sibling pattern ",
        "`%s` is not a valid regular expression, so it was skipped. Fix the ",
        "pattern in the Recodes table."), old_txt, variable, r$sibling_pattern)
    } else if (!has_cols) {
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
      n_after  = NA_integer_,
      severity = "warning"
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

#' Recode rules every one of whose matched cells an earlier rule had claimed.
#'
#' `recode_no_match` cannot see these: the rule DID match, so
#' `cells_matched` is not zero, and the no-match check passes it over. But
#' rules are single pass and first match wins, so a rule whose every matched
#' cell was already claimed changes nothing at all. That is how a newly
#' created rule sitting behind an older one for the same value disappeared in
#' silence.
#'
#' A rule only PARTLY shadowed is not reported: it did some of its work, and
#' the three per-rule cell counts on Preview & export already show the split.
#' A rule whose `new_value` equals the value it matches is not reported
#' either; it changed nothing, but nothing shadowed it.
#'
#' @param rules   Recode rule tibble.
#' @param summary `apply_recodes()$summary` for those rules, same row order.
#' @return Tibble in the `empty_rule_warnings()` schema.
.warn_recodes_fully_shadowed <- function(rules, summary) {
  if (is.null(rules) || nrow(rules) == 0 || is.null(summary) ||
      nrow(summary) == 0) {
    return(empty_rule_warnings())
  }
  n   <- min(nrow(rules), nrow(summary))
  mat <- as.integer(summary$cells_matched[seq_len(n)])
  shd <- as.integer(summary$cells_shadowed[seq_len(n)])
  idx <- which(!is.na(mat) & !is.na(shd) & mat > 0L & shd >= mat)
  if (length(idx) == 0) return(empty_rule_warnings())

  rows <- lapply(idx, function(i) {
    r       <- rules[i, ]
    old_txt <- if (is.na(r$old_value)) "a blank (missing) cell"
               else sprintf("\"%s\"", r$old_value)
    tibble::tibble(
      kind     = "recode_fully_shadowed",
      id       = as.character(r$rule_id),
      target   = as.character(r$variable),
      message  = sprintf(paste0(
        "The rule that changes %s in `%s` did nothing: an earlier rule had ",
        "already claimed every one of the %d cell%s it matched. Rules are ",
        "applied in one pass, in the order of the Recodes table, and the ",
        "first rule to claim a cell wins. Move this rule above the one ",
        "shadowing it, or delete whichever of the two is wrong."),
        old_txt, as.character(r$variable), mat[i],
        if (mat[i] == 1) "" else "s"),
      n_before = mat[i],
      n_after  = 0L,
      severity = "warning"
    )
  })
  dplyr::bind_rows(rows)
}

#' Recat rules that match no row at all.
#'
#' The recat counterpart of `recode_no_match`, from `apply_recat()`'s
#' `rows_matched`. Without it the all-clear line on Preview & export was
#' literally true while a recat rule quietly assigned nothing.
#'
#' A rule skipped because its `out_col` already exists gets no summary row
#' and so is not reported here: `validate_recat()$out_col_exists` says that
#' far more precisely.
#'
#' `exclude_ids` is how the caller keeps this from double-reporting. A rule
#' that `recat_terms_recoded_away` already covers matches no row for a reason
#' that warning states exactly ("it matched 2 rows before the recodes and 0
#' after"); saying again that it matched nothing would add a second, vaguer
#' line about one rule and send the author looking for a spelling mistake
#' that is not there.
#'
#' @param recat   Recat rule tibble.
#' @param summary `apply_recat()$summary` (matched on `recat_id`).
#' @param exclude_ids `recat_id`s already reported by another check.
#' @return Tibble in the `empty_rule_warnings()` schema.
.warn_recat_no_match <- function(recat, summary,
                                 exclude_ids = character(0)) {
  if (is.null(recat) || nrow(recat) == 0 || is.null(summary) ||
      nrow(summary) == 0) {
    return(empty_rule_warnings())
  }
  zero <- summary[!is.na(summary$rows_matched) & summary$rows_matched == 0L, ,
                  drop = FALSE]
  if (length(exclude_ids) > 0) {
    zero <- zero[!(as.character(zero$recat_id) %in%
                     as.character(exclude_ids)), , drop = FALSE]
  }
  if (nrow(zero) == 0) return(empty_rule_warnings())

  rows <- lapply(seq_len(nrow(zero)), function(k) {
    s     <- zero[k, ]
    terms <- recat_split(recat$include_terms[
      match(s$recat_id, recat$recat_id)])
    tibble::tibble(
      kind     = "recat_no_match",
      id       = as.character(s$recat_id),
      target   = sprintf("%s = %s", s$out_col, s$category),
      message  = sprintf(paste0(
        "The rule that sets `%s` to \"%s\" matched no row: no value in the ",
        "columns it reads contains any of its include terms (%s). Check the ",
        "spelling of the terms against the Browse values tab, or remove the ",
        "rule if it no longer applies."),
        s$out_col, s$category,
        if (length(terms) == 0) "it has none"
        else paste(sprintf("\"%s\"", terms), collapse = ", ")),
      n_before = 0L,
      n_after  = NA_integer_,
      severity = "warning"
    )
  })
  dplyr::bind_rows(rows)
}

#' Recat rules that match fewer rows once the recodes have run.
#'
#' Each rule is counted on its own (`recat_rule_hits()`, ignoring priority and
#' first-match-wins between recat rules), once on `df_raw` and once on
#' `df_recoded`. A rule is reported when the recoded count is lower.
#'
#' @param df_raw     The data before recodes (after any normalizations).
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
      n_after  = as.integer(post),
      severity = "warning"
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(empty_rule_warnings())
  dplyr::bind_rows(rows)
}

# --- Normalizations -------------------------------------------------------------

# Short op wording for messages, keyed by op.
.NORMALIZE_OP_PHRASES <- c(
  trim      = "trim spaces",
  squish    = "collapse spaces",
  lowercase = "lower case",
  uppercase = "UPPER CASE",
  titlecase = "Title Case"
)

# "collapse spaces + Title Case"; "no changes selected" when there are none.
.ops_phrase <- function(ops) {
  ops <- .norm_ops_ordered(ops)
  if (length(ops) == 0) return("no changes selected")
  paste(unname(.NORMALIZE_OP_PHRASES[ops]), collapse = " + ")
}

# TRUE when a normalization rule targets a sibling family.
.norm_is_family <- function(rule) {
  sp <- clean_sibling_pattern(rule$sibling_pattern)
  length(rule$apply_to_siblings) == 1 &&
    !is.na(rule$apply_to_siblings) && rule$apply_to_siblings &&
    length(sp) == 1 && !is.na(sp)
}

# What a normalization rule targets, in words.
.normalization_target_phrase <- function(rule) {
  if (.norm_is_family(rule)) sprintf("columns matching `%s`", rule$sibling_pattern)
  else sprintf("`%s`", rule$variable)
}

# A norm_spec stamp in words: "`drink1`: UPPER CASE; `drink2`: UPPER CASE",
# or "no normalization" for "".
.norm_spec_phrase <- function(spec) {
  if (is.na(spec) || !nzchar(spec)) return("no normalization")
  parts <- strsplit(spec, "|", fixed = TRUE)[[1]]
  txt <- vapply(parts, function(p) {
    eq <- regexpr("=", p, fixed = TRUE)
    if (eq < 1) return(p)
    sprintf("`%s`: %s", substr(p, 1, eq - 1),
            .ops_phrase(substr(p, eq + 1, nchar(p))))
  }, character(1), USE.NAMES = FALSE)
  paste(txt, collapse = "; ")
}

#' Normalizations that reach no column, or change no cell.
#'
#' @param normalizations Normalization rule tibble.
#' @param summary `apply_normalizations()$summary` for those rules, in the same
#'   row order.
#' @return Tibble in the `empty_rule_warnings()` schema.
.warn_normalizations <- function(normalizations, summary) {
  if (is.null(normalizations) || !is.data.frame(normalizations) ||
      nrow(normalizations) == 0 || is.null(summary) || nrow(summary) == 0) {
    return(empty_rule_warnings())
  }
  rows <- list()
  for (i in seq_len(min(nrow(normalizations), nrow(summary)))) {
    r   <- normalizations[i, ]
    s   <- summary[i, ]
    tgt <- .normalization_target_phrase(r)
    ops <- .ops_phrase(r$ops)
    target <- if (.norm_is_family(r)) as.character(r$sibling_pattern)
              else as.character(r$variable)
    if (is.na(s$n_cols) || s$n_cols == 0L) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        kind     = "normalize_no_target",
        id       = as.character(s$norm_id),
        target   = target,
        message  = sprintf(paste0(
          "The normalization of %s (%s) did nothing: no column in this data ",
          "matches it. Check the column name, or remove the rule if it no ",
          "longer applies."), tgt, ops),
        n_before = 0L,
        n_after  = NA_integer_,
        severity = "warning")
    } else if (!is.na(s$cells_changed) && s$cells_changed == 0L) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        kind     = "normalize_no_change",
        id       = as.character(s$norm_id),
        target   = target,
        message  = sprintf(paste0(
          "The normalization of %s (%s) did nothing: every value is already ",
          "written that way."), tgt, ops),
        n_before = as.integer(s$n_cols),
        n_after  = 0L,
        severity = "warning")
    }
  }
  if (length(rows) == 0) return(empty_rule_warnings())
  dplyr::bind_rows(rows)
}

# Normalization ops a recode match_type makes irrelevant: the match type
# compares both sides after its own transform, and that transform absorbs
# these ops. trimmed_ci compares str_squish(tolower(x)), which undoes trim,
# squish and any case op; exact_ci compares tolower(x), which undoes the case
# ops only; exact and regex see the value as it is. (Caveat, accepted: a few
# Unicode letters do not round-trip through case mapping, e.g. German sharp s
# upper-cases to "SS"; for those a case op is not strictly invisible.)
.NEUTRALIZED_OPS <- list(
  exact      = character(0),
  exact_ci   = c("lowercase", "uppercase", "titlecase"),
  trimmed_ci = c("trim", "squish", "lowercase", "uppercase", "titlecase"),
  regex      = character(0)
)

# A norm_spec stamp as a named list: column -> ops (character vector).
.parse_norm_spec <- function(spec) {
  if (is.na(spec) || !nzchar(spec)) return(list())
  parts <- strsplit(spec, "|", fixed = TRUE)[[1]]
  out <- list()
  for (p in parts) {
    eq <- regexpr("=", p, fixed = TRUE)
    if (eq < 1) next
    col <- substr(p, 1, eq - 1)
    out[[col]] <- unique(c(out[[col]],
                           normalize_ops_split(substr(p, eq + 1, nchar(p)))))
  }
  out
}

#' Can a change of normalization NOT affect which cells a recode rule matches?
#'
#' TRUE when every op that differs between the stamped and the current
#' normalization, column by column, is neutralized by the rule's match type
#' (.NEUTRALIZED_OPS). An unknown stamp (NA) could have been anything, so it
#' is neutral only when the match type neutralizes every op. A rule for
#' missing cells (NA `old_value`) is always neutral: no op turns a missing
#' cell into a present one or back. An unusable match type is never neutral.
#'
#' @return Single logical.
.stamp_change_is_neutral <- function(match_type, old_value, stamped, current) {
  mt <- as.character(match_type)
  if (length(mt) != 1 || is.na(mt) || !(mt %in% names(.NEUTRALIZED_OPS)))
    return(FALSE)
  if (length(old_value) == 1 && is.na(old_value)) return(TRUE)
  neutral <- .NEUTRALIZED_OPS[[mt]]
  if (is.na(stamped)) return(all(NORMALIZE_OPS %in% neutral))
  a <- .parse_norm_spec(stamped)
  b <- .parse_norm_spec(current)
  for (col in union(names(a), names(b))) {
    x <- if (is.null(a[[col]])) character(0) else a[[col]]
    y <- if (is.null(b[[col]])) character(0) else b[[col]]
    changed <- union(setdiff(x, y), setdiff(y, x))
    if (!all(changed %in% neutral)) return(FALSE)
  }
  TRUE
}

# The sentence that says why a stamp difference is harmless.
.neutral_phrase <- function(match_type, old_value) {
  if (length(old_value) == 1 && is.na(old_value)) {
    return(paste0("This cannot change what the rule matches: it looks for ",
                  "missing cells, and normalizing never adds or removes one."))
  }
  sprintf(paste0("This cannot change what the rule matches: it compares ",
                 "values %s, which already ignores the difference."),
          .match_type_phrase(match_type))
}

#' Recode rules written under a different normalization than the current one.
#'
#' Normalizations run before the recodes, so a recode rule is written against
#' normalized values. Each rule carries the normalization in force when it was
#' made (`norm_spec`, stamped by the rules proxy). This compares that stamp
#' with the normalization in force now:
#'
#'   norm_spec_drift     - the rule carries a stamp and it differs from now.
#'   norm_spec_unstamped - the rule carries no stamp (NA, e.g. from a file
#'                         made before stamps existed) and a normalization
#'                         does apply to its columns now, so what it was
#'                         written against is unknown.
#'
#' Silent for an unstamped rule no normalization touches: there is nothing for
#' it to have shifted under. Decidable from the two rule tables alone;
#' `data_cols` only resolves sibling rules.
#'
#' @param rules Recode rule tibble (a missing `norm_spec` column counts as NA).
#' @param normalizations Normalization rule tibble (may be NULL / empty).
#' @param data_cols Column names of the loaded data, or NULL.
#' @return Tibble in the `empty_rule_warnings()` schema.
.warn_norm_spec_drift <- function(rules, normalizations, data_cols = NULL) {
  if (is.null(rules) || !is.data.frame(rules) || nrow(rules) == 0) {
    return(empty_rule_warnings())
  }
  current <- recode_norm_specs(rules, normalizations, data_cols)
  stored  <- if ("norm_spec" %in% names(rules)) as.character(rules$norm_spec)
             else rep(NA_character_, nrow(rules))

  rule_txt <- function(i) {
    old <- rules$old_value[i]
    sprintf("The rule that changes %s in `%s`",
            if (is.na(old)) "a blank (missing) cell" else sprintf("\"%s\"", old),
            rules$variable[i])
  }

  mts <- if ("match_type" %in% names(rules)) as.character(rules$match_type)
         else rep(NA_character_, nrow(rules))

  rows <- list()
  for (i in seq_len(nrow(rules))) {
    cur <- current[i]
    st  <- stored[i]
    if (is.na(st)) {
      if (!nzchar(cur)) next
      neutral <- .stamp_change_is_neutral(mts[i], rules$old_value[i], st, cur)
      rows[[length(rows) + 1L]] <- tibble::tibble(
        kind     = "norm_spec_unstamped",
        id       = as.character(rules$rule_id[i]),
        target   = as.character(rules$variable[i]),
        message  = if (neutral) sprintf(paste0(
          "%s does not record which normalization it was written under ",
          "(it may come from an older file), and its column is now normalized ",
          "(%s). %s"),
          rule_txt(i), .norm_spec_phrase(cur), .neutral_phrase(mts[i],
                                                               rules$old_value[i]))
        else sprintf(paste0(
          "%s does not record which normalization it was written under (it ",
          "may come from an older file), and its column is now normalized ",
          "(%s). Normalizations run before recodes, so check that the value ",
          "it looks for still appears on the Browse values tab."),
          rule_txt(i), .norm_spec_phrase(cur)),
        n_before = NA_integer_,
        n_after  = NA_integer_,
        severity = if (neutral) "note" else "warning")
      next
    }
    if (identical(st, cur)) next
    neutral <- .stamp_change_is_neutral(mts[i], rules$old_value[i], st, cur)
    rows[[length(rows) + 1L]] <- tibble::tibble(
      kind     = "norm_spec_drift",
      id       = as.character(rules$rule_id[i]),
      target   = as.character(rules$variable[i]),
      message  = if (neutral) sprintf(
        "%s was written when its column had %s, but it now has %s. %s",
        rule_txt(i), .norm_spec_phrase(st), .norm_spec_phrase(cur),
        .neutral_phrase(mts[i], rules$old_value[i]))
      else sprintf(paste0(
        "%s was written when its column had %s, but it now has %s. ",
        "Normalizations run before recodes, so the value this rule looks for ",
        "may no longer be in the column. Check the rule against the values on ",
        "the Browse values tab, and re-create it if needed."),
        rule_txt(i), .norm_spec_phrase(st), .norm_spec_phrase(cur)),
      n_before = NA_integer_,
      n_after  = NA_integer_,
      severity = if (neutral) "note" else "warning")
  }
  if (length(rows) == 0) return(empty_rule_warnings())
  dplyr::bind_rows(rows)
}

#' Run every rule check, applying each step once.
#'
#' Normalizations are applied to `df` first, the recodes to that result, and
#' recat is checked on the frame before and after the recodes, which is the
#' order the export uses.
#'
#' @param df    The raw data.
#' @param rules Recode rule tibble.
#' @param recat Recat rule tibble (default: none).
#' @param normalizations Normalization rule tibble (default: none).
#' @return Tibble in the `empty_rule_warnings()` schema: normalization
#'   warnings, then recode warnings (no match, then stamp drift), then recat
#'   warnings.
collect_rule_warnings <- function(df, rules, recat = empty_recat_tibble(),
                                  normalizations = NULL) {
  if (is.null(rules)) rules <- empty_recodes_tibble()
  if (is.null(recat)) recat <- empty_recat_tibble()
  nres <- apply_normalizations(df, normalizations)
  df_n <- nres$df
  rec  <- apply_recodes(df_n, rules)
  cat_res <- apply_recat(rec$df, recat)
  away    <- warn_recat_terms_recoded_away(df_n, rec$df, recat)
  dplyr::bind_rows(
    empty_rule_warnings(),
    .warn_normalizations(normalizations, nres$summary),
    .warn_recodes_no_match_from_summary(df_n, rules, rec$summary),
    .warn_recodes_fully_shadowed(rules, rec$summary),
    .warn_norm_spec_drift(rules, normalizations, names(df)),
    away,
    .warn_recat_no_match(recat, cat_res$summary, exclude_ids = away$id)
  )
}
