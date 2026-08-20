# =============================================================================
# Recode Studio — Recategorization ("recat") Helpers
# =============================================================================
#
# Pure R helpers (no Shiny). Safe to source from scripts as well as the app.
#
# A *recat rule* DERIVES A NEW OUTPUT COLUMN by combining, across one or more
# source variables, a set of boolean term conditions:
#
#   if ANY of `vars` contains ANY `include_terms`        (OR)
#      AND NONE of `vars` contains ANY `exclude_terms`   (AND-NOT, rule-wide)
#   then the row's `out_col` becomes `category`.
#
# This never rewrites a source cell — unlike the old->new recodes in
# string_helpers.R, which edit values in place.
#
# Rules sharing an `out_col` are applied in ascending `priority` (NA last, then
# row order); the first matching rule wins, cells already assigned are never
# overwritten, and unmatched rows stay NA. A rule with zero include terms
# matches nothing (guards against blanket rules).
#
# Public functions:
#   empty_recat_tibble()                  schema scaffold for a recat rule set
#   recat_split(x) / recat_join(x)        ";"-delimited cell <-> character vector
#   recat_rule_id(...)                    stable hash id for a rule
#   recat_rule_hits(df, rule)             logical row vector: which rows match
#   recat_rule_hits_by_col(df, rule)      per-column breakdown of a rule
#   apply_recat(df, rules)                return list(df, summary)
#   recat_rule_condition_R(rule, cols)    emit the case_when LHS condition string
#   generate_recat_R(rules, dataset_id)   emit copyable R script content
#   read_recat(path) / write_recat(...)   CSV round-trip
# =============================================================================

# --- Schema ------------------------------------------------------------------

# Term-matching strategies for recat rules, single-sourced for the UI.
RECAT_MATCH_TYPES <- c("literal", "regex")

# Multi-value fields in a recat rule are stored as a single delimited string in
# the CSV; this is the in-cell delimiter.
RECAT_LIST_SEP <- ";"

#' Empty tibble matching the recat rules CSV schema.
empty_recat_tibble <- function() {
  tibble::tibble(
    recat_id        = character(0),
    out_col         = character(0),
    category        = character(0),
    vars            = character(0),   # ";"-joined source variable names
    sibling_pattern = character(0),   # optional regex expanding to vars
    include_terms   = character(0),   # ";"-joined, combined with OR
    exclude_terms   = character(0),   # ";"-joined, combined with AND-NOT
    match_type      = character(0),   # literal | regex
    priority        = integer(0),     # lower = evaluated first
    notes           = character(0),
    author          = character(0),
    created_at      = character(0),
    source_dataset  = character(0)
  )
}

#' Split a delimited term/var string into a trimmed, non-empty character vector.
#' NA or "" -> character(0).
recat_split <- function(x, sep = RECAT_LIST_SEP) {
  if (length(x) == 0 || is.na(x) || !nzchar(x)) return(character(0))
  parts <- stringr::str_split(x, stringr::fixed(sep))[[1]]
  parts <- stringr::str_trim(parts)
  parts[nzchar(parts)]
}

#' Join a character vector back into a single delimited cell.
recat_join <- function(x, sep = RECAT_LIST_SEP) {
  if (length(x) == 0) return(NA_character_)
  paste(x[nzchar(x)], collapse = sep)
}

#' Stable 12-char hex id for a recat rule.
recat_rule_id <- function(out_col, category, vars, include_terms,
                          exclude_terms, match_type) {
  raw <- paste(out_col, category, vars, include_terms, exclude_terms,
               match_type, sep = "||")
  substr(rlang::hash(raw), 1, 12)
}

# --- Matching ----------------------------------------------------------------

# This file is Shiny-free and must also be sourceable on its own (the tests do
# exactly that), so it cannot lean on `%||%` from ui_helpers.R.
.recat_match_type <- function(rule) {
  mt <- rule$match_type
  if (is.null(mt) || length(mt) == 0 || is.na(mt) || !nzchar(mt)) "literal"
  else mt[[1]]
}

# Resolve a rule's target columns against a data frame's names.
.recat_cols <- function(df, rule) {
  cols <- character(0)
  if (!is.null(rule$sibling_pattern) && !is.na(rule$sibling_pattern) &&
      nzchar(rule$sibling_pattern)) {
    cols <- grep(rule$sibling_pattern, names(df), value = TRUE)
  }
  cols <- union(cols, recat_split(rule$vars))
  intersect(cols, names(df))
}

# OR across `terms`: TRUE where `values` contains ANY term.
# Empty terms -> all FALSE. Invalid regex -> all FALSE (no error).
.recat_any_term <- function(values, terms, match_type) {
  n <- length(values)
  if (length(terms) == 0) return(rep(FALSE, n))
  hit <- rep(FALSE, n)
  for (t in terms) {
    this <- if (match_type == "regex") {
      suppressWarnings(tryCatch(
        stringr::str_detect(values, stringr::regex(t, ignore_case = TRUE)),
        error = function(e) rep(NA, n)))
    } else {
      stringr::str_detect(tolower(values), stringr::fixed(tolower(t)))
    }
    this[is.na(this)] <- FALSE
    hit <- hit | this
  }
  hit
}

#' Per-column breakdown of a recat rule's matches.
#'
#' recat_rule_hits() answers "does this ROW match", by ORing every selected
#' column together. That is the right answer for applying the rule and the
#' wrong one for reviewing it: a rule over six sibling columns tells you 40
#' rows matched but not WHICH sibling carried the term, so there is no way to
#' see that five of the six contribute nothing, or that the hits are all coming
#' from a column you did not intend to include.
#'
#' @return tibble(column, include_hits, exclude_hits, kept) — counts per
#'   column, where `kept` is rows this column includes that the rule's exclude
#'   terms do not veto ANYWHERE. Zero rows when the rule selects no columns.
recat_rule_hits_by_col <- function(df, rule) {
  cols <- .recat_cols(df, rule)
  empty <- tibble::tibble(column = character(0), include_hits = integer(0),
                          exclude_hits = integer(0), kept = integer(0))
  if (length(cols) == 0) return(empty)
  match_type    <- .recat_match_type(rule)
  include_terms <- recat_split(rule$include_terms)
  exclude_terms <- recat_split(rule$exclude_terms)
  if (length(include_terms) == 0) return(empty)

  n <- nrow(df)
  # Exclusion is rule-wide, not per column: a term found in ANY selected column
  # vetoes the row. Computed once so each column's `kept` reflects the rule as
  # it will actually run.
  exc_any <- rep(FALSE, n)
  for (col in cols) {
    exc_any <- exc_any |
      .recat_any_term(as.character(df[[col]]), exclude_terms, match_type)
  }
  rows <- lapply(cols, function(col) {
    v <- as.character(df[[col]])
    inc <- .recat_any_term(v, include_terms, match_type)
    exc <- .recat_any_term(v, exclude_terms, match_type)
    tibble::tibble(column = col, include_hits = sum(inc),
                   exclude_hits = sum(exc), kept = sum(inc & !exc_any))
  })
  dplyr::bind_rows(rows)
}

#' Logical row vector: which rows of `df` a recat rule matches.
#'
#' A row matches iff, across the rule's target columns:
#'   ANY column contains ANY include term (OR)  AND
#'   NO  column contains ANY exclude term (AND-NOT).
#' A rule with no include terms matches nothing.
recat_rule_hits <- function(df, rule) {
  cols <- .recat_cols(df, rule)
  n <- nrow(df)
  if (length(cols) == 0) return(rep(FALSE, n))

  match_type    <- .recat_match_type(rule)
  include_terms <- recat_split(rule$include_terms)
  exclude_terms <- recat_split(rule$exclude_terms)
  if (length(include_terms) == 0) return(rep(FALSE, n))

  inc_any <- rep(FALSE, n)
  exc_any <- rep(FALSE, n)
  for (col in cols) {
    v <- as.character(df[[col]])
    inc_any <- inc_any | .recat_any_term(v, include_terms, match_type)
    exc_any <- exc_any | .recat_any_term(v, exclude_terms, match_type)
  }
  inc_any & !exc_any
}

# --- Apply -------------------------------------------------------------------

#' Apply a recat rule set to a data frame.
#'
#' Rules are grouped by out_col and applied in ascending `priority` (NA last);
#' within an out_col the first matching rule wins (later rules don't overwrite
#' an already-assigned cell). Unmatched rows keep NA.
#'
#' @return list(df = df with new/updated out_col(s),
#'              summary = tibble(recat_id, out_col, category, rows_matched))
apply_recat <- function(df, rules) {
  summary <- tibble::tibble(recat_id = character(0), out_col = character(0),
                            category = character(0), rows_matched = integer(0))
  if (nrow(rules) == 0) return(list(df = df, summary = summary))

  # Deterministic order: by out_col, then priority (NA last), then row order.
  ord <- order(rules$out_col,
               ifelse(is.na(rules$priority), Inf, rules$priority),
               seq_len(nrow(rules)))
  rules <- rules[ord, , drop = FALSE]

  for (oc in unique(rules$out_col)) {
    if (!(oc %in% names(df))) df[[oc]] <- NA_character_
    already <- !is.na(df[[oc]])
    sub <- rules[rules$out_col == oc, , drop = FALSE]
    for (i in seq_len(nrow(sub))) {
      r <- sub[i, ]
      hit <- recat_rule_hits(df, r) & !already
      matched <- sum(hit)
      if (matched > 0) {
        df[[oc]][hit] <- r$category
        already <- already | hit
      }
      summary <- dplyr::bind_rows(summary, tibble::tibble(
        recat_id = r$recat_id, out_col = oc, category = r$category,
        rows_matched = matched))
    }
  }
  list(df = df, summary = summary)
}

# --- Code generation ---------------------------------------------------------

# Build the str_detect() OR-clause across cols for one term set, as R code.
.recat_terms_code <- function(cols, terms, match_type) {
  if (length(terms) == 0) return(NULL)
  detect1 <- function(col, term) {
    if (match_type == "regex") {
      sprintf("str_detect(`%s`, regex(%s, ignore_case = TRUE))",
              col, deparse(term))
    } else {
      sprintf("str_detect(tolower(`%s`), fixed(%s))",
              col, deparse(tolower(term)))
    }
  }
  clauses <- character(0)
  for (col in cols) for (t in terms) clauses <- c(clauses, detect1(col, t))
  # NA-safe: coalesce each detect to FALSE.
  clauses <- sprintf("tidyr::replace_na(%s, FALSE)", clauses)
  paste(clauses, collapse = " |\n      ")
}

#' Emit the case_when() LHS condition (R code string) for one recat rule,
#' resolved against a set of column names (`cols`).
recat_rule_condition_R <- function(rule, cols) {
  include_terms <- recat_split(rule$include_terms)
  exclude_terms <- recat_split(rule$exclude_terms)
  match_type    <- .recat_match_type(rule)
  if (length(cols) == 0 || length(include_terms) == 0) return("FALSE")

  inc <- .recat_terms_code(cols, include_terms, match_type)
  cond <- sprintf("(\n      %s\n    )", inc)
  if (length(exclude_terms) > 0) {
    exc <- .recat_terms_code(cols, exclude_terms, match_type)
    cond <- sprintf("%s &\n    !(\n      %s\n    )", cond, exc)
  }
  cond
}

#' Generate a copyable R script that applies a recat rule set to `df`.
#'
#' Column resolution is done at GENERATION time against `data_cols` (defaults to
#' the rule's `vars` string as literal names when data isn't supplied), so pass
#' the loaded data's column names for sibling patterns to expand.
#'
#' Uses `case_when()`, never `case_match()` (deprecated in dplyr 1.2.0).
generate_recat_R <- function(rules, dataset_id, data_cols = NULL) {
  header <- c(
    "# =============================================================================",
    sprintf("# Auto-generated RECATEGORIZATION script -- dataset: %s", dataset_id),
    sprintf("# Generated:  %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    sprintf("# Rule count: %d", nrow(rules)),
    "# Derives new category columns from cross-variable term logic.",
    "# Source values are never modified.",
    "# DO NOT EDIT BY HAND -- regenerate from Recode Studio.",
    "# =============================================================================",
    "",
    "library(dplyr)",
    "library(stringr)",
    "library(tidyr)",
    "",
    "# Operates on `df`, your loaded data frame.",
    "# Run this AFTER the generated recode script, if you have one.",
    ""
  )
  if (nrow(rules) == 0) {
    return(paste(c(header, "# No recategorization rules defined.", ""),
                 collapse = "\n"))
  }

  resolve_cols <- function(rule) {
    if (!is.null(data_cols)) {
      cols <- character(0)
      if (!is.na(rule$sibling_pattern) && nzchar(rule$sibling_pattern))
        cols <- grep(rule$sibling_pattern, data_cols, value = TRUE)
      cols <- union(cols, recat_split(rule$vars))
      intersect(cols, data_cols)
    } else {
      recat_split(rule$vars)
    }
  }

  ord <- order(rules$out_col,
               ifelse(is.na(rules$priority), Inf, rules$priority),
               seq_len(nrow(rules)))
  rules <- rules[ord, , drop = FALSE]

  blocks <- character(0)
  for (oc in unique(rules$out_col)) {
    sub <- rules[rules$out_col == oc, , drop = FALSE]
    arms <- character(0)
    for (i in seq_len(nrow(sub))) {
      r <- sub[i, ]
      cols <- resolve_cols(r)
      cond <- recat_rule_condition_R(r, cols)
      cmt <- if (!is.na(r$notes) && nzchar(r$notes))
               paste0("  # ", gsub("[\r\n]", " ", r$notes)) else ""
      arms <- c(arms, sprintf("    %s ~ %s,%s",
                              cond, deparse(as.character(r$category)), cmt))
    }
    block <- c(
      "df <- df |>",
      sprintf("  mutate(`%s` = {", oc),
      sprintf("    .cur <- if (%s %%in%% names(df)) df[[%s]] else NA_character_",
              deparse(oc), deparse(oc)),
      "    case_when(",
      "      !is.na(.cur) ~ .cur,",
      arms,
      "      .default = .cur",
      "    )",
      "  })",
      ""
    )
    blocks <- c(blocks, block)
  }
  paste(c(header, blocks), collapse = "\n")
}

# --- CSV round-trip ----------------------------------------------------------

#' Read a recat rules CSV (empty tibble if the file doesn't exist).
read_recat <- function(path) {
  if (!file.exists(path)) return(empty_recat_tibble())
  readr::read_csv(
    path, na = "<NA>", show_col_types = FALSE,
    col_types = readr::cols(
      recat_id        = readr::col_character(),
      out_col         = readr::col_character(),
      category        = readr::col_character(),
      vars            = readr::col_character(),
      sibling_pattern = readr::col_character(),
      include_terms   = readr::col_character(),
      exclude_terms   = readr::col_character(),
      match_type      = readr::col_character(),
      priority        = readr::col_integer(),
      notes           = readr::col_character(),
      author          = readr::col_character(),
      created_at      = readr::col_character(),
      source_dataset  = readr::col_character()
    )
  )
}

#' Write a recat rules tibble with explicit NA encoding.
write_recat <- function(rules, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(rules, path, na = "<NA>")
  invisible(path)
}
