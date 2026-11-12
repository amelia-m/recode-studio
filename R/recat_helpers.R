# =============================================================================
# Recode Studio - Recategorization ("recat") Helpers
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
# This never rewrites a source cell, unlike the old->new recodes in
# string_helpers.R, which edit values in place.
#
# Rules sharing an `out_col` are applied in ascending `priority` (NA last, then
# row order); the first matching rule wins, cells already assigned are never
# overwritten, and unmatched rows stay NA. A rule with zero include terms
# matches nothing (guards against blanket rules), and so does a rule with an
# unrecognised match type or a sibling pattern that does not compile (skipped
# in the app and the script alike; validate_recat() reports them).
#
# Public functions:
#   empty_recat_tibble()                  schema scaffold for a recat rule set
#   recat_split(x) / recat_join(x)        ";"-delimited cell <-> character vector
#   recat_rule_id(...)                    stable hash id for a rule
#   recat_rule_hits(df, rule)             logical row vector: which rows match
#   recat_rule_hits_by_col(df, rule)      per-column breakdown of a rule
#   apply_recat(df, rules)                return list(df, summary)
#   validate_recat(rules, data = NULL)    named list of issue tibbles
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
# exactly that), so it cannot lean on `%||%` from ui_helpers.R, nor on
# regex_compiles() / clean_sibling_pattern() from string_helpers.R: the small
# private copies below must stay in step with those (a test pins it).

#' A recat rule's match type, read forgivingly.
#'
#' Case and surrounding spaces are ignored (" Regex " -> "regex", "LITERAL"
#' -> "literal"); NULL, NA and blank mean "literal", the default. Anything
#' else is UNRECOGNISED and returns NA: such a rule matches nothing, in the
#' app and in the generated script, and validate_recat() reports it. (It used
#' to act as literal without a word.)
.recat_match_type <- function(rule) {
  mt <- rule$match_type
  if (is.null(mt) || length(mt) == 0 || is.na(mt[[1]])) return("literal")
  mt <- tolower(trimws(as.character(mt[[1]])))
  if (!nzchar(mt)) return("literal")
  if (mt %in% RECAT_MATCH_TYPES) mt else NA_character_
}

# Blank sibling pattern -> NA (mirror of clean_sibling_pattern()).
.recat_clean_pattern <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x <- as.character(x)
  x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_
  x
}

# Does a sibling pattern compile under base grep() (TRE), the engine that
# resolves it (mirror of regex_compiles()).
.recat_tre_compiles <- function(p) {
  vapply(as.character(p), function(x) {
    if (is.na(x) || !nzchar(x)) return(FALSE)
    isTRUE(suppressWarnings(tryCatch({ grepl(x, "probe"); TRUE },
                                     error = function(e) FALSE)))
  }, logical(1), USE.NAMES = FALSE)
}

# Does a TERM compile under stringr (ICU) with ignore_case, the engine that
# matches regex terms in the app and in the script.
.recat_icu_compiles <- function(t) {
  vapply(as.character(t), function(x) {
    if (is.na(x) || !nzchar(x)) return(FALSE)
    isTRUE(suppressWarnings(tryCatch({
      stringr::str_detect("probe", stringr::regex(x, ignore_case = TRUE))
      TRUE
    }, error = function(e) FALSE)))
  }, logical(1), USE.NAMES = FALSE)
}

# Bidi code points R's parser refuses to read raw from source, and a deparse()
# that escapes them. Private mirrors of .BIDI_CODEPOINTS / .escape_bidi() /
# .deparse_literal() in string_helpers.R, because this file must stay
# sourceable on its own; keep them in step (a test pins the two agreeing).
.RECAT_BIDI_CODEPOINTS <- c(0x202a:0x202e, 0x2066:0x2069)

.recat_escape_bidi <- function(txt) {
  if (length(txt) == 0) return(txt)
  for (cp in .RECAT_BIDI_CODEPOINTS) {
    ch <- intToUtf8(cp)
    if (any(grepl(ch, txt, fixed = TRUE))) {
      # fixed = TRUE: the replacement is literal, so this is the ONE
      # backslash that must reach the script (see .escape_bidi()).
      txt <- gsub(ch, sprintf("\\u%04x", cp), txt, fixed = TRUE)
    }
  }
  txt
}

.recat_deparse_literal <- function(x) .recat_escape_bidi(deparse(x))

# The rule's sibling pattern, blank read as NA.
.recat_pattern <- function(rule) {
  sp <- .recat_clean_pattern(rule$sibling_pattern)
  if (length(sp) == 1) sp else NA_character_
}

# Why a rule is skipped (it matches nothing in the app and gets no arm in the
# script), or NA when it is usable: an unrecognised match type, or a sibling
# pattern that does not compile.
.recat_skip_reason <- function(rule) {
  if (is.na(.recat_match_type(rule))) {
    return(sprintf("unrecognised match type \"%s\"",
                   as.character(rule$match_type[[1]])))
  }
  sp <- .recat_pattern(rule)
  if (!is.na(sp) && !.recat_tre_compiles(sp)) {
    return(sprintf("sibling pattern \"%s\" is not a valid regular expression",
                   sp))
  }
  NA_character_
}

# Resolve a rule's target columns against a vector of column names. A skipped
# rule (.recat_skip_reason()) resolves to no column at all, never an error.
.recat_cols_from_names <- function(all_cols, rule) {
  all_cols <- as.character(all_cols)
  if (!is.na(.recat_skip_reason(rule))) return(character(0))
  cols <- character(0)
  sp <- .recat_pattern(rule)
  if (!is.na(sp)) cols <- grep(sp, all_cols, value = TRUE)
  cols <- union(cols, recat_split(rule$vars))
  intersect(cols, all_cols)
}

# Resolve a rule's target columns against a data frame's names.
.recat_cols <- function(df, rule) .recat_cols_from_names(names(df), rule)

# OR across `terms`: TRUE where `values` contains ANY term.
# Empty terms -> all FALSE. Invalid regex -> all FALSE (no error), and the
# generated script emits FALSE for that term too (.recat_terms_code()). An
# unrecognised (NA) match type -> all FALSE.
.recat_any_term <- function(values, terms, match_type) {
  n <- length(values)
  if (length(terms) == 0 || is.na(match_type)) return(rep(FALSE, n))
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
#' @return tibble(column, include_hits, exclude_hits, kept): counts per
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

# --- Validation --------------------------------------------------------------

#' Validate a recat rule set.
#'
#' Reports, never changes. Every issue tibble has the same columns so a UI can
#' render them alike: `recat_id`, `out_col`, `category`, `value` (the
#' offending match type / term / pattern / column / id) and `detail` (plain
#' language). All may be empty.
#'
#'   invalid_match_type      : match_type not literal / regex (case and spaces
#'                             ignored, blank = literal); the rule matches
#'                             nothing
#'   invalid_regex           : a regex rule's include or exclude term that
#'                             does not compile (stringr / ICU); that term
#'                             matches nothing
#'   no_include_terms        : no include terms; the rule matches nothing
#'   invalid_sibling_pattern : a non-blank sibling_pattern that does not
#'                             compile (base grep); the rule is skipped
#'   duplicate_ids           : a recat_id carried by more than one row (one
#'                             row per offending rule)
#'   missing_columns         : (needs `data`) a name in `vars` that is not a
#'                             column of `data`, one row per missing name
#'   no_columns              : (needs `data`) the rule resolves to no column
#'                             of `data` at all
#'
#' @param rules Recat rule tibble (NULL = none).
#' @param data  Optional data frame the rules are meant for.
#' @return Named list of tibbles, in the order above.
validate_recat <- function(rules, data = NULL) {
  none <- tibble::tibble(recat_id = character(0), out_col = character(0),
                         category = character(0), value = character(0),
                         detail = character(0))
  out <- list(invalid_match_type = none, invalid_regex = none,
              no_include_terms = none, invalid_sibling_pattern = none,
              duplicate_ids = none, missing_columns = none, no_columns = none)
  if (is.null(rules) || !is.data.frame(rules) || nrow(rules) == 0) return(out)

  row <- function(i, value, detail) {
    tibble::tibble(recat_id = as.character(rules$recat_id[i]),
                   out_col  = as.character(rules$out_col[i]),
                   category = as.character(rules$category[i]),
                   value    = as.character(value),
                   detail   = as.character(detail))
  }
  collect <- function(lst) {
    lst <- lst[!vapply(lst, is.null, logical(1))]
    if (length(lst) == 0) none else dplyr::bind_rows(lst)
  }

  n <- nrow(rules)
  mts <- lapply(seq_len(n), function(i) .recat_match_type(rules[i, ]))

  out$invalid_match_type <- collect(lapply(seq_len(n), function(i) {
    if (!is.na(mts[[i]])) return(NULL)
    row(i, rules$match_type[i], sprintf(paste0(
      "Unrecognised match type. Use one of: %s. The rule matches nothing ",
      "until it is fixed."), paste(RECAT_MATCH_TYPES, collapse = ", ")))
  }))

  out$invalid_regex <- collect(unlist(lapply(seq_len(n), function(i) {
    if (!identical(mts[[i]], "regex")) return(NULL)
    terms <- c(recat_split(rules$include_terms[i]),
               recat_split(rules$exclude_terms[i]))
    bad <- terms[!.recat_icu_compiles(terms)]
    lapply(unique(bad), function(t) row(i, t, paste0(
      "This term is not a valid regular expression, so it matches nothing.")))
  }), recursive = FALSE))

  out$no_include_terms <- collect(lapply(seq_len(n), function(i) {
    if (length(recat_split(rules$include_terms[i])) > 0) return(NULL)
    row(i, NA_character_,
        "No include terms, so the rule matches nothing.")
  }))

  out$invalid_sibling_pattern <- collect(lapply(seq_len(n), function(i) {
    sp <- .recat_pattern(rules[i, ])
    if (is.na(sp) || .recat_tre_compiles(sp)) return(NULL)
    row(i, sp, paste0("The sibling pattern is not a valid regular ",
                      "expression, so the rule is skipped."))
  }))

  ids <- as.character(rules$recat_id)
  dup <- which(!is.na(ids) & ids %in% ids[duplicated(ids)])
  out$duplicate_ids <- collect(lapply(dup, function(i) row(i, ids[i],
    "More than one rule carries this id.")))

  if (!is.null(data) && is.data.frame(data)) {
    cols <- names(data)
    out$missing_columns <- collect(unlist(lapply(seq_len(n), function(i) {
      miss <- setdiff(recat_split(rules$vars[i]), cols)
      lapply(miss, function(m) row(i, m, "Not a column in the loaded data."))
    }), recursive = FALSE))
    out$no_columns <- collect(lapply(seq_len(n), function(i) {
      if (length(.recat_cols_from_names(cols, rules[i, ])) > 0) return(NULL)
      if (!is.na(.recat_skip_reason(rules[i, ]))) return(NULL)
      row(i, NA_character_,
          "No column in the loaded data matches this rule's variables.")
    }))
  }
  out
}

# --- Code generation ---------------------------------------------------------

# Build the str_detect() OR-clause across cols for one term set, as R code.
.recat_terms_code <- function(cols, terms, match_type) {
  if (length(terms) == 0) return(NULL)
  detect1 <- function(col, term) {
    if (match_type == "regex" && !.recat_icu_compiles(term)) {
      # Matches nothing, as in .recat_any_term(); str_detect() would throw.
      "FALSE"
    } else if (match_type == "regex") {
      sprintf("str_detect(`%s`, regex(%s, ignore_case = TRUE))",
              col, .recat_deparse_literal(term))
    } else {
      sprintf("str_detect(tolower(`%s`), fixed(%s))",
              col, .recat_deparse_literal(tolower(term)))
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
  if (!is.na(.recat_skip_reason(rule))) return("FALSE")

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
      .recat_cols_from_names(data_cols, rule)
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
      why <- .recat_skip_reason(r)
      if (!is.na(why)) {
        # Matches nothing in the app either (recat_rule_hits()): no arm.
        arms <- c(arms, sprintf("      # SKIPPED rule %s (category %s): %s",
                                as.character(r$recat_id),
                                gsub("[\r\n]", " ", paste(deparse(
                                  as.character(r$category)), collapse = "")),
                                gsub("[\r\n]", " ", why)))
        next
      }
      cols <- resolve_cols(r)
      cond <- recat_rule_condition_R(r, cols)
      cmt <- if (!is.na(r$notes) && nzchar(r$notes))
               paste0("  # ", gsub("[\r\n]", " ", r$notes)) else ""
      arms <- c(arms, sprintf("    %s ~ %s,%s",
                              cond,
                              .recat_deparse_literal(as.character(r$category)),
                              cmt))
    }
    block <- c(
      "df <- df |>",
      sprintf("  mutate(`%s` = {", oc),
      sprintf("    .cur <- if (%s %%in%% names(df)) df[[%s]] else NA_character_",
              .recat_deparse_literal(oc), .recat_deparse_literal(oc)),
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
#'
#' A blank `sibling_pattern` reads as NA (an empty pattern is no pattern).
#' `match_type` is kept as written; validate_recat() reports unrecognised
#' values.
read_recat <- function(path) {
  if (!file.exists(path)) return(empty_recat_tibble())
  df <- readr::read_csv(
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
  df$sibling_pattern <- .recat_clean_pattern(df$sibling_pattern)
  df
}

#' Write a recat rules tibble with explicit NA encoding.
write_recat <- function(rules, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(rules, path, na = "<NA>")
  invisible(path)
}
