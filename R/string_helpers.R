# =============================================================================
# Recode Studio — String / Recode Helpers
# =============================================================================
#
# Pure R helpers (no Shiny). Safe to source from scripts as well as the app.
#
# Public functions:
#   empty_recodes_tibble()              schema scaffold for a rule set
#   normalize_value(x, match_type)      value normalization per match strategy
#   cluster_strings(vals, freqs, ...)   string-similarity clustering + rare flag
#   read_recodes(path)                  read recode CSV with NA round-trip
#   write_recodes(rules, path)          write recode CSV with NA round-trip
#   recode_rule_id(variable, match_type, old_value)  stable hash id
#   validate_recodes(rules, data = NULL)  duplicates, chains, blanks, stale
#   apply_recodes(df, rules)            return list(df, summary)
#   generate_recode_R(rules, dataset_id, source_csv_path = NULL)
#                                       emit copyable R script content
# =============================================================================

# --- Schema ------------------------------------------------------------------

#' Empty tibble matching the recodes_master.csv schema.
empty_recodes_tibble <- function() {
  tibble::tibble(
    rule_id           = character(0),
    variable          = character(0),
    apply_to_siblings = logical(0),
    sibling_pattern   = character(0),
    match_type        = character(0),
    old_value         = character(0),
    new_value         = character(0),
    action            = character(0),
    notes             = character(0),
    author            = character(0),
    created_at        = character(0),
    updated_at        = character(0),
    source_dataset    = character(0)
  )
}


# --- Normalization -----------------------------------------------------------

#' Normalize a character vector for comparison.
#'
#' Matches the strategies declared in the recode CSV's `match_type` column:
#'   - exact        : returned as-is
#'   - exact_ci     : tolower()
#'   - trimmed_ci   : str_squish(tolower())  (DEFAULT)
#'   - regex        : returned as-is (caller must handle matching itself)
normalize_value <- function(x,
                            match_type = c("trimmed_ci", "exact_ci",
                                           "exact", "regex")) {
  match_type <- match.arg(match_type)
  switch(match_type,
    exact      = x,
    exact_ci   = tolower(x),
    trimmed_ci = stringr::str_squish(tolower(x)),
    regex      = x
  )
}


# --- Similarity clustering ---------------------------------------------------

#' Cluster a character vector by string similarity.
#'
#' @param values      Character vector of unique values.
#' @param frequencies Integer vector of counts. Defaults to 1 per value.
#' @param threshold   Similarity cutoff (0..1, higher = more similar).
#' @param algorithm   "jw" (Jaro-Winkler, default), "osa" (Optimal String
#'                   Alignment / Damerau-Levenshtein normalized), or
#'                   "soundex" (phonetic encoding bucketing).
#' @return Tibble with value, n, cluster_id, is_rare. `is_rare` is TRUE for
#'         a cluster member representing <5% of the cluster's frequency mass,
#'         when the cluster has more than one member.
cluster_strings <- function(values, frequencies = NULL, threshold = 0.92,
                            algorithm = c("jw", "osa", "soundex")) {
  algorithm <- match.arg(algorithm)
  if (is.null(frequencies)) frequencies <- rep(1L, length(values))
  stopifnot(length(values) == length(frequencies))

  if (length(values) == 0) {
    return(tibble::tibble(
      value = character(0), n = integer(0),
      cluster_id = integer(0), is_rare = logical(0)
    ))
  }

  v <- as.character(values)

  if (algorithm == "soundex") {
    codes <- stringdist::phonetic(v, method = "soundex")
    cluster_id <- as.integer(factor(codes))
  } else {
    if (algorithm == "jw") {
      d <- stringdist::stringdistmatrix(v, v, method = "jw")
      sim <- 1 - d
    } else { # osa
      d <- stringdist::stringdistmatrix(v, v, method = "osa")
      lens <- outer(nchar(v), nchar(v), pmax)
      lens[lens == 0] <- 1L
      sim <- 1 - d / lens
    }
    adj <- sim >= threshold
    diag(adj) <- FALSE
    g <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected", diag = FALSE)
    cluster_id <- igraph::components(g)$membership
  }

  out <- tibble::tibble(
    value      = v,
    n          = as.integer(frequencies),
    cluster_id = as.integer(cluster_id)
  )

  out <- out |>
    dplyr::group_by(cluster_id) |>
    dplyr::mutate(
      .cluster_size = dplyr::n(),
      .cluster_mass = sum(n),
      is_rare       = .cluster_size > 1 & (n / .cluster_mass) < 0.05
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-.cluster_size, -.cluster_mass)

  out
}


# --- CSV read / write --------------------------------------------------------

#' Read a recodes CSV, preserving the explicit NA encoding (`<NA>` in CSV).
read_recodes <- function(path) {
  if (!file.exists(path)) return(empty_recodes_tibble())
  readr::read_csv(
    path,
    na = "<NA>",
    show_col_types = FALSE,
    col_types = readr::cols(
      rule_id           = readr::col_character(),
      variable          = readr::col_character(),
      apply_to_siblings = readr::col_logical(),
      sibling_pattern   = readr::col_character(),
      match_type        = readr::col_character(),
      old_value         = readr::col_character(),
      new_value         = readr::col_character(),
      action            = readr::col_character(),
      notes             = readr::col_character(),
      author            = readr::col_character(),
      created_at        = readr::col_character(),
      updated_at        = readr::col_character(),
      source_dataset    = readr::col_character()
    )
  )
}

#' Write a recodes tibble with explicit NA encoding.
write_recodes <- function(rules, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(rules, path, na = "<NA>")
  invisible(path)
}


# --- Rule id ------------------------------------------------------------------

#' Stable 12-char hex id for a rule, derived from (variable, match_type, old_value).
recode_rule_id <- function(variable, match_type, old_value) {
  raw <- paste(variable, match_type,
               ifelse(is.na(old_value), "<NA>", old_value),
               sep = "||")
  vapply(raw, function(x) substr(rlang::hash(x), 1, 12),
         character(1), USE.NAMES = FALSE)
}


# --- Validation ---------------------------------------------------------------

#' Validate a rule set.
#'
#' Returns a named list of issue tibbles (all may be empty):
#'   duplicate_keys     : (variable, match_type, old_value) appearing >1x
#'   blank_new_value    : action == "recode" but new_value is NA or ""
#'   rule_chains        : per variable, values that appear as both old and new
#'   stale              : rule whose old_value is no longer in any target column
#'                       (computed only if `data` is provided)
validate_recodes <- function(rules, data = NULL) {
  issues <- list()

  issues$duplicate_keys <- rules |>
    dplyr::count(variable, match_type, old_value, name = "n") |>
    dplyr::filter(n > 1)

  issues$blank_new_value <- rules |>
    dplyr::filter(action == "recode",
                  is.na(new_value) | new_value == "")

  chain_rows <- rules |>
    dplyr::filter(action == "recode", !is.na(new_value))
  if (nrow(chain_rows) > 0) {
    issues$rule_chains <- chain_rows |>
      dplyr::group_by(variable) |>
      dplyr::summarise(
        ambiguous = list(intersect(old_value, new_value)),
        .groups = "drop"
      ) |>
      tidyr::unnest(ambiguous) |>
      dplyr::filter(!is.na(ambiguous))
  } else {
    issues$rule_chains <- tibble::tibble(variable = character(0),
                                         ambiguous = character(0))
  }

  if (!is.null(data) && nrow(rules) > 0) {
    stale_flags <- vapply(seq_len(nrow(rules)), function(i) {
      r <- rules[i, ]
      cols <- if (!is.na(r$apply_to_siblings) && r$apply_to_siblings && !is.na(r$sibling_pattern)) {
        grep(r$sibling_pattern, names(data), value = TRUE)
      } else {
        r$variable
      }
      cols <- intersect(cols, names(data))
      if (length(cols) == 0) return(TRUE)
      target <- normalize_value(r$old_value, r$match_type)
      !any(vapply(cols, function(c) {
        target %in% normalize_value(data[[c]], r$match_type)
      }, logical(1)))
    }, logical(1))
    issues$stale <- rules[stale_flags, c("rule_id", "variable", "old_value")]
  } else {
    issues$stale <- tibble::tibble(rule_id = character(0),
                                   variable = character(0),
                                   old_value = character(0))
  }

  issues
}


# --- Apply --------------------------------------------------------------------

#' Apply a rule set to a data frame.
#'
#' @return list(df = modified df, summary = tibble(rule_id, cells_changed))
apply_recodes <- function(df, rules) {
  if (nrow(rules) == 0) {
    return(list(
      df = df,
      summary = tibble::tibble(rule_id = character(0),
                               cells_changed = integer(0))
    ))
  }

  summary <- tibble::tibble(rule_id = character(0),
                            cells_changed = integer(0))

  for (i in seq_len(nrow(rules))) {
    r <- rules[i, ]

    cols <- if (!is.na(r$apply_to_siblings) && r$apply_to_siblings && !is.na(r$sibling_pattern)) {
      grep(r$sibling_pattern, names(df), value = TRUE)
    } else {
      r$variable
    }
    cols <- intersect(cols, names(df))

    if (length(cols) == 0) {
      summary <- dplyr::bind_rows(summary,
        tibble::tibble(rule_id = r$rule_id, cells_changed = 0L))
      next
    }

    target <- normalize_value(r$old_value, r$match_type)
    new_val <- if (r$action == "delete") NA_character_ else r$new_value

    cells_changed <- 0L
    for (col in cols) {
      orig <- df[[col]]
      norm <- normalize_value(orig, r$match_type)
      hit <- !is.na(norm) & norm == target
      if (any(hit)) {
        df[[col]][hit] <- new_val
        cells_changed <- cells_changed + sum(hit)
      }
    }

    summary <- dplyr::bind_rows(summary,
      tibble::tibble(rule_id = r$rule_id, cells_changed = cells_changed))
  }

  list(df = df, summary = summary)
}


# --- Code generation ----------------------------------------------------------

#' Generate copyable R script for a rule set.
#'
#' The generated script expects `df` to be your data frame already in scope.
#' It uses dplyr::case_when() with `.default` to leave unmatched values intact.
generate_recode_R <- function(rules, dataset_id, source_csv_path = NULL) {
  header <- c(
    "# =============================================================================",
    sprintf("# Auto-generated recode script -- dataset: %s", dataset_id),
    sprintf("# Generated:  %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    sprintf("# Rule count: %d", nrow(rules)),
    if (!is.null(source_csv_path))
      sprintf("# Source CSV: %s", source_csv_path)
    else NULL,
    "# DO NOT EDIT BY HAND -- regenerate from Recode Studio after editing the CSV.",
    "# =============================================================================",
    "",
    "library(dplyr)",
    "library(stringr)",
    "",
    "# Operates on `df`, your data frame.",
    ""
  )

  if (nrow(rules) == 0) {
    return(paste(c(header, "# No recode rules defined.", ""),
                 collapse = "\n"))
  }

  # NOTE: isTRUE() is NOT vectorised, so we use a length-safe expression.
  rules <- rules |>
    dplyr::mutate(
      effective_pattern = ifelse(
        !is.na(apply_to_siblings) & apply_to_siblings & !is.na(sibling_pattern),
        sibling_pattern,
        paste0("^", variable, "$")
      )
    )

  groups <- rules |>
    dplyr::group_by(effective_pattern, match_type) |>
    dplyr::group_split()

  blocks <- character(0)
  for (g in groups) {
    pattern    <- g$effective_pattern[1]
    match_type <- g$match_type[1]

    lhs_expr <- switch(match_type,
      exact      = "{{col}}",
      exact_ci   = "tolower({{col}})",
      trimmed_ci = "str_squish(tolower({{col}}))",
      regex      = "{{col}}"
    )

    make_arms <- function(col_token) {
      lhs_expr_f <- gsub("\\{\\{col\\}\\}", col_token, lhs_expr, fixed = FALSE)
      arms <- character(0)
      for (i in seq_len(nrow(g))) {
        r <- g[i, ]
        rhs <- if (r$action == "delete") "NA_character_"
               else deparse(as.character(r$new_value))
        lhs <- deparse(as.character(r$old_value))
        cmt <- if (!is.na(r$notes) && nzchar(r$notes))
                 paste0("  # ", gsub("[\r\n]", " ", r$notes)) else ""
        arms <- c(arms,
          sprintf("    %s == %s ~ %s,%s", lhs_expr_f, lhs, rhs, cmt))
      }
      arms
    }

    if (grepl("^\\^([a-zA-Z_][a-zA-Z0-9_]*)\\$$", pattern)) {
      var <- sub("^\\^([a-zA-Z_][a-zA-Z0-9_]*)\\$$", "\\1", pattern)
      block <- c(
        "df <- df |>",
        sprintf("  mutate(%s = case_when(", var),
        make_arms(var),
        sprintf("    .default = %s", var),
        "  ))",
        ""
      )
    } else {
      block <- c(
        "df <- df |>",
        sprintf("  mutate(across(matches(%s), function(.x) {",
                deparse(as.character(pattern))),
        "    case_when(",
        make_arms(".x"),
        "      .default = .x",
        "    )",
        "  }))",
        ""
      )
    }
    blocks <- c(blocks, block)
  }

  paste(c(header, blocks), collapse = "\n")
}
