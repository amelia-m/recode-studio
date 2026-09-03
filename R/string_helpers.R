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
#   apply_recodes(df, rules)            single pass, first match wins;
#                                       returns list(df, summary)
#   generate_recode_R(rules, dataset_id, source_csv_path = NULL)
#                                       emit copyable R script content
# =============================================================================

# --- Schema ------------------------------------------------------------------

# Allowed enum values, single-sourced for the editor + the validator.
RECODE_MATCH_TYPES <- c("exact", "exact_ci", "trimmed_ci", "regex")
RECODE_ACTIONS     <- c("recode", "delete")

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
#'   - regex        : returned as-is (matching done by grepl, not equality)
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

#' Logical hit vector for a single rule against a raw column.
#'
#' For `regex`, `old_value` is treated as an (unanchored) regular expression
#' matched against raw values with grepl; an invalid pattern yields all-FALSE
#' rather than erroring. For the other match types, values are normalized and
#' compared for equality. A match REPLACES THE WHOLE CELL (regex does not do
#' partial substitution / backreferences).
#'
#' NA handling: an NA `old_value` — which is how the CSV's `<NA>` sentinel
#' round-trips — targets the MISSING cells of the column. It used to produce an
#' all-NA hit vector, which made the caller's `if (any(hit))` throw "missing
#' value where TRUE/FALSE needed" and abort Apply & Export. The returned vector
#' is now always a plain logical with no NAs, whatever the inputs. An
#' NA/unrecognised `match_type` matches nothing (validate_recodes() reports it
#' as `invalid_enum`) rather than erroring.
.rule_hits <- function(values, old_value, match_type) {
  n <- length(values)
  if (n == 0) return(logical(0))

  # A rule with no usable match_type matches nothing (never errors).
  if (length(match_type) != 1 || is.na(match_type) ||
      !(match_type %in% RECODE_MATCH_TYPES)) {
    return(rep(FALSE, n))
  }

  # Explicit NA target: the rule is about the column's missing cells.
  if (length(old_value) != 1 || is.na(old_value)) return(is.na(values))

  hits <- if (match_type == "regex") {
    suppressWarnings(tryCatch(
      !is.na(values) & grepl(old_value, values),
      error = function(e) rep(FALSE, n)
    ))
  } else {
    norm   <- normalize_value(values, match_type)
    target <- normalize_value(old_value, match_type)
    !is.na(norm) & norm == target
  }
  hits[is.na(hits)] <- FALSE
  hits
}

#' Resolve the target columns of one recode rule against a data frame.
#'
#' Sibling rules expand their `sibling_pattern` against the frame's names;
#' everything else targets `variable` alone. Always returns names present in
#' `df` (possibly none).
.recode_cols <- function(df, rule) .recode_cols_from_names(names(df), rule)

# Same resolution against a bare vector of column NAMES, for callers that hold
# the names but not the frame.
.recode_cols_from_names <- function(all_cols, rule) {
  all_cols <- as.character(all_cols)
  cols <- if (length(rule$apply_to_siblings) == 1 &&
              !is.na(rule$apply_to_siblings) && rule$apply_to_siblings &&
              !is.na(rule$sibling_pattern)) {
    grep(rule$sibling_pattern, all_cols, value = TRUE)
  } else {
    rule$variable
  }
  intersect(cols, all_cols)
}

# Cell-level "did this actually change?" test, NA-aware. One side NA and the
# other not counts as a change; NA -> NA and x -> x do not.
.cells_differ <- function(before, after) {
  if (length(before) == 0) return(logical(0))
  after  <- rep(as.character(after), length.out = length(before))
  before <- as.character(before)
  xor(is.na(before), is.na(after)) |
    (!is.na(before) & !is.na(after) & before != after)
}


# --- Similarity clustering ---------------------------------------------------

# Supported clustering algorithms, single-sourced for the UI.
CLUSTER_ALGORITHMS <- c(
  "Jaro-Winkler"                  = "jw",
  "Optimal String Alignment"      = "osa",
  "Levenshtein"                   = "lv",
  "Longest common substring"      = "lcs",
  "Cosine (q-gram)"               = "cosine",
  "Jaccard (q-gram)"              = "jaccard",
  "Key collision (fingerprint)"   = "fingerprint",
  "N-gram fingerprint"            = "ngram_fingerprint",
  "Soundex (phonetic)"            = "soundex",
  "Metaphone (phonetic)"          = "metaphone"
)

# Normalization steps applied to a working copy of the values BEFORE distances
# are computed (the original strings are still what gets displayed/recoded).
CLUSTER_NORMALIZERS <- c(
  "Lowercase"               = "lower",
  "Strip punctuation"       = "punct",
  "Collapse whitespace"     = "squish",
  "Dedupe adjacent words"   = "dedupe_tokens",
  "Ignore word order"       = "sort_tokens"
)

#' Apply selected normalization steps to a character vector (for clustering).
normalize_for_cluster <- function(x, methods = character(0)) {
  out <- x
  if ("lower"  %in% methods) out <- tolower(out)
  if ("punct"  %in% methods) out <- gsub("[^[:alnum:][:space:]]+", " ", out)
  if ("squish" %in% methods) out <- stringr::str_squish(out)
  if ("dedupe_tokens" %in% methods)
    out <- gsub("\\b(\\w+)(\\s+\\1\\b)+", "\\1", out, perl = TRUE)
  if ("sort_tokens" %in% methods)
    out <- vapply(strsplit(out, "\\s+"),
                  function(t) paste(sort(t), collapse = " "), character(1))
  out
}

# Metaphone codes (one per value); non-alpha stripped, failures -> "".
.metaphone_codes <- function(x) {
  vapply(x, function(w) {
    w2 <- gsub("[^A-Za-z]", "", w)
    if (!nzchar(w2)) return("")
    tryCatch(phonics::metaphone(w2), error = function(e) "")
  }, character(1), USE.NAMES = FALSE)
}

# OpenRefine key-collision fingerprint: lowercase -> strip punctuation ->
# split tokens -> dedupe & sort -> join with single space. Linear O(N).
.fingerprint_key <- function(x) {
  vapply(x, function(s) {
    if (is.na(s) || !nzchar(trimws(s))) return("")
    s_clean <- tolower(trimws(s))
    s_clean <- gsub("[^[:alnum:][:space:]]+", " ", s_clean)
    s_clean <- stringr::str_squish(s_clean)
    if (!nzchar(s_clean)) return("")
    toks <- strsplit(s_clean, " ", fixed = TRUE)[[1]]
    toks <- unique(toks[nzchar(toks)])
    if (length(toks) == 0) return("")
    paste(sort(toks), collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

# OpenRefine n-gram fingerprint: lowercase -> strip punctuation AND whitespace
# -> extract character n-grams across the WHOLE remaining string -> dedupe &
# sort -> join. Linear O(N).
#
# The whitespace strip is the point, and it is what separates this from
# .fingerprint_key() rather than duplicating it. Because n-grams span what used
# to be word boundaries, "Wal Mart" / "WalMart" and "e-mail" / "email" collide
# here and cannot collide under key collision (their token sets differ). The
# trade is the mirror image: at n >= 2 a word-order swap changes the boundary
# n-grams, so "oat milk" / "milk oat" do NOT collide here - key collision is
# the right tool for that, and it is one entry up in the dropdown.
#
# At n = 1 the key degenerates to the distinct-character set, which is
# invariant to transposition and duplication ("Krzysztof" / "Kryzysztof").
# Do not "optimize" this back to per-token n-grams: that silently reverts the
# algorithm to a fuzzier restatement of key collision.
.ngram_fingerprint_key <- function(x, n = 2) {
  n <- max(1L, as.integer(n))
  vapply(x, function(s) {
    if (is.na(s) || !nzchar(trimws(s))) return("")
    s_clean <- tolower(trimws(s))
    s_clean <- gsub("[^[:alnum:]]+", "", s_clean)
    len <- nchar(s_clean)
    if (len == 0) return("")
    if (len < n) return(s_clean)
    grams <- vapply(seq_len(len - n + 1),
                    function(i) substr(s_clean, i, i + n - 1),
                    character(1))
    paste(sort(unique(grams)), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

#' Cluster a character vector by string similarity.
#'
#' @param values      Character vector of unique values.
#' @param frequencies Integer vector of counts. Defaults to 1 per value.
#' @param threshold   Similarity cutoff (0..1, higher = more similar). Ignored
#'                   by the phonetic algorithms (soundex/metaphone) and key
#'                   fingerprinting (fingerprint/ngram_fingerprint), which
#'                   bucket by exact key equality in linear O(N) time.
#' @param algorithm   One of CLUSTER_ALGORITHMS.
#' @param q           q-gram size for the cosine / jaccard / ngram_fingerprint metrics.
#' @param normalize   Character vector of CLUSTER_NORMALIZERS keys applied to a
#'                   working copy before distances are computed. The original
#'                   strings are preserved in the output.
#' @return Tibble with value, n, cluster_id, is_rare. `is_rare` is TRUE for
#'         a cluster member representing <5% of the cluster's frequency mass,
#'         when the cluster has more than one member.
cluster_strings <- function(values, frequencies = NULL, threshold = 0.92,
                            algorithm = c("jw", "osa", "lv", "lcs",
                                          "cosine", "jaccard",
                                          "fingerprint", "ngram_fingerprint",
                                          "soundex", "metaphone"),
                            q = 2, normalize = character(0)) {
  algorithm <- match.arg(algorithm)
  if (is.null(frequencies)) frequencies <- rep(1L, length(values))
  stopifnot(length(values) == length(frequencies))

  if (length(values) == 0) {
    return(tibble::tibble(
      value = character(0), n = integer(0),
      cluster_id = integer(0), is_rare = logical(0)
    ))
  }

  v    <- as.character(values)            # original — displayed / recoded
  keys <- normalize_for_cluster(v, normalize)  # working copy — clustered on

  if (algorithm %in% c("soundex", "metaphone")) {
    codes <- if (algorithm == "soundex")
      stringdist::phonetic(keys, method = "soundex")
    else
      .metaphone_codes(keys)
    cluster_id <- as.integer(factor(codes))
  } else if (algorithm %in% c("fingerprint", "ngram_fingerprint")) {
    codes <- if (algorithm == "fingerprint")
      .fingerprint_key(keys)
    else
      .ngram_fingerprint_key(keys, n = q)
    empty_mask <- !nzchar(codes)
    fac <- integer(length(codes))
    if (any(!empty_mask)) {
      fac[!empty_mask] <- as.integer(factor(codes[!empty_mask]))
    }
    if (any(empty_mask)) {
      max_id <- if (any(!empty_mask)) max(fac[!empty_mask]) else 0L
      fac[empty_mask] <- max_id + seq_len(sum(empty_mask))
    }
    cluster_id <- fac
  } else {
    # Metrics already in 0..1 (higher d = less similar): jw, cosine, jaccard.
    # Count metrics (osa, lv, lcs) are normalized by the longer string length.
    if (algorithm %in% c("jw", "cosine", "jaccard")) {
      d   <- stringdist::stringdistmatrix(keys, keys, method = algorithm, q = q)
      sim <- 1 - d
    } else {
      d    <- stringdist::stringdistmatrix(keys, keys, method = algorithm)
      lens <- outer(nchar(keys), nchar(keys), pmax)
      lens[lens == 0] <- 1L
      sim  <- 1 - d / lens
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

#' Match a vector of values against an approved reference taxonomy list.
#'
#' For each unique input value, computes similarity scores against all target
#' terms in `taxonomy_targets` using the chosen string metric. Proposes the
#' highest-scoring target as the matched canonical value if similarity >= `threshold`.
#'
#' @param values           Character vector of values to match (e.g. from dataset column).
#' @param taxonomy_targets Character vector of approved standard/taxonomy terms.
#' @param frequencies      Optional integer vector of counts parallel to `values`.
#' @param method           Distance method for `stringdist` ("jw", "osa", "lv", "cosine", "jaccard", "lcs").
#' @param threshold        Minimum similarity cutoff (0..1) to accept a match.
#' @param q                q-gram size for cosine/jaccard.
#' @return Tibble with columns: `value`, `n`, `matched_target`, `similarity`, `is_matched`, `status`.
match_taxonomy <- function(values, taxonomy_targets, frequencies = NULL,
                           method = c("jw", "osa", "lv", "cosine", "jaccard", "lcs"),
                           threshold = 0.75, q = 2) {
  method <- match.arg(method)
  values <- as.character(values)
  tax_clean <- unique(as.character(taxonomy_targets))
  tax_clean <- tax_clean[!is.na(tax_clean) & nzchar(trimws(tax_clean))]

  if (is.null(frequencies)) frequencies <- rep(1L, length(values))
  stopifnot(length(values) == length(frequencies))

  if (length(values) == 0) {
    return(tibble::tibble(
      value          = character(0),
      n              = integer(0),
      matched_target = character(0),
      similarity     = numeric(0),
      is_matched     = logical(0),
      status         = character(0)
    ))
  }

  if (length(tax_clean) == 0) {
    return(tibble::tibble(
      value          = values,
      n              = as.integer(frequencies),
      matched_target = rep(NA_character_, length(values)),
      similarity     = rep(0.0, length(values)),
      is_matched     = rep(FALSE, length(values)),
      status         = rep("no_taxonomy_targets", length(values))
    ))
  }

  v_lower <- tolower(trimws(values))
  tax_lower <- tolower(trimws(tax_clean))

  if (method %in% c("jw", "cosine", "jaccard")) {
    d <- stringdist::stringdistmatrix(v_lower, tax_lower, method = method, q = q)
    sim <- 1 - d
  } else {
    d <- stringdist::stringdistmatrix(v_lower, tax_lower, method = method)
    lens <- outer(nchar(v_lower), nchar(tax_lower), pmax)
    lens[lens == 0] <- 1L
    sim <- 1 - d / lens
  }
  sim[is.na(sim)] <- 0.0
  # lv/osa distance is bounded by the longer string, so those land in 0..1.
  # lcs is not (it counts insertions AND deletions, up to len_a + len_b), so
  # 1 - d/max_len can go negative and would surface as "-100.0%" in the UI.
  sim[sim < 0] <- 0.0

  best_idx <- max.col(sim, ties.method = "first")
  best_sim <- sim[cbind(seq_along(values), best_idx)]
  best_target <- tax_clean[best_idx]

  exact_hits <- v_lower %in% tax_lower
  for (i in which(exact_hits)) {
    match_pos <- match(v_lower[i], tax_lower)
    best_target[i] <- tax_clean[match_pos]
    best_sim[i] <- 1.0
  }

  is_hit <- best_sim >= threshold

  status <- ifelse(exact_hits, "exact",
            ifelse(is_hit, "fuzzy_match", "below_threshold"))

  tibble::tibble(
    value          = values,
    n              = as.integer(frequencies),
    matched_target = ifelse(is_hit, best_target, NA_character_),
    similarity     = round(best_sim, 3),
    is_matched     = is_hit,
    status         = status
  )
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
#'   invalid_enum       : match_type or action outside the allowed set
#'   invalid_regex      : match_type == "regex" but old_value won't compile
#'   stale              : rule whose old_value is no longer in any target column
#'                       (computed only if `data` is provided)
validate_recodes <- function(rules, data = NULL) {
  issues <- list()

  issues$duplicate_keys <- rules |>
    dplyr::count(variable, match_type, old_value, name = "n") |>
    dplyr::filter(n > 1)

  # Enum columns outside the allowed set (catches hand-edited / imported junk).
  issues$invalid_enum <- rules |>
    dplyr::filter(!(match_type %in% RECODE_MATCH_TYPES) |
                  !(action %in% RECODE_ACTIONS)) |>
    dplyr::select(rule_id, variable, match_type, action)

  # Regex rules whose pattern fails to compile.
  rx <- rules[!is.na(rules$match_type) & rules$match_type == "regex", ]
  if (nrow(rx) > 0) {
    bad <- vapply(rx$old_value, function(p) {
      isTRUE(suppressWarnings(tryCatch({ grepl(p, "probe"); FALSE },
                                       error = function(e) TRUE)))
    }, logical(1))
    issues$invalid_regex <- rx[bad, c("rule_id", "variable", "old_value")]
  } else {
    issues$invalid_regex <- tibble::tibble(
      rule_id = character(0), variable = character(0), old_value = character(0))
  }

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
      r    <- rules[i, ]
      cols <- .recode_cols(data, r)
      if (length(cols) == 0) return(TRUE)
      !any(vapply(cols, function(c) {
        any(.rule_hits(data[[c]], r$old_value, r$match_type))
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
#' SEMANTICS (single pass). Every rule is matched against the ORIGINAL column
#' values, and the FIRST rule to claim a cell wins; a later rule never sees —
#' and so never re-recodes — a value an earlier rule just wrote.
#'
#' This used to be a cascade: rules were applied one after another to the
#' progressively-rewritten frame, so a rule set containing both
#' `asain -> Asian` and `Asian -> Asian or PI` pushed the first rule's output
#' through the second one and left NO cell holding "Asian", which is not what
#' the rule table says. It also made the result depend on the row order of the
#' recodes CSV. Single-pass matches both the rule table and
#' `generate_recode_R()`'s `case_when()` output (see the agreement test in
#' tests/testthat/test-string_helpers.R).
#'
#' @return list(
#'   df      = modified df,
#'   summary = tibble(rule_id, cells_changed, cells_matched, cells_shadowed))
#'   - cells_matched  : cells the rule matched in the original data
#'   - cells_changed  : of those, the ones it actually rewrote to a new value
#'                      (a rule whose new_value equals the old value changes
#'                      nothing, and a shadowed cell is not changed by it)
#'   - cells_shadowed : cells this rule matched but an earlier rule had claimed
apply_recodes <- function(df, rules) {
  empty_summary <- tibble::tibble(rule_id        = character(0),
                                  cells_changed  = integer(0),
                                  cells_matched  = integer(0),
                                  cells_shadowed = integer(0))
  if (nrow(rules) == 0) return(list(df = df, summary = empty_summary))

  n_rules  <- nrow(rules)
  cols_for <- lapply(seq_len(n_rules), function(i) .recode_cols(df, rules[i, ]))

  # Snapshot the ORIGINAL values of every targeted column: all matching is done
  # against these, never against a value another rule has already written.
  target_cols <- unique(unlist(cols_for))
  orig    <- stats::setNames(lapply(target_cols, function(cn) df[[cn]]), target_cols)
  claimed <- stats::setNames(
    lapply(target_cols, function(cn) rep(FALSE, nrow(df))), target_cols)

  n_matched  <- integer(n_rules)
  n_changed  <- integer(n_rules)
  n_shadowed <- integer(n_rules)

  for (i in seq_len(n_rules)) {
    r    <- rules[i, ]
    cols <- cols_for[[i]]
    if (length(cols) == 0) next

    new_val <- if (identical(as.character(r$action), "delete")) NA_character_
               else r$new_value

    for (col in cols) {
      hit <- .rule_hits(orig[[col]], r$old_value, r$match_type)
      if (!any(hit)) next
      n_matched[i]  <- n_matched[i] + sum(hit)
      take          <- hit & !claimed[[col]]
      n_shadowed[i] <- n_shadowed[i] + sum(hit & claimed[[col]])
      if (!any(take)) next
      before <- df[[col]][take]
      df[[col]][take] <- new_val
      n_changed[i] <- n_changed[i] + sum(.cells_differ(before, df[[col]][take]))
      claimed[[col]] <- claimed[[col]] | take
    }
  }

  list(
    df = df,
    summary = tibble::tibble(
      rule_id        = as.character(rules$rule_id),
      cells_changed  = as.integer(n_changed),
      cells_matched  = as.integer(n_matched),
      cells_shadowed = as.integer(n_shadowed)
    )
  )
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

  # One group per target pattern, emitted in the order the patterns first
  # appear in the rule table (deterministic, and it reads in rule order). Every
  # arm inside a case_when() is tested against the ORIGINAL column, so grouping
  # all the match types together is what keeps the script single-pass.
  pattern_order <- unique(rules$effective_pattern)
  groups <- lapply(pattern_order,
                   function(p) rules[rules$effective_pattern == p, , drop = FALSE])

  # A column reached by more than one pattern (e.g. a plain rule on cause1 plus
  # a sibling rule matching "^cause[0-9]+$") lands in two blocks, which run in
  # sequence rather than in one pass — say so rather than diverging silently
  # from apply_recodes().
  overlapping <- unique(rules$variable[vapply(rules$variable, function(v) {
    sum(vapply(pattern_order, function(p) isTRUE(grepl(p, v)), logical(1))) > 1L
  }, logical(1))])
  if (length(overlapping) > 0) {
    header <- c(header,
      "# WARNING: these columns are targeted by more than one rule pattern, so",
      "# their blocks run in sequence (a later block can re-recode what an",
      "# earlier one wrote). Give each column a single pattern to avoid it:",
      paste0("#   ", paste(overlapping, collapse = ", ")),
      "")
  }

  blocks <- character(0)
  for (g in groups) {
    pattern <- g$effective_pattern[1]

    make_arms <- function(col_token) {
      arms <- character(0)
      for (i in seq_len(nrow(g))) {
        r  <- g[i, ]
        mt <- as.character(r$match_type)
        rhs <- if (identical(as.character(r$action), "delete")) "NA_character_"
               else deparse(as.character(r$new_value))
        cmt <- if (!is.na(r$notes) && nzchar(r$notes))
                 paste0("  # ", gsub("[\r\n]", " ", r$notes)) else ""
        lhs <- if (is.na(mt) || !(mt %in% RECODE_MATCH_TYPES)) {
          # Unusable match_type: matches nothing, same as .rule_hits().
          "FALSE"
        } else if (is.na(r$old_value)) {
          # `<NA>` in the CSV: the rule targets the column's missing cells.
          sprintf("is.na(%s)", col_token)
        } else if (identical(mt, "regex")) {
          # Regex match: detect the (raw) pattern, replace the whole cell.
          sprintf("str_detect(%s, %s)", col_token,
                  deparse(as.character(r$old_value)))
        } else {
          # Normalize BOTH sides identically for the case-insensitive types.
          col_expr <- switch(mt,
            exact      = col_token,
            exact_ci   = sprintf("tolower(%s)", col_token),
            trimmed_ci = sprintf("str_squish(tolower(%s))", col_token),
            col_token)
          target <- switch(mt,
            exact      = as.character(r$old_value),
            exact_ci   = tolower(as.character(r$old_value)),
            trimmed_ci = stringr::str_squish(tolower(as.character(r$old_value))),
            as.character(r$old_value))
          sprintf("%s == %s", col_expr, deparse(target))
        }
        arms <- c(arms, sprintf("    %s ~ %s,%s", lhs, rhs, cmt))
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
