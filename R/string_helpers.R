# =============================================================================
# Recode Studio - String / Recode Helpers
# =============================================================================
#
# Pure R helpers (no Shiny). Safe to source from scripts as well as the app.
#
# Public functions:
#   empty_recodes_tibble()              schema scaffold for a rule set
#   normalize_value(x, match_type)      value normalization per match strategy
#   cluster_strings(vals, freqs, ...)   string-similarity clustering + rare flag
#   read_recodes(path, conform = TRUE)  read recode CSV with NA round-trip,
#                                       conformed to the schema by default
#   write_recodes(rules, path)          write recode CSV with NA round-trip
#   merge_recode_rules(base, overlay)   overlay wins per rule_id, in place
#   recode_rule_id(variable, match_type, old_value)  stable hash id
#   recode_key_id(rules)                the id each rule's key implies
#   regex_compiles(pattern)             TRUE iff usable under the app's engine
#   clean_sibling_pattern(x)            blank sibling pattern -> NA
#   validate_recodes(rules, data = NULL)  duplicates, chains, blanks, stale,
#                                       bad regex / sibling pattern, stale
#                                       ids, spellcheck rules that lost words
#   apply_recodes(df, rules)            single pass, first match wins;
#                                       returns list(df, summary)
#   generate_recode_R(rules, dataset_id, source_csv_path = NULL,
#                     normalizations = NULL, data_cols = NULL)
#                                       emit copyable R script content
#
# Column normalizations (whole-column case / whitespace tidying, applied
# BEFORE the recodes) have their own section at the end of this file.
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
    source_dataset    = character(0),
    # The column-normalization policy in force for this rule's target columns
    # when the rule was written (normalization_spec_string()). Normalizations
    # run BEFORE the recodes, so a rule is authored against NORMALIZED values;
    # if the normalization later changes, the rule's meaning changes with it.
    # The stamp is what makes that detectable (warn_norm_spec_drift() in
    # R/rule_warnings.R). "" = no normalization applied; NA = never stamped
    # (a file written before this column existed). Not part of the rule key.
    norm_spec         = character(0)
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
#' Empty values: an `old_value` of "" targets the column's EMPTY cells under
#' `exact` and `exact_ci`, and under `trimmed_ci` also the whitespace-only
#' cells (they squish to ""). A `regex` rule with an EMPTY pattern matches
#' nothing: base grepl("") would match every non-missing cell (wiping the
#' column), which is never what an empty rule meant. Use the pattern `^$` to
#' target empty cells with a regex. validate_recodes() reports the empty
#' pattern as `invalid_regex`.
#'
#' Engine: base `grepl()` (TRE), and the generated script emits `grepl()`
#' with the same arguments, so both sides use one engine. The script used to
#' emit stringr::str_detect() (ICU), and patterns such as `\p{Lu}` (invalid
#' in TRE, so matching nothing here) or `a{,2}` (valid in TRE, an error in
#' ICU) behaved differently in the app and the script. A pattern that does
#' not compile (`regex_compiles()`) matches nothing on both sides.
#'
#' NA handling: an NA `old_value` (which is how the CSV's `<NA>` sentinel
#' round-trips) targets the MISSING cells of the column. It used to produce an
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
    # An empty pattern is not a usable pattern (see above).
    if (!nzchar(old_value)) return(rep(FALSE, n))
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

#' Does a regular expression compile under the app's regex engine?
#'
#' The app matches `regex` recode rules with base `grepl()` and resolves
#' sibling patterns with base `grep()`, both base R's default (TRE) engine,
#' and the generated scripts use the same calls (`grepl()` arms, and
#' tidyselect `matches()`, which is base `grep()` with `perl = FALSE`). So
#' this is the one test of "is this pattern usable", for validation, for
#' codegen, and for the UI to refuse a bad pattern at entry.
#'
#' @param pattern Character vector.
#' @return Logical vector, same length, never NA. FALSE for NA and for the
#'   empty string (neither is a usable pattern); otherwise TRUE iff the
#'   pattern compiles. Whitespace-only patterns are valid regexes (they match
#'   a space); callers holding a SIBLING pattern should pass it through
#'   `clean_sibling_pattern()` first, which turns blanks into NA.
regex_compiles <- function(pattern) {
  pattern <- as.character(pattern)
  vapply(pattern, function(p) {
    if (is.na(p) || !nzchar(p)) return(FALSE)
    isTRUE(suppressWarnings(tryCatch({ grepl(p, "probe"); TRUE },
                                     error = function(e) FALSE)))
  }, logical(1), USE.NAMES = FALSE)
}

# Does `pattern` reach the column called `name`? One pattern, one name,
# always a single TRUE or FALSE, never an error.
#
# Base grepl() THROWS rather than returning FALSE on a pattern TRE cannot
# compile, and length is one way to get there: a pattern of roughly 2,600
# characters fails with "TRE pattern compilation error 'Out of memory'". A
# column name that long becomes a pattern that long once a plain rule escapes
# and anchors it, so the generated script's overlap sweep died on a frame the
# app had loaded quite happily. Testing every loaded column name widens that
# exposure, which is why the guard lives here rather than at the call site.
#
# A throw means we cannot tell whether the pattern reaches the column, and
# "no overlap detected" is the same answer regex_compiles() gives for a
# pattern it cannot compile: report nothing rather than refuse to export.
.pattern_hits_name <- function(pattern, name) {
  isTRUE(suppressWarnings(tryCatch(grepl(pattern, name),
                                   error = function(e) FALSE)))
}

#' Canonical form of a sibling pattern: blank means "no pattern".
#'
#' An empty (or whitespace-only) sibling pattern used to reach EVERY column in
#' the app (`grep("", names)` matches everything), while tidyselect's
#' `matches("")` refuses it outright in the script. A blank pattern is never
#' what anyone meant, so wherever a sibling pattern is resolved or read it is
#' NA instead: the rule then targets its own `variable` alone.
#'
#' @param x Character vector (NULL or zero-length gives character(0)).
#' @return Character vector: NA where `x` is NA, "" or whitespace only,
#'   otherwise `x` unchanged (not trimmed: spaces inside a pattern count).
clean_sibling_pattern <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x <- as.character(x)
  x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_
  x
}

# TRUE for a rule that is a sibling rule (flag set, non-blank pattern) whose
# pattern does not compile. Such a rule is REPORTED (validate_recodes(),
# validate_normalizations()) and SKIPPED: it reaches no column in the app and
# the generated script emits no block for it. It must never throw.
.bad_sibling_rule <- function(apply_to_siblings, sibling_pattern) {
  sp <- clean_sibling_pattern(sibling_pattern)
  if (length(sp) == 0) return(logical(0))
  flag <- rep_len(as.logical(apply_to_siblings), length(sp))
  !is.na(flag) & flag & !is.na(sp) & !regex_compiles(sp)
}

#' Resolve the target columns of one recode rule against a data frame.
#'
#' Sibling rules expand their `sibling_pattern` against the frame's names;
#' everything else targets `variable` alone. Always returns names present in
#' `df` (possibly none). A blank sibling pattern counts as no pattern
#' (`clean_sibling_pattern()`); a pattern that does not compile reaches no
#' column (the rule is skipped, and validation reports it) instead of
#' throwing.
.recode_cols <- function(df, rule) .recode_cols_from_names(names(df), rule)

# Same resolution against a bare vector of column NAMES, for callers that hold
# the names but not the frame.
.recode_cols_from_names <- function(all_cols, rule) {
  all_cols <- as.character(all_cols)
  sp <- clean_sibling_pattern(rule$sibling_pattern)
  sp <- if (length(sp) == 1) sp else NA_character_
  cols <- if (length(rule$apply_to_siblings) == 1 &&
              !is.na(rule$apply_to_siblings) && rule$apply_to_siblings &&
              !is.na(sp)) {
    if (!regex_compiles(sp)) return(character(0))
    grep(sp, all_cols, value = TRUE)
  } else {
    rule$variable
  }
  intersect(cols, all_cols)
}

# Escape every regex metacharacter so the result matches `x` literally.
# stringr (ICU), not base gsub (TRE): a TRE character class holding `{}`
# throws "Invalid contents of {}" (see the Gotchas in CLAUDE.md). The class is
# the one suggest_sibling_pattern() in R/data_loader.R uses; that file keeps
# its own copy so it can be sourced without this one.
.escape_regex <- function(x) {
  stringr::str_replace_all(x, "([.^$*+?()\\[\\]{}|\\\\])", "\\\\\\1")
}

# TRUE for rules that target their `variable` alone (not a sibling family),
# vectorised. The exact complement of the sibling test in
# .recode_cols_from_names() and generate_recode_R(). A blank pattern is no
# pattern (clean_sibling_pattern()).
.is_plain_rule <- function(apply_to_siblings, sibling_pattern) {
  sibling_pattern <- clean_sibling_pattern(sibling_pattern)
  !(!is.na(apply_to_siblings) & apply_to_siblings & !is.na(sibling_pattern))
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

  v    <- as.character(values)            # original: displayed / recoded
  keys <- normalize_for_cluster(v, normalize)  # working copy: clustered on

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
#' For each input value, computes similarity scores against all target terms in
#' `taxonomy_targets` using the chosen string metric. Proposes the
#' highest-scoring target as the matched canonical value if similarity >= `threshold`.
#'
#' `values` is NOT deduplicated: the result has one row per element, in the
#' input order, so it stays parallel to `frequencies`. Callers that want one row
#' per distinct value pass an already-unique vector (the Reference Taxonomy
#' Matcher passes `unique_values_r()`).
#'
#' @param values           Character vector of values to match (e.g. from dataset column).
#'                         One output row per element; duplicates are kept.
#' @param taxonomy_targets Character vector of approved standard/taxonomy terms.
#'                         Deduplicated, and blank/NA terms are dropped.
#' @param frequencies      Optional integer vector of counts parallel to `values`.
#' @param method           Distance method for `stringdist` ("jw", "osa", "lv", "cosine", "jaccard", "lcs").
#' @param threshold        Minimum similarity cutoff (0..1) to accept a match.
#' @param q                q-gram size for cosine/jaccard.
#' @return Tibble with columns: `value`, `n`, `matched_target`, `similarity`,
#'   `is_matched`, `status`. `status` is one of `"exact"` (case/whitespace-
#'   insensitive equality), `"fuzzy_match"`, `"below_threshold"`, or
#'   `"no_taxonomy_targets"`. `matched_target` is NA unless `is_matched`;
#'   `similarity` always carries the best score found, clamped to 0..1.
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

# Coerce an `apply_to_siblings` vector of whatever type a CSV or a hand-built
# frame produced into a plain logical. Accepts TRUE/true/T/1 and FALSE/false/F/0
# (surrounding spaces ignored); anything else, blanks included, becomes NA,
# which every consumer treats as "not a sibling rule".
.as_siblings_flag <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) {
    out <- rep(NA, length(x))
    out[!is.na(x) & x == 1] <- TRUE
    out[!is.na(x) & x == 0] <- FALSE
    return(out)
  }
  s <- toupper(trimws(as.character(x)))
  out <- rep(NA, length(s))
  out[!is.na(s) & s %in% c("TRUE", "T", "1")]  <- TRUE
  out[!is.na(s) & s %in% c("FALSE", "F", "0")] <- FALSE
  out
}

#' Bring a recode rule set up to the recodes CSV schema.
#'
#' Every schema column (`names(empty_recodes_tibble())`) is present, in schema
#' order, with the schema type: a missing column is added as NA,
#' `apply_to_siblings` is coerced to logical (see `.as_siblings_flag()`), and
#' the other schema columns are character. Columns outside the schema are NOT
#' dropped: they are kept, in their original order, after the schema columns,
#' so a read never loses data. `write_recodes()` is where the schema is
#' enforced strictly.
#'
#' Why this exists: a CSV missing, say, `apply_to_siblings` used to read back
#' with fewer columns and only a console warning, and `generate_recode_R()`
#' then errored on the missing column.
#'
#' @param df A data frame of recode rules (any column types).
#' @return A tibble with the schema columns first, then any extras. A file
#'   written before `norm_spec` existed reads back with an NA stamp.
.conform_recodes_schema <- function(df) {
  schema <- empty_recodes_tibble()
  schema_cols <- names(schema)
  df <- tibble::as_tibble(df)
  n  <- nrow(df)
  for (nm in schema_cols) {
    if (!(nm %in% names(df))) {
      df[[nm]] <- if (nm == "apply_to_siblings") rep(NA, n)
                  else rep(NA_character_, n)
    } else if (nm == "apply_to_siblings") {
      df[[nm]] <- .as_siblings_flag(df[[nm]])
    } else {
      df[[nm]] <- as.character(df[[nm]])
    }
  }
  # A blank sibling pattern is no pattern (it used to select every column).
  df$sibling_pattern <- clean_sibling_pattern(df$sibling_pattern)
  extras <- setdiff(names(df), schema_cols)
  df[, c(schema_cols, extras), drop = FALSE]
}

#' Read a recodes CSV, preserving the explicit NA encoding (`<NA>` in CSV).
#'
#' @param path    Path to the CSV. A missing file reads as the empty schema.
#' @param conform When TRUE (default), every column is read as character and
#'   the result is passed through `.conform_recodes_schema()`: every schema
#'   column present, in order, typed, with any extra columns kept after them.
#'   When FALSE, the file is read raw with the schema's column types, exactly
#'   as before this option existed (columns the file lacks are simply absent,
#'   and readr warns). Callers that must inspect the raw columns first, such as
#'   the import module's schema guard, use FALSE and conform afterwards.
read_recodes <- function(path, conform = TRUE) {
  if (!file.exists(path)) return(empty_recodes_tibble())
  use_schema <- length(conform) == 1 && !is.na(conform) && conform
  if (!use_schema) return(.read_recodes_raw(path))
  df <- readr::read_csv(
    path,
    na = "<NA>",
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
  .conform_recodes_schema(df)
}

# The unconformed read: schema column types, no column added or reordered.
#
# `norm_spec` is typed only when the file carries it. It is the one OPTIONAL
# schema column (files written before it existed lack it), and naming it in the
# spec of a file without it would make readr warn about a column that is not
# missing in any sense that matters.
.read_recodes_raw <- function(path) {
  header <- names(readr::read_csv(path, n_max = 0, show_col_types = FALSE,
                                  col_types = readr::cols(
                                    .default = readr::col_character())))
  spec <- list(
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
  if ("norm_spec" %in% header) spec$norm_spec <- readr::col_character()
  readr::read_csv(
    path,
    na = "<NA>",
    show_col_types = FALSE,
    col_types = do.call(readr::cols, spec)
  )
}

#' Write a recodes tibble with explicit NA encoding.
#'
#' The rule set is conformed first and exactly the schema columns are
#' written, in schema order. Extra columns (kept by `read_recodes()`) are not
#' written, so the file on disk always matches the schema.
write_recodes <- function(rules, path) {
  rules <- .conform_recodes_schema(rules)
  rules <- rules[, names(empty_recodes_tibble()), drop = FALSE]
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(rules, path, na = "<NA>")
  invisible(path)
}


# --- Merging rule sets --------------------------------------------------------

#' Combine two recode rule sets, `overlay` winning per `rule_id`.
#'
#' A rule in `overlay` whose `rule_id` is already in `base` replaces that base
#' rule IN PLACE, so the base order is preserved. Rules only in `overlay` are
#' appended, in overlay order.
#'
#' Why in place: rules are single pass, first match wins, so row order decides
#' which rule claims a cell when two rules match it. Dropping the old row and
#' appending the edited one moves the rule to the end, and merely editing its
#' notes could then change the recoded output.
#'
#' @param base,overlay Recode rule tibbles (NULL is treated as empty). The
#'   overlay must carry every column of `base`.
#' @return A tibble with `base`'s columns (plus any overlay-only columns that
#'   `bind_rows()` brings in with appended rules).
merge_recode_rules <- function(base, overlay) {
  if (is.null(base)    || !is.data.frame(base))    base    <- empty_recodes_tibble()
  if (is.null(overlay) || !is.data.frame(overlay)) overlay <- empty_recodes_tibble()
  if (nrow(overlay) == 0) return(base)
  if (nrow(base) == 0)    return(overlay)
  idx <- match(base$rule_id, overlay$rule_id)
  hit <- !is.na(idx)
  out <- base
  if (any(hit)) out[hit, ] <- overlay[idx[hit], names(base), drop = FALSE]
  extra <- overlay[!(overlay$rule_id %in% base$rule_id), , drop = FALSE]
  dplyr::bind_rows(out, extra)
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

#' The id each rule SHOULD carry, recomputed from its key.
#'
#' A stored `rule_id` can go stale (an id saved before its rule's key was
#' edited, or written by an older tool), so a conflict check that compares
#' stored ids can miss two rules with the same key, or pair two rules that
#' merely share a stale id. Compare these instead.
#'
#' @param rules Recode rule tibble (NULL or zero rows gives character(0)).
#' @return Character vector, one id per rule, in row order:
#'   `recode_rule_id(variable, match_type, old_value)`.
recode_key_id <- function(rules) {
  if (is.null(rules) || !is.data.frame(rules) || nrow(rules) == 0)
    return(character(0))
  recode_rule_id(as.character(rules$variable), as.character(rules$match_type),
                 as.character(rules$old_value))
}

# Word count after collapsing whitespace; NA counts as 0 words.
.word_count <- function(x) {
  x <- stringr::str_squish(as.character(x))
  out <- lengths(strsplit(x, " ", fixed = TRUE))
  out[is.na(x) | !nzchar(x)] <- 0L
  as.integer(out)
}

# Rules made by the Spellcheck tab (notes start with "Spellcheck": the old
# format was "Spellcheck (token 'x')", the current one "Spellcheck: token 'x'
# -> 'y'") whose new_value has fewer words than old_value. The old Spellcheck
# replaced the WHOLE value with the corrected word, so "iced coffe" became
# "coffee". Delete rules and rules with a missing new_value are left to the
# other checks.
.spellcheck_lost_words <- function(rules) {
  empty <- tibble::tibble(rule_id = character(0), variable = character(0),
                          old_value = character(0), new_value = character(0),
                          notes = character(0), old_words = integer(0),
                          new_words = integer(0))
  if (is.null(rules) || !is.data.frame(rules) || nrow(rules) == 0 ||
      !all(c("notes", "old_value", "new_value") %in% names(rules))) {
    return(empty)
  }
  notes <- as.character(rules$notes)
  act   <- if ("action" %in% names(rules)) as.character(rules$action)
           else rep("recode", nrow(rules))
  old_w <- .word_count(rules$old_value)
  new_w <- .word_count(rules$new_value)
  hit <- !is.na(notes) & grepl("^[[:space:]]*Spellcheck([^[:alnum:]_]|$)", notes) &
         !is.na(act) & act == "recode" & !is.na(rules$new_value) &
         new_w < old_w
  if (!any(hit)) return(empty)
  tibble::tibble(rule_id   = as.character(rules$rule_id[hit]),
                 variable  = as.character(rules$variable[hit]),
                 old_value = as.character(rules$old_value[hit]),
                 new_value = as.character(rules$new_value[hit]),
                 notes     = notes[hit],
                 old_words = old_w[hit],
                 new_words = new_w[hit])
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
#'                       (regex_compiles()), or is empty (an empty pattern
#'                       matches nothing)
#'                       cols: rule_id, variable, old_value
#'   invalid_sibling_pattern : apply_to_siblings is TRUE and the (non-blank)
#'                       sibling_pattern does not compile; the rule is skipped
#'                       by apply and codegen
#'                       cols: rule_id, variable, sibling_pattern
#'   id_mismatch        : rule_id differs from recode_rule_id(variable,
#'                       match_type, old_value), e.g. an id saved before an
#'                       edit or by an older tool. Ids are NOT rewritten.
#'                       cols: rule_id, expected_id, variable, match_type,
#'                       old_value
#'   spellcheck_lost_words : rules the Spellcheck tab made (notes start with
#'                       "Spellcheck") whose new_value has fewer words than
#'                       old_value: the older Spellcheck replaced the whole
#'                       value with the corrected word. Not changed here.
#'                       cols: rule_id, variable, old_value, new_value, notes,
#'                       old_words, new_words
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

  # Regex rules whose pattern fails to compile, or is empty (an empty pattern
  # matches nothing; see .rule_hits()).
  # An NA old_value is not a pattern at all (it targets missing cells).
  rx <- rules[!is.na(rules$match_type) & rules$match_type == "regex", ]
  bad <- !is.na(rx$old_value) & !regex_compiles(rx$old_value)
  issues$invalid_regex <- rx[bad, c("rule_id", "variable", "old_value"),
                             drop = FALSE]

  # Sibling rules whose pattern does not compile: skipped by apply_recodes()
  # and by the generated script (no block), and reported here.
  bad_sib <- .bad_sibling_rule(rules$apply_to_siblings, rules$sibling_pattern)
  issues$invalid_sibling_pattern <-
    rules[bad_sib, c("rule_id", "variable", "sibling_pattern"), drop = FALSE]

  # Saved ids that no longer match the rule's key. Not rewritten here.
  expected <- recode_key_id(rules)
  mism <- is.na(rules$rule_id) | rules$rule_id != expected
  issues$id_mismatch <- tibble::tibble(
    rule_id     = as.character(rules$rule_id[mism]),
    expected_id = expected[mism],
    variable    = as.character(rules$variable[mism]),
    match_type  = as.character(rules$match_type[mism]),
    old_value   = as.character(rules$old_value[mism]))

  issues$spellcheck_lost_words <- .spellcheck_lost_words(rules)

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
#' values, and the FIRST rule to claim a cell wins; a later rule never sees
#' (and so never re-recodes) a value an earlier rule just wrote.
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
#'
#' @param normalizations Optional column-normalization rules
#'   (`empty_normalizations_tibble()` schema). Their mutate blocks are emitted
#'   BEFORE the recode blocks, which is the order the app applies them in
#'   (`apply_normalizations()` then `apply_recodes()`), so the recode arms
#'   compare against columns the normalization blocks have already rewritten,
#'   exactly as the app does. Carrying them in this one file keeps the script
#'   the whole cleaning rather than half of it.
#' @param data_cols Column names of the data the rules are meant for, or NULL.
#'   Following `generate_recat_R()`'s precedent. Two things need them, and
#'   with NULL both fall back to what the rule table alone can say:
#'   - the overlap WARNING tests every LOADED column name, not only the names
#'     some rule happens to give. Two sibling patterns can share a column no
#'     rule names, and the script then runs two blocks over that column in
#'     sequence while `apply_recodes()` stays single pass, so the two
#'     disagreed with no warning at all.
#'   - a rule whose target is not a column of the data gets no block, only a
#'     `# SKIPPED:` comment, exactly as a rule with an uncompilable sibling
#'     pattern already does. `apply_recodes()` skips such a rule
#'     (`.recode_cols()` resolves to nothing), so without this the script
#'     emitted a `mutate()` naming a column that does not exist and died on
#'     the data the app had just cleaned.
generate_recode_R <- function(rules, dataset_id, source_csv_path = NULL,
                              normalizations = NULL, data_cols = NULL) {
  norm_blocks <- .normalize_blocks(normalizations)
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
    return(paste(c(header, norm_blocks, "# No recode rules defined.", ""),
                 collapse = "\n"))
  }

  # NOTE: isTRUE() is NOT vectorised, so we use a length-safe expression.
  #
  # A plain rule's pattern is its column name ESCAPED and anchored, so it
  # matches that one column and nothing else. Unescaped, a rule on `q1.a` also
  # reached `q1_a` (the `.` is a wildcard) and a name like `price ($)` was not
  # a pattern for its own column at all. The pattern is still what groups the
  # rules (one case_when() per target, which keeps the script single-pass) and
  # what the overlap WARNING below tests column names against; plain groups
  # are then EMITTED as a backticked single-column mutate() (see below), so no
  # regex engine is involved in reaching their column in the script.
  # Sibling patterns are the user's own regexes and are used as given.
  rules <- rules |>
    dplyr::mutate(
      .plain = .is_plain_rule(apply_to_siblings, sibling_pattern),
      effective_pattern = ifelse(
        .plain,
        paste0("^", .escape_regex(variable), "$"),
        clean_sibling_pattern(sibling_pattern)
      ),
      # A sibling pattern that does not compile reaches no column in the app
      # (.recode_cols_from_names()), so its rules get no block here either,
      # only a comment saying they were skipped.
      .skip_reason = ifelse(!.plain & !regex_compiles(effective_pattern),
                            "pattern", NA_character_)
    )

  # The other way a rule reaches no column in the app: its target is not in
  # the data at all. Only decidable with `data_cols`. Same treatment as an
  # uncompilable pattern: a comment, no block, so the script does what the
  # app did instead of naming a column that is not there.
  if (!is.null(data_cols)) {
    dc <- as.character(data_cols)
    absent <- vapply(seq_len(nrow(rules)), function(i) {
      if (!is.na(rules$.skip_reason[i])) return(FALSE)
      length(.recode_cols_from_names(dc, rules[i, ])) == 0L
    }, logical(1))
    rules$.skip_reason[absent] <- "missing"
  }
  rules$.skip <- !is.na(rules$.skip_reason)

  # One group per target pattern, emitted in the order the patterns first
  # appear in the rule table (deterministic, and it reads in rule order). Every
  # arm inside a case_when() is tested against the ORIGINAL column, so grouping
  # all the match types together is what keeps the script single-pass.
  pattern_order <- unique(rules$effective_pattern)
  groups <- lapply(pattern_order,
                   function(p) rules[rules$effective_pattern == p, , drop = FALSE])

  # A column reached by more than one pattern (e.g. a plain rule on drink1 plus
  # a sibling rule matching "^drink[0-9]+$") lands in two blocks, which run in
  # sequence rather than in one pass - say so rather than diverging silently
  # from apply_recodes().
  live_patterns <- unique(rules$effective_pattern[!rules$.skip])
  # Which column names to test. Every LOADED column when we know them: a
  # column no rule names can still be reached by two sibling patterns, and
  # that is exactly the case that used to diverge in silence. Without
  # `data_cols` we can only test the names the rules give, as before.
  sweep_cols <- unique(as.character(
    if (!is.null(data_cols)) data_cols else rules$variable))
  overlapping <- sweep_cols[vapply(sweep_cols, function(v) {
    sum(vapply(live_patterns, function(p) .pattern_hits_name(p, v),
               logical(1))) > 1L
  }, logical(1))]
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

    if (g$.skip[1]) {
      n_txt   <- sprintf("%d rule%s", nrow(g), if (nrow(g) == 1) "" else "s")
      lit     <- function(x) gsub("[\r\n]", " ",
                                  paste(deparse(as.character(x)),
                                        collapse = ""))
      is_fam  <- !any(g$.plain)
      why <- if (identical(g$.skip_reason[1], "missing")) {
        if (is_fam) {
          c(sprintf("# SKIPPED: %s for the sibling pattern %s. No column in",
                    n_txt, lit(pattern)),
            "# this data matches it, so Recode Studio applied none of them",
            "# either. Check the pattern in the Recodes table, or remove the",
            "# rules if they no longer apply.")
        } else {
          c(sprintf("# SKIPPED: %s for the column %s. That column is not in",
                    n_txt, lit(g$variable[g$.plain][1])),
            "# this data, so Recode Studio applied none of them either.",
            "# Check the column name in the Recodes table, or remove the",
            "# rules if they no longer apply.")
        }
      } else {
        c(sprintf(paste0("# SKIPPED: %s for the sibling pattern %s. The ",
                         "pattern is not a"), n_txt, lit(pattern)),
          "# valid regular expression, so Recode Studio applies none of them",
          "# either. Fix the pattern in the Recodes table and export again.")
      }
      blocks <- c(blocks, why,
        paste0("#   rule_id: ", paste(g$rule_id, collapse = ", ")),
        "")
      next
    }

    make_arms <- function(col_token) {
      arms <- character(0)
      for (i in seq_len(nrow(g))) {
        r  <- g[i, ]
        mt <- as.character(r$match_type)
        rhs <- if (identical(as.character(r$action), "delete")) "NA_character_"
               else deparse(as.character(r$new_value))
        cmt <- if (!is.na(r$notes) && nzchar(r$notes))
                 paste0("  # ", gsub("[\r\n]", " ", r$notes)) else ""
        # An arm that matches nothing is a FULL-LENGTH FALSE: a bare scalar
        # FALSE as the only arm makes case_when() fail ("Can't recycle
        # `.default` (size n) to size 1").
        never <- sprintf("rep_len(FALSE, length(%s))", col_token)
        lhs <- if (is.na(mt) || !(mt %in% RECODE_MATCH_TYPES)) {
          # Unusable match_type: matches nothing, same as .rule_hits().
          never
        } else if (is.na(r$old_value)) {
          # `<NA>` in the CSV: the rule targets the column's missing cells.
          sprintf("is.na(%s)", col_token)
        } else if (identical(mt, "regex") &&
                   !regex_compiles(as.character(r$old_value))) {
          # Empty or non-compiling pattern: matches nothing, same as
          # .rule_hits(), and validate_recodes() reports it.
          never
        } else if (identical(mt, "regex")) {
          # Regex match: base grepl(), the call .rule_hits() makes, so the
          # app and the script use ONE regex engine (TRE). str_detect() (ICU)
          # used to disagree on patterns like `\p{Lu}` or `a{,2}`. grepl()
          # is FALSE on a missing cell, as in the app. Replaces the whole cell.
          sprintf("grepl(%s, %s)", deparse(as.character(r$old_value)),
                  col_token)
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

    # Single-column block when the group targets exactly one named column: a
    # plain rule is in it (every rule in the group then reaches exactly that
    # column, because the escaped pattern matches only it), or a sibling
    # pattern that is just `^<syntactic name>$`. The column is written as an
    # R token, backticked when it is not syntactic.
    single_var <- if (any(g$.plain)) {
      g$variable[g$.plain][1]
    } else if (grepl("^\\^([a-zA-Z_][a-zA-Z0-9_]*)\\$$", pattern)) {
      sub("^\\^([a-zA-Z_][a-zA-Z0-9_]*)\\$$", "\\1", pattern)
    } else {
      NULL
    }

    if (!is.null(single_var)) {
      var <- .r_col_token(single_var)
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
        # ignore.case = FALSE: tidyselect's matches() ignores case by default,
        # but apply_recodes() resolves the pattern with case-sensitive grep().
        # Without it the script also recoded e.g. `Drink3` for "^drink[0-9]+$".
        sprintf("  mutate(across(matches(%s, ignore.case = FALSE), function(.x) {",
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

  paste(c(header, norm_blocks, blocks), collapse = "\n")
}


# =============================================================================
# Column normalizations
# =============================================================================
#
# A normalization is a COLUMN-LEVEL text transform ("title case this column",
# "collapse its whitespace"), as distinct from a recode, which maps one VALUE
# to another value.
#
# WHY IT IS NOT A RECODE ROW, deliberately. Adding `lowercase` / `titlecase` to
# RECODE_ACTIONS with `old_value` unused was considered and rejected:
#   - `recode_rule_id()` hashes (variable, match_type, old_value), so two
#     normalizations of one column would collide on a single id.
#   - `apply_recodes()` is a per-cell claim model (first rule to claim a cell
#     wins). A column-wide transform claims nothing and is shadowed by nothing,
#     so it does not belong in that ordering at all.
#   - every recode consumer (the editable rule table, `validate_recodes()`'s
#     duplicate / chain / stale checks, the three per-rule cell counts) is
#     written around old_value / new_value and would need a special case.
# Recategorization is the precedent: also a different KIND of operation, with
# its own tibble, file and codegen. Normalization follows that pattern. The
# generated recode script still carries the normalization blocks, so that one
# file is the whole cleaning.
#
# ORDER: normalizations, THEN recodes, THEN recat.
#   - FIRST, because the app builds its value lists, clusters and spellcheck on
#     normalized values. Capitalization and whitespace variants otherwise fill
#     the cluster list with decisions whose answer never changes, each one
#     producing a recode rule that is pure clutter.
#   - which means a recode rule is authored against a NORMALIZED value, and the
#     rule set only means what it says under the normalization it was written
#     under. That is why every recode rule carries a `norm_spec` stamp (see
#     "Normalization spec stamp" below) and why the Preview & export tab
#     reports drift.
#   - the cost: normalizing first kills an `exact` or `regex` recode written
#     against the values as delivered. Those rules must be re-authored against
#     the normalized values; the Normalize tab says so.
#   - a recode may rewrite a value a normalization just produced. That is the
#     intended shape ("the user clicked this cluster label and chose a
#     target"), so it carries no warning of its own.
#   - BEFORE recat, so recat matches a tidy frame and a recat rule's `category`
#     literal is never re-cased. Recat term matching is already
#     case-insensitive, so the case ops cannot cost it a match.
#
# LOSSY: `titlecase` cannot be undone ("McKenzie" -> "Mckenzie"). There is no
# unnormalize. `normalization_preview()` exists so the user sees the exact
# before / after pairs, with cell counts, before adding the rule.
#
# The five ops all COMMUTE (the case ops do not move whitespace, the whitespace
# ops do not change case), so a rule's ops are stored and applied in one
# canonical order and a rule needs no ordering semantics of its own. Picking
# two conflicting case ops is a validation error, not an ordering question.
#
# Columns are assumed to be character (read_dataset() reads every column as
# text). The ops return character, so a non-character column would be coerced,
# identically in the app and the generated script.
#
# Public functions:
#   empty_normalizations_tibble()                   schema scaffold
#   normalization_rule_id(variable, sibling_pattern)  stable hash id
#   normalize_ops_split(x) / normalize_ops_join(x)  ";"-list round trip
#   apply_normalize_ops(x, ops)                     the transform (vectorised)
#   apply_normalizations(df, rules)                 list(df, summary)
#   recode_value_map(df_before, df_after, cols)     before -> after pairs
#   normalization_preview(df, rules)                pairs + cell counts
#   validate_normalizations(rules, data = NULL)     issue tibbles
#   read_normalizations(path) / write_normalizations(rules, path)
#   merge_normalization_rules(base, overlay)        overlay wins per norm_id
#   normalization_spec_string(cols, normalizations) the per-column stamp
#   recode_norm_specs(rules, normalizations, data_cols)  one stamp per rule
#   stamp_recode_norm_spec(rules, normalizations, data_cols)
# =============================================================================

# The ops, in the canonical order they are applied and stored in.
#
# KEPT: whitespace tidying and the three case forms. Each is presentational:
# it changes how a value is written, never which value it is.
# REJECTED, deliberately:
#   - sentence case: no clearer than title case on the short categorical values
#     these columns hold, and it mangles proper nouns the same way.
#   - punctuation stripping / accent folding: those change MEANING, not
#     presentation ("St. Mark" and "St Mark" are a data decision, not a format
#     one). The Clusters tab's "normalize before clustering" options do that
#     for COMPARING values, and it must not be written back into the data.
#   - blank-to-NA: that is a value mapping, and the recode table already has a
#     shape for it (an `old_value` of "" with action `delete`). Two ways to
#     express one thing is how rule sets start disagreeing with each other.
NORMALIZE_OPS <- c("trim", "squish", "lowercase", "uppercase", "titlecase")

# Ops that set the case. At most one per rule (validate_normalizations()).
NORMALIZE_CASE_OPS <- c("lowercase", "uppercase", "titlecase")

# Plain-language labels, keyed by op.
NORMALIZE_OP_LABELS <- c(
  trim      = "Trim leading and trailing spaces",
  squish    = "Trim, and collapse repeated inner spaces",
  lowercase = "lower case",
  uppercase = "UPPER CASE",
  titlecase = "Title Case"
)

# A rule's ops are stored as one delimited string in the CSV.
NORMALIZE_LIST_SEP <- ";"

#' Empty tibble matching the normalizations CSV schema
#' (`normalizations_master.csv`).
#'
#' `variable` / `apply_to_siblings` / `sibling_pattern` are named exactly as in
#' the recodes schema so `.recode_cols()` resolves a normalization's target
#' columns with no second implementation.
empty_normalizations_tibble <- function() {
  tibble::tibble(
    norm_id           = character(0),
    variable          = character(0),
    apply_to_siblings = logical(0),
    sibling_pattern   = character(0),
    ops               = character(0),   # ";"-joined, canonical order
    notes             = character(0),
    author            = character(0),
    created_at        = character(0),
    updated_at        = character(0),
    source_dataset    = character(0)
  )
}

#' Split an ops value (a ";"-joined string, or a character vector) into tokens.
#'
#' Unknown tokens are KEPT so validate_normalizations() can name them; use
#' `.norm_ops_ordered()` for the ops that will actually be applied.
normalize_ops_split <- function(x, sep = NORMALIZE_LIST_SEP) {
  if (length(x) == 0) return(character(0))
  x <- as.character(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(character(0))
  parts <- unlist(strsplit(x, sep, fixed = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  unique(parts[nzchar(parts)])
}

# The recognised ops only, in canonical order: what is applied and emitted.
.norm_ops_ordered <- function(x) {
  intersect(NORMALIZE_OPS, normalize_ops_split(x))
}

#' Canonical storage form of an ops set: recognised ops, canonical order.
#'
#' Storing canonically means the tick order in the UI cannot change the rule.
normalize_ops_join <- function(x, sep = NORMALIZE_LIST_SEP) {
  paste(.norm_ops_ordered(x), collapse = sep)
}

#' Apply a normalization's ops to a character vector. NA stays NA.
#'
#' This is the single definition of what each op means; the generated R script
#' emits calls to exactly these functions (see `.NORMALIZE_FUNS`), which is
#' what keeps the app and the script agreeing cell for cell.
apply_normalize_ops <- function(x, ops) {
  ops <- .norm_ops_ordered(ops)
  if (length(ops) == 0) return(x)
  for (op in ops) {
    x <- switch(op,
      trim      = stringr::str_trim(x),
      squish    = stringr::str_squish(x),
      lowercase = tolower(x),
      uppercase = toupper(x),
      titlecase = stringr::str_to_title(x),
      x)
  }
  x
}

#' Stable 12-char hex id for a normalization, derived from its TARGET.
#'
#' The ops are deliberately NOT in the id: the target is the identity ("this
#' column is title case"), so re-adding a column with different ops UPDATES
#' the rule instead of stacking a second one behind it.
normalization_rule_id <- function(variable, sibling_pattern = NA_character_) {
  sibling_pattern <- rep(as.character(sibling_pattern),
                         length.out = length(variable))
  raw <- paste(as.character(variable),
               ifelse(is.na(sibling_pattern), "<NA>", sibling_pattern),
               sep = "||")
  vapply(raw, function(x) substr(rlang::hash(x), 1, 12),
         character(1), USE.NAMES = FALSE)
}

#' Apply a normalization rule set to a data frame.
#'
#' Rules are applied in table order. Two rules resolving to the SAME column
#' therefore compose (the second sees the first's output), which is harmless
#' because the ops commute and are idempotent, and `validate_normalizations()`
#' flags the overlap anyway. A rule whose target is not in `df` is skipped.
#'
#' @return list(
#'   df      = modified df,
#'   summary = tibble(norm_id, ops, columns, n_cols, cells_changed))
#'   `columns` is the ";"-joined resolved column list; `ops` the canonical ops.
apply_normalizations <- function(df, rules) {
  empty_summary <- tibble::tibble(norm_id = character(0), ops = character(0),
                                  columns = character(0), n_cols = integer(0),
                                  cells_changed = integer(0))
  if (is.null(rules) || !is.data.frame(rules) || nrow(rules) == 0)
    return(list(df = df, summary = empty_summary))

  n_rules  <- nrow(rules)
  ops_txt  <- character(n_rules)
  cols_txt <- character(n_rules)
  n_cols   <- integer(n_rules)
  changed  <- integer(n_rules)

  for (i in seq_len(n_rules)) {
    r    <- rules[i, ]
    ops  <- .norm_ops_ordered(r$ops)
    cols <- .recode_cols(df, r)
    ops_txt[i]  <- paste(ops, collapse = NORMALIZE_LIST_SEP)
    cols_txt[i] <- paste(cols, collapse = NORMALIZE_LIST_SEP)
    n_cols[i]   <- length(cols)
    if (length(ops) == 0 || length(cols) == 0) next
    for (col in cols) {
      before <- df[[col]]
      after  <- apply_normalize_ops(before, ops)
      changed[i] <- changed[i] + sum(.cells_differ(before, after))
      df[[col]] <- after
    }
  }

  list(
    df = df,
    summary = tibble::tibble(
      norm_id       = as.character(rules$norm_id),
      ops           = ops_txt,
      columns       = cols_txt,
      n_cols        = as.integer(n_cols),
      cells_changed = as.integer(changed)
    )
  )
}

#' Distinct before -> after value pairs for the cells that moved.
#'
#' @param df_before,df_after Frames of identical shape.
#' @param cols Columns to compare. NULL compares every shared column; pass the
#'   rules' target columns on a wide frame.
#' @return tibble(column, raw_value, clean_value, n_cells); zero rows if nothing
#'   moved.
recode_value_map <- function(df_before, df_after, cols = NULL) {
  empty <- tibble::tibble(column = character(0), raw_value = character(0),
                          clean_value = character(0), n_cells = integer(0))
  if (is.null(df_before) || is.null(df_after) || nrow(df_before) == 0)
    return(empty)
  shared <- intersect(names(df_before), names(df_after))
  cols <- if (is.null(cols)) shared else intersect(as.character(cols), shared)
  if (length(cols) == 0) return(empty)

  rows <- lapply(cols, function(cn) {
    b <- as.character(df_before[[cn]])
    a <- as.character(df_after[[cn]])
    if (length(b) != length(a)) return(NULL)
    changed <- .cells_differ(b, a)
    if (!any(changed)) return(NULL)
    tibble::tibble(column = cn, raw_value = b[changed],
                   clean_value = a[changed]) |>
      dplyr::count(column, raw_value, clean_value, name = "n_cells")
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(empty)
  out <- dplyr::bind_rows(rows)
  out$n_cells <- as.integer(out$n_cells)
  out
}

#' Distinct before -> after value pairs a normalization set would produce.
#'
#' Normalization is LOSSY and there is no undo, so this is how the user sees
#' what it does before adding the rule.
#'
#' @return tibble(column, before, after, n_cells), most-affected first.
normalization_preview <- function(df, rules, max_rows = 500L) {
  empty <- tibble::tibble(column = character(0), before = character(0),
                          after = character(0), n_cells = integer(0))
  if (is.null(df) || is.null(rules) || !is.data.frame(rules) ||
      nrow(rules) == 0 || nrow(df) == 0) return(empty)
  res  <- apply_normalizations(df, rules)
  cols <- unique(unlist(lapply(seq_len(nrow(rules)),
                               function(i) .recode_cols(df, rules[i, ]))))
  if (length(cols) == 0) return(empty)
  map <- recode_value_map(df, res$df, cols)
  if (nrow(map) == 0) return(empty)
  names(map)[names(map) == "raw_value"]   <- "before"
  names(map)[names(map) == "clean_value"] <- "after"
  map <- map[order(-map$n_cells, map$column, map$before), , drop = FALSE]
  utils::head(map[, c("column", "before", "after", "n_cells")], max_rows)
}

#' Validate a normalization rule set.
#'
#' Returns a named list of issue tibbles (norm_id, variable, detail; all may be
#' empty):
#'   no_ops              : rule with nothing selected
#'   invalid_ops         : op tokens outside NORMALIZE_OPS
#'   conflicting_case    : more than one of lower / upper / title in one rule
#'   duplicate_targets   : the same (variable, sibling_pattern) more than once
#'   invalid_sibling_pattern : a sibling rule whose pattern does not compile
#'                         (regex_compiles()); apply and codegen skip it
#'   missing_column      : target column absent from `data` (needs `data`)
#'   overlapping_targets : two rules resolving onto a shared column (needs
#'                         `data`); legal, applied in table order, but almost
#'                         always a mistake
validate_normalizations <- function(rules, data = NULL) {
  none <- tibble::tibble(norm_id = character(0), variable = character(0),
                         detail = character(0))
  out <- list(no_ops = none, invalid_ops = none, conflicting_case = none,
              duplicate_targets = none, invalid_sibling_pattern = none,
              missing_column = none, overlapping_targets = none)
  if (is.null(rules) || !is.data.frame(rules) || nrow(rules) == 0) return(out)

  mk <- function(idx, detail) {
    if (length(idx) == 0) return(none)
    tibble::tibble(norm_id  = as.character(rules$norm_id[idx]),
                   variable = as.character(rules$variable[idx]),
                   detail   = detail)
  }

  tokens  <- lapply(rules$ops, normalize_ops_split)
  known   <- lapply(tokens, function(tk) intersect(NORMALIZE_OPS, tk))
  unknown <- lapply(tokens, function(tk) setdiff(tk, NORMALIZE_OPS))

  idx <- which(vapply(known, length, integer(1)) == 0)
  out$no_ops <- mk(idx, "No normalization selected, so the rule does nothing.")

  idx <- which(vapply(unknown, length, integer(1)) > 0)
  out$invalid_ops <- mk(idx, vapply(idx, function(i) sprintf(
    "Unrecognised normalization%s: %s. Allowed: %s.",
    if (length(unknown[[i]]) == 1) "" else "s",
    paste(unknown[[i]], collapse = ", "),
    paste(NORMALIZE_OPS, collapse = ", ")), character(1)))

  idx <- which(vapply(known, function(k)
    length(intersect(NORMALIZE_CASE_OPS, k)) > 1L, logical(1)))
  out$conflicting_case <- mk(idx, vapply(idx, function(i) sprintf(
    "Two case normalizations on one column (%s). Pick one.",
    paste(intersect(NORMALIZE_CASE_OPS, known[[i]]), collapse = ", ")),
    character(1)))

  sp  <- clean_sibling_pattern(rules$sibling_pattern)
  key <- paste(as.character(rules$variable), "||",
               ifelse(is.na(sp), "<NA>", sp))
  idx <- which(key %in% key[duplicated(key)])
  out$duplicate_targets <- mk(idx,
    "The same target has more than one normalization rule.")

  idx <- which(.bad_sibling_rule(rules$apply_to_siblings,
                                 rules$sibling_pattern))
  out$invalid_sibling_pattern <- mk(idx, vapply(idx, function(i) sprintf(
    paste0("The sibling pattern `%s` is not a valid regular expression, so ",
           "the rule is skipped."), sp[i]), character(1)))

  if (!is.null(data) && is.data.frame(data)) {
    cols <- lapply(seq_len(nrow(rules)),
                   function(i) .recode_cols(data, rules[i, ]))
    idx <- which(vapply(cols, length, integer(1)) == 0)
    out$missing_column <- mk(idx,
      "No column in the loaded data matches the target.")

    all_cols <- unlist(cols, use.names = FALSE)
    dup_cols <- unique(all_cols[duplicated(all_cols)])
    idx <- which(vapply(cols, function(cc) any(cc %in% dup_cols), logical(1)))
    out$overlapping_targets <- mk(idx, vapply(idx, function(i) sprintf(
      "Shares column%s %s with another rule; they apply in table order.",
      if (length(intersect(cols[[i]], dup_cols)) == 1) "" else "s",
      paste(intersect(cols[[i]], dup_cols), collapse = ", ")), character(1)))
  }
  out
}


# --- Normalizations CSV read / write -------------------------------------------
#
# Encoding: blanks are written as empty cells, and both "" and `<NA>` read back
# as NA. Unlike a recode's old_value / new_value, no normalization field gives
# "" a meaning distinct from NA (an empty sibling pattern is no pattern), so
# the simpler encoding is safe, and it is the encoding upstream writes.

#' Bring a normalization rule set up to the normalizations CSV schema.
#'
#' Every schema column present, in schema order, typed (`apply_to_siblings`
#' logical, the rest character); a missing column is added as NA; an empty
#' `sibling_pattern` becomes NA. Extra columns are kept after the schema ones.
.conform_normalizations_schema <- function(df) {
  schema_cols <- names(empty_normalizations_tibble())
  df <- tibble::as_tibble(df)
  n  <- nrow(df)
  for (nm in schema_cols) {
    if (!(nm %in% names(df))) {
      df[[nm]] <- if (nm == "apply_to_siblings") rep(NA, n)
                  else rep(NA_character_, n)
    } else if (nm == "apply_to_siblings") {
      df[[nm]] <- .as_siblings_flag(df[[nm]])
    } else {
      df[[nm]] <- as.character(df[[nm]])
    }
  }
  df$sibling_pattern <- clean_sibling_pattern(df$sibling_pattern)
  extras <- setdiff(names(df), schema_cols)
  df[, c(schema_cols, extras), drop = FALSE]
}

#' Read a normalizations CSV.
#'
#' @param path    Path to the CSV. A missing file reads as the empty schema.
#' @param conform When TRUE (default) the result is conformed to the schema.
#'   FALSE returns the file's own columns, all character, for callers (the
#'   import guard) that must inspect them first.
read_normalizations <- function(path, conform = TRUE) {
  if (!file.exists(path)) return(empty_normalizations_tibble())
  df <- readr::read_csv(
    path,
    na = c("", "<NA>"),
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
  if (length(conform) == 1 && !is.na(conform) && conform)
    .conform_normalizations_schema(df)
  else df
}

#' Write a normalizations tibble: exactly the schema columns, in order.
write_normalizations <- function(rules, path) {
  if (is.null(rules) || !is.data.frame(rules))
    rules <- empty_normalizations_tibble()
  rules <- .conform_normalizations_schema(rules)
  rules <- rules[, names(empty_normalizations_tibble()), drop = FALSE]
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(rules, path, na = "")
  invisible(path)
}

#' Combine two normalization rule sets, `overlay` winning per `norm_id`, in
#' place (the counterpart of `merge_recode_rules()`).
merge_normalization_rules <- function(base, overlay) {
  if (is.null(base) || !is.data.frame(base))
    base <- empty_normalizations_tibble()
  if (is.null(overlay) || !is.data.frame(overlay))
    overlay <- empty_normalizations_tibble()
  if (nrow(overlay) == 0) return(base)
  if (nrow(base) == 0)    return(overlay)
  idx <- match(base$norm_id, overlay$norm_id)
  hit <- !is.na(idx)
  out <- base
  if (any(hit)) out[hit, ] <- overlay[idx[hit], names(base), drop = FALSE]
  extra <- overlay[!(overlay$norm_id %in% base$norm_id), , drop = FALSE]
  dplyr::bind_rows(out, extra)
}


# --- Normalization spec stamp -------------------------------------------------
#
# Normalizations run BEFORE the recodes, so a recode rule is written against
# NORMALIZED values: the user clicked "Latte" in a cluster that only reads
# "Latte" because the column is title-cased first. Change the normalization
# afterwards and the rule's meaning changes with it, silently: an `exact` rule
# on "Latte" stops matching the moment the column becomes UPPER CASE, and
# nothing on screen says why the rule went dead.
#
# So the policy in force is STAMPED onto each rule when the rule is added to
# the session (make_rules_proxy() in R/rule_proxies.R), and compared with the
# policy in force on Preview & export (warn_norm_spec_drift()). The stamp is
# per-COLUMN, because normalization is per-column policy: a rule on `city` is
# unaffected by a normalization added to `drink1`.
#
# The stamp is a canonical string, not a hash, so a person reading
# recodes_master.csv can see what it says:
#   ""                                      no normalization on the target
#   "city=squish;titlecase"                 one column
#   "drink1=uppercase|drink2=uppercase"     a sibling rule's columns
# Columns are sorted, ops are in NORMALIZE_OPS order, so the same policy always
# produces the same string.

# Canonical ops string applying to ONE column, "" when nothing does. Several
# normalization rules can reach one column (validate_normalizations() flags the
# overlap); they apply in table order, so the stamp keeps that order too.
.normalization_ops_for_column <- function(col, normalizations) {
  if (is.null(normalizations) || !is.data.frame(normalizations) ||
      nrow(normalizations) == 0) return("")
  ops <- character(0)
  for (i in seq_len(nrow(normalizations))) {
    if (length(.recode_cols_from_names(col, normalizations[i, ])) == 0) next
    ops <- c(ops, .norm_ops_ordered(normalizations$ops[i]))
  }
  paste(ops, collapse = NORMALIZE_LIST_SEP)
}

#' Canonical stamp of the normalization policy covering a set of columns.
#'
#' @param cols Character vector of column names.
#' @param normalizations Normalization rule set (may be NULL / empty).
#' @return Single string; "" when no normalization touches any of `cols`.
normalization_spec_string <- function(cols, normalizations) {
  cols <- sort(unique(as.character(cols[!is.na(cols)])))
  if (length(cols) == 0) return("")
  parts <- character(0)
  for (col in cols) {
    ops <- .normalization_ops_for_column(col, normalizations)
    if (nzchar(ops)) parts <- c(parts, paste0(col, "=", ops))
  }
  paste(parts, collapse = "|")
}

#' Per-rule normalization stamps for a recode rule set.
#'
#' @param rules Recode rules tibble.
#' @param normalizations Normalization rule set (may be NULL / empty).
#' @param data_cols Column names of the loaded data. Needed to resolve a
#'   sibling rule's targets; when NULL, a rule is stamped against its own
#'   `variable` plus every column the normalizations name, which is the most
#'   that can be known without data.
#' @return Character vector, one stamp per rule.
recode_norm_specs <- function(rules, normalizations, data_cols = NULL) {
  if (is.null(rules) || !is.data.frame(rules) || nrow(rules) == 0)
    return(character(0))
  if (is.null(data_cols)) {
    data_cols <- unique(c(as.character(rules$variable),
                          if (is.data.frame(normalizations))
                            as.character(normalizations$variable)))
    data_cols <- data_cols[!is.na(data_cols)]
  }
  vapply(seq_len(nrow(rules)), function(i) {
    cols <- .recode_cols_from_names(data_cols, rules[i, ])
    normalization_spec_string(cols, normalizations)
  }, character(1))
}

#' Stamp a recode rule set with the normalization policy in force.
#'
#' Overwrites `norm_spec` on every row given. Called by make_rules_proxy()'s
#' add(), so a rule records the policy it was authored under.
stamp_recode_norm_spec <- function(rules, normalizations, data_cols = NULL) {
  if (is.null(rules) || !is.data.frame(rules)) return(rules)
  if (nrow(rules) == 0) {
    rules$norm_spec <- character(0)
    return(rules)
  }
  rules$norm_spec <- recode_norm_specs(rules, normalizations, data_cols)
  rules
}


# --- Normalization code generation -------------------------------------------

# The op -> function map. The ONLY thing that has to stay in step with
# apply_normalize_ops(); the agreement test in
# tests/testthat/test-string_helpers.R runs both over the same frame.
.NORMALIZE_FUNS <- c(trim      = "str_trim",
                     squish    = "str_squish",
                     lowercase = "tolower",
                     uppercase = "toupper",
                     titlecase = "str_to_title")

# The nested call for one target token, innermost op first.
.normalize_expr <- function(token, ops) {
  expr <- token
  for (op in .norm_ops_ordered(ops))
    expr <- sprintf("%s(%s)", .NORMALIZE_FUNS[[op]], expr)
  expr
}

# A column name as an R token: bare when syntactic, backticked otherwise.
.r_col_token <- function(name) {
  name <- as.character(name)
  if (!is.na(name) && nzchar(name) && identical(make.names(name), name)) {
    return(name)
  }
  paste0("`", gsub("([`\\\\])", "\\\\\\1", name), "`")
}

# The mutate blocks a normalization set contributes to the generated script.
# Emitted BEFORE the recode blocks, which is the applied order.
#
# Sibling rules use `matches(pattern, ignore.case = FALSE)`: tidyselect's
# matches() ignores case by default, while apply_normalizations() resolves the
# pattern with case-sensitive grep(). Without the flag a pattern could reach a
# differently-capitalized column in the script only.
.normalize_blocks <- function(rules) {
  if (is.null(rules) || !is.data.frame(rules) || nrow(rules) == 0)
    return(character(0))
  blocks <- character(0)
  for (i in seq_len(nrow(rules))) {
    r   <- rules[i, ]
    ops <- .norm_ops_ordered(r$ops)
    if (length(ops) == 0) next
    notes <- if ("notes" %in% names(r)) r$notes else NA_character_
    cmt <- if (!is.na(notes) && nzchar(notes))
             paste0("  # ", gsub("[\r\n]", " ", notes)) else ""
    sp <- clean_sibling_pattern(r$sibling_pattern)
    sp <- if (length(sp) == 1) sp else NA_character_
    siblings <- length(r$apply_to_siblings) == 1 &&
                !is.na(r$apply_to_siblings) && r$apply_to_siblings &&
                !is.na(sp)
    if (siblings && !regex_compiles(sp)) {
      # Reaches no column in the app (.recode_cols_from_names()), so no block
      # here either; validate_normalizations() reports it.
      blocks <- c(blocks,
        sprintf("# SKIPPED: the normalization for the sibling pattern %s. The",
                gsub("[\r\n]", " ", paste(deparse(sp), collapse = ""))),
        "# pattern is not a valid regular expression, so Recode Studio does not",
        "# apply it either. Fix the pattern on the Normalize tab.",
        "")
      next
    }
    if (siblings) {
      blocks <- c(blocks,
        "df <- df |>",
        sprintf(paste0("  mutate(across(matches(%s, ignore.case = FALSE), ",
                       "function(.x) %s))%s"),
                deparse(as.character(sp)),
                .normalize_expr(".x", ops), cmt),
        "")
    } else {
      tok <- .r_col_token(r$variable)
      blocks <- c(blocks,
        "df <- df |>",
        sprintf("  mutate(%s = %s)%s", tok, .normalize_expr(tok, ops), cmt),
        "")
    }
  }
  if (length(blocks) == 0) return(character(0))
  c("# --- Column normalizations ---------------------------------------------------",
    "# Applied BEFORE every recode below, because the recode rules were written",
    "# against normalized values (Recode Studio shows values that way).",
    "# Lossy and not reversible: Title Case turns McKenzie into Mckenzie.",
    "",
    blocks,
    "# --- Recodes ------------------------------------------------------------------",
    "")
}
