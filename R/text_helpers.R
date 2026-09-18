# =============================================================================
# Recode Studio - Long-text analysis helpers (pure R, no Shiny)
# =============================================================================
# Tools for columns that hold sentences / paragraphs rather than short labels:
# length classification, tokenization, token + n-gram frequency, and
# keyword-in-context (KWIC). Used by mod_text_analysis and build_meta().
# =============================================================================

# A compact English stopword list (for optional removal in frequency views).
EN_STOPWORDS <- c(
  "a","an","and","are","as","at","be","been","being","but","by","for","from",
  "had","has","have","he","her","hers","him","his","i","if","in","into","is",
  "it","its","me","my","no","nor","not","of","off","on","or","our","ours","out",
  "over","own","she","so","some","such","than","that","the","their","theirs",
  "them","then","there","these","they","this","those","to","too","under","until",
  "up","very","was","we","were","what","when","where","which","while","who",
  "whom","why","will","with","would","you","your","yours"
)

#' Tokenize a character vector into per-value word vectors.
#'
#' Lowercases (optional), replaces non-alphanumeric runs (apostrophes kept)
#' with spaces, and splits on whitespace. Returns a list parallel to `x`.
.tokens_of <- function(x, lower = TRUE) {
  x <- as.character(x)
  if (lower) x <- tolower(x)
  x <- gsub("[^[:alnum:]']+", " ", x)
  x <- stringr::str_squish(x)
  lapply(strsplit(x, " ", fixed = TRUE), function(t) t[nzchar(t)])
}

#' Count sentences in each value (>= 1 for any non-empty text).
count_sentences <- function(x) {
  has_text <- !is.na(x) & nzchar(trimws(x))
  term <- stringr::str_count(x, "[.!?]+(?=\\s|$)")
  ifelse(has_text, pmax(1L, term), 0L)
}

#' Classify a column's values as "short" or "long".
#'
#' "long" when the typical value reads like a sentence/paragraph: median word
#' count >= 12, OR at least 30% of values contain two or more sentences.
classify_text_length <- function(values) {
  v <- values[!is.na(values) & nzchar(values)]
  if (length(v) == 0) return("short")
  med_words <- stats::median(lengths(.tokens_of(v)))
  multi_sentence_share <- mean(count_sentences(v) >= 2)
  if (med_words >= 12 || multi_sentence_share >= 0.30) "long" else "short"
}

#' Per-value length metrics (chars / words / sentences).
text_length_stats <- function(values) {
  v <- as.character(values)
  tibble::tibble(
    chars     = ifelse(is.na(v), 0L, nchar(v)),
    words     = lengths(.tokens_of(v)),
    sentences = count_sentences(v)
  )
}

#' Top tokens across a column.
#'
#' @param values            Character vector (all rows, not just unique).
#' @param n                 Max rows to return.
#' @param remove_stopwords  Drop the tokens listed in `stopwords`.
#' @param min_chars         Drop tokens shorter than this.
#' @param stopwords         The list `remove_stopwords` drops. Defaults to the
#'   built-in EN_STOPWORDS; the Text analysis tab passes the active stopword
#'   tiers (see R/stopword_helpers.R).
top_tokens <- function(values, n = 25, remove_stopwords = FALSE, min_chars = 1,
                       stopwords = EN_STOPWORDS) {
  toks <- unlist(.tokens_of(values), use.names = FALSE)
  if (min_chars > 1)        toks <- toks[nchar(toks) >= min_chars]
  if (remove_stopwords)     toks <- toks[!toks %in% stopwords]
  if (length(toks) == 0)
    return(tibble::tibble(token = character(0), n = integer(0)))
  tibble::tibble(token = toks) |>
    dplyr::count(token, sort = TRUE, name = "n") |>
    head(n)
}

#' Top n-grams (contiguous word sequences) across a column.
#'
#' Tokens dropped by `min_chars` / `stopwords` are removed BEFORE the n-grams
#' are formed, so "alpha the gamma" with "the" removed yields "alpha gamma".
#' `stopwords` works as in top_tokens().
top_ngrams <- function(values, ngram = 2, n = 25,
                       remove_stopwords = FALSE, min_chars = 1,
                       stopwords = EN_STOPWORDS) {
  per <- .tokens_of(values)
  grams <- unlist(lapply(per, function(t) {
    if (min_chars > 1)    t <- t[nchar(t) >= min_chars]
    if (remove_stopwords) t <- t[!t %in% stopwords]
    if (length(t) < ngram) return(character(0))
    vapply(seq_len(length(t) - ngram + 1),
           function(i) paste(t[i:(i + ngram - 1)], collapse = " "),
           character(1))
  }), use.names = FALSE)
  if (length(grams) == 0)
    return(tibble::tibble(ngram = character(0), n = integer(0)))
  tibble::tibble(ngram = grams) |>
    dplyr::count(ngram, sort = TRUE, name = "n") |>
    head(n)
}

#' Keyword-in-context: matches of `term` with surrounding word windows.
#'
#' @return tibble(before, keyword, after), at most `max` rows.
kwic <- function(values, term, window = 6, ignore_case = TRUE, max = 100) {
  empty <- tibble::tibble(before = character(0), keyword = character(0),
                          after = character(0))
  if (is.null(term) || !nzchar(trimws(term))) return(empty)
  term <- trimws(term)
  toks_list <- .tokens_of(values, lower = FALSE)
  before <- keyword <- after <- character(0)
  for (toks in toks_list) {
    if (length(toks) == 0) next
    hits <- grep(term, toks, ignore.case = ignore_case, perl = TRUE)
    for (h in hits) {
      b <- if (h > 1) toks[max(1, h - window):(h - 1)] else character(0)
      a <- if (h < length(toks)) toks[(h + 1):min(length(toks), h + window)] else character(0)
      before  <- c(before,  paste(b, collapse = " "))
      keyword <- c(keyword, toks[h])
      after   <- c(after,   paste(a, collapse = " "))
      if (length(keyword) >= max) break
    }
    if (length(keyword) >= max) break
  }
  tibble::tibble(before = before, keyword = keyword, after = after)
}

# --- Language detection -------------------------------------------------------

# Values shorter than this are not classified: detection on a few characters
# is a guess, and a wrong guess reads as a fact in the table.
LANG_MIN_CHARS <- 20

#' Is per-value language detection available in this R session?
#'
#' `cld2` is an optional runtime dependency (compiled). Its own function so the
#' UI can show an install hint and tests can stand in an unavailable package.
lang_detection_available <- function() {
  requireNamespace("cld2", quietly = TRUE)
}

#' Detect the language of each value.
#'
#' Wraps `cld2::detect_language()`. When `cld2` is not installed this returns
#' all-NA rather than erroring, so the Text analysis tab stays usable on an
#' install that skipped the compiled package.
#'
#' @param values    character vector (any length, NA allowed).
#' @param min_chars values with fewer characters (after trimming) are NA.
#' @return character vector parallel to `values`: an ISO 639-1 code such as
#'   "en", or NA when the value is missing, too short, or undetectable.
detect_languages <- function(values, min_chars = LANG_MIN_CHARS) {
  v   <- as.character(values)
  out <- rep(NA_character_, length(v))
  if (length(v) == 0L || !lang_detection_available()) return(out)
  ok <- !is.na(v) & nchar(trimws(v)) >= min_chars
  if (!any(ok)) return(out)
  got <- tryCatch(as.character(cld2::detect_language(v[ok])),
                  error = function(e) rep(NA_character_, sum(ok)))
  out[ok] <- got
  out
}

#' Language breakdown for a column.
#'
#' @return tibble(language, n, share), sorted by n descending. Values that
#'   could not be classified are counted as "(undetected)". `share` is a
#'   proportion (0 to 1) and sums to 1; format it for display, do not store it
#'   as text.
language_summary <- function(values, min_chars = LANG_MIN_CHARS) {
  summarise_languages(detect_languages(values, min_chars = min_chars))
}

#' Language breakdown from already-detected codes.
#'
#' The counting half of language_summary(), for callers that hold the codes
#' already and should not run detection twice.
#'
#' @param langs output of detect_languages().
#' @return tibble(language, n, share), as language_summary().
summarise_languages <- function(langs) {
  if (length(langs) == 0L) {
    return(tibble::tibble(language = character(0), n = integer(0),
                          share = numeric(0)))
  }
  langs[is.na(langs)] <- "(undetected)"
  tibble::tibble(language = langs) |>
    dplyr::count(language, sort = TRUE, name = "n") |>
    dplyr::mutate(share = n / sum(n))
}
