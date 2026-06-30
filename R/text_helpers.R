# =============================================================================
# Recode Studio — Long-text analysis helpers (pure R, no Shiny)
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
#' @param remove_stopwords  Drop EN_STOPWORDS.
#' @param min_chars         Drop tokens shorter than this.
top_tokens <- function(values, n = 25, remove_stopwords = FALSE, min_chars = 1) {
  toks <- unlist(.tokens_of(values), use.names = FALSE)
  if (min_chars > 1)        toks <- toks[nchar(toks) >= min_chars]
  if (remove_stopwords)     toks <- toks[!toks %in% EN_STOPWORDS]
  if (length(toks) == 0)
    return(tibble::tibble(token = character(0), n = integer(0)))
  tibble::tibble(token = toks) |>
    dplyr::count(token, sort = TRUE, name = "n") |>
    head(n)
}

#' Top n-grams (contiguous word sequences) across a column.
top_ngrams <- function(values, ngram = 2, n = 25,
                       remove_stopwords = FALSE, min_chars = 1) {
  per <- .tokens_of(values)
  grams <- unlist(lapply(per, function(t) {
    if (min_chars > 1)    t <- t[nchar(t) >= min_chars]
    if (remove_stopwords) t <- t[!t %in% EN_STOPWORDS]
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
#' @return tibble(before, keyword, after) — at most `max` rows.
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
