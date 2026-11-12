# =============================================================================
# Recode Studio - Stopword tiers (pure R, no Shiny)
# =============================================================================
# Mirrors the Spellcheck dictionary tiers. A token is a stopword when any
# ACTIVE tier lists it:
#
#   stopwords/base_stopwords.txt    committed, seeded from EN_STOPWORDS
#   stopwords/custom_stopwords.txt  committed, project-shared ("project" tier)
#   stopwords/user_stopwords.txt    GITIGNORED, per-user ("user" tier)
#
# If the base file is missing (for example these helpers sourced from a script
# outside the repo), the base tier falls back to the built-in EN_STOPWORDS
# from R/text_helpers.R, so callers keep working.
#
# Used by the Text analysis tab's frequency tables and word cloud. Stopwords
# only change what those views count; they never change data or rules.
# =============================================================================

STOPWORD_DIR <- "stopwords"

# Tier keys, in the order the UI lists them.
STOPWORD_TIERS <- c("base", "project", "user")

# Tiers the app may write to. The base list is edited by hand, not from the UI.
STOPWORD_WRITABLE_TIERS <- c("project", "user")

#' File path of each stopword tier.
#'
#' @param dir folder holding the tier files.
#' @return named list(base, project, user) of file paths.
.stopword_paths <- function(dir = STOPWORD_DIR) {
  list(
    base    = file.path(dir, "base_stopwords.txt"),
    project = file.path(dir, "custom_stopwords.txt"),
    user    = file.path(dir, "user_stopwords.txt")
  )
}

#' Read one stopword list: one lowercase term per line, `#` starts a comment.
#'
#' Same cleaning as the Spellcheck dictionary loader (`.read_dict_terms()`),
#' so a word list pasted from either place reads the same way: trims and
#' lowercases, drops comments and blanks, strips Hunspell affix flags after a
#' slash, and drops a purely numeric line (a `.dic` word-count header) and any
#' line with inner whitespace (the tokenizer never yields a multi-word token,
#' so such an entry could never match).
#'
#' @return unique character vector, in file order. Empty for a missing or
#'   unreadable file (a stopword list that cannot be read must not take the
#'   Text analysis tab down with it).
.read_stopword_terms <- function(path) {
  x <- .read_stopword_lines(path)
  x <- x[!startsWith(trimws(x), "#")]
  x <- tolower(trimws(x))
  x <- sub("/.*$", "", x)
  x <- x[!grepl("^[0-9]+$", x)]
  x <- x[!grepl("\\s", x)]
  unique(x[nzchar(x)])
}

#' The merged stopword list for the requested tiers.
#'
#' @param tiers any of STOPWORD_TIERS. An empty selection means no stopwords.
#' @param dir   folder holding the tier files.
#' @return unique character vector. The base tier reads as EN_STOPWORDS when
#'   its file does not exist.
active_stopwords <- function(tiers = STOPWORD_TIERS, dir = STOPWORD_DIR) {
  paths <- .stopword_paths(dir)
  tiers <- intersect(as.character(tiers), STOPWORD_TIERS)
  terms <- lapply(tiers, function(t) {
    if (t == "base" && !file.exists(paths$base)) return(EN_STOPWORDS)
    .read_stopword_terms(paths[[t]])
  })
  unique(as.character(unlist(terms, use.names = FALSE)))
}

#' Append one line to a text file, starting a new line first if the file does
#' not end with one. Its own function so tests can stand in a writer that
#' fails, loudly or silently.
.append_stopword_line <- function(line, path) {
  size <- suppressWarnings(file.size(path))
  needs_newline <- FALSE
  if (!is.na(size) && size > 0) {
    con <- file(path, open = "rb")
    seek(con, size - 1)
    last <- readBin(con, what = "raw", n = 1L)
    close(con)
    needs_newline <- !identical(last, as.raw(0x0a))
  }
  con <- file(path, open = "a", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  if (needs_newline) writeLines("", con)
  writeLines(line, con)
  invisible(path)
}

#' Add a term to a writable stopword tier.
#'
#' The write is verified by reading the file back: file writes on Windows can
#' fail without an error, so success is reported only when the term is then
#' present exactly once.
#'
#' @param term   one token (lowercased and trimmed here). Must not contain
#'   whitespace.
#' @param tier   "project" or "user".
#' @param dir    folder holding the tier files (created if missing).
#' @param writer function(line, path) that appends `line` to `path`.
#' @return TRUE when the term was added, FALSE when the tier already had it.
#'   Errors, with a plain-language message, when the term is unusable or the
#'   write did not land.
add_stopword <- function(term, tier = STOPWORD_WRITABLE_TIERS,
                         dir = STOPWORD_DIR, writer = .append_stopword_line) {
  tier <- match.arg(tier)
  term <- as.character(term)
  if (length(term) != 1L || is.na(term)) {
    stop("A stopword must be a single word.", call. = FALSE)
  }
  term <- tolower(trimws(term))
  if (!nzchar(term) || grepl("\\s", term) || startsWith(term, "#") ||
      grepl("/", term, fixed = TRUE) || grepl("^[0-9]+$", term)) {
    stop(sprintf("'%s' cannot be stored as a stopword.", term), call. = FALSE)
  }
  path <- .stopword_paths(dir)[[tier]]
  if (term %in% .read_stopword_terms(path)) return(FALSE)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  err <- tryCatch({
    writer(term, path)
    NULL
  },
  error   = function(e) conditionMessage(e),
  warning = function(w) conditionMessage(w))
  if (!is.null(err)) {
    stop(sprintf("Could not write %s (%s).", basename(path), err),
         call. = FALSE)
  }
  n_now <- sum(.read_stopword_terms_all(path) == term)
  if (n_now != 1L) {
    stop(sprintf("'%s' was not saved to %s.", term, basename(path)),
         call. = FALSE)
  }
  TRUE
}

# Like .read_stopword_terms() but without the dedupe, so a verification can
# tell "present once" from "present twice".
.read_stopword_terms_all <- function(path) {
  x <- .read_stopword_lines(path)
  x <- x[!startsWith(trimws(x), "#")]
  x <- tolower(trimws(x))
  x[nzchar(x)]
}

# Raw lines of a tier file; empty when the path is missing, is a folder, or
# cannot be read.
.read_stopword_lines <- function(path) {
  if (!file.exists(path) || dir.exists(path)) return(character(0))
  tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
           error = function(e) character(0))
}
