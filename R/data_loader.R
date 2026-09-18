# =============================================================================
# Data loading + metadata (dataset-agnostic)
# =============================================================================
#
# Responsibilities:
#   - Read any CSV / Excel file into an all-character tibble.
#   - Build per-column metadata: NA share, length, free-text-candidate flag.
#   - Derive a generic "group" from a column-name prefix.
#   - Suggest sibling-column regex patterns from a column name.
# =============================================================================

#' Read a dataset (CSV or Excel) into an all-character tibble.
#'
#' @param path  File path.
#' @param sheet For Excel files, the sheet name or index (default 1).
#' @return tibble; every column read as character to avoid type coercion.
read_dataset <- function(path, sheet = 1) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    df <- readxl::read_excel(path, sheet = sheet, col_types = "text")
    tibble::as_tibble(df)
  } else {
    readr::read_csv(
      path,
      show_col_types = FALSE,
      col_types      = readr::cols(.default = "c"),
      locale         = readr::locale(encoding = "UTF-8")
    )
  }
}

#' List sheet names in an Excel file (empty for CSV).
dataset_sheets <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) readxl::excel_sheets(path) else character(0)
}

#' Strip any directory part from a file name, on either separator.
#'
#' `basename()` only splits on the host's separators, so on Linux a Windows
#' style "C:\\dir\\file.csv" would keep its folders. This splits on both.
file_base_name <- function(name) {
  if (length(name) == 0 || is.na(name[1])) return(NA_character_)
  sub("^.*[/\\\\]", "", as.character(name[1]))
}

#' Describe the file a dataset was loaded from, for the export provenance.
#'
#' Computed at LOAD time: an uploaded file lives in a temp path that does not
#' outlast the upload, so its size and checksum cannot be taken later.
#'
#' @param path      Where the bytes are now (an upload's temp `datapath`).
#' @param file_name The name the user knows the file by (an upload's `name`).
#'   Only its base name is kept, never a folder.
#' @param sheet     Excel sheet name or index; ignored for CSV. An index is
#'   resolved to the sheet's name.
#' @param loaded_at Load time (POSIXct). Defaults to now.
#' @return list(file_name, sheet, file_size_bytes, sha256, loaded_at).
#'   `sheet` is NA for a CSV.
source_file_meta <- function(path, file_name = path, sheet = NULL,
                             loaded_at = Sys.time()) {
  is_excel <- tolower(tools::file_ext(path)) %in% c("xlsx", "xls") ||
    tolower(tools::file_ext(file_name)) %in% c("xlsx", "xls")
  sheet_name <- NA_character_
  if (is_excel) {
    s <- if (is.null(sheet) || length(sheet) == 0 || is.na(sheet[1])) 1 else sheet[1]
    if (is.numeric(s)) {
      sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
      sheet_name <- if (length(sheets) >= s) sheets[[s]] else as.character(s)
    } else {
      sheet_name <- as.character(s)
    }
  }
  list(
    file_name       = file_base_name(file_name),
    sheet           = sheet_name,
    file_size_bytes = as.numeric(file.size(path)),
    sha256          = digest::digest(file = path, algo = "sha256"),
    loaded_at       = loaded_at
  )
}

#' Build column metadata for a dataset.
#'
#' A column is a free-text candidate iff ALL hold:
#'   - its NAME does not end in `id` / `_id` (case-insensitive). Identifier
#'     columns are keys, not text to recode, and high-cardinality alphanumeric
#'     ids (e.g. "AB123456") otherwise sail past every content heuristic
#'   - median non-empty value length > 3
#'   - NA / empty share < 60%
#'   - more than 10 unique non-empty values (excludes Yes/No, TRUE/FALSE,
#'     and similar small choice sets)
#'   - does NOT look like a date column (regex, 80% threshold)
#'   - does NOT look like a numeric column (regex, 80% threshold)
#'
#' A free-text candidate is additionally flagged `is_long_text` when its values
#' read like sentences/paragraphs (see classify_text_length); `median_words`
#' and `text_kind` ("short"/"long"/"-") are reported for the picker.
#'
#' `lang_share_en` is the share of a free-text candidate's detectable values
#' that were detected as English (see detect_languages(); up to
#' LANG_SAMPLE_N values are sampled). NA for non-candidates, and NA when
#' nothing was detectable (values too short, or `cld2` not installed). It is
#' reported only: it never affects the candidate flag.
#'
#' @param df  tibble from read_dataset().
#' @return tibble of per-column meta.
build_meta <- function(df) {
  n <- nrow(df)
  empty <- tibble::tibble(
    column = character(0), group = character(0), n_unique = integer(0),
    na_share = numeric(0), median_len = numeric(0), median_words = numeric(0),
    is_free_text_candidate = logical(0), is_long_text = logical(0),
    text_kind = character(0), lang_share_en = numeric(0)
  )
  if (n == 0 || ncol(df) == 0) return(empty)

  purrr::map_dfr(names(df), function(col) {
    x <- as.character(df[[col]])
    non_na <- x[!is.na(x) & nzchar(x)]
    lens <- if (length(non_na) > 0) nchar(non_na) else 0
    n_unique <- length(unique(non_na))
    na_share <- if (n > 0) sum(is.na(x) | !nzchar(x)) / n else 0
    median_len <- if (length(lens) > 0) stats::median(lens) else 0

    # Cheap structural check first: identifier columns are keys, never free
    # text. Deliberately anchored as `(^|_)id$` so `id` / `case_id` match but
    # `paid` / `grid` / `valid` do not.
    is_candidate <- !.is_id_name(col) &&
                    median_len > 3 &&
                    na_share < 0.6 &&
                    n_unique > 10

    if (is_candidate) {
      if (.is_date_like(non_na) || .is_numeric_like(non_na)) {
        is_candidate <- FALSE
      }
    }

    # Long-text detection only matters for free-text candidates.
    if (is_candidate) {
      median_words <- stats::median(lengths(.tokens_of(non_na)))
      is_long      <- classify_text_length(non_na) == "long"
      kind         <- if (is_long) "long" else "short"
      lang_en      <- .lang_share_en(non_na)
    } else {
      median_words <- 0
      is_long      <- FALSE
      kind         <- "-"
      lang_en      <- NA_real_
    }

    tibble::tibble(
      column                 = col,
      group                  = column_group(col),
      n_unique               = n_unique,
      na_share               = na_share,
      median_len             = median_len,
      median_words           = median_words,
      is_free_text_candidate = is_candidate,
      is_long_text           = is_long,
      text_kind              = kind,
      lang_share_en          = lang_en
    )
  })
}

# --- Internal classifiers ----------------------------------------------------

# How many values build_meta() hands to language detection per column.
LANG_SAMPLE_N <- 200

# Share of a column's detectable values detected as English, from the first
# `sample_n` non-empty values (the same head-of-column sampling the date and
# number checks use). NA when nothing was detectable.
.lang_share_en <- function(values, sample_n = LANG_SAMPLE_N) {
  v <- values[!is.na(values) & nzchar(values)]
  if (length(v) == 0L) return(NA_real_)
  langs <- detect_languages(utils::head(v, sample_n))
  langs <- langs[!is.na(langs)]
  if (length(langs) == 0L) return(NA_real_)
  mean(langs == "en")
}

# Identifier-named: the column name is `id` or ends in `_id` (case-insensitive).
# NOTE: this repo deliberately does NOT also exclude names ending in "label"
# (the upstream copy does). "label" there means a coded-value twin column;
# in a domain-neutral tool a column called `product_label` or `warning_label`
# is plausibly real free text worth recoding. "id", by contrast, is a near-universal key convention.
.is_id_name <- function(col) {
  grepl("(^|_)id$", col, ignore.case = TRUE)
}

# Date-like: 80%+ of (up to 200 sampled) values match a common date pattern.
.is_date_like <- function(values) {
  if (length(values) == 0) return(FALSE)
  v <- head(values, 200)
  date_pat <- paste0(
    "^",
    "(",
      "\\d{1,4}[/-]\\d{1,2}[/-]\\d{1,4}",            # YYYY-MM-DD or M/D/YYYY
      "|",
      "\\d{1,2}[ -][A-Za-z]{3,9}[ -]\\d{2,4}",       # 4-Jan-2026
    ")",
    "([ T]\\d{1,2}:\\d{2}([:.]\\d+)?Z?)?",           # optional time
    "$"
  )
  mean(grepl(date_pat, v)) > 0.8
}

# Number-like: 80%+ of (up to 200 sampled) values match a plain numeric pattern.
.is_numeric_like <- function(values) {
  if (length(values) == 0) return(FALSE)
  v <- head(values, 200)
  num_pat <- "^-?\\d+(\\.\\d+)?$"
  mean(grepl(num_pat, v)) > 0.8
}

#' Derive a generic group label from a column name's prefix.
#'
#' Returns the token before the first underscore (e.g. "shop_drink1" ->
#' "shop"). Columns with no underscore return "(none)". Useful for datasets
#' whose columns are namespaced by a source-table prefix; harmless otherwise.
column_group <- function(cols) {
  m <- stringr::str_match(cols, "^([^_]+)_")
  out <- m[, 2]
  out[is.na(out)] <- "(none)"
  out
}

#' Suggest a sibling-column regex pattern from a column name.
#'
#' Looks for a trailing digit run. If found and the family has >= 2 members
#' in `all_cols`, returns a regex matching the family (e.g. `drink1` ->
#' `^drink[0-9]+$`). Otherwise returns NA_character_.
suggest_sibling_pattern <- function(col_name, all_cols) {
  m <- stringr::str_match(col_name, "^(.*?)(\\d+)$")
  if (is.na(m[1, 2])) return(NA_character_)
  stem <- m[1, 2]
  # Escape regex metacharacters in the stem so arbitrary column names are safe.
  stem_escaped <- stringr::str_replace_all(stem, "([.^$*+?()\\[\\]{}|\\\\])", "\\\\\\1")
  pattern <- paste0("^", stem_escaped, "[0-9]+$")
  hits <- grep(pattern, all_cols, value = TRUE)
  if (length(hits) >= 2) pattern else NA_character_
}
