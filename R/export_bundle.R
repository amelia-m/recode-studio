# =============================================================================
# Recode Studio - Cleaned data export bundle
# =============================================================================
#
# Pure R helpers (no Shiny). Requires string_helpers.R, recat_helpers.R,
# rule_warnings.R and data_loader.R (file_base_name()) to be sourced first.
#
# The bundle is a zip the user downloads: a cleaned COPY of the loaded data,
# the three rule files, one R script that reproduces the cleaned copy from the
# original file, and a provenance record (JSON for tools, plain text for
# people). The original file is never touched: everything is built in a fresh
# temp directory and zipped from there.
#
# ORDER, fixed: normalize, then recode, then recategorize. The same order as
# the Preview & export tab and the generated scripts.
#
# ONE SCRIPT, not two. The bundle carries a single `clean_<dataset_id>.R` that
# reads the original file, normalizes, recodes, recategorizes and writes a
# CSV. A spreadsheet-fluent user then has one instruction ("put your original
# file next to this script and run it"), not "run this script, then that one,
# on a data frame you have to load yourself". The body is the output of
# generate_recode_R() and generate_recat_R() verbatim, so the agreement those
# generators already guarantee carries over unchanged.
#
# NA ENCODING of the cleaned CSV: missing values are written as EMPTY cells.
# read_dataset() reads both "" and "NA" as missing, so an empty cell reads
# back as missing in this app, in readr, in pandas and in a spreadsheet, while
# the text "NA" is ambiguous to several of those. The cost: an empty-string
# cell and a missing cell are indistinguishable in the CSV. The rule files keep
# their own `<NA>` convention.
#
# Public functions:
#   bundle_dataset_id(file_name)            filesystem-safe id from a file name
#   bundle_file_names(dataset_id)           the files a bundle contains
#   app_version_from_description(path)      Version: from DESCRIPTION, or NA
#   build_cleaned_dataset(df, rules, recat, normalizations = NULL)
#   build_provenance(source_meta, df_raw, cleaned, rules, recat,
#                    normalizations, app_version)
#   provenance_text(prov)
#   generate_clean_script(rules, recat, normalizations, dataset_id,
#                         source_file_name, sheet, sha256, data_cols)
#   write_export_bundle(dir, cleaned, prov, rules, recat, normalizations,
#                       dataset_id)
#   export_bundle_zip(zipfile, df_raw, source_meta, rules, recat,
#                     normalizations, app_version)
# =============================================================================

# How the cleaned CSV writes a missing value. Single-sourced for the writer,
# the reproduction script and the provenance text.
BUNDLE_CSV_NA <- ""

# --- Names --------------------------------------------------------------------

#' Filesystem-safe dataset id from a file name.
#'
#' Drops any folder and the extension, replaces every run of characters other
#' than letters, digits, `_` and `-` with `_`, and trims `_` from the ends.
#' "Cafe orders (March).xlsx" -> "Cafe_orders_March". Falls back to "dataset".
bundle_dataset_id <- function(file_name) {
  nm <- file_base_name(file_name)
  if (is.na(nm) || !nzchar(nm)) return("dataset")
  id <- tools::file_path_sans_ext(nm)
  id <- gsub("[^A-Za-z0-9_-]+", "_", id)
  id <- gsub("^_+|_+$", "", id)
  if (!nzchar(id)) "dataset" else id
}

#' The files a bundle contains, with a one-line description of each.
#'
#' @return tibble(file, description), in the order they are written.
bundle_file_names <- function(dataset_id) {
  tibble::tibble(
    file = c(
      sprintf("%s_cleaned.csv", dataset_id),
      "normalizations_master.csv",
      "recodes_master.csv",
      sprintf("recat_%s.csv", dataset_id),
      sprintf("clean_%s.R", dataset_id),
      "provenance.json",
      "provenance.txt"
    ),
    description = c(
      "The cleaned copy of the data.",
      "The column normalization rules (applied first).",
      "The recode rules (applied second).",
      "The recategorization rules (applied third).",
      "R script that rebuilds the cleaned copy from the original file.",
      "This record, for software.",
      "This record, for people."
    )
  )
}

#' The app version from a DESCRIPTION file, or NA when it cannot be read.
app_version_from_description <- function(path = "DESCRIPTION") {
  if (!file.exists(path)) return(NA_character_)
  v <- tryCatch(read.dcf(path, fields = "Version")[1, 1],
                error = function(e) NA_character_)
  if (is.null(v) || length(v) == 0) NA_character_ else unname(as.character(v))
}

# ISO 8601 local time with offset, e.g. 2026-01-01T09:30:00-0700.
.bundle_time <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  if (is.character(x)) return(x[1])
  if (all(is.na(x))) return(NA_character_)
  format(x[1], "%Y-%m-%dT%H:%M:%S%z")
}

# --- Pipeline -------------------------------------------------------------------

.stop_if_rows_changed <- function(n_expected, df, step) {
  if (nrow(df) != n_expected) {
    stop(sprintf(paste0(
      "The %s step changed the number of rows from %d to %d. Cleaning must ",
      "never add or drop rows, so the export was stopped."),
      step, n_expected, nrow(df)), call. = FALSE)
  }
}

#' Clean a data frame: normalize, then recode, then recategorize.
#'
#' Each step is applied exactly once. The rule warnings are assembled from the
#' same intermediate results with the helpers collect_rule_warnings() is built
#' from, in the same order, so they equal collect_rule_warnings() without
#' applying the pipeline a second time (a test pins the equality).
#'
#' Fails loudly if any step changes the row count.
#'
#' @param df    The raw data (all-character tibble from read_dataset()).
#' @param rules Recode rule tibble (NULL = none).
#' @param recat Recat rule tibble (NULL = none).
#' @param normalizations Normalization rule tibble (NULL = none).
#' @return list(df, normalization_summary, recode_summary, recat_summary,
#'   recat_columns_added, warnings).
build_cleaned_dataset <- function(df, rules, recat, normalizations = NULL) {
  if (is.null(rules)) rules <- empty_recodes_tibble()
  if (is.null(recat)) recat <- empty_recat_tibble()
  if (is.null(normalizations) || !is.data.frame(normalizations))
    normalizations <- empty_normalizations_tibble()
  n <- nrow(df)

  nres <- apply_normalizations(df, normalizations)
  .stop_if_rows_changed(n, nres$df, "normalization")
  rec  <- apply_recodes(nres$df, rules)
  .stop_if_rows_changed(n, rec$df, "recode")
  cat_res <- apply_recat(rec$df, recat)
  .stop_if_rows_changed(n, cat_res$df, "recategorization")

  warnings <- dplyr::bind_rows(
    empty_rule_warnings(),
    .warn_normalizations(normalizations, nres$summary),
    .warn_recodes_no_match_from_summary(nres$df, rules, rec$summary),
    .warn_norm_spec_drift(rules, normalizations, names(df)),
    warn_recat_terms_recoded_away(nres$df, rec$df, recat)
  )

  list(
    df                    = cat_res$df,
    normalization_summary = nres$summary,
    recode_summary        = rec$summary,
    recat_summary         = cat_res$summary,
    recat_columns_added   = setdiff(names(cat_res$df), names(df)),
    warnings              = warnings
  )
}

# --- Provenance -----------------------------------------------------------------

.n_rules <- function(x) if (is.null(x) || !is.data.frame(x)) 0L else nrow(x)

.sum_col <- function(tbl, col) {
  if (is.null(tbl) || !is.data.frame(tbl) || !(col %in% names(tbl))) return(0L)
  as.integer(sum(tbl[[col]], na.rm = TRUE))
}

#' Provenance record for a bundle.
#'
#' @param source_meta list from source_file_meta() (file_name, sheet,
#'   file_size_bytes, sha256, loaded_at). Only the file's BASE name is kept.
#' @param df_raw  The raw data the pipeline started from.
#' @param cleaned build_cleaned_dataset() result for `df_raw`.
#' @param rules,recat,normalizations The rule sets it was built with.
#' @param app_version App version string (see app_version_from_description()),
#'   or NA. Passed in so this function reads no files.
#' @return Named list; see the fields below.
build_provenance <- function(source_meta, df_raw, cleaned, rules, recat,
                             normalizations, app_version) {
  if (nrow(cleaned$df) != nrow(df_raw)) {
    stop(sprintf(paste0(
      "The cleaned data has %d rows but the original has %d. Cleaning must ",
      "never add or drop rows, so the export was stopped."),
      nrow(cleaned$df), nrow(df_raw)), call. = FALSE)
  }
  sm <- if (is.null(source_meta)) list() else source_meta
  get <- function(nm, default = NA) {
    v <- sm[[nm]]
    if (is.null(v) || length(v) == 0) default else v
  }
  file_name  <- file_base_name(get("file_name", NA_character_))
  dataset_id <- bundle_dataset_id(file_name)
  w <- cleaned$warnings
  if (is.null(w)) w <- empty_rule_warnings()

  list(
    original_file_name              = file_name,
    sheet                           = as.character(get("sheet", NA_character_)),
    file_size_bytes                 = as.numeric(get("file_size_bytes", NA_real_)),
    input_sha256                    = as.character(get("sha256", NA_character_)),
    source_loaded_at                = .bundle_time(get("loaded_at", NULL)),
    exported_at                     = .bundle_time(Sys.time()),
    # No `%||%` here: it lives in ui_helpers.R and this file is Shiny-free.
    app_version                     = if (is.null(app_version) ||
                                          length(app_version) == 0)
                                        NA_character_
                                      else as.character(app_version[1]),
    rows_before                     = nrow(df_raw),
    columns_before                  = ncol(df_raw),
    rows_after                      = nrow(cleaned$df),
    columns_after                   = ncol(cleaned$df),
    recat_columns_added             = I(as.character(cleaned$recat_columns_added)),
    normalization_rules_applied     = .n_rules(normalizations),
    recode_rules_applied            = .n_rules(rules),
    recat_rules_applied             = .n_rules(recat),
    cells_changed_by_normalizations = .sum_col(cleaned$normalization_summary,
                                               "cells_changed"),
    cells_changed_by_recodes        = .sum_col(cleaned$recode_summary,
                                               "cells_changed"),
    cleaned_csv_missing_values      = "empty cell",
    cleaned_csv_encoding            = "UTF-8",
    warnings                        = tibble::tibble(
                                        kind    = as.character(w$kind),
                                        message = as.character(w$message)),
    files                           = bundle_file_names(dataset_id)
  )
}

.txt_value <- function(x, unit = "") {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return("(not recorded)")
  paste0(format(x, big.mark = ",", scientific = FALSE, trim = TRUE), unit)
}

#' Plain-language rendering of a provenance record, for people.
#'
#' @param prov build_provenance() result.
#' @return A single string (lines joined with "\n").
provenance_text <- function(prov) {
  id    <- bundle_dataset_id(prov$original_file_name)
  fname <- prov$original_file_name
  added <- as.character(prov$recat_columns_added)
  w     <- prov$warnings
  n_w   <- if (is.null(w)) 0L else nrow(w)

  field <- function(label, value) sprintf("%-38s %s", paste0(label, ":"), value)

  header <- c(
    "RECODE STUDIO: CLEANED DATA EXPORT",
    "==================================",
    "",
    "What this is",
    "------------",
    "This folder holds a cleaned COPY of your data, the rules used to clean it,",
    "and an R script that repeats the cleaning. Your original file was not",
    "changed.",
    "",
    "The cleaning ran in three steps, always in this order:",
    "  1. Column normalizations (tidy spaces and capitalization)",
    "  2. Recodes (replace one value with another)",
    "  3. Recategorization (add new category columns)",
    "",
    "How to repeat the cleaning",
    "--------------------------",
    sprintf("1. Put your original file, %s, in R's working folder,",
            if (is.na(fname)) "(file name not recorded)" else fname),
    sprintf("   or edit the `input_file` line at the top of clean_%s.R.", id),
    sprintf("2. Open clean_%s.R in R, Positron or RStudio and run the whole", id),
    "   script (Source).",
    sprintf("3. It writes %s_cleaned_reproduced.csv next to the original file.", id),
    sprintf("   That file should match %s_cleaned.csv in this folder.", id),
    "The script warns you if the file it reads is not the one recorded below.",
    "",
    "About the cleaned CSV",
    "---------------------",
    "Text is UTF-8. A missing value is an EMPTY cell (no \"NA\" text), which",
    "spreadsheets, R and Python all read as missing. An empty-text value and a",
    "missing value therefore look the same in this file.",
    ""
  )

  details <- c(
    "Details",
    "-------",
    field("Original file name", .txt_value(fname)),
    field("Excel sheet", if (is.na(prov$sheet)) "(not an Excel file)" else prov$sheet),
    field("File size", .txt_value(prov$file_size_bytes, " bytes")),
    field("File fingerprint (SHA-256)", .txt_value(prov$input_sha256)),
    field("Data loaded at", .txt_value(prov$source_loaded_at)),
    field("Exported at", .txt_value(prov$exported_at)),
    field("Recode Studio version", .txt_value(prov$app_version)),
    field("Rows before / after",
          sprintf("%s / %s", .txt_value(prov$rows_before),
                  .txt_value(prov$rows_after))),
    field("Columns before / after",
          sprintf("%s / %s", .txt_value(prov$columns_before),
                  .txt_value(prov$columns_after))),
    field("Columns added by recategorization",
          if (length(added) == 0) "(none)" else paste(added, collapse = ", ")),
    field("Normalization rules applied", .txt_value(prov$normalization_rules_applied)),
    field("Recode rules applied", .txt_value(prov$recode_rules_applied)),
    field("Recategorization rules applied", .txt_value(prov$recat_rules_applied)),
    field("Cells changed by normalizations",
          .txt_value(prov$cells_changed_by_normalizations)),
    field("Cells changed by recodes", .txt_value(prov$cells_changed_by_recodes)),
    ""
  )

  files <- prov$files
  file_lines <- c(
    "Files in this folder",
    "--------------------",
    if (!is.null(files) && nrow(files) > 0)
      sprintf("  %-34s %s", files$file, files$description),
    ""
  )

  warn_lines <- c(
    sprintf("Warnings (%d)", n_w),
    "------------",
    if (n_w == 0) "None. No rule was found to do less than expected."
    else paste0("- ", w$message),
    ""
  )

  paste(c(header, details, file_lines, warn_lines), collapse = "\n")
}

# --- Script -----------------------------------------------------------------------

#' The single R script that rebuilds the cleaned copy from the original file.
#'
#' Reads the file exactly as read_dataset() does (every column as text), then
#' runs the generate_recode_R() body (normalizations, then recodes) and the
#' generate_recat_R() body, and writes `<dataset_id>_cleaned_reproduced.csv`
#' next to the input with the bundle's NA encoding.
#'
#' @param data_cols Column names of the raw data, used to expand recat sibling
#'   patterns at generation time.
#' @return A single string.
generate_clean_script <- function(rules, recat, normalizations, dataset_id,
                                  source_file_name, sheet = NA_character_,
                                  sha256 = NA_character_, data_cols = NULL) {
  if (is.null(rules)) rules <- empty_recodes_tibble()
  if (is.null(recat)) recat <- empty_recat_tibble()
  fname    <- file_base_name(source_file_name)
  if (is.na(fname)) fname <- "your_original_file.csv"
  is_excel <- tolower(tools::file_ext(fname)) %in% c("xlsx", "xls")
  sheet_arg <- if (is.na(sheet)) "1" else deparse(as.character(sheet))

  read_block <- if (is_excel) {
    c("df <- tibble::as_tibble(",
      sprintf("  readxl::read_excel(input_file, sheet = %s, col_types = \"text\")",
              sheet_arg),
      ")")
  } else {
    c("df <- readr::read_csv(",
      "  input_file,",
      "  show_col_types = FALSE,",
      "  col_types      = readr::cols(.default = \"c\"),",
      "  locale         = readr::locale(encoding = \"UTF-8\")",
      ")")
  }

  header <- c(
    "# =============================================================================",
    sprintf("# Recode Studio cleaning script -- dataset: %s", dataset_id),
    sprintf("# Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "#",
    "# HOW TO USE",
    "#   1. Put the original file in R's working folder, or change `input_file`",
    "#      below to the file's full location.",
    "#   2. Run this whole script (Source).",
    sprintf("#   3. It writes %s_cleaned_reproduced.csv next to the original file.",
            dataset_id),
    "#",
    "# Steps, in order: read the file (every column as text), normalize columns,",
    "# recode values, add recategorization columns, write the CSV.",
    "# Your original file is only read, never changed.",
    "# =============================================================================",
    "",
    "library(dplyr)",
    "library(stringr)",
    "library(tidyr)",
    "",
    sprintf("input_file  <- %s", deparse(fname)),
    sprintf("output_file <- file.path(dirname(input_file), %s)",
            deparse(sprintf("%s_cleaned_reproduced.csv", dataset_id))),
    sprintf("expected_sha256 <- %s", deparse(as.character(sha256))),
    "",
    "if (!file.exists(input_file)) {",
    "  stop(\"Cannot find \", input_file, \". Put it in the working folder \",",
    "       \"(see getwd()) or edit `input_file` above.\", call. = FALSE)",
    "}",
    "if (!is.na(expected_sha256) && requireNamespace(\"digest\", quietly = TRUE)) {",
    "  if (!identical(digest::digest(file = input_file, algo = \"sha256\"),",
    "                 expected_sha256)) {",
    "    warning(\"This file is not byte-for-byte the file the rules were \",",
    "            \"exported from. The result may differ.\", call. = FALSE)",
    "  }",
    "}",
    "",
    "# --- Read the original file ---------------------------------------------------",
    read_block,
    "",
    "n_rows_in <- nrow(df)",
    ""
  )

  recode_part <- generate_recode_R(rules, dataset_id = dataset_id,
                                   normalizations = normalizations)
  recat_part  <- generate_recat_R(recat, dataset_id = dataset_id,
                                  data_cols = data_cols)

  footer <- c(
    "",
    "# --- Write the cleaned copy ---------------------------------------------------",
    "stopifnot(nrow(df) == n_rows_in)",
    sprintf("readr::write_csv(df, output_file, na = %s)", deparse(BUNDLE_CSV_NA)),
    "message(\"Wrote \", output_file)",
    ""
  )

  paste(c(header, recode_part, "", recat_part, footer), collapse = "\n")
}

# --- Writing ------------------------------------------------------------------------

#' Write a bundle's files into `dir`.
#'
#' @param dir     Directory to write into (created if needed). Callers pass a
#'   fresh temp directory.
#' @param cleaned build_cleaned_dataset() result.
#' @param prov    build_provenance() result.
#' @param rules,recat,normalizations The rule sets. Each rules file is written
#'   even when empty, so every bundle has the same shape.
#' @param dataset_id bundle_dataset_id() of the original file name.
#' @return Character vector of the file names written (relative to `dir`).
write_export_bundle <- function(dir, cleaned, prov, rules, recat,
                                normalizations, dataset_id) {
  if (is.null(rules)) rules <- empty_recodes_tibble()
  if (is.null(recat)) recat <- empty_recat_tibble()
  if (is.null(normalizations) || !is.data.frame(normalizations))
    normalizations <- empty_normalizations_tibble()
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  files <- bundle_file_names(dataset_id)$file
  path  <- function(f) file.path(dir, f)

  readr::write_csv(cleaned$df, path(files[1]), na = BUNDLE_CSV_NA)
  write_normalizations(normalizations, path(files[2]))
  write_recodes(rules, path(files[3]))
  write_recat(recat, path(files[4]))

  data_cols <- setdiff(names(cleaned$df), cleaned$recat_columns_added)
  script <- generate_clean_script(rules, recat, normalizations,
                                  dataset_id       = dataset_id,
                                  source_file_name = prov$original_file_name,
                                  sheet            = prov$sheet,
                                  sha256           = prov$input_sha256,
                                  data_cols        = data_cols)
  writeLines(script, path(files[5]), useBytes = TRUE)

  json <- jsonlite::toJSON(prov, pretty = TRUE, auto_unbox = TRUE,
                           na = "null", null = "null", digits = NA)
  writeLines(json, path(files[6]), useBytes = TRUE)
  writeLines(provenance_text(prov), path(files[7]), useBytes = TRUE)

  files
}

#' Build a complete bundle and zip it to `zipfile`.
#'
#' Writes only into a fresh `tempfile()` directory and to `zipfile`. Uses the
#' `zip` package, which needs no external zip program.
#'
#' @param zipfile Destination path of the zip (e.g. a downloadHandler `file`).
#' @param df_raw  The loaded data.
#' @param source_meta source_file_meta() list, or NULL (fields then read
#'   "not recorded").
#' @return Invisibly, the file names inside the zip.
export_bundle_zip <- function(zipfile, df_raw, source_meta, rules, recat,
                              normalizations, app_version = NA_character_) {
  cleaned <- build_cleaned_dataset(df_raw, rules, recat, normalizations)
  prov    <- build_provenance(source_meta, df_raw, cleaned, rules, recat,
                              normalizations, app_version)
  id  <- bundle_dataset_id(prov$original_file_name)
  dir <- tempfile("recode_bundle_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  files <- write_export_bundle(dir, cleaned, prov, rules, recat,
                               normalizations, id)
  zip::zip(zipfile = zipfile, files = files, root = dir)
  invisible(files)
}
