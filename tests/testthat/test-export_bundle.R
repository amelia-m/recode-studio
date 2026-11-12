# Run with: testthat::test_dir("tests/testthat")
#
# Cleaned data export bundle (R/export_bundle.R): the normalize -> recode ->
# recat pipeline, the provenance record, the files written, the zip, the
# reproduction script, and the wiring on the Data and Preview & export tabs.

source(file.path("..", "..", "R", "ui_helpers.R"))
source(file.path("..", "..", "R", "text_helpers.R"))
source(file.path("..", "..", "R", "data_loader.R"))
source(file.path("..", "..", "R", "string_helpers.R"))
source(file.path("..", "..", "R", "recat_helpers.R"))
source(file.path("..", "..", "R", "rule_warnings.R"))
source(file.path("..", "..", "R", "export_bundle.R"))
source(file.path("..", "..", "R", "mod_data_input.R"))
source(file.path("..", "..", "R", "mod_preview_export.R"))

# --- Fixture --------------------------------------------------------------------
#
# Built so the ORDER matters at every step:
#   - normalizations lower-case and squish, which is what lets the `exact`
#     recodes ("expresso", "portand", "oat milk") match at all;
#   - the recodes rewrite "portand" to "portland", which is what lets the
#     "Portland area" recat rule match rows 1, 3 and 6;
#   - the recodes rewrite "expresso" to "espresso", so the recat rule written
#     against "expresso" loses every row (a recat_terms_recoded_away warning).
# `drink notes` is a non-syntactic name; drink1 / drink2 are a sibling family.

.fx_df <- function() {
  tibble::tibble(
    order_id      = as.character(1:6),
    drink1        = c("Expresso", "  LATTE ", "expresso", "mocha", NA, "cap"),
    drink2        = c("latte", "EXPRESSO", NA, "Mocha ", "cap", "latte"),
    `drink notes` = c("OAT  milk", "extra hot", NA, "oat milk", "no whip", "Decaf"),
    city          = c("  PORTAND ", "Seattle", "portand", "Portland", "seattle",
                      "Portand")
  )
}

.fx_norm <- function(variable, ops, pattern = NA_character_) {
  fam <- !is.na(pattern)
  tibble::tibble(
    norm_id = normalization_rule_id(variable, pattern), variable = variable,
    apply_to_siblings = fam, sibling_pattern = pattern,
    ops = normalize_ops_join(ops), notes = NA_character_, author = "test",
    created_at = "2026-01-01T00:00:00", updated_at = "2026-01-01T00:00:00",
    source_dataset = "cafe")
}

.fx_norms <- function() {
  dplyr::bind_rows(
    .fx_norm("drink1", c("squish", "lowercase"), "^drink[0-9]+$"),
    .fx_norm("drink notes", c("squish", "lowercase")),
    .fx_norm("city", c("squish", "lowercase")))
}

.fx_rule <- function(variable, old_value, new_value, pattern = NA_character_) {
  tibble::tibble(
    rule_id = recode_rule_id(variable, "exact", old_value), variable = variable,
    apply_to_siblings = !is.na(pattern), sibling_pattern = pattern,
    match_type = "exact", old_value = old_value, new_value = new_value,
    action = "recode", notes = NA_character_, author = "test",
    created_at = "2026-01-01T00:00:00", updated_at = "2026-01-01T00:00:00",
    source_dataset = "cafe")
}

.fx_rules <- function() {
  r <- dplyr::bind_rows(
    .fx_rule("drink1", "expresso", "espresso", "^drink[0-9]+$"),
    .fx_rule("city", "portand", "portland"),
    .fx_rule("drink notes", "oat milk", "oat milk (dairy free)"),
    .fx_rule("drink notes", NA_character_, "none"))
  # Stamped under the fixture normalizations, so no drift warnings.
  stamp_recode_norm_spec(r, .fx_norms(), names(.fx_df()))
}

.fx_recat_rule <- function(out_col, category, include, exclude = NA_character_,
                           vars, pattern = NA_character_, priority = 1L) {
  tibble::tibble(
    recat_id = recat_rule_id(out_col, category, vars, include,
                             exclude, "literal"),
    out_col = out_col, category = category, vars = vars,
    sibling_pattern = pattern, include_terms = include,
    exclude_terms = exclude, match_type = "literal", priority = priority,
    notes = NA_character_, author = "test",
    created_at = "2026-01-01T00:00:00", source_dataset = "cafe")
}

.fx_recat <- function() {
  dplyr::bind_rows(
    .fx_recat_rule("region", "Portland area", "portland", vars = "city"),
    .fx_recat_rule("drink_family", "Espresso-based", "expresso",
                   vars = "drink1", pattern = "^drink[0-9]+$", priority = 1L),
    .fx_recat_rule("drink_family", "Espresso drinks", "espresso;latte",
                   exclude = "mocha", vars = "drink1",
                   pattern = "^drink[0-9]+$", priority = 2L))
}

.fx_expected <- function() {
  tibble::tibble(
    order_id      = as.character(1:6),
    drink1        = c("espresso", "latte", "espresso", "mocha", NA, "cap"),
    drink2        = c("latte", "espresso", NA, "mocha", "cap", "latte"),
    `drink notes` = c("oat milk (dairy free)", "extra hot", "none",
                      "oat milk (dairy free)", "no whip", "decaf"),
    city          = c("portland", "seattle", "portland", "portland", "seattle",
                      "portland"),
    # apply_recat() adds out_cols in alphabetical order.
    drink_family  = c("Espresso drinks", "Espresso drinks", "Espresso drinks",
                      NA, NA, "Espresso drinks"),
    region        = c("Portland area", NA, "Portland area", "Portland area",
                      NA, "Portland area")
  )
}

# The fixture as a CSV on disk, read the way the app reads it.
.fx_file <- function(dir = tempfile("fx_")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, "cafe orders.csv")
  readr::write_csv(.fx_df(), path, na = "")
  path
}

.cols_identical <- function(got, want) {
  expect_identical(names(got), names(want))
  for (col in names(want)) {
    expect_identical(as.character(got[[col]]), as.character(want[[col]]),
                     info = sprintf("column %s", col))
  }
}

.em_dash <- "\u2014"


# --- Names ------------------------------------------------------------------------

test_that("bundle_dataset_id is filesystem safe and drops folders", {
  expect_identical(bundle_dataset_id("Cafe orders (March).xlsx"),
                   "Cafe_orders_March")
  expect_identical(bundle_dataset_id("C:\\data\\cafe orders.csv"), "cafe_orders")
  expect_identical(bundle_dataset_id("/tmp/x/example_messy.csv"), "example_messy")
  expect_identical(bundle_dataset_id(NA_character_), "dataset")
  expect_identical(bundle_dataset_id("###.csv"), "dataset")
})

test_that("app version comes from DESCRIPTION, NA when absent", {
  expect_true(is.na(app_version_from_description(tempfile())))
  d <- tempfile(fileext = ".dcf")
  writeLines(c("Package: x", "Version: 9.8.7"), d)
  expect_identical(app_version_from_description(d), "9.8.7")
})


# --- Pipeline ----------------------------------------------------------------------

test_that("pipeline order: normalize, then recode, then recat", {
  res <- build_cleaned_dataset(.fx_df(), .fx_rules(), .fx_recat(), .fx_norms())
  .cols_identical(res$df, .fx_expected())
  expect_identical(res$recat_columns_added, c("drink_family", "region"))
  expect_identical(res$warnings$kind, "recat_terms_recoded_away")
  expect_equal(sum(res$normalization_summary$cells_changed), 10L)
  expect_equal(sum(res$recode_summary$cells_changed), 9L)
})

test_that("pipeline warnings equal collect_rule_warnings()", {
  res <- build_cleaned_dataset(.fx_df(), .fx_rules(), .fx_recat(), .fx_norms())
  expect_identical(res$warnings,
                   collect_rule_warnings(.fx_df(), .fx_rules(), .fx_recat(),
                                         .fx_norms()))
})

test_that("pipeline with no rules at all returns the data unchanged", {
  res <- build_cleaned_dataset(.fx_df(), NULL, NULL, NULL)
  .cols_identical(res$df, .fx_df())
  expect_identical(res$recat_columns_added, character(0))
  expect_equal(nrow(res$warnings), 0L)
})

test_that("a step that changes the row count stops the export loudly", {
  expect_error(.stop_if_rows_changed(6L, .fx_df()[1:5, ], "recode"),
               "changed the number of rows from 6 to 5")

  # Swap in a broken apply_recat() for the duration of this test.
  real <- apply_recat
  assign("apply_recat", function(df, rules) {
    out <- real(df, rules); out$df <- out$df[-1, ]; out
  }, envir = globalenv())
  on.exit(assign("apply_recat", real, envir = globalenv()), add = TRUE)
  expect_error(build_cleaned_dataset(.fx_df(), .fx_rules(), .fx_recat(),
                                     .fx_norms()),
               "recategorization step changed the number of rows")

  # And the provenance refuses a mismatched pair.
  fake <- list(df = .fx_df()[1:3, ], warnings = empty_rule_warnings())
  expect_error(build_provenance(list(), .fx_df(), fake, NULL, NULL, NULL, NA),
               "never add or drop rows")
})


# --- Source metadata -----------------------------------------------------------------

test_that("source_file_meta keeps the base name, size and SHA-256", {
  path <- .fx_file()
  at   <- as.POSIXct("2026-01-02 03:04:05", tz = "UTC")
  m <- source_file_meta(path, file_name = path, loaded_at = at)
  expect_identical(m$file_name, "cafe orders.csv")
  expect_false(grepl("[/\\\\]", m$file_name))
  expect_true(is.na(m$sheet))
  expect_equal(m$file_size_bytes, as.numeric(file.size(path)))
  expect_identical(m$sha256, digest::digest(file = path, algo = "sha256"))
  expect_identical(m$loaded_at, at)
  # A Windows-style name is stripped on any OS.
  expect_identical(
    source_file_meta(path, file_name = "C:\\Users\\x\\cafe orders.csv")$file_name,
    "cafe orders.csv")
})

test_that("loading a file on the Data tab stores its source metadata", {
  path <- .fx_file()
  st <- shiny::reactiveValues(data = NULL, meta = NULL, dataset_name = NULL,
                              source_meta = NULL)
  shiny::testServer(mod_data_input_server, args = list(shared_state = st), {
    session$setInputs(file = data.frame(
      name = "cafe orders.csv", size = file.size(path), type = "text/csv",
      datapath = path, stringsAsFactors = FALSE))
    sm <- st$source_meta
    expect_identical(sm$file_name, "cafe orders.csv")
    expect_identical(sm$sha256, digest::digest(file = path, algo = "sha256"))
    expect_equal(sm$file_size_bytes, as.numeric(file.size(path)))
    expect_true(inherits(sm$loaded_at, "POSIXct"))
    expect_equal(nrow(st$data), 6L)
    expect_identical(st$dataset_name, "cafe orders.csv")
  })
})


# --- Provenance ------------------------------------------------------------------------

.fx_prov <- function(path = .fx_file()) {
  df  <- read_dataset(path)
  res <- build_cleaned_dataset(df, .fx_rules(), .fx_recat(), .fx_norms())
  sm  <- source_file_meta(path, file_name = path)
  list(path = path, df = df, cleaned = res,
       prov = build_provenance(sm, df, res, .fx_rules(), .fx_recat(),
                               .fx_norms(), "0.1.0"))
}

test_that("provenance carries every required field, correctly", {
  x <- .fx_prov()
  p <- x$prov
  required <- c("original_file_name", "sheet", "file_size_bytes",
                "input_sha256", "source_loaded_at", "exported_at",
                "app_version", "rows_before", "columns_before", "rows_after",
                "columns_after", "recat_columns_added",
                "normalization_rules_applied", "recode_rules_applied",
                "recat_rules_applied", "cells_changed_by_normalizations",
                "cells_changed_by_recodes", "warnings", "files")
  expect_true(all(required %in% names(p)))
  expect_identical(p$original_file_name, "cafe orders.csv")
  expect_false(grepl("[/\\\\]", p$original_file_name))
  expect_identical(p$input_sha256, digest::digest(file = x$path, algo = "sha256"))
  expect_equal(p$file_size_bytes, as.numeric(file.size(x$path)))
  expect_identical(p$app_version, "0.1.0")
  expect_equal(c(p$rows_before, p$columns_before, p$rows_after, p$columns_after),
               c(6, 5, 6, 7))
  expect_identical(as.character(p$recat_columns_added),
                   c("drink_family", "region"))
  expect_equal(c(p$normalization_rules_applied, p$recode_rules_applied,
                 p$recat_rules_applied), c(3, 4, 3))
  expect_equal(p$cells_changed_by_normalizations, 10L)
  expect_equal(p$cells_changed_by_recodes, 9L)
  expect_identical(p$warnings$kind, "recat_terms_recoded_away")
  expect_identical(p$warnings$message, x$cleaned$warnings$message)
  expect_match(p$exported_at, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}")
  expect_identical(p$files$file, bundle_file_names("cafe_orders")$file)
})

test_that("provenance with no metadata still builds, fields not recorded", {
  res <- build_cleaned_dataset(.fx_df(), NULL, NULL, NULL)
  p <- build_provenance(NULL, .fx_df(), res, NULL, NULL, NULL, NA_character_)
  expect_true(is.na(p$original_file_name))
  expect_true(is.na(p$input_sha256))
  expect_identical(p$files$file[1], "dataset_cleaned.csv")
  txt <- provenance_text(p)
  expect_match(txt, "(not recorded)", fixed = TRUE)
})

test_that("provenance.txt is plain language with no em dashes", {
  p   <- .fx_prov()$prov
  txt <- provenance_text(p)
  expect_false(grepl(.em_dash, txt, fixed = TRUE))
  expect_false(grepl(paste0("&", "mdash;"), txt, fixed = TRUE))
  expect_match(txt, "Your original file was not", fixed = TRUE)
  expect_match(txt, "clean_cafe_orders.R", fixed = TRUE)
  expect_match(txt, "EMPTY cell", fixed = TRUE)
  expect_match(txt, p$input_sha256, fixed = TRUE)
  expect_match(txt, "Warnings (1)", fixed = TRUE)
  expect_match(txt, p$warnings$message, fixed = TRUE)
})


# --- Writing the bundle ------------------------------------------------------------------

test_that("write_export_bundle writes exactly the expected files", {
  x   <- .fx_prov()
  dir <- tempfile("bundle_")
  files <- write_export_bundle(dir, x$cleaned, x$prov, .fx_rules(), .fx_recat(),
                               .fx_norms(), "cafe_orders")
  expected <- c("cafe_orders_cleaned.csv", "normalizations_master.csv",
                "recodes_master.csv", "recat_cafe_orders.csv",
                "clean_cafe_orders.R", "provenance.json", "provenance.txt")
  expect_identical(files, expected)
  expect_setequal(list.files(dir), expected)
  # The manifest in the record lists the same files.
  expect_identical(x$prov$files$file, files)
  json <- jsonlite::fromJSON(file.path(dir, "provenance.json"))
  expect_identical(json$files$file, files)
  expect_identical(json$original_file_name, "cafe orders.csv")
  expect_identical(json$recat_columns_added, c("drink_family", "region"))
  expect_identical(json$warnings$kind, "recat_terms_recoded_away")
  # No local folder leaks into the records.
  folder <- basename(dirname(x$path))
  for (f in c("provenance.json", "provenance.txt", "clean_cafe_orders.R")) {
    expect_false(grepl(folder, paste(readLines(file.path(dir, f)), collapse = "\n"),
                       fixed = TRUE), info = f)
  }
  expect_false(any(grepl(.em_dash, readLines(file.path(dir, "provenance.txt"),
                                             encoding = "UTF-8"), fixed = TRUE)))
})

test_that("rule files are written even with zero rules", {
  res <- build_cleaned_dataset(.fx_df(), NULL, NULL, NULL)
  p   <- build_provenance(list(file_name = "cafe.csv"), .fx_df(), res,
                          NULL, NULL, NULL, NA)
  dir <- tempfile("bundle_empty_")
  files <- write_export_bundle(dir, res, p, NULL, NULL, NULL, "cafe")
  expect_setequal(list.files(dir), files)
  expect_equal(nrow(read_recodes(file.path(dir, "recodes_master.csv"))), 0L)
  expect_equal(nrow(read_normalizations(file.path(dir, "normalizations_master.csv"))), 0L)
  expect_equal(nrow(read_recat(file.path(dir, "recat_cafe.csv"))), 0L)
  expect_identical(names(readr::read_csv(file.path(dir, "recodes_master.csv"),
                                         show_col_types = FALSE)),
                   names(empty_recodes_tibble()))
})

test_that("cleaned CSV writes missing values as empty cells and round-trips", {
  x   <- .fx_prov()
  dir <- tempfile("bundle_rt_")
  write_export_bundle(dir, x$cleaned, x$prov, .fx_rules(), .fx_recat(),
                      .fx_norms(), "cafe_orders")
  csv <- file.path(dir, "cafe_orders_cleaned.csv")
  lines <- readLines(csv, encoding = "UTF-8")
  expect_false(any(grepl("(^|,)NA(,|$)", lines)))
  expect_identical(lines[6], "5,,cap,no whip,seattle,,")
  back <- read_dataset(csv)
  .cols_identical(back, x$cleaned$df)
})


# --- The reproduction script (most important) -----------------------------------------------

test_that("the bundle script rebuilds the cleaned data from the original file", {
  x   <- .fx_prov()
  dir <- tempfile("bundle_e2e_")
  write_export_bundle(dir, x$cleaned, x$prov, .fx_rules(), .fx_recat(),
                      .fx_norms(), "cafe_orders")
  script <- file.path(dir, "clean_cafe_orders.R")
  expect_silent(parse(script))

  # Run it the way the user would: from the folder holding the original file.
  # The eval() runs code this test just generated from its own rule tables.
  old <- setwd(dirname(x$path))
  on.exit(setwd(old), add = TRUE)
  env  <- new.env(parent = globalenv())
  warns <- character(0)
  withCallingHandlers(
    suppressMessages(eval(parse(script), envir = env)),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_false(any(grepl("byte-for-byte", warns, fixed = TRUE)))

  # Cell for cell against the bundle's cleaned data, including the
  # non-syntactic `drink notes` and the drink1 / drink2 sibling family.
  .cols_identical(env$df, x$cleaned$df)
  .cols_identical(env$df, .fx_expected())

  # And the CSV it writes is byte-identical to the bundle's.
  out <- file.path(dirname(x$path), "cafe_orders_cleaned_reproduced.csv")
  expect_true(file.exists(out))
  expect_identical(readBin(out, "raw", file.size(out)),
                   readBin(file.path(dir, "cafe_orders_cleaned.csv"), "raw",
                           file.size(file.path(dir, "cafe_orders_cleaned.csv"))))
})

# Build a bundle for `df` (written to CSV and read back the way the app reads
# it), run its script from the original file's folder, and return both.
.run_bundle_script <- function(df, rules, recat, norms, file_name) {
  src <- tempfile("fx_meta_")
  dir.create(src)
  path <- file.path(src, file_name)
  readr::write_csv(df, path, na = "")
  raw <- read_dataset(path)
  res <- build_cleaned_dataset(raw, rules, recat, norms)
  prov <- build_provenance(source_file_meta(path, file_name = path), raw, res,
                           rules, recat, norms, "0.1.0")
  id  <- bundle_dataset_id(file_name)
  dir <- tempfile("bundle_meta_")
  write_export_bundle(dir, res, prov, rules, recat, norms, id)
  old <- setwd(src)
  on.exit(setwd(old), add = TRUE)
  env <- new.env(parent = globalenv())
  suppressMessages(eval(parse(file.path(dir, sprintf("clean_%s.R", id))),
                        envir = env))
  list(cleaned = res$df, script_df = env$df,
       bundle_csv = file.path(dir, sprintf("%s_cleaned.csv", id)),
       script_csv = file.path(src, sprintf("%s_cleaned_reproduced.csv", id)))
}

test_that("the bundle script reproduces columns whose names hold regex metacharacters", {
  # Before the fix the recode rule on `q1.a` also rewrote `q1_a` in the script
  # (unescaped `.`), and `price ($)` was not reachable by its own pattern.
  df <- tibble::tibble(
    order_id    = as.character(1:5),
    `q1.a`      = c("yes", "no", "Yes ", "yes", NA),
    q1_a        = c("yes", "yes", "no", "yes", "yes"),
    `price ($)` = c("free", "2.50", "FREE", NA, "free"),
    `my col`    = c("hot", "iced", "HOT", "hot", NA))
  norms <- dplyr::bind_rows(
    .fx_norm("q1.a", c("squish", "lowercase")),
    .fx_norm("price ($)", "uppercase"))
  rules <- dplyr::bind_rows(
    .fx_rule("q1.a", "yes", "Y"),
    .fx_rule("price ($)", "FREE", "0.00"),
    .fx_rule("my col", "hot", "Hot"))
  recat <- .fx_recat_rule("temp", "Hot drink", "hot", vars = "my col")
  out <- .run_bundle_script(df, rules, recat, norms, "meta names.csv")

  .cols_identical(out$script_df, out$cleaned)
  expect_identical(out$cleaned$`q1.a`, c("Y", "no", "Y", "Y", NA))
  expect_identical(out$cleaned$q1_a, df$q1_a)
  expect_identical(out$cleaned$`price ($)`, c("0.00", "2.50", "0.00", NA, "0.00"))
  expect_identical(out$cleaned$`my col`, c("Hot", "iced", "HOT", "Hot", NA))
  expect_identical(out$cleaned$temp, c("Hot drink", NA, "Hot drink", "Hot drink", NA))
  expect_identical(readBin(out$script_csv, "raw", file.size(out$script_csv)),
                   readBin(out$bundle_csv, "raw", file.size(out$bundle_csv)))
})

test_that("the bundle script skips unusable rules exactly as the app does", {
  # Rules that used to crash the export (a sibling pattern that does not
  # compile, in all three rule types) or that the script ran differently
  # (a regex recode valid in one regex engine only, a blank sibling pattern,
  # an unrecognised recat match type, a recat regex term that does not
  # compile). The app skips each one and the script must skip it identically.
  df <- tibble::tibble(
    order_id = as.character(1:4),
    drink1   = c("Latte", "mocha", "latte", NA),
    drink2   = c("mocha", "Latte", NA, "cap"),
    other    = c("latte", "latte", "latte", "latte"))
  norms <- dplyr::bind_rows(
    .fx_norm("drink1", "uppercase", "drink[0-9"),          # does not compile
    .fx_norm("other", "titlecase"))
  bad_sib   <- .fx_rule("drink1", "mocha", "MOCHA", "drink[")
  blank_sib <- .fx_rule("drink2", "cap", "cappuccino", "  ")
  rx_tre    <- .fx_rule("drink1", "a{,2}", "HIT")        # TRE ok, ICU error
  rx_tre$match_type <- "regex"
  rx_lu     <- .fx_rule("drink2", "\\p{Lu}", "UPPER")    # ICU ok, TRE error
  rx_lu$match_type  <- "regex"
  rules <- dplyr::bind_rows(bad_sib, blank_sib, rx_tre, rx_lu)
  recat <- dplyr::bind_rows(
    .fx_recat_rule("fam", "Bad pattern", "latte", vars = "drink1",
                   pattern = "("),
    .fx_recat_rule("fam", "Bogus type", "latte", vars = "drink1"),
    .fx_recat_rule("fam", "Bad term", "[x;latte", vars = "drink2"),
    .fx_recat_rule("fam", "Blank pattern", "mocha", vars = "drink2",
                   pattern = ""))
  recat$match_type[2] <- "fuzzy"
  recat$match_type[3] <- "Regex "
  expect_no_error(out <- .run_bundle_script(df, rules, recat, norms,
                                            "unusable rules.csv"))
  .cols_identical(out$script_df, out$cleaned)
  expect_identical(readBin(out$script_csv, "raw", file.size(out$script_csv)),
                   readBin(out$bundle_csv, "raw", file.size(out$bundle_csv)))
  # What the app did: bad-pattern rules skipped, blank pattern = own column.
  expect_identical(out$cleaned$drink1, c("HIT", "HIT", "HIT", NA))
  expect_identical(out$cleaned$drink2, c("mocha", "Latte", NA, "cappuccino"))
  expect_identical(out$cleaned$other, rep("Latte", 4))
  # Recat: the bad-pattern and bogus-type rules match nothing; the bad term
  # is dropped and "latte" still matches; the blank pattern reads as none.
  expect_identical(out$cleaned$fam, c("Blank pattern", "Bad term", NA, NA))
})

test_that("the bundle script skips a rule whose column is not in the data", {
  # engine 3: the app resolves such a rule to no column and leaves the data
  # alone, but the script emitted a mutate() naming the missing column and
  # died, while provenance told the reader to run that script.
  df <- tibble::tibble(order_id = as.character(1:3),
                       drink1   = c("cap", "latte", "cap"))
  rules <- dplyr::bind_rows(
    .fx_rule("drink1", "cap", "cappuccino"),
    .fx_rule("gone", "cap", "cappuccino"),                 # no such column
    .fx_rule("drink1", "latte", "LATTE", "absent[0-9]+"))  # matches nothing
  expect_no_error(out <- .run_bundle_script(df, rules, NULL, NULL,
                                            "absent column.csv"))
  .cols_identical(out$script_df, out$cleaned)
  expect_identical(readBin(out$script_csv, "raw", file.size(out$script_csv)),
                   readBin(out$bundle_csv, "raw", file.size(out$bundle_csv)))
  expect_identical(out$cleaned$drink1, c("cappuccino", "latte", "cappuccino"))
  expect_identical(names(out$cleaned), names(df))
})

test_that("the bundle script skips a recat out_col that already exists", {
  # engine 5: apply_recat() leaves an existing column alone, so the script
  # must too, or the reproduced CSV stops matching the cleaned CSV.
  df <- tibble::tibble(order_id = as.character(1:3),
                       drink1   = c("latte", "mocha", "latte"),
                       fam      = c("Manual", NA, NA))
  recat <- .fx_recat_rule("fam", "Milk-based", "latte", vars = "drink1")
  expect_no_error(out <- .run_bundle_script(df, NULL, recat, NULL,
                                            "recat collision.csv"))
  .cols_identical(out$script_df, out$cleaned)
  expect_identical(readBin(out$script_csv, "raw", file.size(out$script_csv)),
                   readBin(out$bundle_csv, "raw", file.size(out$bundle_csv)))
  # The gaps stay empty: the source column is not rewritten.
  expect_identical(out$cleaned$fam, c("Manual", NA, NA))
})

test_that("provenance carries warning severity, and notes are marked", {
  df <- tibble::tibble(city = c("Portland", "portland "))
  rule <- .fx_rule("city", "portland", "Portland")
  rule$match_type <- "trimmed_ci"
  rule$rule_id <- recode_rule_id("city", "trimmed_ci", "portland")
  rule$norm_spec <- ""                              # written unnormalized
  norms <- .fx_norm("city", "titlecase")            # now title case: a note
  res  <- build_cleaned_dataset(df, rule, NULL, norms)
  expect_identical(res$warnings$severity, "note")
  prov <- build_provenance(list(file_name = "x.csv"), df, res, rule, NULL,
                           norms, NA)
  expect_named(prov$warnings, c("kind", "severity", "message"))
  expect_identical(prov$warnings$severity, "note")
  txt <- provenance_text(prov)
  expect_match(txt, "Warnings (0)", fixed = TRUE)
  expect_match(txt, "Notes (1)", fixed = TRUE)
  expect_match(txt, "- (note) ", fixed = TRUE)
})

test_that("an empty-to-missing rule leaves no visible trace in the cleaned CSV", {
  # Documented limitation, pinned: the cleaned CSV writes "" and NA the same
  # way (an empty cell), so a rule turning "" into NA changes nothing a reader
  # of that one file can see. The rule file still carries it.
  cleaned <- list(df = tibble::tibble(a = c("", NA, "x")))
  f1 <- tempfile(fileext = ".csv"); f2 <- tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(a = c("", "", "x")), f1, na = BUNDLE_CSV_NA)
  readr::write_csv(cleaned$df, f2, na = BUNDLE_CSV_NA)
  expect_identical(readLines(f1), readLines(f2))
  rule <- .fx_rule("a", "", NA_character_)
  rule$action <- "delete"
  rf <- tempfile(fileext = ".csv")
  write_recodes(rule, rf)
  back <- read_recodes(rf)
  expect_identical(back$old_value, "")
  expect_identical(back$action, "delete")
})

test_that("the script warns when the input is not the recorded file", {
  x   <- .fx_prov()
  dir <- tempfile("bundle_sha_")
  write_export_bundle(dir, x$cleaned, x$prov, .fx_rules(), .fx_recat(),
                      .fx_norms(), "cafe_orders")
  cat("7,latte,,,Seattle\n", file = x$path, append = TRUE)
  old <- setwd(dirname(x$path))
  on.exit(setwd(old), add = TRUE)
  env <- new.env(parent = globalenv())
  expect_warning(
    suppressMessages(eval(parse(file.path(dir, "clean_cafe_orders.R")), envir = env)),
    "byte-for-byte")
})


# --- The zip ----------------------------------------------------------------------------------

test_that("export_bundle_zip zips every expected file with zip::zip", {
  path <- .fx_file()
  zf   <- tempfile(fileext = ".zip")
  files <- export_bundle_zip(zf, read_dataset(path),
                             source_file_meta(path, file_name = path),
                             .fx_rules(), .fx_recat(), .fx_norms(), "0.1.0")
  expect_true(file.exists(zf))
  expect_setequal(zip::zip_list(zf)$filename, files)
  expect_setequal(files, bundle_file_names("cafe_orders")$file)
})


# --- Preview & export wiring ---------------------------------------------------------------------

.proxy <- function(x) list(get = function() x)

test_that("download card explains itself when no data is loaded", {
  st <- shiny::reactiveValues(data = NULL, meta = NULL, dataset_name = NULL,
                              source_meta = NULL)
  shiny::testServer(mod_preview_export_server,
    args = list(shared_state = st, rules_proxy = .proxy(empty_recodes_tibble())), {
      expect_match(output$bundle_summary$html, "No data loaded", fixed = TRUE)
      expect_null(bundle_preview_r())
      expect_error(bundle_content(tempfile(fileext = ".zip")), "No dataset loaded")
    })
})

test_that("the download handler content builds a zip with every entry", {
  path <- .fx_file()
  st <- shiny::reactiveValues(
    data = read_dataset(path), meta = NULL, dataset_name = "cafe orders.csv",
    source_meta = source_file_meta(path, file_name = "cafe orders.csv"))
  shiny::testServer(mod_preview_export_server,
    args = list(shared_state = st, rules_proxy = .proxy(.fx_rules()),
                recat_proxy = .proxy(.fx_recat()),
                norms_proxy = .proxy(.fx_norms())), {
      expect_match(output$bundle_summary$html,
                   "6 rows. Columns: 5 before, 7 after.", fixed = TRUE)
      expect_match(output$bundle_summary$html, "1 warning.", fixed = TRUE)
      expect_identical(bundle_id(), "cafe_orders")
      zf <- tempfile(fileext = ".zip")
      bundle_content(zf)
      expect_setequal(zip::zip_list(zf)$filename,
                      bundle_file_names("cafe_orders")$file)
    })
})

# The em dash check over these two sources used to live here as a grep. It is
# gone: test-em_dash_guard.R makes the same check over every project text
# file, which is strictly more than these two.
