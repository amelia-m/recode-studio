# Run with: testthat::test_dir("tests/testthat")
#
# .read_dict_terms(): the Spellcheck tab imports any .txt, so a raw Hunspell
# .dic pasted in must not add dead entries (affix flags, the count line,
# multi-word lines) that hunspell can never match against a token.
# mod_spellcheck_view.R only defines functions at top level, so sourcing it
# does not need a running Shiny session.

source(file.path("..", "..", "R", "ui_helpers.R"))
source(file.path("..", "..", "R", "mod_spellcheck_view.R"))

.write_dict <- function(lines) {
  f <- tempfile(fileext = ".txt")
  writeLines(lines, f, useBytes = TRUE)
  f
}

test_that("plain word lists read as lowercase, trimmed, deduped, no comments", {
  f <- .write_dict(c("# a comment", "Latte", "  mocha ", "", "latte"))
  on.exit(unlink(f))
  expect_equal(.read_dict_terms(f), c("latte", "mocha"))
})

test_that("Hunspell affix flags are stripped", {
  f <- .write_dict(c("espresso/SM", "macchiato/S", "cortado"))
  on.exit(unlink(f))
  expect_equal(.read_dict_terms(f), c("espresso", "macchiato", "cortado"))
})

test_that("a purely numeric count line is dropped", {
  f <- .write_dict(c("3", "espresso/SM", "mocha", "cortado"))
  on.exit(unlink(f))
  terms <- .read_dict_terms(f)
  expect_false("3" %in% terms)
  expect_equal(terms, c("espresso", "mocha", "cortado"))
})

test_that("multi-word entries are dropped", {
  f <- .write_dict(c("flat white", "cold\tbrew", "americano"))
  on.exit(unlink(f))
  expect_equal(.read_dict_terms(f), "americano")
})

test_that("a flag-only or slash-only line leaves nothing behind", {
  f <- .write_dict(c("/SM", "latte"))
  on.exit(unlink(f))
  expect_equal(.read_dict_terms(f), "latte")
})

test_that("a missing file reads as no terms", {
  expect_equal(.read_dict_terms(tempfile()), character(0))
})

test_that("the bundled discipline dictionaries are unaffected by the cleaning", {
  files <- list.files(file.path("..", "..", "dictionary", "disciplines"),
                      pattern = "\\.txt$", full.names = TRUE)
  skip_if(length(files) == 0)
  for (f in files) {
    raw <- readLines(f, warn = FALSE, encoding = "UTF-8")
    raw <- raw[!startsWith(trimws(raw), "#")]
    raw <- unique(tolower(trimws(raw)))
    raw <- raw[nzchar(raw)]
    expect_equal(.read_dict_terms(f), raw, info = basename(f))
  }
})
