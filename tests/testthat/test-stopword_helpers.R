# Run with: testthat::test_dir("tests/testthat")
#
# Stopword tiers (R/stopword_helpers.R): reading, merging, the EN_STOPWORDS
# fallback, verified appends, and the committed tier files themselves.
#
# cspell:ignore oatmilk

source(file.path("..", "..", "R", "text_helpers.R"))
source(file.path("..", "..", "R", "stopword_helpers.R"))

.make_tier_dir <- function(env = parent.frame()) {
  d <- withr::local_tempdir(pattern = "stop words ", .local_envir = env)
  writeLines(c("# base", "the", "and"), file.path(d, "base_stopwords.txt"))
  writeLines(c("# project", "oatmilk"), file.path(d, "custom_stopwords.txt"))
  d
}

test_that(".stopword_paths names the three tier files", {
  p <- .stopword_paths("x")
  expect_named(p, STOPWORD_TIERS)
  expect_equal(basename(unlist(p, use.names = FALSE)),
               c("base_stopwords.txt", "custom_stopwords.txt",
                 "user_stopwords.txt"))
})

test_that(".read_stopword_terms strips comments, blanks, case and .dic artifacts", {
  d <- withr::local_tempdir()
  p <- file.path(d, "x.txt")
  writeLines(c("# a comment", "", "  The ", "AND", "the", "   # indented",
               "120", "scone/SM", "flat white"), p)
  expect_equal(.read_stopword_terms(p), c("the", "and", "scone"))
})

test_that(".read_stopword_terms returns empty for a missing file", {
  expect_equal(.read_stopword_terms(file.path(tempdir(), "nope-nope.txt")),
               character(0))
})

test_that("active_stopwords merges the requested tiers", {
  d <- .make_tier_dir()
  writeLines("latte", file.path(d, "user_stopwords.txt"))
  expect_setequal(active_stopwords(dir = d),
                  c("the", "and", "oatmilk", "latte"))
  expect_setequal(active_stopwords(c("base", "project"), dir = d),
                  c("the", "and", "oatmilk"))
  expect_setequal(active_stopwords("base", dir = d), c("the", "and"))
  expect_setequal(active_stopwords("user", dir = d), "latte")
})

test_that("active_stopwords: no tiers selected means no stopwords", {
  d <- .make_tier_dir()
  expect_equal(active_stopwords(character(0), dir = d), character(0))
  expect_equal(active_stopwords(NULL, dir = d), character(0))
  expect_equal(active_stopwords("nonsense", dir = d), character(0))
})

test_that("active_stopwords falls back to EN_STOPWORDS when the base file is missing", {
  d <- withr::local_tempdir()
  expect_equal(active_stopwords(dir = d), EN_STOPWORDS)
  expect_equal(active_stopwords(dir = file.path(d, "absent")), EN_STOPWORDS)
  # Only the base tier falls back; an empty project tier stays empty.
  expect_equal(active_stopwords("project", dir = d), character(0))
  writeLines("oatmilk", file.path(d, "custom_stopwords.txt"))
  expect_setequal(active_stopwords(dir = d), c(EN_STOPWORDS, "oatmilk"))
})

test_that("add_stopword appends once and is idempotent", {
  d <- .make_tier_dir()
  expect_true(add_stopword("  Scone ", tier = "project", dir = d))
  expect_true("scone" %in% active_stopwords(dir = d))
  expect_false(add_stopword("scone", tier = "project", dir = d))
  expect_false(add_stopword("SCONE", tier = "project", dir = d))
  lines <- readLines(file.path(d, "custom_stopwords.txt"))
  expect_equal(sum(lines == "scone"), 1L)
  # The existing content is kept, comment included.
  expect_equal(lines, c("# project", "oatmilk", "scone"))
})

test_that("add_stopword writes to the chosen tier only", {
  d <- .make_tier_dir()
  expect_true(add_stopword("muffin", tier = "user", dir = d))
  expect_equal(readLines(file.path(d, "user_stopwords.txt")), "muffin")
  expect_false("muffin" %in% active_stopwords("project", dir = d))
  # A word already in another tier is still added to this one.
  expect_true(add_stopword("the", tier = "project", dir = d))
})

test_that("add_stopword creates a missing folder and file", {
  d <- file.path(withr::local_tempdir(), "new stopwords")
  expect_true(add_stopword("apex", tier = "user", dir = d))
  expect_equal(readLines(file.path(d, "user_stopwords.txt")), "apex")
})

test_that("add_stopword starts a new line when the file lacks a final newline", {
  d <- withr::local_tempdir()
  p <- file.path(d, "custom_stopwords.txt")
  cat("# project\noatmilk", file = p)          # no trailing newline
  expect_true(add_stopword("scone", tier = "project", dir = d))
  expect_equal(readLines(p), c("# project", "oatmilk", "scone"))
})

test_that("add_stopword refuses the base tier and unusable terms", {
  d <- .make_tier_dir()
  expect_error(add_stopword("scone", tier = "base", dir = d))
  expect_error(add_stopword("flat white", tier = "project", dir = d),
               "cannot be stored")
  expect_error(add_stopword("", tier = "project", dir = d), "cannot be stored")
  expect_error(add_stopword("#x", tier = "project", dir = d), "cannot be stored")
  expect_error(add_stopword("a/b", tier = "project", dir = d), "cannot be stored")
  expect_error(add_stopword("2026", tier = "project", dir = d), "cannot be stored")
  expect_error(add_stopword(NA, tier = "project", dir = d), "single word")
  expect_error(add_stopword(c("a", "b"), tier = "project", dir = d),
               "single word")
  expect_equal(readLines(file.path(d, "custom_stopwords.txt")),
               c("# project", "oatmilk"))
})

test_that("add_stopword reports a write that fails loudly", {
  d <- .make_tier_dir()
  boom <- function(line, path) stop("disk full")
  expect_error(add_stopword("scone", tier = "project", dir = d, writer = boom),
               "Could not write custom_stopwords.txt \\(disk full\\)")
})

test_that("add_stopword reports a write that fails silently", {
  d <- .make_tier_dir()
  silent <- function(line, path) invisible(path)       # writes nothing
  expect_error(add_stopword("scone", tier = "project", dir = d,
                            writer = silent),
               "was not saved to custom_stopwords.txt")
  # A writer that writes the term twice is caught as well.
  twice <- function(line, path) {
    .append_stopword_line(line, path)
    .append_stopword_line(line, path)
  }
  expect_error(add_stopword("muffin", tier = "project", dir = d,
                            writer = twice),
               "was not saved")
})

test_that("add_stopword keeps non-ASCII terms intact", {
  d <- withr::local_tempdir()
  term <- paste0("caf", intToUtf8(0xe9))   # a non-ASCII letter
  expect_true(add_stopword(term, tier = "user", dir = d))
  expect_true(term %in% active_stopwords("user", dir = d))
  expect_false(add_stopword(term, tier = "user", dir = d))
})

# --- The committed tier files ---------------------------------------------------

.repo_stopword_dir <- file.path("..", "..", "stopwords")

test_that("the committed base tier matches EN_STOPWORDS", {
  skip_if_not(dir.exists(.repo_stopword_dir), "stopwords/ not found")
  expect_setequal(
    .read_stopword_terms(.stopword_paths(.repo_stopword_dir)$base),
    EN_STOPWORDS)
})

test_that("the committed project tier file exists", {
  # Its CONTENT is not pinned: the team adds to it from the app and commits.
  skip_if_not(dir.exists(.repo_stopword_dir), "stopwords/ not found")
  expect_true(file.exists(.stopword_paths(.repo_stopword_dir)$project))
})

test_that("the user tier file is gitignored", {
  gi <- file.path("..", "..", ".gitignore")
  skip_if_not(file.exists(gi), ".gitignore not found")
  expect_true("stopwords/user_stopwords.txt" %in% trimws(readLines(gi)))
})
