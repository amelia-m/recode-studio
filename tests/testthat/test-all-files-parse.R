# Run with: testthat::test_dir("tests/testthat")
#
# Every R file that ships must parse.
#
# Most R files are sourced by some test, so a syntax error there fails loudly.
# A file nothing sources (a module no test loads, a one-off script) can carry
# a syntax error unnoticed until renv's dependency scan or a user's runApp()
# hits it. Parsing is cheap, so do it for everything.

# Walk up from the working directory to the repo root. test_dir() runs with
# tests/testthat as the working directory, an interactive session usually sits
# at the root; both find the same place.
.parse_repo_root <- function(start = getwd()) {
  dir <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(dir, "DESCRIPTION")) &&
        file.exists(file.path(dir, "app.R"))) {
      return(dir)
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("Could not find the repo root above ", start)
    dir <- parent
  }
}

test_that("every R file in the project parses", {
  root <- .parse_repo_root()

  files <- c(
    list.files(file.path(root, "R"), pattern = "\\.[Rr]$",
               recursive = TRUE, full.names = TRUE),
    list.files(file.path(root, "tests"), pattern = "\\.[Rr]$",
               recursive = TRUE, full.names = TRUE),
    file.path(root, "app.R"))
  files <- gsub("\\\\", "/", files)
  rel   <- substring(files, nchar(root) + 2L)

  # The renv library is other people's code. It does not live under R/ or
  # tests/ today; the skip keeps it that way if a layout change moves it.
  keep  <- !grepl("(^|/)renv/", rel)
  files <- files[keep]
  rel   <- rel[keep]

  # Sanity: 39 files at the time of writing. A floor a little below that
  # catches a directory deleted or a filter that eats everything.
  expect_gt(length(files), 34)

  failures <- character(0)
  for (i in seq_along(files)) {
    err <- tryCatch({
      parse(files[i], keep.source = FALSE, encoding = "UTF-8")
      NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(err)) failures <- c(failures, paste0(rel[i], ": ", err))
  }

  expect_equal(failures, character(0))
})
