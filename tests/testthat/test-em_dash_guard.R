# Run with: testthat::test_dir("tests/testthat")
#
# Source-level guard: no em dashes anywhere in the project's own text.
#
# The project rules forbid them in code, comments, UI strings and docs. A rule
# nothing enforces decays: they come back out of habit, or a test that checks
# FOR an em dash contains a literal one and trips its own rule.
#
# When a file genuinely needs to talk about em dashes, write the ESCAPE
# "\u2014" rather than the character. R parses it to the same character at run
# time, so behaviour is identical and the file stays plain ASCII. This file
# follows that rule, and spells the HTML entities as split strings, so it
# needs no exemption for itself.

EM_DASH <- "\u2014"

# Spellings that render as an em dash without containing one. Split so this
# file does not contain them.
EM_ENTITIES <- c(paste0("&", "mdash;"), paste0("&#", "8212;"),
                 paste0("&#x", "2014;"))

# Not ours to police: renv/ and renv.lock are package-manager output, ext/ is
# archived third-party reference material, and inst/extdata/ holds data
# fixtures whose exact bytes other tests depend on.
EM_SKIP_PREFIXES <- c("renv/", "renv.lock", "ext/", "inst/extdata/")

# Walk up from the working directory to the repo root. test_dir() runs with
# tests/testthat as the working directory, an interactive session usually sits
# at the root; both find the same place.
.em_repo_root <- function(start = getwd()) {
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

# Tracked plus untracked-but-not-ignored files when git can list them (so a
# new file is checked before it is committed); otherwise every file under the
# root.
.em_candidate_files <- function(root) {
  files <- character(0)
  git <- Sys.which("git")
  if (nzchar(git)) {
    files <- tryCatch(
      suppressWarnings(system2(
        git,
        c("-C", shQuote(root), "ls-files", "--cached", "--others",
          "--exclude-standard"),
        stdout = TRUE, stderr = FALSE)),
      error = function(e) character(0))
    if (!is.null(attr(files, "status"))) files <- character(0)
  }
  if (!length(files)) {
    files <- list.files(root, recursive = TRUE, all.files = TRUE,
                        full.names = FALSE)
    files <- files[!startsWith(files, ".git/")]
  }
  files <- gsub("\\\\", "/", files)
  skip <- Reduce(`|`, lapply(EM_SKIP_PREFIXES, function(p) startsWith(files, p)))
  files <- files[!skip & file.exists(file.path(root, files))]
  sort(unique(files))
}

# NULL for a binary file (a NUL byte in the first 8 KB, the same test git
# uses), else the file's lines.
.em_read_text <- function(path) {
  size <- file.size(path)
  if (is.na(size)) return(NULL)
  if (size == 0) return(character(0))
  head <- readBin(path, "raw", n = min(size, 8000))
  if (any(head == as.raw(0))) return(NULL)
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

# `file:line (what)` for every line holding any of `patterns`.
.em_offenders <- function(root, files, patterns) {
  out <- character(0)
  for (f in files) {
    txt <- .em_read_text(file.path(root, f))
    if (is.null(txt) || !length(txt)) next
    for (p in patterns) {
      hit <- which(grepl(p, txt, fixed = TRUE, useBytes = TRUE))
      if (length(hit)) {
        label <- if (identical(p, EM_DASH)) "em dash" else p
        out <- c(out, sprintf("%s:%d (%s)", f, hit, label))
      }
    }
  }
  out
}

test_that("no project text file contains an em dash or an em dash entity", {
  root  <- .em_repo_root()
  files <- .em_candidate_files(root)
  # A sweep that finds nothing to check would pass silently forever.
  expect_gt(length(files), 40)

  offenders <- .em_offenders(root, files, c(EM_DASH, EM_ENTITIES))
  if (length(offenders)) {
    fail(paste0(
      "Em dashes found. Replace each with ' - ', a comma, a colon, or split ",
      "the sentence. An HTML entity for one renders as an em dash too. If a ",
      "file needs to REFER to an em dash, write the escape instead of the ",
      "character:\n  ", paste(offenders, collapse = "\n  ")))
  }
  expect_identical(offenders, character(0))
})

test_that("the guard can actually see an em dash and an entity", {
  # A guard that cannot fail is not a guard. Prove detection works rather
  # than trusting that an empty result means clean.
  dir <- tempfile("emguard")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  con <- file(file.path(dir, "dash.R"), open = "wb")
  writeLines(enc2utf8(c("# an ordinary line",
                        paste0("x <- 'before", EM_DASH, "after'"))),
             con, useBytes = TRUE)
  close(con)
  writeLines(c("<p>ok</p>", paste0("<p>a", EM_ENTITIES[1], "b</p>")),
             file.path(dir, "page.html"))
  # The sanctioned escape form and a plain hyphen must NOT trip it.
  writeLines(c('x <- grepl("\\u2014", txt)', "a - b"),
             file.path(dir, "clean.R"))
  # Binary files are skipped rather than misread.
  writeBin(as.raw(c(0x00, 0xe2, 0x80, 0x94)), file.path(dir, "blob.bin"))

  found <- .em_offenders(dir, c("dash.R", "page.html", "clean.R", "blob.bin"),
                         c(EM_DASH, EM_ENTITIES))
  expect_setequal(found, c("dash.R:2 (em dash)",
                           sprintf("page.html:2 (%s)", EM_ENTITIES[1])))
})

test_that("the repo root is found from tests/testthat and from the root", {
  root <- .em_repo_root()
  expect_true(file.exists(file.path(root, "DESCRIPTION")))
  expect_identical(.em_repo_root(file.path(root, "tests", "testthat")), root)
  expect_identical(.em_repo_root(root), root)
})
