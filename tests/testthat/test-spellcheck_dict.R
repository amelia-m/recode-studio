# Run with: testthat::test_dir("tests/testthat")
#
# .read_dict_terms(): the Spellcheck tab imports any .txt, so a raw Hunspell
# .dic pasted in must not add dead entries (affix flags, the count line,
# multi-word lines) that hunspell can never match against a token.
# Importing a discipline dictionary: the copy is verified before success is
# reported, and a failed copy selects nothing.
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


# --- Verified copy -----------------------------------------------------------
# file.copy() can return without copying and without an error on Windows
# (paths with spaces are one trigger), so the import copies byte for byte and
# reads the result back before it reports success.

# A source file in a folder whose name has spaces, holding bytes a text-mode
# copy would mangle: CRLF, UTF-8 and no trailing newline.
.dict_bytes <- c(charToRaw("latte\r\nmocha\r\ncaf"), as.raw(c(0xc3, 0xa9)))
.make_src <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(pattern = "dict src with spaces ", .local_envir = env)
  src <- file.path(dir, "My Words.txt")
  writeBin(.dict_bytes, src)
  src
}

test_that(".copy_file_verified copies byte for byte, spaces in the path and all", {
  src  <- .make_src()
  dest <- file.path(withr::local_tempdir(pattern = "dest dir "), "out file.txt")
  res  <- .copy_file_verified(src, dest)
  expect_true(res$ok)
  expect_true(is.na(res$message))
  expect_identical(readBin(dest, "raw", n = 100), .dict_bytes)

  # Overwrites an existing, longer file completely.
  writeBin(as.raw(rep(0x41, 200)), dest)
  expect_true(.copy_file_verified(src, dest)$ok)
  expect_identical(readBin(dest, "raw", n = 1000), .dict_bytes)
})

test_that(".copy_file_verified reports a writer that silently does nothing", {
  src  <- .make_src()
  dest <- file.path(withr::local_tempdir(), "never.txt")
  res  <- .copy_file_verified(src, dest,
                              writer = function(bytes, path) invisible(TRUE))
  expect_false(res$ok)
  expect_match(res$message, "never.txt was not created", fixed = TRUE)
})

test_that(".copy_file_verified reports a short or altered copy", {
  src  <- .make_src()
  dest <- file.path(withr::local_tempdir(), "copy.txt")

  short <- .copy_file_verified(src, dest, writer = function(bytes, path)
    writeBin(bytes[1:4], path))
  expect_false(short$ok)
  expect_match(short$message, "incompletely (4 of 19 bytes)", fixed = TRUE)

  altered <- .copy_file_verified(src, dest, writer = function(bytes, path)
    writeBin(rev(bytes), path))
  expect_false(altered$ok)
  expect_match(altered$message, "does not match", fixed = TRUE)
})

test_that(".copy_file_verified reports a writer error or an unreadable source", {
  src  <- .make_src()
  dest <- file.path(withr::local_tempdir(), "copy.txt")
  err <- .copy_file_verified(src, dest, writer = function(bytes, path)
    stop("disk full"))
  expect_false(err$ok)
  expect_match(err$message, "Could not write copy.txt (disk full)", fixed = TRUE)
  expect_false(file.exists(dest))

  gone <- .copy_file_verified(file.path(tempdir(), "no such upload"), dest)
  expect_false(gone$ok)
  expect_match(gone$message, "could not be read", fixed = TRUE)
})


# --- The import handler ------------------------------------------------------

# Records what the handler asks Shiny to do: MockShinySession drops update
# messages, and a notification's type is the only sign of failure.
.record_import <- function(env = parent.frame()) {
  rec <- new.env(parent = emptyenv())
  rec$selected <- list()
  rec$notes <- list()
  testthat::local_mocked_bindings(
    updateSelectizeInput = function(session, inputId, label = NULL,
                                    choices = NULL, selected = NULL, ...) {
      if (identical(inputId, "disciplines")) {
        rec$selected[[length(rec$selected) + 1L]] <- selected
      }
      invisible(NULL)
    },
    showNotification = function(ui, ..., type = "default") {
      rec$notes[[length(rec$notes) + 1L]] <- list(ui = ui, type = type)
      invisible(NULL)
    },
    .package = "shiny", .env = env)
  rec
}

.upload <- function(src, name = "My Words.txt") {
  data.frame(name = name, size = file.size(src), type = "text/plain",
             datapath = src, stringsAsFactors = FALSE)
}

.import_args <- function() {
  list(shared_state    = shiny::reactiveValues(),
       selected_var_r  = shiny::reactiveVal(NULL),
       unique_values_r = shiny::reactive(NULL),
       rules_proxy     = list())
}

.dest <- file.path("dictionary", "disciplines", "My_Words.txt")
.was_selected <- function(rec) {
  any(vapply(rec$selected, function(s) .dest %in% s, logical(1)))
}

test_that("a successful import copies the file, selects it and says so", {
  src <- .make_src()
  # .dict_paths() is relative to the app root; run in a scratch folder.
  withr::local_dir(withr::local_tempdir(pattern = "app root "))
  rec <- .record_import()
  shiny::testServer(mod_spellcheck_view_server, args = .import_args(), {
    session$flushReact()
    session$setInputs(import_discipline = .upload(src))
    expect_identical(readBin(.dest, "raw", n = 100), .dict_bytes)
    expect_true(.was_selected(rec))
    last <- rec$notes[[length(rec$notes)]]
    expect_equal(last$type, "message")
    # The message names the file that was written: it differs from the
    # uploaded name whenever that name was already taken.
    expect_match(last$ui, "Imported 'My Words.txt' as My_Words.txt (3 terms).",
                 fixed = TRUE)
  })
})

test_that("an import whose write fails reports an error and selects nothing", {
  src <- .make_src()
  withr::local_dir(withr::local_tempdir())
  # A FILE where the disciplines folder should be makes the real write fail.
  # (A folder in the destination's place no longer does: the import picks a
  # free name instead of overwriting whatever is there.)
  dir.create("dictionary")
  writeLines("not a folder", file.path("dictionary", "disciplines"))
  rec <- .record_import()
  shiny::testServer(mod_spellcheck_view_server, args = .import_args(), {
    session$flushReact()
    session$setInputs(import_discipline = .upload(src))
    expect_false(.was_selected(rec))
    expect_length(rec$notes, 1L)
    expect_equal(rec$notes[[1]]$type, "error")
    expect_match(rec$notes[[1]]$ui, "Import of 'My Words.txt' failed",
                 fixed = TRUE)
  })
})

test_that("an import does not overwrite a dictionary already there (security F2)", {
  src <- .make_src()
  withr::local_dir(withr::local_tempdir(pattern = "app root "))
  dir.create(file.path("dictionary", "disciplines"), recursive = TRUE)
  kept <- file.path("dictionary", "disciplines", "medical.txt")
  writeLines(c("tracheostomy", "bronchoscopy"), kept)
  before <- readBin(kept, "raw", n = 1000)

  rec <- .record_import()
  shiny::testServer(mod_spellcheck_view_server, args = .import_args(), {
    session$flushReact()
    session$setInputs(import_discipline = .upload(src, name = "medical.txt"))
    # The committed file is untouched; the upload went to a free name.
    expect_identical(readBin(kept, "raw", n = 1000), before)
    dupe <- file.path("dictionary", "disciplines", "medical_2.txt")
    expect_true(file.exists(dupe))
    expect_identical(readBin(dupe, "raw", n = 100), .dict_bytes)
    # The user is told which file was written, and it is the one selected.
    last <- rec$notes[[length(rec$notes)]]
    expect_equal(last$type, "message")
    expect_match(last$ui, "medical_2.txt", fixed = TRUE)
    expect_true(any(vapply(rec$selected, function(s) dupe %in% s, logical(1))))
  })
})

test_that("repeated imports of one name keep counting up and keep every file", {
  src <- .make_src()
  withr::local_dir(withr::local_tempdir(pattern = "app root "))
  rec <- .record_import()
  shiny::testServer(mod_spellcheck_view_server, args = .import_args(), {
    session$flushReact()
    for (k in 1:3) {
      session$setInputs(import_discipline = .upload(src, name = "notes.txt"))
    }
  })
  expect_setequal(
    list.files(file.path("dictionary", "disciplines")),
    c("notes.txt", "notes_2.txt", "notes_3.txt"))
})

test_that("a binary file renamed .txt is refused, nothing written (security F7)", {
  bin <- file.path(withr::local_tempdir(), "words.txt")
  writeBin(as.raw(c(0x50, 0x4b, 0x03, 0x04, 0x00, 0x00, 0xff, 0xfe, 0x00)), bin)
  withr::local_dir(withr::local_tempdir(pattern = "app root "))
  rec <- .record_import()
  shiny::testServer(mod_spellcheck_view_server, args = .import_args(), {
    session$flushReact()
    session$setInputs(import_discipline = .upload(bin, name = "words.txt"))
    expect_false(file.exists(file.path("dictionary", "disciplines",
                                       "words.txt")))
    expect_length(rec$notes, 1L)
    expect_equal(rec$notes[[1]]$type, "error")
    # A plain-language reason, not a raw encoding error.
    expect_match(rec$notes[[1]]$ui, "not a text file", fixed = TRUE)
    expect_false(any(vapply(rec$selected, length, integer(1)) > 0))
  })
})

test_that("a file that is not valid UTF-8 text is refused with a plain reason", {
  latin1 <- file.path(withr::local_tempdir(), "words.txt")
  # "café" in latin-1: the 0xe9 byte is not valid UTF-8.
  writeBin(c(charToRaw("caf"), as.raw(0xe9), charToRaw("\n")), latin1)
  withr::local_dir(withr::local_tempdir(pattern = "app root "))
  rec <- .record_import()
  shiny::testServer(mod_spellcheck_view_server, args = .import_args(), {
    session$flushReact()
    session$setInputs(import_discipline = .upload(latin1, name = "words.txt"))
    expect_false(file.exists(file.path("dictionary", "disciplines",
                                       "words.txt")))
    expect_equal(rec$notes[[1]]$type, "error")
    expect_match(rec$notes[[1]]$ui, "UTF-8", fixed = TRUE)
  })
})

test_that(".read_dict_terms never throws on a file it cannot read as text", {
  dir <- withr::local_tempdir()
  nul <- file.path(dir, "nul.txt")
  writeBin(as.raw(c(0x6c, 0x61, 0x74, 0x74, 0x65, 0x00, 0xff, 0xfe)), nul)
  expect_silent(terms <- .read_dict_terms(nul))
  expect_type(terms, "character")

  bad_utf8 <- file.path(dir, "bad.txt")
  writeBin(c(charToRaw("latte\n"), as.raw(0xe9), charToRaw("\nmocha\n")),
           bad_utf8)
  expect_silent(terms <- .read_dict_terms(bad_utf8))
  # The readable lines still come through; the unreadable one is dropped.
  expect_true(all(c("latte", "mocha") %in% terms))

  # A directory, a missing path and a nonsense path are all "no terms".
  expect_identical(.read_dict_terms(dir), character(0))
  expect_identical(.read_dict_terms(NA_character_), character(0))
  expect_identical(.read_dict_terms(character(0)), character(0))
})

test_that("an import whose copy silently does nothing is not reported as a success", {
  src <- .make_src()
  withr::local_dir(withr::local_tempdir())
  rec <- .record_import()
  # Stand in the silent file.copy() failure: a writer that returns normally
  # and writes nothing. The module looks .write_bytes up where this file
  # sourced it, so swap it there and put it back afterwards.
  home <- environment(mod_spellcheck_view_server)
  real <- get(".write_bytes", envir = home)
  assign(".write_bytes", function(bytes, path) invisible(path), envir = home)
  withr::defer(assign(".write_bytes", real, envir = home))
  shiny::testServer(mod_spellcheck_view_server, args = .import_args(), {
    session$flushReact()
    session$setInputs(import_discipline = .upload(src))
    expect_false(file.exists(.dest))
    expect_false(.was_selected(rec))
    expect_length(rec$notes, 1L)
    expect_equal(rec$notes[[1]]$type, "error")
    expect_match(rec$notes[[1]]$ui, "My_Words.txt was not created",
                 fixed = TRUE)
  })
})
