# Guard: no bare `escape = FALSE` anywhere in the app's R source.
#
# This app renders third-party CSV / Excel content in DT tables. `escape =
# FALSE` unescapes EVERY column, so a cell holding `<img src=x onerror=...>`
# would run on render (see the Gotchas in CLAUDE.md). The safe pattern is
# `escape = -ncol(df)` with the generated-markup column last.
#
# The check reads R's own parse tokens rather than grepping text, so any
# spacing or line break between `escape`, `=` and `FALSE` (or `F`) is caught,
# while comments and strings that merely mention it are not. This file lives
# under tests/, which the scan does not read, so it cannot trip itself.

# Find `escape = FALSE` / `escape = F` argument uses in R source text.
# Returns "file:line" strings.
.find_bare_escape_false <- function(text, label) {
  pd <- utils::getParseData(parse(text = text, keep.source = TRUE))
  if (is.null(pd) || nrow(pd) == 0) return(character(0))
  tok <- pd[pd$terminal & !(pd$token %in% c("COMMENT")), ]
  tok <- tok[order(tok$line1, tok$col1), ]
  hits <- character(0)
  n <- nrow(tok)
  for (i in seq_len(max(0, n - 2))) {
    if (tok$token[i] == "SYMBOL_SUB" && tok$text[i] == "escape" &&
        tok$token[i + 1] == "EQ_SUB" &&
        ((tok$token[i + 2] == "NUM_CONST" && tok$text[i + 2] == "FALSE") ||
         (tok$token[i + 2] == "SYMBOL" && tok$text[i + 2] == "F"))) {
      hits <- c(hits, sprintf("%s:%d", label, tok$line1[i]))
    }
  }
  hits
}

test_that("the detector catches the forms it is meant to", {
  flag <- function(code) length(.find_bare_escape_false(code, "x")) > 0
  expect_true(flag("DT::datatable(df, escape = FALSE)"))
  expect_true(flag("DT::datatable(df, escape=FALSE)"))
  expect_true(flag("DT::datatable(df,\n  escape\n  =\n  FALSE)"))
  expect_true(flag("DT::datatable(df, escape = F)"))
  expect_false(flag("DT::datatable(df, escape = -ncol(df))"))
  expect_false(flag("DT::datatable(df, escape = TRUE)"))
  expect_false(flag("# never write escape = FALSE here\nx <- 1"))
  expect_false(flag("msg <- 'escape = FALSE is banned'"))
  expect_identical(.find_bare_escape_false("f(1)\ng(escape = FALSE)", "a.R"),
                   "a.R:2")
})

test_that("no R source file in the app uses a bare escape = FALSE", {
  root  <- file.path("..", "..")
  files <- c(list.files(file.path(root, "R"), pattern = "\\.[Rr]$",
                        full.names = TRUE),
             file.path(root, "app.R"))
  files <- files[file.exists(files)]
  expect_gt(length(files), 5)
  hits <- unlist(lapply(files, function(f) {
    .find_bare_escape_false(readLines(f, warn = FALSE, encoding = "UTF-8"),
                            sub("^[.][.]/[.][.]/", "", f))
  }))
  expect(length(hits) == 0,
         sprintf(paste0("Bare escape = FALSE renders dataset text as HTML. ",
                        "Use escape = -ncol(df) with the markup column last. ",
                        "Found at: %s"), paste(hits, collapse = ", ")))
})
