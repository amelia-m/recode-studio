# Run with: testthat::test_dir("tests/testthat")
#
# Help layer in R/ui_helpers.R: help_line(), help_btn(), guide_note(),
# card_header_help(). Checks the accessibility attributes, that guide_note()
# is collapsed unless asked, and that text passed in is escaped.

source(file.path("..", "..", "R", "ui_helpers.R"))

.html <- function(x) as.character(htmltools::renderTags(x)$html)

test_that("help_btn() trigger is a labelled, focusable button", {
  h <- .html(help_btn("Some help.", title = "Clusters"))
  expect_match(h, 'class="rs-help-btn"', fixed = TRUE)
  expect_match(h, 'role="button"', fixed = TRUE)
  expect_match(h, 'tabindex="0"', fixed = TRUE)
  expect_match(h, 'aria-label="Help: Clusters"', fixed = TRUE)
  expect_match(h, "Some help.", fixed = TRUE)
})

test_that("help_btn() without a title is labelled plainly", {
  h <- .html(help_btn("x"))
  expect_match(h, 'aria-label="Help"', fixed = TRUE)
})

test_that("help_btn() ships the outside-click dismiss script once", {
  tags <- htmltools::renderTags(shiny::tagList(help_btn("a"), help_btn("b")))
  head <- as.character(tags$head)
  expect_equal(lengths(regmatches(head, gregexpr("rsHelpDismissBound =", head))),
               1L)
})

test_that("guide_note() is collapsed by default and can open", {
  closed <- .html(guide_note("Body text", title = "How it works"))
  expect_match(closed, "<details", fixed = TRUE)
  expect_match(closed, "<summary", fixed = TRUE)
  expect_false(grepl("<details[^>]*\\bopen\\b", closed))
  expect_match(closed, "How it works", fixed = TRUE)

  opened <- .html(guide_note("Body text", open = TRUE))
  expect_match(opened, "<details[^>]*open=\"open\"")
})

test_that("guide_note() summary gets a visible keyboard focus style", {
  head <- as.character(htmltools::renderTags(guide_note("x"))$head)
  expect_match(head, ".rs-guide-note > summary:focus-visible", fixed = TRUE)
})

test_that("user-supplied text is escaped in every helper", {
  evil <- "<img src=x onerror=alert(1)>"
  for (h in list(.html(help_btn(evil, title = evil)),
                 .html(guide_note(evil, title = evil)),
                 .html(help_line(evil)),
                 .html(card_header_help(evil, evil)))) {
    expect_false(grepl(evil, h, fixed = TRUE))
    expect_match(h, "&lt;img src=x onerror=alert(1)&gt;", fixed = TRUE)
  }
})

test_that("card_header_help() puts the title and a help button in the header", {
  h <- .html(card_header_help("Load data", "Pick a file."))
  expect_match(h, "card-header", fixed = TRUE)
  expect_match(h, "Load data", fixed = TRUE)
  expect_match(h, 'aria-label="Help: Load data"', fixed = TRUE)
})
