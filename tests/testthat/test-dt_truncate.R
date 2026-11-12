# Run with: testthat::test_dir("tests/testthat")
#
# dt_truncate_defs() (columnDefs that shorten long cells to one line and put a
# longer, capped copy in the cell title), dt_truncate_css() and
# dt_truncate_hint() in R/ui_helpers.R. Pure builders; every string here is
# synthetic.
#
# The renderer is JavaScript that only a browser runs, and this project has no
# JS engine among its dependencies, so the cuts themselves are asserted on the
# emitted renderer rather than executed. The two "wired into a real datatable"
# tests below cover everything the R side does decide.

source(file.path("..", "..", "R", "ui_helpers.R"))

.LONG <- paste(rep("the latte was lovely and the pastry was fresh", 20),
               collapse = " ")

.render_js <- function(defs) paste(as.character(defs[[1]]$render), collapse = "\n")

# --- shape ---------------------------------------------------------------------

test_that("dt_truncate_defs returns a single columnDefs entry for all columns", {
  defs <- dt_truncate_defs()
  expect_type(defs, "list")
  expect_length(defs, 1L)
  expect_named(defs[[1]], c("targets", "render"))
  expect_identical(defs[[1]]$targets, "_all")
  expect_s3_class(defs[[1]]$render, "JS_EVAL")
})

test_that("dt_truncate_defs bakes the character budget into the renderer", {
  expect_match(.render_js(dt_truncate_defs()), "s.length <= 60", fixed = TRUE)
  expect_match(.render_js(dt_truncate_defs(chars = 25)),
               "s.length <= 25", fixed = TRUE)
  expect_match(.render_js(dt_truncate_defs(chars = 25)),
               "s.substr(0, 25)", fixed = TRUE)
})

test_that("dt_truncate_defs targets named columns when given indices", {
  defs <- dt_truncate_defs(targets = c(2, 5))
  expect_equal(defs[[1]]$targets, c(2, 5))
})

# --- what the renderer promises ------------------------------------------------

test_that("the renderer only touches the display path", {
  js <- .render_js(dt_truncate_defs())
  # Search, sort and type detection arrive with a non-display `type` and must
  # get the untouched value, or filtering would only see the visible head.
  expect_match(js, "if (type !== 'display') { return data; }", fixed = TRUE)
  expect_match(js, "data === null", fixed = TRUE)
  expect_match(js, "typeof data === 'undefined'", fixed = TRUE)
})

test_that("the renderer puts a longer, but bounded, value in the cell title", {
  js <- .render_js(dt_truncate_defs())
  expect_match(js, "title=", fixed = TRUE)
  # The title is built from the CAPPED copy, not from the whole value: it used
  # to carry every byte, which made the page as big as the data (security F9).
  expect_match(js, "full.replace(/\"/g, '&quot;')", fixed = TRUE)
  expect_no_match(js, "s.replace(/\"/g, '&quot;')", fixed = TRUE)
  expect_match(js, "\\\\u2026")
  expect_match(js, "rs-dt-trunc", fixed = TRUE)
})

test_that("the cell title is capped, well above the visible cut (security F9)", {
  js <- .render_js(dt_truncate_defs())
  # Default: 60 characters shown, 1000 kept for hover.
  expect_match(js, "s.length <= 1000", fixed = TRUE)
  expect_match(js, "s.substr(0, 1000)", fixed = TRUE)

  js <- .render_js(dt_truncate_defs(chars = 20, title_chars = 200))
  expect_match(js, "s.length <= 20", fixed = TRUE)
  expect_match(js, "s.length <= 200", fixed = TRUE)
  expect_match(js, "s.substr(0, 200)", fixed = TRUE)

  # Both cuts trim a half-cut HTML entity, not just the visible one.
  expect_equal(length(gregexpr("/&[#a-zA-Z0-9]*$/", js, fixed = TRUE)[[1]]), 2L)
})

test_that("the renderer will not let a value break out of the title attribute", {
  js <- .render_js(dt_truncate_defs())
  # DT's escape = TRUE handles & < > server-side but leaves the double quote,
  # the one character that could close the attribute early.
  expect_match(js, "&quot;", fixed = TRUE)
  # A cut that lands mid-entity ("&am") is trimmed rather than shown raw.
  expect_match(js, "/&[#a-zA-Z0-9]*$/", fixed = TRUE)
})

# --- validation ----------------------------------------------------------------

test_that("dt_truncate_defs rejects a nonsense budget or target", {
  expect_error(dt_truncate_defs(chars = 0), "positive")
  expect_error(dt_truncate_defs(chars = c(10, 20)), "positive")
  expect_error(dt_truncate_defs(chars = NA), "positive")
  expect_error(dt_truncate_defs(chars = "60"), "positive")
  expect_error(dt_truncate_defs(targets = "review"), "0-based")
  expect_error(dt_truncate_defs(title_chars = 0), "title_chars")
  expect_error(dt_truncate_defs(title_chars = NA), "title_chars")
  expect_error(dt_truncate_defs(title_chars = c(10, 20)), "title_chars")
})

# --- wired into a real datatable -----------------------------------------------

test_that("a datatable using the defs still carries the complete values", {
  df <- data.frame(order = c("A-1", "A-2"), review = c(.LONG, "short"),
                   stringsAsFactors = FALSE)
  w <- DT::datatable(df, rownames = FALSE,
                     options = list(dom = "t", columnDefs = dt_truncate_defs()))
  expect_true(any(grepl(.LONG, unlist(w$x$data), fixed = TRUE)))
  has_trunc <- vapply(w$x$options$columnDefs, function(d) {
    isTRUE(grepl("rs-dt-trunc", paste(as.character(d$render), collapse = ""),
                 fixed = TRUE))
  }, logical(1))
  expect_true(any(has_trunc))
})

test_that("dataset markup still reaches the renderer HTML-escaped", {
  # The renderer builds HTML from the cell, so it is only safe while DT's
  # default escape = TRUE is in force for that column.
  df <- data.frame(review = "<img src=x onerror=alert(1)>",
                   stringsAsFactors = FALSE)
  w <- DT::datatable(df, rownames = FALSE,
                     options = list(columnDefs = dt_truncate_defs()))
  # DT escapes in its pre-render hook (and in the server-side filter), so run
  # the hook to see what is actually sent to the browser.
  cells <- unlist(w$preRenderHook(w)$x$data)
  expect_false(any(grepl("<img", cells, fixed = TRUE)))
  expect_true(any(grepl("&lt;img", cells, fixed = TRUE)))
})

# The grep over mod_value_table.R / mod_preview_export.R that used to sit here
# was deleted: it read those files' SOURCE TEXT for "dt_truncate_defs()" and
# "escape = FALSE", which proves nothing runs, passes on a broken renderer and
# fails on a harmless rewording. The two tests above exercise the real
# datatable, and test-escape_guard.R checks escape = FALSE across every app
# source from parse tokens, so comments and strings cannot fool it.

# --- css + hint ----------------------------------------------------------------

test_that("dt_truncate_css holds the span to one line", {
  # singleton(tags$head(...)) prints as nothing; render it to reach the head.
  html <- as.character(htmltools::renderTags(dt_truncate_css())$head)
  expect_match(html, "rs-dt-trunc", fixed = TRUE)
  expect_match(html, "white-space: nowrap", fixed = TRUE)
  expect_match(html, "text-overflow: ellipsis", fixed = TRUE)
})

test_that("dt_truncate_hint does not promise the full value on hover (security F9)", {
  # Rendered tags wrap over lines; compare the text as one run of words.
  html <- gsub("\\s+", " ", paste(as.character(dt_truncate_hint()),
                                  collapse = " "))
  expect_match(html, "text-muted")
  expect_match(html, "Hover")
  # It used to say hovering shows "the full value", which stopped being true
  # when the title was capped. It now points at the export for that.
  expect_no_match(html, "full value", fixed = TRUE)
  expect_match(html, "read more of it", fixed = TRUE)
  expect_match(html, "exported files always use the complete value",
               fixed = TRUE)
})

test_that("dt_truncate_hint appends an extra sentence", {
  html <- as.character(
    dt_truncate_hint(shiny::tagList("See ", shiny::strong("the export"), ".")))
  expect_match(html, "<strong>the export</strong>")
})
