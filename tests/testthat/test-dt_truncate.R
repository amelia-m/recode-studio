# Run with: testthat::test_dir("tests/testthat")
#
# dt_truncate_defs() (columnDefs that shorten long cells to one line and put
# the whole value in the cell title), dt_truncate_css() and dt_truncate_hint()
# in R/ui_helpers.R. Pure builders; every string here is synthetic.

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

test_that("the renderer keeps the full value reachable in the cell title", {
  js <- .render_js(dt_truncate_defs())
  expect_match(js, "title=", fixed = TRUE)
  expect_match(js, "s.replace(/\"/g, '&quot;')", fixed = TRUE)
  expect_match(js, "\\\\u2026")
  expect_match(js, "rs-dt-trunc", fixed = TRUE)
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

test_that("both motivating tables use the shared helper and keep escaping", {
  for (f in c("mod_value_table.R", "mod_preview_export.R")) {
    src <- paste(readLines(file.path("..", "..", "R", f), warn = FALSE),
                 collapse = "\n")
    expect_match(src, "dt_truncate_defs()", fixed = TRUE, info = f)
    expect_match(src, "dt_truncate_css()", fixed = TRUE, info = f)
    expect_false(grepl("escape\\s*=\\s*FALSE", src), info = f)
  }
})

# --- css + hint ----------------------------------------------------------------

test_that("dt_truncate_css holds the span to one line", {
  # singleton(tags$head(...)) prints as nothing; render it to reach the head.
  html <- as.character(htmltools::renderTags(dt_truncate_css())$head)
  expect_match(html, "rs-dt-trunc", fixed = TRUE)
  expect_match(html, "white-space: nowrap", fixed = TRUE)
  expect_match(html, "text-overflow: ellipsis", fixed = TRUE)
})

test_that("dt_truncate_hint says how to reach the full value", {
  html <- as.character(dt_truncate_hint())
  expect_match(html, "text-muted")
  expect_match(html, "Hover")
  expect_match(html, "full value")
})

test_that("dt_truncate_hint appends an extra sentence", {
  html <- as.character(
    dt_truncate_hint(shiny::tagList("See ", shiny::strong("the export"), ".")))
  expect_match(html, "<strong>the export</strong>")
})
