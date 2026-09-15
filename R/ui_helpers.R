# =============================================================================
# UI helpers (bslib cards, badges, banners) + small utilities
# =============================================================================

# null-coalescing helper used across modules
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Yellow info banner.
warning_banner <- function(text) {
  shiny::div(
    class = "alert alert-warning",
    role  = "alert",
    shiny::tags$strong("Heads up: "),
    text
  )
}

#' One line of small muted help text under or next to a control.
#'
#' For when a control's effect is not obvious but a full banner would be
#' overkill.
help_line <- function(...) {
  shiny::div(class = "small text-muted mb-2", ...)
}

#' Muted placeholder shown instead of a table that has nothing to show.
#'
#' Use this in a uiOutput next to the DTOutput, and have renderDT return NULL
#' in the same state. Do NOT render `DT::datatable(tibble(message = ...))`:
#' DT draws a header row with the literal column name "message" and a sort
#' arrow, which reads as an unfinished placeholder.
empty_state <- function(text) {
  shiny::div(class = "text-muted fst-italic py-2", text)
}

#' DT column definitions that keep long cells to one readable line.
#'
#' A table with one long free-text column (a sentence-length review, a long
#' note) wraps that cell over many lines, so a page of 25 rows is far taller
#' than the screen. Drop the result into `options$columnDefs` of
#' `DT::datatable()`, and put `dt_truncate_css()` somewhere in the same UI.
#'
#' Every display cell is wrapped in a span that CSS holds to one line with an
#' ellipsis (a character cap alone cannot promise one line: a short value still
#' wraps in a narrow column). Cells longer than `chars` are also cut to their
#' first `chars` characters, so a huge value does not bloat the page. The WHOLE
#' value rides in the span's `title`, so hovering shows everything.
#'
#' Display-only: DataTables calls the renderer with `type` "filter", "sort"
#' and "type" as well as "display", and those paths return the value untouched,
#' so search and ordering still see full values. The data behind the table is
#' unchanged.
#'
#' Escaping: this relies on DT's default `escape = TRUE` for the targeted
#' columns. The data then arrives already HTML-escaped for `&`, `<` and `>`;
#' the renderer additionally escapes the double quote (which DT leaves alone)
#' so a value cannot break out of the `title` attribute, and trims a half-cut
#' entity such as `&am` off the end of the visible text. Never target a column
#' that is unescaped generated markup.
#'
#' @param chars   Characters shown before cutting. Default 60.
#' @param targets DataTables column targets: `"_all"` (default) or a vector of
#'   0-based column indices.
#' @return A list holding one columnDefs entry, ready for `options$columnDefs`.
dt_truncate_defs <- function(chars = 60, targets = "_all") {
  if (!is.numeric(chars) || length(chars) != 1L || is.na(chars) || chars < 1) {
    stop("dt_truncate_defs(): `chars` must be a single positive number.")
  }
  if (!(identical(targets, "_all") || is.numeric(targets))) {
    stop("dt_truncate_defs(): `targets` must be \"_all\" or 0-based column ",
         "indices.")
  }
  chars <- as.integer(chars)
  js <- paste(c(
    "function(data, type, row, meta) {",
    "  if (type !== 'display') { return data; }",
    "  if (data === null || typeof data === 'undefined') { return data; }",
    "  var s = String(data);",
    sprintf("  var head = s.length <= %d ? s :", chars),
    sprintf("    s.substr(0, %d).replace(/&[#a-zA-Z0-9]*$/, '') + '\\u2026';",
            chars),
    "  return '<span class=\"rs-dt-trunc\" title=\"' +",
    "         s.replace(/\"/g, '&quot;') +",
    "         '\">' + head + '</span>';",
    "}"), collapse = "\n")
  list(list(targets = targets, render = DT::JS(js)))
}

#' CSS that enforces the one-line promise of `dt_truncate_defs()`.
#'
#' Emitted once per page via `shiny::singleton()`, so every table that uses the
#' truncation can include it. `display:block` makes the ellipsis work inside a
#' table cell; `max-width` stops a nowrap value forcing an auto-layout column
#' wide open. A fixed-layout table can lift the cap (see mod_value_table.R).
dt_truncate_css <- function() {
  shiny::singleton(shiny::tags$head(shiny::tags$style(shiny::HTML("
    td .rs-dt-trunc {
      display: block;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      max-width: 34ch;
    }
  "))))
}

#' One-line hint for a table that uses `dt_truncate_defs()`.
#'
#' Keeps the wording identical wherever truncation is used.
#'
#' @param extra Optional extra sentence appended after the hover hint.
dt_truncate_hint <- function(extra = NULL) {
  shiny::tags$p(
    class = "text-muted small mb-1 rs-dt-trunc-hint",
    "Long values are shortened to one line so more rows fit. ",
    "Hover a shortened cell to read the full value; sorting, the table ",
    "search and the exported files always use the complete value.",
    if (!is.null(extra)) shiny::tagList(" ", extra)
  )
}

#' Small coloured badge for inline status.
badge <- function(text, color = c("grey", "green", "yellow", "red", "blue")) {
  color <- match.arg(color)
  bg <- switch(color,
    grey   = "#6c757d",
    green  = "#198754",
    yellow = "#ffc107",
    red    = "#dc3545",
    blue   = "#0d6efd"
  )
  shiny::tags$span(
    style = sprintf(
      "display:inline-block;padding:.15em .55em;border-radius:.5em;background:%s;color:white;font-size:.8em;",
      bg
    ),
    text
  )
}
