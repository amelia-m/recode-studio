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

# --- Help layer ----------------------------------------------------------------
# Three sizes of in-app guidance, smallest first:
#   help_line()  one muted line under or next to a control
#   help_btn()   a small "?" in a card header that opens a short popover
#   guide_note() a collapsed <details> block for longer "how this works" text
# All user-visible text passed in goes through htmltools, so it is escaped
# unless the caller deliberately wraps it in HTML().

#' One line of small muted help text under or next to a control.
#'
#' For when a control's effect is not obvious but a full banner would be
#' overkill.
help_line <- function(...) {
  shiny::div(class = "small text-muted mb-2 rs-help-line", ...)
}

#' Shared CSS + JS for help_btn() and guide_note().
#'
#' Emitted once per page via shiny::singleton(), so every help component can
#' include it without duplicating the block.
#'
#' The script closes an open help popover on a click anywhere outside it.
#' bslib popovers otherwise close only on a second click of the trigger, or on
#' Esc, which bslib already handles. While a Bootstrap popover is open its
#' trigger carries `aria-describedby`, so `.rs-help-btn[aria-describedby]` is
#' exactly the set of open help popovers; other popovers are left alone. It
#' listens in the capture phase so a widget that stops propagation still
#' dismisses.
guide_css <- function() {
  shiny::singleton(shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      .rs-help-btn { display: inline-block; margin-left: .35rem;
        color: var(--bs-secondary-color, #6c757d); opacity: .75;
        cursor: pointer; font-size: .85em; line-height: 1;
        vertical-align: baseline; }
      .rs-help-btn:hover, .rs-help-btn:focus-visible {
        color: var(--bs-primary, #2c3e50); opacity: 1; }
      .rs-help-btn:focus-visible {
        outline: 2px solid var(--bs-primary, #2c3e50); outline-offset: 2px;
        border-radius: 50%; }
      .rs-help-popover { font-size: .85rem; }
      .rs-guide-note { border: 1px solid var(--bs-border-color, #dee2e6);
        border-radius: .4rem; margin-bottom: .75rem;
        background: var(--bs-body-bg, #fff); }
      .rs-guide-note > summary { cursor: pointer; padding: .45rem .7rem;
        font-size: .85rem; font-weight: 600;
        color: var(--bs-secondary-color, #6c757d);
        background: var(--bs-secondary-bg, #eef1f4);
        border-radius: .35rem; }
      .rs-guide-note > summary:hover {
        background: var(--bs-tertiary-bg, #e2e6ea);
        color: var(--bs-body-color, #212529); }
      .rs-guide-note > summary:focus-visible {
        outline: 2px solid var(--bs-primary, #2c3e50); outline-offset: 1px; }
      .rs-guide-note[open] > summary {
        border-bottom: 1px solid var(--bs-border-color, #dee2e6);
        border-bottom-left-radius: 0; border-bottom-right-radius: 0; }
      .rs-guide-note-body { padding: .5rem .75rem .65rem; font-size: .85rem; }
    ")),
    shiny::tags$script(shiny::HTML("
      (function() {
        if (window.rsHelpDismissBound) { return; }
        window.rsHelpDismissBound = true;
        document.addEventListener('click', function(ev) {
          if (!window.bootstrap || !window.bootstrap.Popover) { return; }
          var open = document.querySelectorAll('.rs-help-btn[aria-describedby]');
          for (var i = 0; i < open.length; i++) {
            var trig = open[i];
            if (trig.contains(ev.target)) { continue; }
            var pop = document.getElementById(trig.getAttribute('aria-describedby'));
            if (pop && pop.contains(ev.target)) { continue; }
            var inst = window.bootstrap.Popover.getInstance(trig);
            if (inst) { inst.hide(); }
          }
        }, true);
      })();
    "))
  ))
}

#' Small "?" trigger that opens a popover with short help text.
#'
#' Put it in a card header next to the title. The trigger is keyboard
#' reachable (`tabindex="0"`), announced as a button (`role="button"`) and
#' labelled for screen readers (`aria-label`). Click or press Enter to open;
#' click it again, click elsewhere, or press Esc to close.
#'
#' @param ... popover content (text and/or tags). Plain strings are escaped.
#' @param title optional popover header; also used in the aria-label.
help_btn <- function(..., title = NULL) {
  trigger <- shiny::tags$span(
    class        = "rs-help-btn",
    role         = "button",
    tabindex     = "0",
    `aria-label` = if (is.null(title)) "Help" else paste("Help:", title),
    shiny::icon("circle-question")
  )
  body <- shiny::div(class = "rs-help-popover", ...)
  shiny::tagList(guide_css(), bslib::popover(trigger, body, title = title))
}

#' Collapsed "how this works" block.
#'
#' A styled <details>/<summary>: closed by default, so it costs one line until
#' the user opens it. The summary is keyboard focusable by default and gets a
#' visible focus ring.
#'
#' @param ... block content (text and/or tags). Plain strings are escaped.
#' @param title summary line shown while collapsed.
#' @param icon Font Awesome icon name for the summary line.
#' @param open TRUE to render expanded. Default FALSE.
guide_note <- function(..., title = "How this works", icon = "circle-info",
                       open = FALSE) {
  det <- shiny::tags$details(
    class = "rs-guide-note",
    shiny::tags$summary(shiny::icon(icon, class = "me-1"), title),
    shiny::div(class = "rs-guide-note-body", ...)
  )
  if (isTRUE(open)) det$attribs$open <- "open"
  shiny::tagList(guide_css(), det)
}

#' Card header text with a help_btn() next to it.
#'
#' @param title header text.
#' @param ... help_btn() content.
card_header_help <- function(title, ...) {
  bslib::card_header(shiny::span(title, help_btn(..., title = title)))
}

#' Readable title for a validation or warning class name.
#'
#' Renderers look class names up in a fixed vector of titles. A plain
#' `titles[[nm]]` throws "subscript out of bounds" on a name the vector does
#' not have, so a class added to a validator later would crash the panel that
#' shows it. This returns the known title, or a readable fallback built from
#' the class name itself ("invalid_sibling_pattern" -> "Invalid sibling
#' pattern").
#'
#' @param nm     class name (single string).
#' @param titles named character vector of known titles.
#' @return single string.
issue_class_title <- function(nm, titles = character(0)) {
  nm <- as.character(nm)
  if (length(nm) != 1 || is.na(nm) || !nzchar(nm)) return("Other issue")
  if (nm %in% names(titles)) return(unname(titles[nm][1]))
  words <- gsub("[_.]+", " ", nm)
  paste0(toupper(substr(words, 1, 1)), substring(words, 2))
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

#' JSON-encode a value for use inside inline JavaScript.
#'
#' A JSON string or number is also a valid JavaScript literal, and jsonlite
#' escapes quotes, backslashes and control characters. Never build an inline
#' handler by interpolating data into `'%s'`: escaping only the single quote
#' leaves `"`, `<` and `\` free to break out (see the Gotchas in CLAUDE.md).
.js_value <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
}

#' Escape a whole attribute value.
#'
#' The browser decodes the entities before the JavaScript is parsed, so the
#' script sees exactly what was built here, and a `"` in the data can no longer
#' end the attribute. Use it on the finished handler string, not on the pieces.
.attr_value <- function(x) {
  as.character(htmltools::htmlEscape(x, attribute = TRUE))
}


# --- Per-row rule delete buttons ----------------------------------------------
# The Recodes, Normalize and Recategorize tables each carry a Delete / Remove
# button per rule row. Both halves of that round trip used to be unsafe:
#
#   * The button pasted the stored id straight into a quoted JavaScript string
#     in the one table column DT does not escape. Rule ids come from imported
#     CSVs and are never rewritten, so a crafted id ran as script.
#   * The handler then deleted by matching that id string, so a duplicated or
#     blank id removed every row sharing it, and an NA id matched nothing
#     under `!=` and left an all-NA row behind, which rode into the export.
#
# The payload now names the row POSITION and carries the stored id only as a
# cross-check, and everything that reaches the markup goes through
# `.js_value()` and then `.attr_value()`.

#' The JavaScript one Delete button runs.
#'
#' Sends `{"row": <1-based row>, "id": <stored id, or null when missing>}` as a
#' JSON string, which `.rule_delete_target()` parses server-side.
#'
#' @param input_id namespaced input id to set.
#' @param row_idx  1-based row of the rule in the CURRENT rule set.
#' @param id       the row's stored id, used only to detect a stale click.
.rule_delete_js <- function(input_id, row_idx, id) {
  payload <- .js_value(list(row = as.integer(row_idx), id = as.character(id)))
  sprintf("Shiny.setInputValue(%s, JSON.stringify(%s), {priority: 'event'});",
          .js_value(input_id), payload)
}

#' One row's Delete / Remove button, as an HTML string.
#'
#' Goes in the LAST column of its table, the only one with escaping turned off.
#' The handler string is built from JSON-encoded values and then
#' attribute-escaped whole, so no part of a rule id can end the attribute or
#' close the script.
#'
#' @param label button text ("Delete", or "Remove" on the Normalize tab).
#' @return one HTML string.
.rule_delete_button <- function(input_id, row_idx, id, label = "Delete") {
  sprintf('<button class="btn btn-xs btn-outline-danger" onclick="%s">%s</button>',
          .attr_value(.rule_delete_js(input_id, row_idx, id)),
          htmltools::htmlEscape(label))
}

#' Resolve a Delete payload to exactly ONE row of the current rule set.
#'
#' The payload is whatever the browser sent, so it is validated before use: a
#' client can set any input value it likes. The row must be a single whole
#' number inside the rule set, and the id it carries must still match the id
#' stored at that row, which rejects a click on a button drawn before another
#' tab added or removed a rule.
#'
#' Pure (no Shiny), so the three handlers share one implementation and it can
#' be tested without a session.
#'
#' @param payload the raw input value from `.rule_delete_js()`.
#' @param ids     stored ids of the current rule set, in row order.
#' @return list(ok = lgl, row = integer or NA, message = chr or NULL). `row` is
#'   usable only when `ok` is TRUE.
.rule_delete_target <- function(payload, ids) {
  bad <- function(msg) list(ok = FALSE, row = NA_integer_, message = msg)
  unknown <- paste0("That Delete button did not name a rule in this session. ",
                    "Nothing was deleted.")
  ids <- as.character(ids)
  n <- length(ids)
  if (n == 0) return(bad(unknown))
  if (is.null(payload) || length(payload) != 1L) return(bad(unknown))
  payload <- as.character(payload)
  if (is.na(payload)) return(bad(unknown))

  parsed <- tryCatch(jsonlite::fromJSON(payload), error = function(e) NULL)
  if (!is.list(parsed)) return(bad(unknown))

  raw_row <- parsed$row
  if (is.null(raw_row) || length(raw_row) != 1L) return(bad(unknown))
  row <- suppressWarnings(as.numeric(raw_row))
  if (is.na(row) || row != floor(row) || row < 1 || row > n) {
    return(bad(unknown))
  }
  row <- as.integer(row)

  # A missing id round-trips as JSON null, which fromJSON() drops entirely.
  want <- parsed$id
  want <- if (is.null(want) || length(want) != 1L) NA_character_
          else as.character(want)
  if (!identical(want, ids[row])) {
    return(bad(paste0(
      "The rule list changed since that button was drawn, so nothing was ",
      "deleted. Check the table and try again.")))
  }
  list(ok = TRUE, row = row, message = NULL)
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
