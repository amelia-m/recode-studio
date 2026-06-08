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
