# =============================================================================
# Rule store proxies
# =============================================================================
# The in-session rule sets live in a `reactiveValues` owned by server() in
# app.R. Modules never touch it directly: they get a small proxy,
# list(get = reactive, add = fn, set = fn). The proxies are built here, not
# inline in app.R, so the add() semantics can be tested without launching the
# app.
#
# add() REPLACES A COLLIDING RULE IN PLACE and appends only new ids. Both rule
# sets are first-match-wins by row order (recodes outright; recat after
# priority), so the older drop-and-append behaviour meant that editing a rule,
# even just its notes, could move it behind another rule that matches the same
# cell and silently change the output.
#
# Requires string_helpers.R (merge_recode_rules(), empty_recodes_tibble()) and
# recat_helpers.R (empty_recat_tibble()). Shiny is only touched inside the
# returned functions, so this file sources cleanly without a session.
# =============================================================================

#' Merge recat rules: overlay wins per `recat_id`, in place.
#'
#' The recat counterpart of `merge_recode_rules()`. A rule in `overlay` whose
#' `recat_id` is already in `base` replaces that row where it stands; the rest
#' of `overlay` is appended in its own order.
#'
#' @param base    current recat rule tibble.
#' @param overlay incoming recat rule tibble.
#' @return recat rule tibble.
merge_recat_rules <- function(base, overlay) {
  if (is.null(base)    || !is.data.frame(base))    base    <- empty_recat_tibble()
  if (is.null(overlay) || !is.data.frame(overlay)) overlay <- empty_recat_tibble()
  if (nrow(overlay) == 0) return(base)
  if (nrow(base) == 0)    return(overlay)
  idx <- match(base$recat_id, overlay$recat_id)
  hit <- !is.na(idx)
  out <- base
  if (any(hit)) out[hit, ] <- overlay[idx[hit], names(base), drop = FALSE]
  extra <- overlay[!(overlay$recat_id %in% base$recat_id), , drop = FALSE]
  dplyr::bind_rows(out, extra)
}

#' Proxy over the recode rule store `rv$rules`.
#'
#' @param rv reactiveValues holding `rules`.
#' @return list(get = reactive, add = fn, set = fn).
make_rules_proxy <- function(rv) {
  list(
    get = shiny::reactive({ rv$rules }),
    add = function(new_rules) {
      # Conform the incoming rows to the schema first, so a module that leaves
      # a column out (or passes a typed NA) cannot break the in-place assign.
      new_rules <- dplyr::bind_rows(empty_recodes_tibble(), new_rules)
      rv$rules <- merge_recode_rules(shiny::isolate(rv$rules), new_rules)
    },
    set = function(new_rules) { rv$rules <- new_rules }
  )
}

#' Proxy over the recategorization rule store `rv$recat`.
#'
#' @param rv reactiveValues holding `recat`.
#' @return list(get = reactive, add = fn, set = fn).
make_recat_proxy <- function(rv) {
  list(
    get = shiny::reactive({ rv$recat }),
    add = function(new_rules) {
      new_rules <- dplyr::bind_rows(empty_recat_tibble(), new_rules)
      rv$recat <- merge_recat_rules(shiny::isolate(rv$recat), new_rules)
    },
    set = function(new_rules) { rv$recat <- new_rules }
  )
}
