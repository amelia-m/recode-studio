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
# Requires string_helpers.R (merge_recode_rules(), empty_recodes_tibble(),
# empty_normalizations_tibble(), stamp_recode_norm_spec()) and
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
#' add() STAMPS each incoming rule's `norm_spec` with the normalization policy
#' in force for its target columns (stamp_recode_norm_spec()), so a rule
#' records the normalization it was authored under. This is the one place
#' stamping happens: the modules that create rules (Browse values, Clusters,
#' Spellcheck) just call add(). set() never stamps: it is used for edits in
#' the Recodes table and for imports, and both must keep the stamp a rule
#' already carries, or a later normalization change could never be detected.
#'
#' @param rv reactiveValues holding `rules`.
#' @param norms_r optional function (typically a reactive) returning the
#'   current normalization rule set. When NULL, add() does not stamp and an
#'   incoming `norm_spec` is kept as given (NA when absent).
#' @param data_cols_r optional function returning the loaded data's column
#'   names, used to resolve a sibling rule's columns for the stamp. When NULL
#'   or it returns NULL, `recode_norm_specs()` falls back to the rule's own
#'   variable plus the columns the normalizations name.
#' What counts as a collision is `merge_recode_rules()`'s decision, not this
#' proxy's, and it is the rule KEY (variable, match_type, old_value), not the
#' stored `rule_id`. That matters because a stored id can be stale: once a
#' rule with a stale id is in the set, a newly minted rule for the same key
#' would be appended BEHIND it and never run, since recodes are
#' first-match-wins by row order, and no warning would fire because the new
#' rule matched cells but changed none. Import already compared by key
#' (`merge_incoming_rules(key_fn = recode_key_id)`); add() gets the same
#' comparison from the shared merge, so the two paths cannot drift apart.
#'
#' @return list(get = reactive, add = fn, set = fn).
make_rules_proxy <- function(rv, norms_r = NULL, data_cols_r = NULL) {
  current <- function(f) {
    if (is.null(f)) return(NULL)
    shiny::isolate(tryCatch(f(), error = function(e) NULL))
  }
  list(
    get = shiny::reactive({ rv$rules }),
    add = function(new_rules) {
      # Conform the incoming rows to the schema first, so a module that leaves
      # a column out (or passes a typed NA) cannot break the in-place assign.
      new_rules <- dplyr::bind_rows(empty_recodes_tibble(), new_rules)
      if (!is.null(norms_r) && nrow(new_rules) > 0) {
        norms <- current(norms_r)
        if (is.null(norms)) norms <- empty_normalizations_tibble()
        new_rules <- stamp_recode_norm_spec(new_rules, norms,
                                            current(data_cols_r))
      }
      rv$rules <- merge_recode_rules(shiny::isolate(rv$rules), new_rules)
    },
    set = function(new_rules) { rv$rules <- new_rules }
  )
}

#' Upsert normalization rules: one rule per target column.
#'
#' A normalization's id is keyed on its target (variable + sibling pattern), so
#' re-adding a column with different ops already collides on `norm_id`. A rule
#' is ALSO replaced when an existing rule has the same `variable` but a
#' different sibling choice (the user ticked or unticked "apply to sibling
#' columns"): otherwise the old single-column rule and the new family rule
#' would both reach that column. The incoming rule takes the position of the
#' first rule it replaces; other rules it replaces are dropped. New targets are
#' appended.
#'
#' @param base    current normalization tibble.
#' @param overlay incoming normalization tibble.
#' @return normalization tibble.
upsert_normalization_rules <- function(base, overlay) {
  if (is.null(base) || !is.data.frame(base))
    base <- empty_normalizations_tibble()
  if (is.null(overlay) || !is.data.frame(overlay) || nrow(overlay) == 0)
    return(base)
  out <- base
  for (i in seq_len(nrow(overlay))) {
    r   <- overlay[i, names(out), drop = FALSE]
    r_family <- !is.na(r$sibling_pattern) &&
      !is.na(r$apply_to_siblings) && r$apply_to_siblings
    same_family <- r_family &
      !is.na(out$sibling_pattern) & out$sibling_pattern %in% r$sibling_pattern &
      !is.na(out$apply_to_siblings) & out$apply_to_siblings
    hit <- which(out$norm_id == r$norm_id |
                 (!is.na(out$variable) & out$variable == r$variable) |
                 same_family)
    if (length(hit) == 0) {
      out <- dplyr::bind_rows(out, r)
    } else {
      out[hit[1], ] <- r
      if (length(hit) > 1) out <- out[-hit[-1], , drop = FALSE]
    }
  }
  out
}

#' Proxy over the normalization rule store `rv$norms`.
#'
#' add() is an in-place upsert (upsert_normalization_rules()); set() replaces
#' the store.
#'
#' @param rv reactiveValues holding `norms`.
#' @return list(get = reactive, add = fn, set = fn).
make_norms_proxy <- function(rv) {
  list(
    get = shiny::reactive({
      n <- rv$norms
      if (is.null(n)) empty_normalizations_tibble() else n
    }),
    add = function(new_norms) {
      if (is.null(new_norms) || nrow(new_norms) == 0) return(invisible(NULL))
      new_norms <- dplyr::bind_rows(empty_normalizations_tibble(), new_norms)
      rv$norms <- upsert_normalization_rules(shiny::isolate(rv$norms),
                                             new_norms)
    },
    set = function(new_norms) { rv$norms <- new_norms }
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
