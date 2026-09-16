# Run with: testthat::test_dir("tests/testthat")
#
# Similarity clusters: what happens when the value the user chose to KEEP is
# also ticked as EXCLUDED.
#
# The two choices contradict each other and the app has no way to know which
# one was meant, so it refuses: no rules for that cluster, an error that names
# the offending value, and the chosen target left exactly as it was set. It
# must never silently retarget the cluster onto a value nobody picked.

source(file.path("..", "..", "R", "ui_helpers.R"))
source(file.path("..", "..", "R", "string_helpers.R"))
source(file.path("..", "..", "R", "data_loader.R"))
source(file.path("..", "..", "R", "mod_cluster_view.R"))


# --- The decision itself (pure) ----------------------------------------------

test_that("a keep target that is not excluded is used as chosen", {
  d <- cluster_target_decision(
    members  = c("latte", "Latte ", "LATTE"),
    radio    = "Latte ",
    custom   = NULL,
    excluded = "LATTE")
  expect_equal(d$status, "ok")
  expect_equal(d$target, "Latte ")
  expect_equal(d$origin, "radio")
})

test_that("excluding the chosen keep target is refused, not silently rerouted", {
  # THE BUG. "Latte " is the user's target and is also ticked as excluded.
  # Retargeting would return "latte" here (most frequent remaining member).
  d <- cluster_target_decision(
    members  = c("latte", "Latte ", "LATTE"),
    radio    = "Latte ",
    custom   = NULL,
    excluded = "Latte ")
  expect_equal(d$status, "conflict")
  expect_true(is.na(d$target))
  # Never resolves to some other member behind the user's back.
  expect_false(identical(d$target, "latte"))
  # The message has to name the value, or "resolve the contradiction" is
  # advice the user cannot act on.
  expect_true(grepl("Latte", d$message, fixed = TRUE))
  # It may SUGGEST the alternative, but suggesting is not choosing.
  expect_equal(d$suggestion, "latte")
  expect_equal(d$chosen, "Latte ")
})

test_that("a typed custom target wins over the radio", {
  d <- cluster_target_decision(
    members  = c("latte", "Latte ", "LATTE"),
    radio    = "latte",
    custom   = "  Caffe Latte  ",
    excluded = character(0))
  expect_equal(d$status, "ok")
  expect_equal(d$target, "Caffe Latte")   # trimmed
  expect_equal(d$origin, "custom")
})

test_that("excluding a typed custom target is refused too", {
  # Same contradiction, reached from the text box rather than the radio.
  d <- cluster_target_decision(
    members  = c("latte", "Latte ", "LATTE"),
    radio    = "latte",
    custom   = "LATTE",
    excluded = "LATTE")
  expect_equal(d$status, "conflict")
  expect_true(is.na(d$target))
  expect_equal(d$origin, "custom")
})

test_that("blank / whitespace-only custom text falls back to the radio", {
  d <- cluster_target_decision(
    members  = c("latte", "Latte "),
    radio    = "latte",
    custom   = "   ",
    excluded = character(0))
  expect_equal(d$status, "ok")
  expect_equal(d$target, "latte")
  expect_equal(d$origin, "radio")
})

test_that("with nothing chosen the most frequent remaining member is used", {
  # No contradiction to resolve: the user stated no target, so falling back
  # is not overriding anyone.
  d <- cluster_target_decision(
    members  = c("latte", "Latte ", "LATTE"),
    radio    = NULL, custom = "",
    excluded = "latte")
  expect_equal(d$status, "ok")
  expect_equal(d$target, "Latte ")
  expect_equal(d$origin, "fallback")
})

test_that("excluding every member with no target chosen is 'empty', not 'conflict'", {
  d <- cluster_target_decision(
    members  = c("latte", "Latte "),
    radio    = NULL, custom = NULL,
    excluded = c("latte", "Latte "))
  expect_equal(d$status, "empty")
  expect_true(is.na(d$target))
  expect_true(nzchar(d$message))
  expect_true(is.na(d$chosen))
  expect_equal(d$origin, "none")
})

test_that("NULL excluded is treated as no exclusions", {
  d <- cluster_target_decision(
    members  = c("latte", "Latte "),
    radio    = "latte",
    custom   = NULL,
    excluded = NULL)
  expect_equal(d$status, "ok")
  expect_equal(d$target, "latte")
})


# --- Pair building (pure) ----------------------------------------------------

test_that("cluster_recode_pairs maps every non-target member onto the target", {
  p <- cluster_recode_pairs(c("latte", "Latte ", "LATTE"), keep = "latte")
  expect_equal(p$old_value, c("Latte ", "LATTE"))
  expect_equal(p$new_value, c("latte", "latte"))
})

test_that("excluded members get no pair", {
  p <- cluster_recode_pairs(c("latte", "Latte ", "LATTE"), keep = "latte",
                            excluded = "LATTE")
  expect_equal(p$old_value, "Latte ")
  expect_equal(p$new_value, "latte")
  expect_false("LATTE" %in% p$old_value)
})

test_that("a keep target absent from the members is allowed", {
  # The correct spelling may simply not occur in the data.
  p <- cluster_recode_pairs(c("latte", "Latte "), keep = "Caffe Latte")
  expect_equal(p$old_value, c("latte", "Latte "))
  expect_true(all(p$new_value == "Caffe Latte"))
})

test_that("an excluded or missing keep target yields zero pairs", {
  expect_equal(nrow(cluster_recode_pairs(c("latte", "Latte "), keep = "latte",
                                         excluded = "latte")), 0)
  expect_equal(nrow(cluster_recode_pairs(c("latte", "Latte "),
                                         keep = NA_character_)), 0)
  expect_equal(nrow(cluster_recode_pairs(c("latte", "Latte "),
                                         keep = character(0))), 0)
})

test_that("excluding everything but the target yields zero pairs", {
  p <- cluster_recode_pairs(c("latte", "Latte ", "LATTE"), keep = "latte",
                            excluded = c("Latte ", "LATTE"))
  expect_equal(nrow(p), 0)
  expect_type(p$old_value, "character")
  expect_type(p$new_value, "character")
})


# --- The Apply handler (reactive wiring) -------------------------------------

# Three spellings of one drink plus an unrelated value. All three normalize to
# "latte" under lower + squish, so they cluster at any threshold and the test
# does not depend on how a distance metric scores a typo.
.uv <- tibble::tibble(
  value = c("latte", "Latte ", "mocha", "LATTE"),
  n     = c(10L, 3L, 2L, 1L))

.state <- function() {
  shiny::reactiveValues(
    dataset_name = "example_messy.csv",
    data = data.frame(
      order_id = c("O1", "O2", "O3", "O4"),
      drink1   = c("latte", "Latte ", "mocha", "LATTE"),
      stringsAsFactors = FALSE),
    meta = NULL)
}

# Minimal stand-in for server()'s rules_proxy.
.rules_proxy <- function() {
  e <- new.env(parent = emptyenv())
  e$rules <- empty_recodes_tibble()
  list(
    get  = function() e$rules,
    add  = function(new_rules) e$rules <- dplyr::bind_rows(e$rules, new_rules),
    set  = function(new_rules) e$rules <- new_rules,
    peek = function() e$rules)
}

.cluster_inputs <- function(session, cid, target = NULL, exclude = NULL,
                            custom = NULL) {
  args <- list()
  if (!is.null(target))  args[[paste0("target_", cid)]]  <- target
  if (!is.null(exclude)) args[[paste0("exclude_", cid)]] <- exclude
  if (!is.null(custom))  args[[paste0("custom_", cid)]]  <- custom
  do.call(session$setInputs, args)
}

.latte_cid <- function(cl) unique(cl$cluster_id[cl$value == "latte"])

test_that("Apply refuses when the keep target is also excluded, and adds no rules", {
  proxy <- .rules_proxy()
  shiny::testServer(
    mod_cluster_view_server,
    args = list(shared_state    = .state(),
                selected_var_r  = shiny::reactiveVal("drink1"),
                unique_values_r = shiny::reactive(.uv),
                rules_proxy     = proxy), {
    session$setInputs(algorithm = "jw", threshold = 0.92, qgram = 2,
                      normalize = c("lower", "squish"),
                      apply_siblings = FALSE)
    cid <- .latte_cid(clusters_r())

    # User deliberately keeps the trailing-space spelling, then ticks that very
    # value as excluded.
    .cluster_inputs(session, cid, target = "Latte ", exclude = "Latte ",
                    custom = "")
    session$setInputs(recode_cluster_clicked = list(cid = cid, nonce = 1))

    # Retargeting would have written two rules recoding onto "latte", a target
    # the user never picked. Refusal writes nothing.
    expect_equal(nrow(proxy$peek()), 0)
  })
})

test_that("the contradiction is surfaced on the page, not only at Apply time", {
  proxy <- .rules_proxy()
  shiny::testServer(
    mod_cluster_view_server,
    args = list(shared_state    = .state(),
                selected_var_r  = shiny::reactiveVal("drink1"),
                unique_values_r = shiny::reactive(.uv),
                rules_proxy     = proxy), {
    session$setInputs(algorithm = "jw", threshold = 0.92, qgram = 2,
                      normalize = c("lower", "squish"),
                      apply_siblings = FALSE)
    cid <- .latte_cid(clusters_r())

    .cluster_inputs(session, cid, target = "Latte ", exclude = character(0),
                    custom = "")
    expect_null(output$conflicts)          # nothing wrong yet

    .cluster_inputs(session, cid, exclude = "Latte ")
    warning_html <- output$conflicts
    expect_false(is.null(warning_html))
    expect_true(grepl("Latte", warning_html$html %||% paste(warning_html,
                                                            collapse = " "),
                      fixed = TRUE))
  })
})

test_that("excluding a member that is not the target still recodes normally", {
  # The feature the exclusion checkbox exists for must keep working: drop the
  # member that does not belong, recode the rest onto the chosen target.
  proxy <- .rules_proxy()
  shiny::testServer(
    mod_cluster_view_server,
    args = list(shared_state    = .state(),
                selected_var_r  = shiny::reactiveVal("drink1"),
                unique_values_r = shiny::reactive(.uv),
                rules_proxy     = proxy), {
    session$setInputs(algorithm = "jw", threshold = 0.92, qgram = 2,
                      normalize = c("lower", "squish"),
                      apply_siblings = FALSE)
    cid <- .latte_cid(clusters_r())

    .cluster_inputs(session, cid, target = "latte", exclude = "LATTE",
                    custom = "")
    session$setInputs(recode_cluster_clicked = list(cid = cid, nonce = 2))

    rules <- proxy$peek()
    expect_equal(nrow(rules), 1)
    expect_equal(rules$old_value, "Latte ")
    expect_equal(rules$new_value, "latte")
    expect_false("LATTE" %in% rules$old_value)   # excluded, so no rule
  })
})

test_that("excluding every member other than the target writes no rules", {
  proxy <- .rules_proxy()
  shiny::testServer(
    mod_cluster_view_server,
    args = list(shared_state    = .state(),
                selected_var_r  = shiny::reactiveVal("drink1"),
                unique_values_r = shiny::reactive(.uv),
                rules_proxy     = proxy), {
    session$setInputs(algorithm = "jw", threshold = 0.92, qgram = 2,
                      normalize = c("lower", "squish"),
                      apply_siblings = FALSE)
    cid <- .latte_cid(clusters_r())

    .cluster_inputs(session, cid, target = "latte",
                    exclude = c("Latte ", "LATTE"), custom = "")
    session$setInputs(recode_cluster_clicked = list(cid = cid, nonce = 3))

    expect_equal(nrow(proxy$peek()), 0)
  })
})


# --- The member table (render) -----------------------------------------------

test_that("each cluster renders as one Keep / n / Exclude / Value table", {
  # Replaces two side-by-side lists (radio + checkbox group) that drifted out
  # of line whenever a value wrapped, so an Exclude tick could sit beside the
  # wrong value. One row per member keeps each tick on its own value.
  #
  # "fingerprint" (key collision) strips punctuation and sorts tokens, so the
  # clusters here do not depend on a similarity score: the three lattes share
  # the key "latte", and "<b>x</b>" / "x b" share "b x".
  uv <- tibble::tibble(
    value = c("latte", "Latte ", "LATTE", "<b>x</b>", "x b"),
    n     = c(10L, 3L, 1L, 2L, 1L))
  proxy <- .rules_proxy()
  shiny::testServer(
    mod_cluster_view_server,
    args = list(shared_state    = .state(),
                selected_var_r  = shiny::reactiveVal("drink1"),
                unique_values_r = shiny::reactive(uv),
                rules_proxy     = proxy), {
    session$setInputs(algorithm = "fingerprint", threshold = 0.92, qgram = 2,
                      normalize = c("lower", "squish"),
                      apply_siblings = FALSE)
    cl  <- clusters_r()
    cid <- .latte_cid(cl)
    tag_cid <- unique(cl$cluster_id[cl$value == "<b>x</b>"])
    expect_equal(sum(cl$cluster_id == cid), 3L)
    expect_equal(sum(cl$cluster_id == tag_cid), 2L)

    html <- as.character(output$clusters_ui$html)
    target_id  <- session$ns(paste0("target_", cid))
    exclude_id <- session$ns(paste0("exclude_", cid))

    # One table per cluster, headers in order: fixed-width controls first,
    # Value last.
    tables <- regmatches(html, gregexpr("<table[^>]*rs-cluster-table", html))[[1]]
    expect_length(tables, 2)
    heads <- regmatches(html, gregexpr("<th[^>]*>[^<]*</th>", html))[[1]]
    expect_equal(gsub("<[^>]+>", "", heads),
                 rep(c("Keep", "n", "Exclude", "Value"), 2))

    # The old two-list inputs are gone.
    expect_false(grepl("Recode the rest to:", html, fixed = TRUE))
    expect_false(grepl("Exclude from cluster:", html, fixed = TRUE))

    # Group containers keep the ids the Apply handler and the conflict check
    # read, and every row input's name equals its container id: that is how
    # Shiny's radio / checkbox-group bindings find them.
    expect_true(grepl(sprintf('id="%s"', target_id), html, fixed = TRUE))
    expect_true(grepl(sprintf('id="%s"', exclude_id), html, fixed = TRUE))
    inputs <- regmatches(html, gregexpr("<input[^>]*>", html))[[1]]
    radios <- inputs[grepl('type="radio"', inputs, fixed = TRUE) &
                     grepl(sprintf('name="%s"', target_id), inputs, fixed = TRUE)]
    checks <- inputs[grepl('type="checkbox"', inputs, fixed = TRUE) &
                     grepl(sprintf('name="%s"', exclude_id), inputs, fixed = TRUE)]
    expect_length(radios, 3)
    expect_length(checks, 3)
    # No id on the row inputs, or the single-checkbox binding claims them.
    row_inputs <- inputs[grepl('type="(radio|checkbox)"', inputs)]
    expect_length(row_inputs, 10)
    expect_false(any(grepl("\\sid=", row_inputs)))
    # The most frequent member is pre-selected, and only that one.
    expect_equal(sum(grepl("checked", radios, fixed = TRUE)), 1L)
    expect_true(grepl('value="latte"', radios[grepl("checked", radios)],
                      fixed = TRUE))

    # Per-row accessible labels.
    expect_true(any(grepl('aria-label="Keep Latte "', radios, fixed = TRUE)))
    expect_true(any(grepl('aria-label="Exclude LATTE"', checks, fixed = TRUE)))

    # Dataset text is escaped everywhere (cell, value, aria-label), never
    # injected as markup.
    expect_false(grepl("<b>x</b>", html, fixed = TRUE))
    expect_true(grepl("<td>&lt;b&gt;x&lt;/b&gt;</td>", html, fixed = TRUE))
    expect_true(grepl('aria-label="Keep &lt;b&gt;x&lt;/b&gt;"', html,
                      fixed = TRUE))

    # shiny.css caps .shiny-input-container at 300px, so width: 100% on the
    # table alone is 100% of 300px and the Value column is crushed. Both
    # wrappers must carry an INLINE width. The custom-target textInput is also
    # a .shiny-input-container and keeps Shiny's default, so match only the
    # group wrappers.
    wrappers <- regmatches(html,
      gregexpr("<div[^>]*shiny-input-(radiogroup|checkboxgroup)[^>]*>",
               html))[[1]]
    expect_length(wrappers, 4)
    bad <- wrappers[!grepl("width:[[:space:]]*100%", wrappers)]
    expect_identical(bad, character(0),
      info = paste("wrapper(s) without an inline width: 100% -",
                   paste(bad, collapse = " | ")))
  })
})

test_that("both cluster sub-tabs say their rules are trimmed_ci", {
  html <- as.character(mod_cluster_view_ui("cv"))
  hits <- gregexpr("case and surrounding spaces are ignored", html,
                   fixed = TRUE)[[1]]
  expect_equal(sum(hits > 0), 2L)
})


# --- Blank values, recompute cap, card cap (pure) ----------------------------

test_that("cluster_drop_blank drops empty, whitespace-only and NA values", {
  uv <- tibble::tibble(value = c("latte", "", "   ", NA, " mocha "),
                       n     = c(5L, 4L, 3L, 2L, 1L))
  out <- cluster_drop_blank(uv)
  expect_equal(out$value, c("latte", " mocha "))
  expect_equal(out$n, c(5L, 1L))
  expect_null(cluster_drop_blank(NULL))
  expect_equal(nrow(cluster_drop_blank(uv[0, ])), 0L)
})

test_that("only the matrix-based algorithms count as pairwise", {
  expect_true(all(vapply(c("jw", "osa", "lv", "lcs", "cosine", "jaccard"),
                         cluster_is_pairwise, logical(1))))
  expect_false(any(vapply(c("fingerprint", "ngram_fingerprint", "soundex",
                            "metaphone"), cluster_is_pairwise, logical(1))))
  # Every pairwise key is an algorithm the UI offers.
  expect_true(all(CLUSTER_PAIRWISE_ALGORITHMS %in% CLUSTER_ALGORITHMS))
  expect_false(cluster_is_pairwise(NULL))
  expect_false(cluster_is_pairwise(NA_character_))
})

test_that("cluster_cap_values keeps the most frequent values for pairwise algorithms only", {
  uv <- tibble::tibble(value = letters[1:6], n = c(1L, 9L, 3L, 9L, 7L, 2L))
  capped <- cluster_cap_values(uv, "jw", cap = 3)
  expect_true(capped$capped)
  expect_equal(capped$n_total, 6L)
  expect_equal(capped$cap, 3L)
  # Highest counts first; the tie at 9 keeps input order.
  expect_equal(capped$uv$value, c("b", "d", "e"))

  linear <- cluster_cap_values(uv, "fingerprint", cap = 3)
  expect_false(linear$capped)
  expect_equal(nrow(linear$uv), 6L)

  at_cap <- cluster_cap_values(uv, "cosine", cap = 6)
  expect_false(at_cap$capped)
  expect_identical(at_cap$uv, uv)

  expect_equal(PAIRWISE_CLUSTER_CAP, 1000L)
})

test_that("cluster_cap_note is silent when nothing was capped and explains the cap otherwise", {
  expect_null(cluster_cap_note(NULL))
  expect_null(cluster_cap_note(list(capped = FALSE, n_total = 5L, cap = 10L)))
  note <- as.character(cluster_cap_note(
    list(capped = TRUE, n_total = 2500L, cap = 1000L)))
  expect_match(note, "rs-help-line", fixed = TRUE)
  expect_match(note, "1,000 most frequent of 2,500 distinct values",
               fixed = TRUE)
  expect_match(note, "Key collision", fixed = TRUE)
})

test_that("cluster_cards_to_show caps the cards, largest clusters first", {
  cl <- tibble::tibble(
    value      = paste0("v", 1:9),
    n          = 1L,
    cluster_id = c(1L, 1L, 2L, 3L, 3L, 3L, 4L, 4L, 5L))
  all <- cluster_cards_to_show(cl, max_cards = 10)
  # Singletons (2 and 5) never get a card.
  expect_equal(all$ids, c(3L, 1L, 4L))
  expect_equal(all$sizes, c(3L, 2L, 2L))
  expect_equal(all$n_total, 3L)

  two <- cluster_cards_to_show(cl, max_cards = 2)
  expect_equal(two$ids, c(3L, 1L))
  expect_equal(two$n_total, 3L)

  expect_equal(cluster_cards_to_show(NULL)$n_total, 0L)
  expect_length(cluster_cards_to_show(cl[0, ])$ids, 0)
})


# --- Debounce, cache, caps and blanks (reactive wiring) ----------------------

# Counts calls to cluster_strings() made by the module, which looks the
# function up in the environment this file sourced it into.
# local_mocked_bindings() only handles package namespaces, so the binding is
# swapped by hand and restored when the calling test ends.
.count_clustering <- function(env = parent.frame()) {
  counter <- new.env(parent = emptyenv())
  counter$n <- 0L
  home <- environment(mod_cluster_view_server)
  real <- get("cluster_strings", envir = home)
  assign("cluster_strings", function(...) {
    counter$n <- counter$n + 1L
    real(...)
  }, envir = home)
  withr::defer(assign("cluster_strings", real, envir = home), envir = env)
  counter
}

.cluster_server_args <- function(uv, ...) {
  c(list(shared_state    = .state(),
         selected_var_r  = shiny::reactiveVal("drink1"),
         unique_values_r = shiny::reactive(uv),
         rules_proxy     = .rules_proxy()),
    list(...))
}

.cluster_defaults <- function(session, algorithm = "jw") {
  session$setInputs(algorithm = algorithm, threshold = 0.92, qgram = 2,
                    normalize = c("lower", "squish"),
                    apply_siblings = FALSE)
}

test_that("a burst of threshold changes clusters once, after the debounce", {
  calls <- .count_clustering()
  shiny::testServer(
    mod_cluster_view_server, args = .cluster_server_args(.uv), {
    .cluster_defaults(session)
    clusters_r()
    expect_equal(calls$n, 1L)

    # A slider drag: several values in quick succession.
    for (t in c(0.91, 0.90, 0.89, 0.88)) session$setInputs(threshold = t)
    clusters_r()
    expect_equal(calls$n, 1L)          # nothing yet: still inside the wait

    session$elapse(600)
    clusters_r()
    expect_equal(calls$n, 2L)          # one run for the whole drag
  })
})

test_that("returning to an earlier setting is served from the cache", {
  calls <- .count_clustering()
  shiny::testServer(
    mod_cluster_view_server, args = .cluster_server_args(.uv), {
    .cluster_defaults(session)
    first <- clusters_r()
    session$setInputs(threshold = 0.80); session$elapse(600)
    clusters_r()
    expect_equal(calls$n, 2L)

    session$setInputs(threshold = 0.92); session$elapse(600)
    expect_identical(clusters_r(), first)
    expect_equal(calls$n, 2L)          # cache hit, no rerun

    # Normalizer tick order does not change the key.
    session$setInputs(normalize = c("squish", "lower")); session$elapse(600)
    clusters_r()
    expect_equal(calls$n, 2L)

    # Under a linear algorithm the threshold is not part of the key.
    session$setInputs(algorithm = "fingerprint"); session$elapse(600)
    clusters_r()
    expect_equal(calls$n, 3L)
    session$setInputs(threshold = 0.75); session$elapse(600)
    clusters_r()
    expect_equal(calls$n, 3L)
  })
})

test_that("a pairwise algorithm over too many values is capped and says so", {
  uv <- tibble::tibble(value = c("latte", "Latte ", "mocha", "LATTE",
                                 "chai", "tea"),
                       n     = c(10L, 3L, 2L, 1L, 1L, 1L))
  shiny::testServer(
    mod_cluster_view_server,
    args = .cluster_server_args(uv, pairwise_cap = 4L), {
    .cluster_defaults(session)
    expect_equal(nrow(clusters_r()), 4L)
    expect_setequal(clusters_r()$value, c("latte", "Latte ", "mocha", "LATTE"))
    note <- as.character(output$cap_note$html)
    expect_match(note, "Clustering the 4 most frequent of 6 distinct values",
                 fixed = TRUE)
    expect_match(note, "rs-help-line", fixed = TRUE)

    # A linear algorithm clusters every value and shows no note.
    session$setInputs(algorithm = "fingerprint"); session$elapse(600)
    expect_equal(nrow(clusters_r()), 6L)
    expect_null(output$cap_note)
  })
})

test_that("the number of cluster cards is capped, largest first", {
  uv <- tibble::tibble(
    value = c("latte", "Latte ", "LATTE", "<b>x</b>", "x b"),
    n     = c(10L, 3L, 1L, 2L, 1L))
  shiny::testServer(
    mod_cluster_view_server,
    args = .cluster_server_args(uv, max_cards = 1L), {
    .cluster_defaults(session, "fingerprint")
    html <- as.character(output$clusters_ui$html)
    tables <- regmatches(html, gregexpr("<table[^>]*rs-cluster-table", html))[[1]]
    expect_length(tables, 1)
    expect_match(html, "Showing 1 of 2 clusters, largest first", fixed = TRUE)
    # The three-member latte cluster is the one drawn.
    cid <- .latte_cid(clusters_r())
    expect_true(grepl(sprintf('id="%s"', session$ns(paste0("target_", cid))),
                      html, fixed = TRUE))
  })
})

test_that("no count line when every cluster fits", {
  uv <- tibble::tibble(
    value = c("latte", "Latte ", "LATTE", "<b>x</b>", "x b"),
    n     = c(10L, 3L, 1L, 2L, 1L))
  shiny::testServer(
    mod_cluster_view_server, args = .cluster_server_args(uv), {
    .cluster_defaults(session, "fingerprint")
    html <- as.character(output$clusters_ui$html)
    expect_false(grepl("Showing", html, fixed = TRUE))
  })
})

test_that("empty and whitespace-only values never form a cluster", {
  # Under lower + squish, "" and "   " are identical, so without the guard
  # they cluster together at any threshold.
  uv <- tibble::tibble(value = c("latte", "", "   ", "mocha", " "),
                       n     = c(5L, 4L, 3L, 2L, 1L))
  shiny::testServer(
    mod_cluster_view_server, args = .cluster_server_args(uv), {
    .cluster_defaults(session)
    for (alg in c("jw", "osa", "cosine", "fingerprint", "soundex")) {
      session$setInputs(algorithm = alg)
      session$elapse(600)
      cl <- clusters_r()
      expect_setequal(cl$value, c("latte", "mocha"))
      expect_false(any(duplicated(cl$cluster_id)), info = alg)
    }
    expect_match(as.character(output$clusters_ui$html),
                 "No multi-member clusters", fixed = TRUE)
  })
})

test_that("an all-blank column gives no clusters and no error", {
  uv <- tibble::tibble(value = c("", "  "), n = c(3L, 1L))
  shiny::testServer(
    mod_cluster_view_server, args = .cluster_server_args(uv), {
    .cluster_defaults(session)
    expect_null(clusters_r())
    expect_null(output$cap_note)
    expect_match(paste(output$clusters_ui$html), "No data.", fixed = TRUE)
  })
})

test_that("the taxonomy matcher ignores empty and whitespace-only values", {
  uv <- tibble::tibble(value = c("texas", "", "   ", "Ohio"),
                       n     = c(4L, 3L, 2L, 1L))
  shiny::testServer(
    mod_cluster_view_server, args = .cluster_server_args(uv), {
    session$setInputs(tax_source_type = "builtin",
                      tax_builtin = "US 50 States & DC",
                      tax_method = "jw", tax_threshold = 0.75)
    m <- taxonomy_matches_r()
    expect_setequal(m$value, c("texas", "Ohio"))

    session$setInputs(tax_source_type = "paste", tax_paste = "Ohio\nTexas")
    m <- taxonomy_matches_r()
    expect_equal(nrow(m), 2L)
    expect_true(all(nzchar(trimws(m$value))))
  })
})

test_that("the taxonomy matcher shows its empty state for an all-blank column", {
  uv <- tibble::tibble(value = c("", "  "), n = c(3L, 1L))
  shiny::testServer(
    mod_cluster_view_server, args = .cluster_server_args(uv), {
    session$setInputs(tax_source_type = "builtin",
                      tax_builtin = "US 50 States & DC",
                      tax_method = "jw", tax_threshold = 0.75)
    expect_null(taxonomy_matches_r())
    expect_match(paste(output$tax_empty$html),
                 "No taxonomy targets or dataset values", fixed = TRUE)
  })
})

test_that("the Similarity Clustering UI has a slot for the cap note", {
  html <- as.character(mod_cluster_view_ui("cv"))
  expect_true(grepl('id="cv-cap_note"', html, fixed = TRUE))
})
