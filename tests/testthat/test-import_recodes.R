# Run with: testthat::test_dir("tests/testthat")
#
# Import guard: .check_incoming_recodes() on hand-built frames, so these tests
# do not depend on read_recodes(). Upstream rule files name the provenance
# column `source_quarter`, which is renamed. `norm_spec` is part of this
# repo's schema: kept when present, and its ABSENCE is tolerated because files
# written before it existed lack it. Anything else off-schema is refused.

source(file.path("..", "..", "R", "ui_helpers.R"))
source(file.path("..", "..", "R", "string_helpers.R"))
source(file.path("..", "..", "R", "mod_import_recodes.R"))

source(file.path("..", "..", "R", "rule_proxies.R"))

# A 14-column (current schema) rule.
.one_rule <- function() {
  tibble::tibble(
    rule_id = "abc123def456", variable = "city", apply_to_siblings = FALSE,
    sibling_pattern = NA_character_, match_type = "trimmed_ci",
    old_value = "springfeld", new_value = "Springfield", action = "recode",
    notes = NA_character_, author = "someone",
    created_at = "2026-01-01T00:00:00", updated_at = "2026-01-01T00:00:00",
    source_dataset = "example", norm_spec = "city=titlecase"
  )
}

# A 13-column rule, as written before norm_spec existed.
.old_rule <- function() {
  r <- .one_rule()
  r$norm_spec <- NULL
  r
}
.schema <- names(empty_recodes_tibble())

test_that("a schema-exact frame is accepted unchanged", {
  chk <- .check_incoming_recodes(.one_rule())
  expect_true(chk$ok)
  expect_false(chk$renamed)
  expect_length(chk$dropped, 0)
  expect_named(chk$df, .schema)
  expect_null(.import_adjust_msg(chk))
})

test_that("norm_spec is part of the schema and is kept", {
  chk <- .check_incoming_recodes(.one_rule())
  expect_true(chk$ok)
  expect_length(chk$dropped, 0)
  expect_length(chk$added, 0)
  expect_equal(chk$df$norm_spec, "city=titlecase")
})

test_that("an older file without norm_spec is accepted, unstamped", {
  chk <- .check_incoming_recodes(.old_rule())
  expect_true(chk$ok)
  expect_length(chk$missing, 0)
  expect_equal(chk$added, "norm_spec")
  expect_named(chk$df, .schema)
  expect_true(is.na(chk$df$norm_spec))
  expect_match(.import_adjust_msg(chk), "norm_spec")
})

test_that("source_quarter is renamed, with or without norm_spec", {
  inc <- .old_rule()
  names(inc)[names(inc) == "source_dataset"] <- "source_quarter"
  inc <- inc[, rev(names(inc))]  # column order must not matter
  chk <- .check_incoming_recodes(inc)
  expect_true(chk$ok)
  expect_true(chk$renamed)
  expect_named(chk$df, .schema)
  expect_equal(chk$df$source_dataset, "example")
  msg <- .import_adjust_msg(chk)
  expect_match(msg, "source_quarter")

  inc2 <- .one_rule()
  names(inc2)[names(inc2) == "source_dataset"] <- "source_quarter"
  chk2 <- .check_incoming_recodes(inc2)
  expect_true(chk2$ok)
  expect_equal(chk2$df$norm_spec, "city=titlecase")
  expect_false(grepl("norm_spec", .import_adjust_msg(chk2), fixed = TRUE))
})

test_that("an unknown extra column is refused", {
  inc <- .one_rule()
  inc$reviewer <- "x"
  chk <- .check_incoming_recodes(inc)
  expect_false(chk$ok)
  expect_null(chk$df)
  expect_equal(chk$extra, "reviewer")
  expect_match(.schema_error_msg(chk), "unexpected column\\(s\\): reviewer")
})

test_that("a missing required column is refused", {
  inc <- .one_rule()
  inc$action <- NULL
  chk <- .check_incoming_recodes(inc)
  expect_false(chk$ok)
  expect_equal(chk$missing, "action")
  expect_match(.schema_error_msg(chk), "missing required column\\(s\\): action")
})

test_that("both source_dataset and source_quarter are refused", {
  inc <- .one_rule()
  inc$source_quarter <- "other"
  chk <- .check_incoming_recodes(inc)
  expect_false(chk$ok)
  expect_false(chk$renamed)
  expect_equal(chk$extra, "source_quarter")
})

test_that("a NULL read is refused without error", {
  chk <- .check_incoming_recodes(NULL)
  expect_false(chk$ok)
})

test_that("a 13-column file read from disk round-trips through the guard", {
  path <- tempfile(fileext = ".csv"); on.exit(unlink(path), add = TRUE)
  readr::write_csv(.old_rule(), path, na = "<NA>")
  expect_no_warning(raw <- read_recodes(path, conform = FALSE))
  chk <- .check_incoming_recodes(raw)
  expect_true(chk$ok)
  df <- .conform_recodes_schema(chk$df)
  expect_named(df, .schema)
  out <- tempfile(fileext = ".csv"); on.exit(unlink(out), add = TRUE)
  write_recodes(df, out)
  back <- read_recodes(out)
  expect_named(back, .schema)
  expect_true(is.na(back$norm_spec))
  expect_identical(back$old_value, "springfeld")
})

# --- Normalizations CSV --------------------------------------------------------

.one_norm <- function(variable = "city", ops = "squish;titlecase",
                      updated_at = "2026-01-01T00:00:00") {
  tibble::tibble(
    norm_id = normalization_rule_id(variable, NA_character_),
    variable = variable, apply_to_siblings = FALSE,
    sibling_pattern = NA_character_, ops = ops, notes = NA_character_,
    author = "someone", created_at = "2026-01-01T00:00:00",
    updated_at = updated_at, source_dataset = "example")
}

test_that("a schema-exact normalizations frame is accepted", {
  chk <- .check_incoming_normalizations(.one_norm())
  expect_true(chk$ok)
  expect_named(chk$df, names(empty_normalizations_tibble()))
  expect_null(.import_adjust_msg(chk))
})

test_that("an off-schema normalizations frame is refused, source_quarter renamed", {
  bad <- .one_norm()
  bad$ops <- NULL
  chk <- .check_incoming_normalizations(bad)
  expect_false(chk$ok)
  expect_equal(chk$missing, "ops")
  expect_match(.schema_error_msg(chk, "normalizations"),
               "normalizations schema")

  q <- .one_norm()
  names(q)[names(q) == "source_dataset"] <- "source_quarter"
  chk2 <- .check_incoming_normalizations(q)
  expect_true(chk2$ok)
  expect_true(chk2$renamed)
})

test_that("merge_incoming_rules resolves collisions in place", {
  cur <- dplyr::bind_rows(.one_norm("city", "uppercase", "2026-01-02"),
                          .one_norm("drink1", "lowercase", "2026-01-02"))
  inc <- dplyr::bind_rows(.one_norm("drink1", "titlecase", "2026-01-03"),
                          .one_norm("notes", "squish", "2026-01-01"))
  mine   <- merge_incoming_rules(cur, inc, "norm_id", "mine")
  theirs <- merge_incoming_rules(cur, inc, "norm_id", "theirs")
  newer  <- merge_incoming_rules(cur, inc, "norm_id", "newer")
  expect_equal(mine$variable, c("city", "drink1", "notes"))
  expect_equal(mine$ops, c("uppercase", "lowercase", "squish"))
  expect_equal(theirs$ops, c("uppercase", "titlecase", "squish"))
  expect_equal(newer$ops, c("uppercase", "titlecase", "squish"))

  older <- inc
  older$updated_at[1] <- "2026-01-01"
  expect_equal(merge_incoming_rules(cur, older, "norm_id", "newer")$ops[2],
               "lowercase")
})

test_that("importing normalizations merges into the proxy", {
  path <- tempfile(fileext = ".csv"); on.exit(unlink(path), add = TRUE)
  write_normalizations(.one_norm("drink1", "titlecase", "2026-02-01"), path)
  rv <- shiny::reactiveValues(rules = empty_recodes_tibble(),
                              norms = .one_norm("city", "uppercase"))
  np <- make_norms_proxy(rv)
  rp <- make_rules_proxy(rv)
  st <- shiny::reactiveValues(data = NULL, meta = NULL, dataset_name = NULL)
  shiny::testServer(mod_import_recodes_server,
    args = list(shared_state = st, rules_proxy = rp, norms_proxy = np), {
      session$setInputs(conflict = "newer",
                        norm_file = list(datapath = path, name = "n.csv"))
      expect_true(norm_checked_r()$ok)
      session$setInputs(apply_norms = 1)
      out <- shiny::isolate(np$get())
      expect_equal(out$variable, c("city", "drink1"))
      expect_equal(out$ops, c("uppercase", "titlecase"))
    })
})

test_that("importing recodes keeps their norm_spec stamp", {
  path <- tempfile(fileext = ".csv"); on.exit(unlink(path), add = TRUE)
  write_recodes(.one_rule(), path)
  rv <- shiny::reactiveValues(rules = empty_recodes_tibble(),
                              norms = empty_normalizations_tibble())
  rp <- make_rules_proxy(rv, norms_r = make_norms_proxy(rv)$get)
  st <- shiny::reactiveValues(data = NULL, meta = NULL, dataset_name = NULL)
  shiny::testServer(mod_import_recodes_server,
    args = list(shared_state = st, rules_proxy = rp), {
      session$setInputs(conflict = "theirs",
                        file = list(datapath = path, name = "r.csv"))
      session$setInputs(apply = 1)
      out <- shiny::isolate(rp$get())
      expect_equal(nrow(out), 1L)
      # set(), not add(): the file's stamp survives even though no
      # normalization is in force in this session.
      expect_equal(out$norm_spec, "city=titlecase")
    })
})

test_that("the UI builds", {
  expect_no_error(mod_import_recodes_ui("imp"))
})
