# Run with: testthat::test_dir("tests/testthat")
#
# Import guard: .check_incoming_recodes() on hand-built frames, so these tests
# do not depend on read_recodes(). An earlier tool wrote the provenance column
# under a different name, which is renamed on import. `norm_spec` is part of
# this repo's schema: kept when present, and its ABSENCE is tolerated because
# files written before it existed lack it. Anything else off-schema is
# refused.

source(file.path("..", "..", "R", "ui_helpers.R"))
source(file.path("..", "..", "R", "string_helpers.R"))
source(file.path("..", "..", "R", "mod_import_recodes.R"))

source(file.path("..", "..", "R", "rule_proxies.R"))

# The alias pair under test, read from the module's own table rather than
# spelled out again here: the literal is the contract with those files and
# lives in one place (R/mod_import_recodes.R).
.ALIAS_OLD <- names(.PROVENANCE_ALIASES)[[1]]
.ALIAS_NEW <- unname(.PROVENANCE_ALIASES)[[1]]

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

test_that("a provenance alias is renamed, with or without norm_spec", {
  inc <- .old_rule()
  names(inc)[names(inc) == .ALIAS_NEW] <- .ALIAS_OLD
  inc <- inc[, rev(names(inc))]  # column order must not matter
  chk <- .check_incoming_recodes(inc)
  expect_true(chk$ok)
  expect_true(chk$renamed)
  expect_named(chk$df, .schema)
  expect_equal(chk$df$source_dataset, "example")
  # The note names both the name read and the name it was read as.
  msg <- .import_adjust_msg(chk)
  expect_match(msg, .ALIAS_OLD, fixed = TRUE)
  expect_match(msg, .ALIAS_NEW, fixed = TRUE)

  inc2 <- .one_rule()
  names(inc2)[names(inc2) == .ALIAS_NEW] <- .ALIAS_OLD
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

test_that("a file carrying both the alias and its target is refused", {
  inc <- .one_rule()
  inc[[.ALIAS_OLD]] <- "other"
  chk <- .check_incoming_recodes(inc)
  expect_false(chk$ok)
  expect_false(chk$renamed)
  expect_equal(chk$extra, .ALIAS_OLD)
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

test_that("an off-schema normalizations frame is refused, an alias renamed", {
  bad <- .one_norm()
  bad$ops <- NULL
  chk <- .check_incoming_normalizations(bad)
  expect_false(chk$ok)
  expect_equal(chk$missing, "ops")
  expect_match(.schema_error_msg(chk, "normalizations"),
               "normalizations schema")

  q <- .one_norm()
  names(q)[names(q) == .ALIAS_NEW] <- .ALIAS_OLD
  chk2 <- .check_incoming_normalizations(q)
  expect_true(chk2$ok)
  expect_true(chk2$renamed)
  expect_equal(chk2$df$source_dataset, q[[.ALIAS_OLD]])
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


# --- "Take newer" compares times, not text (Shiny F6) -------------------------

test_that(".parse_stamp reads the formats a rule file can carry", {
  # The app's own format, and what a hand edit or a spreadsheet produces.
  expect_equal(.parse_stamp("2026-01-02T10:30:00"),
               .parse_stamp("2026-01-02 10:30:00"))
  expect_equal(.parse_stamp("2026-01-02T10:30:00Z"),
               .parse_stamp("2026-01-02 10:30:00"))
  expect_equal(.parse_stamp("  2026-01-02 10:30:00  "),
               .parse_stamp("2026-01-02 10:30:00"))
  expect_false(is.na(.parse_stamp("2026-01-02 10:30")))
  expect_false(is.na(.parse_stamp("2026-01-02")))

  # Times order as times.
  expect_lt(.parse_stamp("2026-01-02T01:00:00"),
            .parse_stamp("2026-01-02 23:00:00"))
  expect_lt(.parse_stamp("2026-01-02 23:00:00"),
            .parse_stamp("2026-01-03T00:00:00"))

  # Anything it cannot read is NA, never an error.
  for (v in c(NA_character_, "", "   ", "not a date", "02/01/2026",
              "2026-13-45T99:99:99", "yesterday")) {
    expect_true(is.na(.parse_stamp(v)))
  }
  expect_length(.parse_stamp(character(0)), 0L)

  # Vectorised, mixing formats in one column.
  got <- .parse_stamp(c("2026-01-02T01:00:00", "2026-01-02 23:00:00", "junk"))
  expect_length(got, 3L)
  expect_lt(got[1], got[2])
  expect_true(is.na(got[3]))
})

test_that("a space-separated stamp is compared as a time, not as text (Shiny F6)", {
  # The defect: "T" (0x54) sorts after " " (0x20), so comparing the raw
  # strings made every space-separated stamp lose to a "T" one on the same
  # date, whatever the clock said.
  expect_true(.stamp_is_newer("2026-01-02 23:00:00", "2026-01-02T01:00:00"))
  expect_false(.stamp_is_newer("2026-01-02 01:00:00", "2026-01-02T23:00:00"))
  # Equal times are not newer, in either spelling.
  expect_false(.stamp_is_newer("2026-01-02 10:00:00", "2026-01-02T10:00:00"))
})

test_that("an unreadable stamp is never newer (Shiny F6)", {
  for (v in c(NA_character_, "", "not a date", "02/01/2026")) {
    expect_false(.stamp_is_newer(v, "2026-01-02T01:00:00"))
  }
  # A readable incoming stamp still beats an unreadable current one: a known
  # time beats no time, which is what "newer" already did for a missing stamp.
  expect_true(.stamp_is_newer("2026-01-02T01:00:00", NA_character_))
  expect_true(.stamp_is_newer("2026-01-02T01:00:00", "not a date"))
  # Neither side readable: nothing to prefer, so keep the session's rule.
  expect_false(.stamp_is_newer("junk", "also junk"))
})

test_that("merge newer takes the later rule across stamp formats (Shiny F6)", {
  mine <- .one_rule()
  mine$updated_at <- "2026-01-02T01:00:00"
  mine$new_value  <- "MINE"
  theirs <- mine
  # Later in the day, written with a space separator (spreadsheet round trip).
  theirs$updated_at <- "2026-01-02 23:00:00"
  theirs$new_value  <- "THEIRS"

  out <- merge_incoming_rules(mine, theirs, "rule_id", "newer",
                              key_fn = recode_key_id)
  expect_equal(nrow(out), 1L)
  expect_identical(out$new_value, "THEIRS")

  # The other direction: an earlier incoming rule does not win.
  theirs$updated_at <- "2026-01-01 23:00:00"
  out <- merge_incoming_rules(mine, theirs, "rule_id", "newer",
                              key_fn = recode_key_id)
  expect_identical(out$new_value, "MINE")

  # An unreadable incoming stamp never wins under "newer".
  theirs$updated_at <- "whenever"
  out <- merge_incoming_rules(mine, theirs, "rule_id", "newer",
                              key_fn = recode_key_id)
  expect_identical(out$new_value, "MINE")
  # ... but "Take incoming file" still loads it.
  out <- merge_incoming_rules(mine, theirs, "rule_id", "theirs",
                              key_fn = recode_key_id)
  expect_identical(out$new_value, "THEIRS")
})

test_that("the import preview reports a stamp it cannot read (Shiny F6)", {
  mine <- .one_rule()
  mine$updated_at <- "2026-01-02T01:00:00"
  theirs <- mine
  theirs$updated_at <- "whenever"

  sm <- import_recodes_summary(mine, theirs)
  expect_equal(sm$n_conflicts, 1L)
  expect_equal(sm$n_unreadable_stamps, 1L)
  h <- paste(as.character(htmltools::renderTags(
    import_recodes_preview_ui(sm))$html), collapse = " ")
  expect_match(h, "cannot read as a time", fixed = TRUE)
  expect_match(h, "Take incoming file", fixed = TRUE)

  # A readable stamp says nothing about stamps.
  theirs$updated_at <- "2026-01-03 09:00:00"
  sm <- import_recodes_summary(mine, theirs)
  expect_equal(sm$n_unreadable_stamps, 0L)
  h <- paste(as.character(htmltools::renderTags(
    import_recodes_preview_ui(sm))$html), collapse = " ")
  expect_false(grepl("cannot read as a time", h, fixed = TRUE))
})
