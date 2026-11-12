# Run with: testthat::test_dir("tests/testthat")
#
# The UI side of the rule-engine follow-ups (core in test-rule_checks.R):
#
#   * validation panels look class names up robustly: a class they do not
#     know renders under a readable title instead of crashing the panel
#     (normalization_issues_ui() threw "subscript out of bounds" on
#     invalid_sibling_pattern);
#   * the Recodes Validate report explains invalid_sibling_pattern,
#     id_mismatch and spellcheck_lost_words;
#   * invalid sibling patterns are refused where they are typed (Recodes
#     cells, the Normalize tab's custom pattern), and a cleared one is NA;
#   * a Spellcheck rule cannot be switched to regex;
#   * Recategorize validates its rules and refuses an Add that would match
#     nothing;
#   * import finds conflicts by key, not by the stored rule_id, and marks
#     rules with an unusable sibling pattern as "will not apply".

source(file.path("..", "..", "R", "ui_helpers.R"))
source(file.path("..", "..", "R", "string_helpers.R"))
source(file.path("..", "..", "R", "recat_helpers.R"))
source(file.path("..", "..", "R", "rule_proxies.R"))
source(file.path("..", "..", "R", "mod_value_table.R"))
source(file.path("..", "..", "R", "mod_normalize.R"))
source(file.path("..", "..", "R", "mod_recode_editor.R"))
source(file.path("..", "..", "R", "mod_recategorize.R"))
source(file.path("..", "..", "R", "mod_import_recodes.R"))

.html <- function(x) {
  h <- if (is.list(x) && !is.null(x$html)) x$html
       else htmltools::renderTags(x)$html
  gsub("\\s+", " ", paste(as.character(h), collapse = " "))
}

.rule <- function(variable, match_type, old_value, new_value,
                  notes = NA_character_, siblings = FALSE,
                  pattern = NA_character_, updated = "2026-01-01T00:00:00") {
  tibble::tibble(
    rule_id           = recode_rule_id(variable, match_type, old_value),
    variable          = variable,
    apply_to_siblings = siblings,
    sibling_pattern   = pattern,
    match_type        = match_type,
    old_value         = old_value,
    new_value         = new_value,
    action            = "recode",
    notes             = notes,
    author            = "test",
    created_at        = "2026-01-01T00:00:00",
    updated_at        = updated,
    source_dataset    = "cafe",
    norm_spec         = ""
  )
}

.cafe <- data.frame(drink1 = c("latte", "mocha"), drink2 = c("Latte", NA),
                    city = c("Uptown", "uptown"), stringsAsFactors = FALSE)

# Capture notifications instead of showing them; `env$msgs` / `env$types`.
.notes_env <- function() {
  e <- new.env()
  e$msgs <- character(0)
  e$types <- character(0)
  e
}
.mock_notes <- function(e, frame = parent.frame()) {
  testthat::local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      e$msgs  <- c(e$msgs, paste(as.character(ui), collapse = ""))
      e$types <- c(e$types, type)
      invisible("id")
    },
    .package = "shiny", .env = frame)
}


# --- issue_class_title() ------------------------------------------------------

test_that("issue_class_title returns the known title or a readable fallback", {
  titles <- c(no_ops = "Nothing selected")
  expect_identical(issue_class_title("no_ops", titles), "Nothing selected")
  expect_identical(issue_class_title("invalid_sibling_pattern", titles),
                   "Invalid sibling pattern")
  expect_identical(issue_class_title("some.new_class"), "Some new class")
  expect_identical(issue_class_title(NA_character_), "Other issue")
  expect_identical(issue_class_title(""), "Other issue")
})


# --- Task 1: Normalize issues panel -------------------------------------------

.norm <- function(variable, ops, siblings = FALSE, pattern = NA_character_) {
  tibble::tibble(
    norm_id = normalization_rule_id(variable, pattern), variable = variable,
    apply_to_siblings = siblings, sibling_pattern = pattern, ops = ops,
    notes = NA_character_, author = "test",
    created_at = "2026-01-01T00:00:00", updated_at = "2026-01-01T00:00:00",
    source_dataset = "cafe")
}

test_that("the normalization panel renders invalid_sibling_pattern", {
  bad <- .norm("drink1", "uppercase", TRUE, "drink[")
  iss <- validate_normalizations(bad, data = .cafe)
  expect_equal(nrow(iss$invalid_sibling_pattern), 1L)
  h <- .html(normalization_issues_ui(iss))
  expect_match(h, "Sibling pattern is not a valid regular expression",
               fixed = TRUE)
  expect_match(h, "<code>drink1</code>", fixed = TRUE)
})

test_that("the normalization panel survives a class it does not know", {
  iss <- validate_normalizations(.norm("drink1", "uppercase"), data = .cafe)
  iss$brand_new_check <- tibble::tibble(norm_id = "x", variable = "<b>v</b>",
                                        detail = "needs a look")
  h <- .html(normalization_issues_ui(iss))
  expect_match(h, "Brand new check", fixed = TRUE)
  expect_match(h, "needs a look", fixed = TRUE)
  expect_match(h, "&lt;b&gt;v&lt;/b&gt;", fixed = TRUE)
  # A class without the usual columns, or a non-frame entry, does not throw.
  iss$odd <- tibble::tibble(something = 1)
  iss$not_a_frame <- "x"
  expect_no_error(.html(normalization_issues_ui(iss)))
})

.norms_proxy <- function(rules = empty_normalizations_tibble()) {
  make_norms_proxy(shiny::reactiveValues(norms = rules))
}

test_that("the Normalize tab renders a stored rule with an invalid pattern", {
  proxy <- .norms_proxy(.norm("drink1", "uppercase", TRUE, "drink["))
  shiny::testServer(mod_normalize_server,
    args = list(shared_state = shiny::reactiveValues(data = .cafe,
                                                     dataset_name = "cafe"),
                selected_var_r = shiny::reactive("city"),
                norms_proxy = proxy), {
      h <- .html(output$validation)
      expect_match(h, "Sibling pattern is not a valid regular expression",
                   fixed = TRUE)
    })
})


# --- Task 4: Normalize refuses an invalid typed pattern -----------------------

test_that("the Normalize tab refuses an invalid typed pattern and adds nothing", {
  proxy <- .norms_proxy()
  e <- .notes_env()
  .mock_notes(e)
  shiny::testServer(mod_normalize_server,
    args = list(shared_state = shiny::reactiveValues(data = .cafe,
                                                     dataset_name = "cafe"),
                selected_var_r = shiny::reactive("drink1"),
                norms_proxy = proxy), {
      session$setInputs(ops = "uppercase", apply_siblings = TRUE,
                        sibling_mode = "custom", sibling_pattern = "drink[")
      expect_true(pattern_invalid_r())
      expect_match(.html(output$preview_note), "not a valid regular expression",
                   fixed = TRUE)
      session$setInputs(add_rule = 1)
      expect_equal(nrow(shiny::isolate(proxy$get())), 0L)
    })
  expect_identical(e$types, "error")
  expect_match(e$msgs, "drink[", fixed = TRUE)
})

test_that("a cleared custom pattern is NA, never an empty pattern", {
  proxy <- .norms_proxy()
  shiny::testServer(mod_normalize_server,
    args = list(shared_state = shiny::reactiveValues(data = .cafe,
                                                     dataset_name = "cafe"),
                selected_var_r = shiny::reactive("drink1"),
                norms_proxy = proxy), {
      session$setInputs(ops = "uppercase", apply_siblings = TRUE,
                        sibling_mode = "custom", sibling_pattern = "   ")
      expect_true(is.na(sibling_pattern_r()))
      expect_false(pattern_invalid_r())
      session$setInputs(add_rule = 1)
      out <- shiny::isolate(proxy$get())
      expect_equal(nrow(out), 1L)
      expect_true(is.na(out$sibling_pattern))
      expect_false(out$apply_to_siblings)
    })
})


# --- Task 2: Recodes Validate report ------------------------------------------

test_that("the report explains the three new classes, with counts", {
  rules <- dplyr::bind_rows(
    .rule("drink1", "trimmed_ci", "latte", "Latte", siblings = TRUE,
          pattern = "drink["),
    .rule("drink1", "exact", "iced coffe", "coffee",
          notes = "Spellcheck (token 'coffe')"),
    .rule("city", "exact", "uptown", "Uptown"))
  rules$rule_id[3] <- "staleid00000"
  iss <- validate_recodes(rules)
  expect_equal(nrow(iss$invalid_sibling_pattern), 1L)
  expect_equal(nrow(iss$id_mismatch), 1L)
  expect_equal(nrow(iss$spellcheck_lost_words), 1L)

  h <- .html(recode_validation_ui(iss))
  expect_match(h, "Invalid sibling pattern (1)", fixed = TRUE)
  expect_match(h, "This sibling rule has a pattern that is not a valid",
               fixed = TRUE)
  expect_match(h, "<code>drink[</code>", fixed = TRUE)
  expect_match(h, "Rule id does not match the rule (1)", fixed = TRUE)
  expect_match(h, "Ids are not rewritten here", fixed = TRUE)
  expect_match(h, "<code>staleid00000</code>", fixed = TRUE)
  expect_match(h, recode_rule_id("city", "exact", "uptown"), fixed = TRUE)
  expect_match(h, "Spellcheck rule may have dropped words (1)", fixed = TRUE)
  expect_match(h, "(2 words to 1 word)", fixed = TRUE)
  expect_false(grepl("All good", h, fixed = TRUE))
})

test_that("the report uses plural wording for several rules", {
  rules <- dplyr::bind_rows(
    .rule("drink1", "exact", "a", "A", siblings = TRUE, pattern = "("),
    .rule("drink2", "exact", "b", "B", siblings = TRUE, pattern = "["))
  h <- .html(recode_validation_ui(validate_recodes(rules)))
  expect_match(h, "Invalid sibling pattern (2)", fixed = TRUE)
  expect_match(h, "These 2 sibling rules have a pattern", fixed = TRUE)
})

test_that("values in the report are escaped", {
  r <- .rule("drink1", "exact", "<img src=x onerror=alert(1)> coffe",
             "<b>x</b>", notes = "Spellcheck: token 'coffe' -> 'x'")
  r$rule_id <- "<script>"
  h <- .html(recode_validation_ui(validate_recodes(r)))
  expect_false(grepl("<img", h, fixed = TRUE))
  expect_false(grepl("<b>x</b>", h, fixed = TRUE))
  expect_false(grepl("<script>", h, fixed = TRUE))
  expect_match(h, "&lt;img", fixed = TRUE)
})

test_that("the report explains a rule whose column is not in the data", {
  # validate_recodes()$missing_column, columns rule_id / variable /
  # sibling_pattern. Built by hand here: the renderer is pure, so this covers
  # the report whatever the validator that produces the class does.
  one <- tibble::tibble(rule_id = "aaaaaaaaaaaa", variable = "town",
                        sibling_pattern = NA_character_)
  h <- .html(recode_validation_ui(list(missing_column = one)))
  expect_match(h, "Column not in the loaded data (1)", fixed = TRUE)
  expect_match(h, "<code>town</code>", fixed = TRUE)
  expect_match(h, "<code>aaaaaaaaaaaa</code>", fixed = TRUE)
  # It says this is normal across datasets, and offers the two ways out,
  # rather than reading as a corrupt rule.
  expect_match(h, "does not clear the rule set", fixed = TRUE)
  expect_match(h, "skipped in the exported script", fixed = TRUE)

  # Plural wording, and a sibling pattern is named when there is one.
  two <- tibble::tibble(rule_id = c("aaaaaaaaaaaa", "bbbbbbbbbbbb"),
                        variable = c("town", "drink1"),
                        sibling_pattern = c(NA_character_, "^drink[0-9]+$"))
  h <- .html(recode_validation_ui(list(missing_column = two)))
  expect_match(h, "Column not in the loaded data (2)", fixed = TRUE)
  expect_match(h, "These 2 rules target a column", fixed = TRUE)
  expect_match(h, "sibling pattern", fixed = TRUE)
  expect_match(h, "<code>^drink[0-9]+$</code>", fixed = TRUE)
})

test_that("a column name from the data cannot inject markup into the report", {
  nasty <- tibble::tibble(rule_id = "<b>id</b>",
                          variable = "<img src=x onerror=alert(1)>",
                          sibling_pattern = "<script>alert(1)</script>")
  h <- .html(recode_validation_ui(list(missing_column = nasty)))
  expect_no_match(h, "<img", fixed = TRUE)
  expect_no_match(h, "<script>", fixed = TRUE)
  expect_match(h, "&lt;img", fixed = TRUE)
})

test_that("no issues gives All good, and an unknown class is still shown", {
  expect_match(.html(recode_validation_ui(validate_recodes(
    .rule("city", "exact", "uptown", "Uptown")))), "All good", fixed = TRUE)
  iss <- validate_recodes(.rule("city", "exact", "uptown", "Uptown"))
  iss$future_check <- tibble::tibble(rule_id = "r1", why = "later")
  h <- .html(recode_validation_ui(iss))
  expect_match(h, "Future check (1)", fixed = TRUE)
  expect_match(h, "later", fixed = TRUE)
})


# --- Tasks 4 and 6: Recodes cell edits ----------------------------------------

test_that("an invalid sibling pattern edit is refused, the rules unchanged", {
  rules <- .rule("drink1", "exact", "latte", "Latte", siblings = TRUE,
                 pattern = "^drink[0-9]+$")
  res <- apply_recode_edit(rules, 1L, "sibling_pattern", "drink[")
  expect_identical(res$status, "refused")
  expect_identical(res$rules, rules)
  expect_match(res$message, "'drink[' is not a valid regular expression",
               fixed = TRUE)
})

test_that("a valid sibling pattern edit is stored as typed", {
  rules <- .rule("drink1", "exact", "latte", "Latte", siblings = TRUE,
                 pattern = "^drink[0-9]+$")
  res <- apply_recode_edit(rules, 1L, "sibling_pattern", "^drink[12]$",
                           now = "2026-02-02T00:00:00")
  expect_identical(res$status, "ok")
  expect_identical(res$rules$sibling_pattern, "^drink[12]$")
  expect_identical(res$rules$updated_at, "2026-02-02T00:00:00")
})

test_that("a cleared sibling pattern is stored as NA, never an empty string", {
  rules <- .rule("drink1", "exact", "latte", "Latte", siblings = TRUE,
                 pattern = "^drink[0-9]+$")
  for (v in c("", "   ", "\t")) {
    res <- apply_recode_edit(rules, 1L, "sibling_pattern", v)
    expect_identical(res$status, "ok")
    expect_true(is.na(res$rules$sibling_pattern))
  }
})

test_that("a Spellcheck rule cannot be switched to regex; others can", {
  sp <- .rule("drink1", "exact", "iced coffe", "iced coffee",
              notes = "Spellcheck: token 'coffe' -> 'coffee'")
  res <- apply_recode_edit(sp, 1L, "match_type", "regex")
  expect_identical(res$status, "refused")
  expect_identical(res$rules, sp)
  expect_match(res$message, "Spellcheck", fixed = TRUE)
  # The older notes format counts too.
  sp$notes <- "Spellcheck (token 'coffe')"
  expect_identical(apply_recode_edit(sp, 1L, "match_type", "regex")$status,
                   "refused")
  # Other match types stay allowed for a Spellcheck rule.
  expect_identical(apply_recode_edit(sp, 1L, "match_type", "exact_ci")$status,
                   "ok")
  # A rule that merely mentions spellcheck later in its notes is not one.
  other <- .rule("drink1", "exact", "latte", "Latte",
                 notes = "not from Spellcheck")
  expect_identical(apply_recode_edit(other, 1L, "match_type", "regex")$status,
                   "ok")
})

test_that("the editor shows refusals as errors and keeps the rules", {
  rules <- dplyr::bind_rows(
    .rule("drink1", "exact", "latte", "Latte", siblings = TRUE,
          pattern = "^drink[0-9]+$"),
    .rule("drink1", "exact", "iced coffe", "iced coffee",
          notes = "Spellcheck: token 'coffe' -> 'coffee'"))
  rv <- shiny::reactiveValues(rules = rules)
  proxy <- make_rules_proxy(rv)
  e <- .notes_env()
  .mock_notes(e)
  col <- function(name) match(name, .RECODE_EDITOR_COLS) - 1L
  shiny::testServer(mod_recode_editor_server,
    args = list(shared_state = shiny::reactiveValues(data = NULL),
                rules_proxy = proxy), {
      session$setInputs(tbl_cell_edit = list(
        row = 1L, col = col("sibling_pattern"), value = "(unclosed"))
      session$setInputs(tbl_cell_edit = list(
        row = 2L, col = col("match_type"), value = "regex"))
      expect_identical(shiny::isolate(rv$rules), rules)
      session$setInputs(tbl_cell_edit = list(
        row = 1L, col = col("sibling_pattern"), value = ""))
      expect_true(is.na(shiny::isolate(rv$rules)$sibling_pattern[1]))
    })
  expect_identical(e$types, c("error", "error"))
  expect_match(e$msgs[1], "(unclosed", fixed = TRUE)
  expect_match(e$msgs[2], "cannot be switched to regex", fixed = TRUE)
})


# --- Task 3: Recategorize validation ------------------------------------------

.recat_rule <- function(include = "espresso", match_type = "literal",
                        pattern = NA_character_) {
  tibble::tibble(
    recat_id = recat_rule_id("drink_family", "Espresso", "drink1", include, "",
                             match_type),
    out_col = "drink_family", category = "Espresso", vars = "drink1",
    sibling_pattern = pattern, include_terms = include, exclude_terms = NA,
    match_type = match_type, priority = 10L, notes = NA_character_,
    author = NA_character_, created_at = "2026-01-01T00:00:00",
    source_dataset = "cafe")
}

test_that("recat_add_refusal names each problem, or returns NULL", {
  expect_null(recat_add_refusal(.recat_rule()))
  expect_match(recat_add_refusal(.recat_rule("caf(", "regex")),
               "not a valid regular expression: 'caf('", fixed = TRUE)
  expect_match(recat_add_refusal(.recat_rule(match_type = "fuzzy")),
               "match type 'fuzzy'", fixed = TRUE)
  expect_match(recat_add_refusal(.recat_rule(NA_character_)),
               "no include terms", fixed = TRUE)
  expect_match(recat_add_refusal(.recat_rule(pattern = "drink[")),
               "sibling pattern 'drink['", fixed = TRUE)
})

test_that("recat_issues_ui lists issues with counts, escaped, and survives new classes", {
  expect_null(recat_issues_ui(NULL))
  ok <- .html(recat_issues_ui(validate_recat(.recat_rule(), data = .cafe)))
  expect_match(ok, "No problems found", fixed = TRUE)

  bad <- dplyr::bind_rows(.recat_rule("<b>(", "regex"),
                          .recat_rule(pattern = "drink["))
  iss <- validate_recat(bad, data = .cafe)
  iss$later_check <- tibble::tibble(recat_id = "x", out_col = "o",
                                    category = "c", value = "v",
                                    detail = "d")
  h <- .html(recat_issues_ui(iss))
  expect_match(h, "Regex term is not a valid regular expression (1)",
               fixed = TRUE)
  expect_match(h, "Sibling pattern is not a valid regular expression (1)",
               fixed = TRUE)
  expect_match(h, "Later check (1)", fixed = TRUE)
  expect_false(grepl("<b>(", h, fixed = TRUE))
  expect_match(h, "&lt;b&gt;(", fixed = TRUE)
})

test_that("Recategorize refuses an Add whose regex term does not compile", {
  store <- shiny::reactiveVal(empty_recat_tibble())
  proxy <- list(get = function() store(),
                add = function(r) store(dplyr::bind_rows(store(), r)),
                set = function(r) store(r))
  meta <- tibble::tibble(column = names(.cafe),
                         is_free_text_candidate = TRUE)
  e <- .notes_env()
  .mock_notes(e)
  shiny::testServer(mod_recategorize_server,
    args = list(shared_state = shiny::reactiveValues(
                  data = .cafe, meta = meta, dataset_name = "cafe.csv"),
                recat_proxy = proxy), {
      session$setInputs(vars = "drink1", out_col = "drink_family",
                        category = "Espresso", match_type = "regex",
                        include_terms = "caf(\nlatte", exclude_terms = "",
                        priority = 10, notes = "")
      session$setInputs(add_rule = 1)
      expect_equal(nrow(store()), 0L)
      expect_null(recat_issues_r())

      # A valid rule is added, and the issues panel then reads all clear.
      session$setInputs(include_terms = "latte")
      session$setInputs(add_rule = 2)
      expect_equal(nrow(store()), 1L)
      expect_match(.html(output$rules_issues), "No problems found",
                   fixed = TRUE)

      # A stored rule with a problem (e.g. from a file) shows in the panel.
      store(dplyr::bind_rows(store(), .recat_rule("caf(", "regex")))
      session$flushReact()
      expect_match(.html(output$rules_issues),
                   "Regex term is not a valid regular expression",
                   fixed = TRUE)
    })
  expect_identical(e$types[1], "error")
  expect_match(e$msgs[1], "'caf('", fixed = TRUE)
})

test_that("the Recategorize UI has the issues panel above the rules table", {
  ui <- as.character(mod_recategorize_ui("recat"))
  expect_lt(regexpr('id="recat-rules_issues"', ui, fixed = TRUE),
            regexpr('id="recat-rules_tbl_ui"', ui, fixed = TRUE))
})


# --- Task 5: import by key, and "will not apply" ------------------------------

test_that("merge by key catches a collision a stale stored id hides", {
  cur <- .rule("city", "exact", "uptown", "Uptown",
               updated = "2026-01-01T00:00:00")
  inc <- .rule("city", "exact", "uptown", "UPTOWN",
               updated = "2026-02-01T00:00:00")
  inc$rule_id <- "staleid00000"
  # By stored id (the old behaviour) both survive: two rules, one key.
  expect_equal(nrow(merge_incoming_rules(cur, inc, "rule_id", "theirs")), 2L)
  # By key, they collide and the strategy decides, in place.
  theirs <- merge_incoming_rules(cur, inc, "rule_id", "theirs",
                                 key_fn = recode_key_id)
  expect_equal(nrow(theirs), 1L)
  expect_identical(theirs$new_value, "UPTOWN")
  # The winner keeps the id it carries (ids are not rewritten).
  expect_identical(theirs$rule_id, "staleid00000")
  mine <- merge_incoming_rules(cur, inc, "rule_id", "mine",
                               key_fn = recode_key_id)
  expect_identical(mine$new_value, "Uptown")
  newer <- merge_incoming_rules(cur, inc, "rule_id", "newer",
                                key_fn = recode_key_id)
  expect_identical(newer$new_value, "UPTOWN")
})

test_that("merge by key keeps positions and appends novel rules", {
  cur <- dplyr::bind_rows(.rule("city", "exact", "a", "A"),
                          .rule("city", "exact", "b", "B"),
                          .rule("city", "exact", "c", "C"))
  cur$rule_id[2] <- "stale-in-cur"
  inc <- dplyr::bind_rows(.rule("city", "exact", "d", "D"),
                          .rule("city", "exact", "b", "BB"))
  out <- merge_incoming_rules(cur, inc, "rule_id", "theirs",
                              key_fn = recode_key_id)
  expect_identical(out$old_value, c("a", "b", "c", "d"))
  expect_identical(out$new_value, c("A", "BB", "C", "D"))
})

test_that("merge by key does not pair two different rules sharing a stale id", {
  cur <- .rule("city", "exact", "a", "A")
  inc <- .rule("city", "exact", "z", "Z")
  inc$rule_id <- cur$rule_id
  out <- merge_incoming_rules(cur, inc, "rule_id", "theirs",
                              key_fn = recode_key_id)
  expect_identical(out$old_value, c("a", "z"))
})

test_that("import_recodes_summary counts by key and lists unusable patterns", {
  cur <- .rule("city", "exact", "uptown", "Uptown")
  inc <- dplyr::bind_rows(
    .rule("city", "exact", "uptown", "UPTOWN"),
    .rule("drink1", "exact", "latte", "Latte", siblings = TRUE,
          pattern = "drink["),
    .rule("drink1", "exact", "mocha", "Mocha", siblings = FALSE,
          pattern = "drink["))
  inc$rule_id[1] <- "staleid00000"
  sm <- import_recodes_summary(cur, inc)
  expect_identical(sm$n_incoming, 3L)
  expect_identical(sm$n_conflicts, 1L)
  expect_identical(sm$n_novel, 2L)
  expect_identical(sm$n_stale_id_conflicts, 1L)
  # Only the rule that is a sibling rule is affected by its bad pattern.
  expect_identical(sm$will_not_apply$old_value, "latte")

  h <- .html(import_recodes_preview_ui(sm))
  expect_match(h, "1 incoming rule will not apply.", fixed = TRUE)
  expect_match(h, "<code>drink[</code>", fixed = TRUE)
  expect_match(h, "1 of these carries a different rule id", fixed = TRUE)
  expect_match(h, "Conflicts (same column, match type and old value): </strong> 1",
               fixed = TRUE)
})

test_that("importing through the module merges by key and accepts bad patterns", {
  path <- tempfile(fileext = ".csv"); on.exit(unlink(path), add = TRUE)
  inc <- dplyr::bind_rows(
    .rule("city", "exact", "uptown", "UPTOWN", updated = "2026-03-01T00:00:00"),
    .rule("drink1", "exact", "latte", "Latte", siblings = TRUE,
          pattern = "drink["))
  inc$rule_id[1] <- "staleid00000"
  write_recodes(inc, path)
  rv <- shiny::reactiveValues(rules = .rule("city", "exact", "uptown", "Uptown"),
                              norms = empty_normalizations_tibble())
  rp <- make_rules_proxy(rv)
  e <- .notes_env()
  .mock_notes(e)
  shiny::testServer(mod_import_recodes_server,
    args = list(shared_state = shiny::reactiveValues(data = NULL),
                rules_proxy = rp), {
      session$setInputs(conflict = "newer",
                        file = list(datapath = path, name = "r.csv"))
      h <- .html(output$preview)
      expect_match(h, "1 incoming rule will not apply.", fixed = TRUE)
      session$setInputs(apply = 1)
      out <- shiny::isolate(rp$get())
      expect_equal(nrow(out), 2L)
      expect_identical(out$new_value, c("UPTOWN", "Latte"))
      expect_identical(out$sibling_pattern[2], "drink[")
    })
  expect_identical(tail(e$types, 1), "warning")
  expect_match(tail(e$msgs, 1), "1 will not apply", fixed = TRUE)
})


# --- Per-row rule delete: payload, escaping, and the three handlers -----------
#
# Two defects meet here (docs/review-2026-10-adversarial.md items 1 and 2):
# Delete matched the stored id, so a duplicated or blank id removed several
# rules and an NA id left an all-NA row behind; and the button pasted that id
# straight into a quoted JavaScript string in the one unescaped DT column.

# A Delete payload exactly as .rule_delete_js() sends it.
.del_payload <- function(row, id) {
  as.character(jsonlite::toJSON(
    list(row = as.integer(row), id = as.character(id)), auto_unbox = TRUE))
}

# The onclick attribute of a one-button cell, as the BROWSER would see it: the
# text between the quotes, with entities decoded. `[^"]*` is deliberate: a raw
# `"` reaching the attribute ends it early, so the match is then truncated and
# the payload assertion below cannot pass.
.onclick_of <- function(html) {
  m <- regmatches(html, regexpr('onclick="[^"]*"', html))
  if (length(m) != 1L) return(NA_character_)
  out <- sub('"$', "", sub('^onclick="', "", m))
  for (p in list(c("&quot;", '"'), c("&#39;", "'"), c("&#x27;", "'"),
                 c("&lt;", "<"), c("&gt;", ">"), c("&amp;", "&"))) {
    out <- gsub(p[1], p[2], out, fixed = TRUE)
  }
  out
}

test_that(".rule_delete_target resolves one row and refuses anything else", {
  ids <- c("aaa", "aaa", NA_character_, "")

  # The clicked row wins even where the id is shared, blank or missing.
  for (i in seq_along(ids)) {
    hit <- .rule_delete_target(.del_payload(i, ids[i]), ids)
    expect_true(hit$ok)
    expect_identical(hit$row, i)
  }

  # Anything else the client could send is ignored, with a message.
  junk <- list(NULL, NA_character_, c("a", "b"), "not json", "", "5",
               "[1,2]", '{"id":"aaa"}', '{"row":"a","id":"aaa"}',
               '{"row":2.5,"id":"aaa"}', .del_payload(0, "aaa"),
               .del_payload(5, "aaa"), .del_payload(-1, "aaa"))
  for (p in junk) {
    hit <- .rule_delete_target(p, ids)
    expect_false(hit$ok)
    expect_true(is.na(hit$row))
    expect_true(nzchar(hit$message))
  }

  # An id that no longer matches the row is a stale click, named as such.
  stale <- .rule_delete_target(.del_payload(1, "bbb"), ids)
  expect_false(stale$ok)
  expect_match(stale$message, "changed since that button was drawn",
               fixed = TRUE)

  # No rules at all: nothing to delete, and no error.
  expect_false(.rule_delete_target(.del_payload(1, "aaa"), character(0))$ok)
})

test_that("a hostile rule id cannot break out of the Delete button (security F8)", {
  # A `"` in the id used to end the onclick attribute, and the `'` to end the
  # quoted JS string, so the rest ran as script. Ids come from imported CSVs.
  hostile <- "x', 'y'), alert(1); //<\"\\end"
  rules <- dplyr::bind_rows(
    .rule("city", "exact", "uptown",   "Uptown"),
    .rule("city", "exact", "downtown", "Downtown"))
  rules$rule_id[2] <- hostile
  rv <- shiny::reactiveValues(rules = rules)
  proxy <- make_rules_proxy(rv)

  shiny::testServer(mod_recode_editor_server,
    args = list(shared_state = shiny::reactiveValues(data = NULL),
                rules_proxy = proxy), {
      # Prime the ignoreInit observer first. shiny:::bindEvent.Observer flips
      # `initialized` on the observer's FIRST evaluation whatever the value,
      # and that gate sits before the ignoreNULL check, so without this the
      # setInputs below would run nothing and the assertions after it would
      # pass while asserting nothing.
      session$flushReact()
      expect_identical(shiny::isolate(rv$rules), rules)

      cell <- shiny::isolate(table_data()$delete[2])

      # Exactly one onclick attribute, and it is COMPLETE: a raw quote from
      # the id would end it early and cut the call short.
      js <- .onclick_of(cell)
      expect_false(is.na(js))
      expect_match(js, "});$")

      # The payload the browser decodes is the JSON the handler expects, so
      # the id travels as data inside a JSON string, not as script.
      want <- .del_payload(2L, hostile)
      expect_match(js, want, fixed = TRUE)
      expect_match(cell, "&quot;", fixed = TRUE)

      # End to end: that payload deletes the clicked rule and only that one.
      session$setInputs(delete_row = want)
      out <- shiny::isolate(rv$rules)
      expect_equal(nrow(out), 1L)
      expect_identical(out$old_value, "uptown")
    })
})

test_that("Recodes Delete removes only the clicked rule with duplicate ids (Shiny F1)", {
  rules <- dplyr::bind_rows(
    .rule("city", "exact", "uptown",   "Uptown"),
    .rule("city", "exact", "downtown", "Downtown"),
    .rule("city", "exact", "midtown",  "Midtown"))
  # An imported rule file legitimately carries ids like these.
  rules$rule_id <- rep("dup000000000", 3L)
  rv <- shiny::reactiveValues(rules = rules)
  proxy <- make_rules_proxy(rv)
  e <- .notes_env()
  .mock_notes(e)
  shiny::testServer(mod_recode_editor_server,
    args = list(shared_state = shiny::reactiveValues(data = NULL),
                rules_proxy = proxy), {
      # The priming flush must not fire the handler: otherwise every
      # assertion below could pass while asserting nothing.
      session$flushReact()
      expect_identical(shiny::isolate(rv$rules), rules)

      session$setInputs(delete_row = .del_payload(2, "dup000000000"))
      r <- shiny::isolate(rv$rules)
      expect_equal(nrow(r), 2L)
      expect_identical(r$old_value, c("uptown", "midtown"))
    })
})

test_that("Recodes Delete does not leave an all-NA row for an NA id (Shiny F1)", {
  rules <- dplyr::bind_rows(
    .rule("city", "exact", "uptown",   "Uptown"),
    .rule("city", "exact", "downtown", "Downtown"),
    .rule("city", "exact", "midtown",  "Midtown"))
  rules$rule_id[2] <- NA_character_
  rv <- shiny::reactiveValues(rules = rules)
  proxy <- make_rules_proxy(rv)
  e <- .notes_env()
  .mock_notes(e)
  shiny::testServer(mod_recode_editor_server,
    args = list(shared_state = shiny::reactiveValues(data = NULL),
                rules_proxy = proxy), {
      session$flushReact()
      expect_identical(shiny::isolate(rv$rules), rules)

      # Deleting the FIRST rule must leave the NA-id rule intact, not blank it.
      session$setInputs(delete_row = .del_payload(1, rules$rule_id[1]))
      r <- shiny::isolate(rv$rules)
      expect_equal(nrow(r), 2L)
      expect_identical(r$old_value, c("downtown", "midtown"))
      expect_identical(r$variable, c("city", "city"))
      expect_false(any(is.na(r$old_value)))

      # The NA-id rule can still be deleted, by its own position.
      session$setInputs(delete_row = .del_payload(1, NA_character_))
      r <- shiny::isolate(rv$rules)
      expect_equal(nrow(r), 1L)
      expect_identical(r$old_value, "midtown")
    })
})

test_that("Recodes Delete ignores a client value it did not send (security F12)", {
  rules <- dplyr::bind_rows(
    .rule("city", "exact", "uptown",   "Uptown"),
    .rule("city", "exact", "downtown", "Downtown"))
  rv <- shiny::reactiveValues(rules = rules)
  proxy <- make_rules_proxy(rv)
  e <- .notes_env()
  .mock_notes(e)
  shiny::testServer(mod_recode_editor_server,
    args = list(shared_state = shiny::reactiveValues(data = NULL),
                rules_proxy = proxy), {
      session$flushReact()
      expect_identical(shiny::isolate(rv$rules), rules)

      for (p in list(NA_character_, c("a", "b"), "not json", "9",
                     .del_payload(99, "x"), .del_payload(1, "wrong-id"))) {
        session$setInputs(delete_row = p)
        expect_identical(shiny::isolate(rv$rules), rules)
      }
    })
  expect_equal(length(e$types), 6L)
  expect_true(all(e$types == "warning"))
})

test_that("Normalize Remove removes only the clicked rule (Shiny F1)", {
  norms <- dplyr::bind_rows(
    .norm("city",   "squish",    FALSE, NA_character_),
    .norm("drink1", "uppercase", FALSE, NA_character_))
  norms$norm_id <- rep("dup000000000", 2L)
  rv <- shiny::reactiveValues(norms = norms)
  proxy <- make_norms_proxy(rv)
  e <- .notes_env()
  .mock_notes(e)
  shiny::testServer(mod_normalize_server,
    args = list(shared_state = shiny::reactiveValues(data = .cafe,
                                                     dataset_name = "cafe"),
                selected_var_r = shiny::reactive("city"),
                norms_proxy = proxy), {
      session$flushReact()
      expect_identical(shiny::isolate(rv$norms), norms)

      session$setInputs(delete_row = .del_payload(2, "dup000000000"))
      out <- shiny::isolate(rv$norms)
      expect_equal(nrow(out), 1L)
      expect_identical(out$variable, "city")

      # A row index past the end of the set deletes nothing.
      session$setInputs(delete_row = .del_payload(7, "dup000000000"))
      expect_equal(nrow(shiny::isolate(rv$norms)), 1L)
    })
})

test_that("Recategorize Delete removes only the clicked rule (Shiny F1)", {
  rules <- dplyr::bind_rows(.recat_rule("espresso"), .recat_rule("latte"))
  rules$recat_id <- rep("dup000000000", 2L)
  store <- shiny::reactiveVal(rules)
  proxy <- list(get = function() store(),
                add = function(r) store(dplyr::bind_rows(store(), r)),
                set = function(r) store(r))
  meta <- tibble::tibble(column = names(.cafe), is_free_text_candidate = TRUE)
  e <- .notes_env()
  .mock_notes(e)
  shiny::testServer(mod_recategorize_server,
    args = list(shared_state = shiny::reactiveValues(
                  data = .cafe, meta = meta, dataset_name = "cafe.csv"),
                recat_proxy = proxy), {
      session$flushReact()
      expect_identical(shiny::isolate(store()), rules)

      session$setInputs(delete_row = .del_payload(1, "dup000000000"))
      out <- shiny::isolate(store())
      expect_equal(nrow(out), 1L)
      expect_identical(out$include_terms, "latte")
    })
})

test_that("Recategorize Delete corrupts no rule on a hostile client value (Shiny F1)", {
  # Asserting on CONTENT, not nrow(): the old handler's `recat_id != <NA>`
  # comparison was all-NA, and row-subsetting a tibble with logical NA keeps
  # the row COUNT while replacing every rule with an all-NA row. A count check
  # would pass while the whole set was destroyed.
  rules <- dplyr::bind_rows(.recat_rule("espresso"), .recat_rule("latte"))
  store <- shiny::reactiveVal(rules)
  proxy <- list(get = function() store(),
                add = function(r) store(dplyr::bind_rows(store(), r)),
                set = function(r) store(r))
  meta <- tibble::tibble(column = names(.cafe), is_free_text_candidate = TRUE)
  e <- .notes_env()
  .mock_notes(e)
  shiny::testServer(mod_recategorize_server,
    args = list(shared_state = shiny::reactiveValues(
                  data = .cafe, meta = meta, dataset_name = "cafe.csv"),
                recat_proxy = proxy), {
      session$flushReact()
      expect_identical(shiny::isolate(store()), rules)

      for (p in list(NA_character_, c("aaa", "bbb"), character(0), "not json",
                     .del_payload(3, "x"))) {
        session$setInputs(delete_row = p)
        expect_identical(shiny::isolate(store()), rules)
      }
    })
})

test_that("a stored NA recat_id does not poison deleting another rule (Shiny F1)", {
  # No hostile client needed: with the old id comparison, one NA id anywhere
  # in the set turned an ordinary click on a DIFFERENT rule into an all-NA row.
  rules <- dplyr::bind_rows(.recat_rule("espresso"), .recat_rule("latte"),
                            .recat_rule("mocha"))
  rules$recat_id[2] <- NA_character_
  store <- shiny::reactiveVal(rules)
  proxy <- list(get = function() store(),
                add = function(r) store(dplyr::bind_rows(store(), r)),
                set = function(r) store(r))
  meta <- tibble::tibble(column = names(.cafe), is_free_text_candidate = TRUE)
  e <- .notes_env()
  .mock_notes(e)
  shiny::testServer(mod_recategorize_server,
    args = list(shared_state = shiny::reactiveValues(
                  data = .cafe, meta = meta, dataset_name = "cafe.csv"),
                recat_proxy = proxy), {
      session$flushReact()
      expect_identical(shiny::isolate(store()), rules)

      session$setInputs(delete_row = .del_payload(3, rules$recat_id[3]))
      out <- shiny::isolate(store())
      expect_equal(nrow(out), 2L)
      expect_identical(out$include_terms, c("espresso", "latte"))
      expect_false(any(is.na(out$include_terms)))
      expect_identical(out$out_col, c("drink_family", "drink_family"))
    })
})
