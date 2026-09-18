# Tests for the rule checks ported from upstream's follow-up review:
#   - regex_compiles() / clean_sibling_pattern(): one engine, blank = none
#   - invalid and blank sibling patterns: reported and skipped, never thrown
#   - recat match types read forgivingly; unrecognised ones match nothing
#   - validate_recat()
#   - validate_recodes(): id_mismatch, spellcheck_lost_words,
#     invalid_sibling_pattern; recode_key_id()
#   - rule warning severity ("warning" / "note") for stamp-only findings
#   - tidyselect matches() uses base grep (TRE), the app's engine

source(file.path("..", "..", "R", "string_helpers.R"))
source(file.path("..", "..", "R", "recat_helpers.R"))
source(file.path("..", "..", "R", "rule_warnings.R"))

rc_rule <- function(variable, match_type, old_value, new_value = "X",
                    siblings = FALSE, pattern = NA_character_,
                    notes = NA_character_, action = "recode",
                    norm_spec = NA_character_) {
  tibble::tibble(
    rule_id = recode_rule_id(variable, match_type, old_value),
    variable = variable, apply_to_siblings = siblings,
    sibling_pattern = pattern, match_type = match_type,
    old_value = old_value, new_value = new_value, action = action,
    notes = notes, author = "t", created_at = "", updated_at = "",
    source_dataset = "d", norm_spec = norm_spec)
}

rc_norm <- function(variable, ops, pattern = NA_character_) {
  tibble::tibble(
    norm_id = normalization_rule_id(variable, pattern), variable = variable,
    apply_to_siblings = !is.na(pattern), sibling_pattern = pattern,
    ops = normalize_ops_join(ops), notes = NA_character_, author = "t",
    created_at = "", updated_at = "", source_dataset = "d")
}

rc_recat <- function(id, include, match_type = "literal", vars = "v",
                     pattern = NA_character_, exclude = NA_character_,
                     out_col = "o", category = "K", priority = 1L) {
  tibble::tibble(
    recat_id = id, out_col = out_col, category = category, vars = vars,
    sibling_pattern = pattern, include_terms = include,
    exclude_terms = exclude, match_type = match_type, priority = priority,
    notes = NA_character_, author = "t", created_at = "",
    source_dataset = "d")
}

# The eval() here runs code this file just generated from its own in-test
# rule tables; that is the thing under test.
eval_script <- function(code, df) {
  env <- new.env(parent = globalenv())
  env$df <- df
  suppressWarnings(suppressMessages(eval(parse(text = code), envir = env)))
  env$df
}


# --- regex_compiles / clean_sibling_pattern -----------------------------------

test_that("regex_compiles judges patterns by the app's engine, never NA", {
  got <- regex_compiles(c("^a", "[a", "\\p{Lu}", "a{,2}", "", NA, " ", "(?i)x"))
  # `\p{Lu}` is ICU-only (TRE rejects it); `a{,2}` is TRE-only.
  expect_identical(got, c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE))
  expect_identical(regex_compiles(character(0)), logical(0))
})

test_that("clean_sibling_pattern turns blanks into NA and keeps the rest", {
  expect_identical(clean_sibling_pattern(c("", "  ", NA, " a", "^d[0-9]$")),
                   c(NA, NA, NA, " a", "^d[0-9]$"))
  expect_identical(clean_sibling_pattern(NULL), character(0))
})

test_that("recat's private copies agree with the string_helpers versions", {
  pats <- c("^a", "[a", "\\p{Lu}", "a{,2}", "", NA, " ", "(")
  expect_identical(.recat_tre_compiles(pats), regex_compiles(pats))
  expect_identical(.recat_clean_pattern(pats), clean_sibling_pattern(pats))
})


# --- Invalid sibling patterns: report and skip, never throw -----------------

test_that("a recode sibling rule with a bad pattern is reported and skipped", {
  df <- tibble::tibble(d1 = c("a", "b"), d2 = c("a", "c"))
  r  <- dplyr::bind_rows(
    rc_rule("d1", "exact", "a", "A", siblings = TRUE, pattern = "d["),
    rc_rule("d2", "exact", "c", "C"))
  expect_no_error(v <- validate_recodes(r, df))
  expect_identical(v$invalid_sibling_pattern$rule_id, r$rule_id[1])
  expect_named(v$invalid_sibling_pattern,
               c("rule_id", "variable", "sibling_pattern"))
  expect_no_error(res <- apply_recodes(df, r))
  expect_identical(res$df$d1, df$d1)
  expect_identical(res$df$d2, c("a", "C"))
  expect_identical(res$summary$cells_matched, c(0L, 1L))
  expect_no_error(code <- generate_recode_R(r, "d"))
  expect_true(grepl("# SKIPPED: 1 rule for the sibling pattern \"d[\"", code,
                    fixed = TRUE))
  expect_identical(eval_script(code, df), res$df)
  # Stamping and warnings go through the same resolution.
  expect_no_error(recode_norm_specs(r, rc_norm("d1", "uppercase", "d["),
                                    names(df)))
  expect_no_error(w <- collect_rule_warnings(df, r, NULL,
                                             rc_norm("d1", "uppercase", "d[")))
  expect_match(w$message[w$kind == "recode_no_match"],
               "sibling pattern `d[` is not a valid", fixed = TRUE)
})

test_that("a normalization with a bad sibling pattern is reported and skipped", {
  df <- tibble::tibble(d1 = c("a", "b"), d2 = c("a", "c"))
  n  <- rc_norm("d1", "uppercase", "d[")
  expect_no_error(v <- validate_normalizations(n, df))
  expect_identical(v$invalid_sibling_pattern$norm_id, n$norm_id)
  expect_match(v$invalid_sibling_pattern$detail, "not a valid regular")
  expect_identical(v$missing_column$norm_id, n$norm_id)
  expect_no_error(res <- apply_normalizations(df, n))
  expect_identical(res$df, df)
  code <- generate_recode_R(empty_recodes_tibble(), "d", normalizations = n)
  expect_true(grepl("# SKIPPED: the normalization", code, fixed = TRUE))
  expect_false(grepl("matches(", code, fixed = TRUE))
  expect_identical(eval_script(code, df), df)
  # Every issue class is present even with no issues.
  expect_true("invalid_sibling_pattern" %in%
                names(validate_normalizations(rc_norm("d1", "trim"))))
})

test_that("a recat rule with a bad sibling pattern is skipped in app and script", {
  df <- tibble::tibble(v = c("latte", "mocha"), v2 = c("mocha", "latte"))
  r  <- dplyr::bind_rows(rc_recat("c1", "latte", pattern = "v["),
                         rc_recat("c2", "mocha"))
  expect_no_error(res <- apply_recat(df, r))
  expect_identical(res$df$o, c(NA, "K"))
  expect_equal(nrow(recat_rule_hits_by_col(df, r[1, ])), 0L)
  expect_no_error(code <- generate_recat_R(r, "d", names(df)))
  expect_true(grepl("# SKIPPED rule c1", code, fixed = TRUE))
  expect_identical(eval_script(code, df)$o, res$df$o)
})


# --- Blank sibling patterns ------------------------------------------------------

test_that("a blank sibling pattern targets the rule's own column only", {
  df <- tibble::tibble(d1 = c("a", "b"), other = c("a", "a"))
  for (p in c("", "   ")) {
    r <- rc_rule("d1", "exact", "a", "A", siblings = TRUE, pattern = p)
    expect_identical(.recode_cols(df, r), "d1")
    res <- apply_recodes(df, r)$df
    expect_identical(res$other, df$other)
    expect_identical(eval_script(generate_recode_R(r, "d"), df), res)
    n <- rc_norm("d1", "uppercase")
    n$apply_to_siblings <- TRUE
    n$sibling_pattern <- p
    nres <- apply_normalizations(df, n)$df
    expect_identical(nres$other, df$other)
    expect_identical(
      eval_script(generate_recode_R(empty_recodes_tibble(), "d",
                                    normalizations = n), df), nres)
  }
})

test_that("rule files carrying a blank sibling pattern read it back as NA", {
  dir <- tempfile("blank_sp_"); dir.create(dir)
  r <- rc_rule("d1", "exact", "a", "A", siblings = TRUE, pattern = "")
  readr::write_csv(r, file.path(dir, "r.csv"), na = "<NA>")
  expect_true(is.na(read_recodes(file.path(dir, "r.csv"))$sibling_pattern))
  expect_true(is.na(.conform_recodes_schema(r)$sibling_pattern))
  n <- rc_norm("d1", "trim"); n$sibling_pattern <- " "
  readr::write_csv(n, file.path(dir, "n.csv"), na = "<NA>")
  expect_true(is.na(read_normalizations(file.path(dir, "n.csv"))$sibling_pattern))
  rc <- rc_recat("c1", "a", pattern = "")
  write_recat(rc, file.path(dir, "c.csv"))
  expect_true(is.na(read_recat(file.path(dir, "c.csv"))$sibling_pattern))
})


# --- Recat match types and validate_recat() -----------------------------------------

test_that(".recat_match_type reads case and spaces forgivingly", {
  mt <- function(x) .recat_match_type(list(match_type = x))
  expect_identical(mt(" Regex "), "regex")
  expect_identical(mt("LITERAL"), "literal")
  expect_identical(mt(NA_character_), "literal")
  expect_identical(mt(""), "literal")
  expect_identical(mt("  "), "literal")
  expect_identical(mt(NULL), "literal")
  expect_identical(mt("fuzzy"), NA_character_)
})

test_that("recat match types agree between app and script, unknown ones match nothing", {
  df <- tibble::tibble(v = c("Latte", "mocha", "flat"))
  cases <- list(
    list(mt = " Regex ", inc = "^lat", want = c("K", NA, NA)),
    list(mt = "LITERAL", inc = "lat",  want = c("K", NA, "K")),
    list(mt = "literal", inc = "^lat", want = rep(NA_character_, 3)),
    list(mt = "bogus",   inc = "lat",  want = rep(NA_character_, 3)))
  for (cs in cases) {
    r <- rc_recat("c1", cs$inc, match_type = cs$mt)
    app <- apply_recat(df, r)$df$o
    expect_identical(app, cs$want, info = cs$mt)
    scr <- eval_script(generate_recat_R(r, "d", names(df)), df)$o
    expect_identical(scr, app, info = cs$mt)
  }
  expect_identical(recat_rule_condition_R(rc_recat("c", "lat", "bogus"), "v"),
                   "FALSE")
})

test_that("a recat regex term that does not compile matches nothing in both", {
  df <- tibble::tibble(v = c("latte", "mocha", "[x"))
  r  <- rc_recat("c1", "[x;moc", match_type = "regex", exclude = "(")
  app <- apply_recat(df, r)$df$o
  expect_identical(app, c(NA, "K", NA))
  code <- generate_recat_R(r, "d", names(df))
  expect_identical(eval_script(code, df)$o, app)
})

test_that("validate_recat reports each issue class", {
  df <- tibble::tibble(v = "a", w = "b")
  rules <- dplyr::bind_rows(
    rc_recat("ok",  "a"),
    rc_recat("mt",  "a", match_type = "fuzzy"),
    rc_recat("rx",  "[a;ok", match_type = " Regex ", exclude = "("),
    rc_recat("inc", NA_character_),
    rc_recat("sib", "a", pattern = "v["),
    rc_recat("dup", "a"),
    rc_recat("dup", "b"),
    rc_recat("mis", "a", vars = "v;nope"),
    rc_recat("none", "a", vars = "gone"))
  v <- validate_recat(rules, df)
  expect_named(v, c("invalid_match_type", "invalid_regex", "no_include_terms",
                    "invalid_sibling_pattern", "duplicate_ids",
                    "missing_columns", "no_columns"))
  for (nm in names(v)) {
    expect_named(v[[nm]], c("recat_id", "out_col", "category", "value",
                            "detail"))
  }
  expect_identical(v$invalid_match_type$recat_id, "mt")
  expect_identical(v$invalid_match_type$value, "fuzzy")
  expect_identical(v$invalid_regex$recat_id, c("rx", "rx"))
  expect_identical(v$invalid_regex$value, c("[a", "("))
  expect_identical(v$no_include_terms$recat_id, "inc")
  expect_identical(v$invalid_sibling_pattern$value, "v[")
  expect_identical(v$duplicate_ids$recat_id, c("dup", "dup"))
  expect_identical(v$missing_columns$value, c("nope", "gone"))
  expect_identical(v$no_columns$recat_id, "none")
  # Without data, the data checks are empty; with no rules, everything is.
  expect_equal(nrow(validate_recat(rules)$missing_columns), 0L)
  expect_true(all(vapply(validate_recat(NULL), nrow, integer(1)) == 0L))
  expect_true(all(vapply(validate_recat(rules[1, ], df), nrow,
                         integer(1)) == 0L))
})


# --- validate_recodes: id_mismatch, spellcheck_lost_words ---------------------------

test_that("id_mismatch lists rules whose id is not their key's, without rewriting", {
  r <- dplyr::bind_rows(rc_rule("x", "exact", "a"), rc_rule("x", "exact", "b"))
  r$rule_id[2] <- "stale0000000"
  v <- validate_recodes(r)
  expect_identical(v$id_mismatch$rule_id, "stale0000000")
  expect_identical(v$id_mismatch$expected_id,
                   recode_rule_id("x", "exact", "b"))
  expect_named(v$id_mismatch, c("rule_id", "expected_id", "variable",
                                "match_type", "old_value"))
  expect_identical(r$rule_id[2], "stale0000000")
  expect_equal(nrow(validate_recodes(r[1, ])$id_mismatch), 0L)
})

test_that("recode_key_id recomputes ids from the key, NA old_value included", {
  r <- dplyr::bind_rows(rc_rule("x", "exact", "a"),
                        rc_rule("y", "trimmed_ci", NA_character_))
  r$rule_id <- c("junk", "junk")
  expect_identical(recode_key_id(r),
                   c(recode_rule_id("x", "exact", "a"),
                     recode_rule_id("y", "trimmed_ci", NA_character_)))
  expect_identical(recode_key_id(NULL), character(0))
  expect_identical(recode_key_id(empty_recodes_tibble()), character(0))
})

test_that("spellcheck_lost_words finds Spellcheck rules that dropped words", {
  r <- dplyr::bind_rows(
    # The old bug: the whole value replaced by the corrected word.
    rc_rule("x", "trimmed_ci", "iced coffe", "coffee",
            notes = "Spellcheck (token 'coffe')"),
    # Current format, words kept.
    rc_rule("x", "trimmed_ci", "hot choclate", "hot chocolate",
            notes = "Spellcheck: token 'choclate' -> 'chocolate'"),
    # Current format, a word lost (e.g. edited by hand).
    rc_rule("x", "trimmed_ci", "oat  milk latte", "latte",
            notes = "Spellcheck: token 'oat' -> 'oat'"),
    # Not a spellcheck rule.
    rc_rule("x", "trimmed_ci", "big mocha", "mocha", notes = "merge"),
    # Spellcheck, but deleting: left to the other checks.
    rc_rule("x", "trimmed_ci", "a b", NA_character_, action = "delete",
            notes = "Spellcheck: x"),
    # "Spellchecked" is not the Spellcheck tab's marker.
    rc_rule("x", "trimmed_ci", "c d", "c", notes = "Spellchecked by hand"))
  v <- validate_recodes(r)$spellcheck_lost_words
  expect_named(v, c("rule_id", "variable", "old_value", "new_value", "notes",
                    "old_words", "new_words"))
  expect_identical(v$old_value, c("iced coffe", "oat  milk latte"))
  expect_identical(v$old_words, c(2L, 3L))
  expect_identical(v$new_words, c(1L, 1L))
})

test_that("validate_recodes always returns every issue class", {
  v <- validate_recodes(rc_rule("x", "exact", "a"))
  expect_true(all(c("duplicate_keys", "invalid_enum", "invalid_regex",
                    "invalid_sibling_pattern", "id_mismatch",
                    "spellcheck_lost_words", "blank_new_value",
                    "rule_chains", "stale") %in% names(v)))
  expect_true(all(vapply(v[c("invalid_sibling_pattern", "id_mismatch",
                             "spellcheck_lost_words")], nrow,
                         integer(1)) == 0L))
})


# --- Severity of stamp-only findings ---------------------------------------------------

# One stamped rule on `city`, stamped under `from`, checked under `to`.
sev_of <- function(match_type, from, to, old_value = "latte") {
  r <- rc_rule("city", match_type, old_value,
               norm_spec = if (identical(from, NA)) NA_character_
                           else normalization_spec_string("city",
                                  if (length(from)) rc_norm("city", from)
                                  else NULL))
  n <- if (length(to)) rc_norm("city", to) else NULL
  w <- .warn_norm_spec_drift(r, n, "city")
  if (nrow(w) == 0) return("none")
  w$severity
}

test_that("stamp drift is a note only when the match type ignores the change", {
  case_ops  <- c("lowercase", "uppercase", "titlecase")
  space_ops <- c("trim", "squish")
  for (op in c(case_ops, space_ops)) {
    # Added since the rule was written.
    expect_identical(sev_of("trimmed_ci", character(0), op), "note", info = op)
    expect_identical(sev_of("exact_ci", character(0), op),
                     if (op %in% case_ops) "note" else "warning", info = op)
    expect_identical(sev_of("exact", character(0), op), "warning", info = op)
    expect_identical(sev_of("regex", character(0), op), "warning", info = op)
    # Removed since.
    expect_identical(sev_of("trimmed_ci", op, character(0)), "note", info = op)
    expect_identical(sev_of("exact_ci", op, character(0)),
                     if (op %in% case_ops) "note" else "warning", info = op)
    expect_identical(sev_of("exact", op, character(0)), "warning", info = op)
    expect_identical(sev_of("regex", op, character(0)), "warning", info = op)
  }
  # Swapping one case op for another under exact_ci is still only case.
  expect_identical(sev_of("exact_ci", c("squish", "lowercase"),
                          c("squish", "titlecase")), "note")
  expect_identical(sev_of("exact_ci", "lowercase", c("squish", "lowercase")),
                   "warning")
  # No change, no finding.
  expect_identical(sev_of("exact", "squish", "squish"), "none")
})

test_that("unstamped rules are notes only when every op is ignored", {
  expect_identical(sev_of("trimmed_ci", NA, c("squish", "uppercase")), "note")
  expect_identical(sev_of("exact_ci", NA, "uppercase"), "warning")
  expect_identical(sev_of("exact", NA, "trim"), "warning")
  expect_identical(sev_of("regex", NA, "trim"), "warning")
})

test_that("a rule for missing cells is never harmed by a normalization change", {
  for (mt in c("exact", "exact_ci", "trimmed_ci", "regex")) {
    expect_identical(sev_of(mt, character(0), "uppercase", NA_character_),
                     "note", info = mt)
    expect_identical(sev_of(mt, NA, "squish", NA_character_), "note",
                     info = mt)
  }
  expect_identical(sev_of("bogus", character(0), "uppercase"), "warning")
})

test_that("note messages say why, and every warning row carries a severity", {
  df <- tibble::tibble(city = c("latte", "Latte "), x = c("a", "b"))
  r <- dplyr::bind_rows(
    rc_rule("city", "trimmed_ci", "latte", norm_spec = ""),
    rc_rule("city", "exact", "latte", norm_spec = ""),
    rc_rule("x", "exact", "zzz", norm_spec = ""))
  w <- collect_rule_warnings(df, r, NULL, rc_norm("city", "titlecase"))
  expect_true(all(w$severity %in% RULE_WARNING_SEVERITIES))
  expect_identical(w$severity[w$kind == "norm_spec_drift"],
                   c("note", "warning"))
  expect_identical(w$severity[w$kind == "recode_no_match"],
                   rep("warning", sum(w$kind == "recode_no_match")))
  expect_match(w$message[w$severity == "note"], "cannot change what the rule",
               fixed = TRUE)
})


# --- tidyselect matches() uses the app's regex engine ------------------------------

test_that("tidyselect matches() resolves a pattern with base grep (TRE)", {
  # `[\d]` is a bracket holding `\` and `d` in TRE, a digit class in ICU.
  df <- tibble::tibble(d = "x", `9` = "y", `\\` = "z")
  pat <- "[\\d]"
  tre <- grep(pat, names(df), value = TRUE)
  icu <- names(df)[stringr::str_detect(names(df), pat)]
  expect_false(identical(tre, icu))           # the engines really differ
  sel <- names(tidyselect::eval_select(
    rlang::expr(tidyselect::matches(!!pat, ignore.case = FALSE)), df))
  expect_identical(sel, tre)
})
