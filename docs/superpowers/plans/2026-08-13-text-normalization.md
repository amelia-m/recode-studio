# Text Normalization & Corpus Analysis Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give Recode Studio the token-level normalization, language detection, and corpus clustering demonstrated in `ext/tokenization_reference.Rmd`.

**Architecture:** Four new pure-R helper files (`lang` folded into `text_helpers.R`, plus `stopword_helpers.R`, `token_helpers.R`, `doc_cluster_helpers.R`), one new `match_type` (`token`) threaded through the existing apply/codegen pair, and new UI sections on the Text analysis tab. Every pure file stays Shiny-free and unit-tested; the Shiny layer only wires reactives.

**Tech Stack:** R 4.5.2, Shiny + bslib + DT, tidyverse (`dplyr`/`stringr`/`tibble`/`tidyr`), `hunspell` (already a dep, used for lemmas), `Matrix` (ships with R), `stats::hclust`. One new dependency: `cld2`.

**Spec:** `docs/superpowers/specs/2026-08-13-text-normalization.md`

## Global Constraints

- R + tidyverse. Native pipe `|>`, **never** `%>%`. 2-space indent.
- `string_helpers.R`, `text_helpers.R`, `recat_helpers.R`, `data_loader.R`, and every new `*_helpers.R` stay **Shiny-free**. Consequence: they cannot use `%||%`, which lives in `ui_helpers.R`.
- Module files define `mod_<name>_ui(id)` + `mod_<name>_server(id, ...)`.
- `case_when`, never `case_match` (deprecated in dplyr 1.2.0).
- `isTRUE()` is not vectorised — use `!is.na(x) & x`.
- Every `bslib::card` sets `fill = FALSE` and its `card_body` sets `fillable = FALSE`.
- **Never `escape = FALSE` on a DT holding dataset content.** Use `escape = -ncol(df)` with the generated-markup column LAST.
- Test runner: `Rscript -e "testthat::test_dir('tests/testthat')"`
- Baseline before this plan: **383 assertions, 0 failures.** Every task must leave the suite green.
- Tests `source()` pure-R files by relative path, e.g. `source(file.path("..", "..", "R", "text_helpers.R"))`. Follow `tests/testthat/test-text_helpers.R`.
- Do not edit the external upstream data pipeline — read-only reference.
- The bundled example `inst/extdata/example_messy.csv` must keep classifying `drink1, drink2, city, notes, review` as free-text candidates and `order_id, size, price, order_date` as non-candidates.

---

## File Structure

| File | Responsibility | Task |
|---|---|---|
| `R/text_helpers.R` (modify) | + `detect_languages()`, `language_summary()`; `top_tokens`/`top_ngrams` take a `stopwords` argument | 1, 5 |
| `R/data_loader.R` (modify) | + `lang_share_en` metadata column | 2 |
| `R/stopword_helpers.R` (create) | Stopword tier files: read, merge, append | 4 |
| `R/token_helpers.R` (create) | Token vocabulary, stem grouping | 6 |
| `R/string_helpers.R` (modify) | `token` match_type: hits, single-pass token application, validation, codegen | 7, 8 |
| `R/doc_cluster_helpers.R` (create) | DTM, TF-IDF, document clustering, per-cluster top terms | 11 |
| `R/mod_text_analysis.R` (modify) | Language card, stopword controls, token-normalize card, document-cluster card | 3, 5, 9, 12 |
| `stopwords/*.txt` (create) | Three committed/gitignored tier files | 4 |
| `DESCRIPTION`, `README.md`, `CLAUDE.md`, `.gitignore` (modify) | Dependency + docs | 1, 4, 13 |

---

## Task 1: Language detection helpers

**Files:**
- Modify: `R/text_helpers.R` (append after `kwic()`)
- Modify: `DESCRIPTION` (add `cld2` to Imports)
- Test: `tests/testthat/test-text_helpers.R`

**Interfaces:**
- Consumes: nothing
- Produces:
  - `detect_languages(values, min_chars = 20) -> character` — ISO-639-1 code or `NA_character_`, one per input element, same length as input.
  - `language_summary(values, min_chars = 20) -> tibble(language chr, n int, share dbl)` — undetected values bucketed as `"(undetected)"`, sorted by `n` descending.

- [ ] **Step 1: Install the new dependency**

```bash
Rscript -e "install.packages('cld2', repos='https://cloud.r-project.org'); renv::snapshot(prompt = FALSE)"
```

Expected: `cld2` installs and appears in `renv.lock`.

- [ ] **Step 2: Write the failing tests**

Append to `tests/testthat/test-text_helpers.R`:

```r
test_that("detect_languages returns one code per value and NA for short strings", {
  x <- c("This is a reasonably long English sentence about coffee.",
         "hi",
         NA_character_)
  out <- detect_languages(x)
  expect_length(out, 3)
  expect_type(out, "character")
  expect_true(is.na(out[2]))  # under min_chars
  expect_true(is.na(out[3]))  # NA input
})

test_that("detect_languages identifies English when cld2 is available", {
  skip_if_not_installed("cld2")
  x <- "This is a reasonably long English sentence about coffee and pastries."
  expect_equal(detect_languages(x), "en")
})

test_that("detect_languages degrades to all-NA without cld2", {
  # Simulate the package being unavailable.
  local_mocked_bindings(
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
  expect_true(all(is.na(detect_languages(
    c("This is a reasonably long English sentence about coffee.", "another one here")
  ))))
})

test_that("language_summary counts and shares sum to 1", {
  skip_if_not_installed("cld2")
  x <- c("This is a reasonably long English sentence about coffee.",
         "This is another reasonably long English sentence about tea.",
         "hi")
  s <- language_summary(x)
  expect_true(all(c("language", "n", "share") %in% names(s)))
  expect_equal(sum(s$n), 3L)
  expect_equal(sum(s$share), 1)
  expect_true("(undetected)" %in% s$language)
})
```

- [ ] **Step 3: Run tests to verify they fail**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-text_helpers.R')"
```

Expected: FAIL — `could not find function "detect_languages"`.

- [ ] **Step 4: Implement**

Append to `R/text_helpers.R`:

```r
#' Detect the language of each value.
#'
#' Wraps cld2::detect_language(). `cld2` is OPTIONAL: when it is not installed
#' this returns all-NA rather than erroring, so the Text analysis tab stays
#' usable on installs that skip the compiled package.
#'
#' Detection is unreliable on very short strings, so values shorter than
#' `min_chars` are reported as NA rather than guessed.
#'
#' @return Character vector of ISO-639-1 codes (or NA), parallel to `values`.
detect_languages <- function(values, min_chars = 20) {
  v   <- as.character(values)
  out <- rep(NA_character_, length(v))
  if (length(v) == 0) return(out)
  if (!requireNamespace("cld2", quietly = TRUE)) return(out)
  ok <- !is.na(v) & nchar(v) >= min_chars
  if (!any(ok)) return(out)
  out[ok] <- as.character(cld2::detect_language(v[ok]))
  out
}

#' Language breakdown for a column.
#'
#' @return tibble(language, n, share); undetected values bucket as
#'   "(undetected)". Sorted by n descending.
language_summary <- function(values, min_chars = 20) {
  langs <- detect_languages(values, min_chars = min_chars)
  if (length(langs) == 0)
    return(tibble::tibble(language = character(0), n = integer(0),
                          share = numeric(0)))
  langs[is.na(langs)] <- "(undetected)"
  tibble::tibble(language = langs) |>
    dplyr::count(language, sort = TRUE, name = "n") |>
    dplyr::mutate(share = n / sum(n))
}
```

Add `cld2` to `DESCRIPTION` Imports, after `htmltools`.

- [ ] **Step 5: Run tests to verify they pass**

```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```

Expected: PASS, 0 failures, total above 383.

- [ ] **Step 6: Commit**

```bash
git add R/text_helpers.R DESCRIPTION renv.lock tests/testthat/test-text_helpers.R
git commit -m "feat(text): per-value language detection via cld2"
```

---

## Task 2: Language share in column metadata

**Files:**
- Modify: `R/data_loader.R` (`build_meta()`)
- Test: `tests/testthat/test-data_loader.R`

**Interfaces:**
- Consumes: `detect_languages()` from Task 1
- Produces: `build_meta(df)` gains a `lang_share_en` numeric column — share of non-missing values detected as `"en"`, or `NA_real_` when nothing was detectable. Computed only for columns where `is_free_text_candidate` is TRUE; `NA_real_` otherwise.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-data_loader.R`:

```r
test_that("build_meta reports lang_share_en for free-text columns only", {
  df <- tibble::tibble(
    note = c(rep("This is a reasonably long English sentence about coffee.", 12),
             "Ceci est une phrase francaise assez longue sur le cafe."),
    code = as.character(1:13)
  )
  m <- build_meta(df)
  expect_true("lang_share_en" %in% names(m))
  expect_true(is.numeric(m$lang_share_en))
  # Non-candidate columns are not scanned.
  expect_true(is.na(m$lang_share_en[m$column == "code"]))
})

test_that("build_meta lang_share_en is a share between 0 and 1 when detected", {
  skip_if_not_installed("cld2")
  df <- tibble::tibble(
    note = c(rep("This is a reasonably long English sentence about coffee.", 12),
             "Ceci est une phrase francaise assez longue sur le cafe et les gateaux.")
  )
  m <- build_meta(df)
  s <- m$lang_share_en[m$column == "note"]
  expect_gte(s, 0)
  expect_lte(s, 1)
  expect_gt(s, 0.5)
})
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-data_loader.R')"
```

Expected: FAIL — `"lang_share_en" %in% names(m)` is FALSE.

- [ ] **Step 3: Implement**

In `R/data_loader.R`, add this helper next to the other internal classifiers:

```r
# Share of a column's non-missing values detected as English. NA when nothing
# was detectable (too short, or cld2 unavailable). Sampled for speed.
.lang_share_en <- function(values, sample_n = 200) {
  v <- values[!is.na(values) & nzchar(values)]
  if (length(v) == 0) return(NA_real_)
  if (length(v) > sample_n) v <- v[seq_len(sample_n)]
  langs <- detect_languages(v)
  if (all(is.na(langs))) return(NA_real_)
  mean(langs[!is.na(langs)] == "en")
}
```

Inside `build_meta()`'s per-column loop, after `is_candidate` is computed, add:

```r
lang_share_en <- if (is_candidate) .lang_share_en(col_values) else NA_real_
```

and add `lang_share_en = lang_share_en` to the tibble row the loop builds.

> Adapt `col_values` to whatever the loop already calls the current column's
> vector — read the surrounding code rather than assuming the name.

- [ ] **Step 4: Run tests to verify they pass**

```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```

Expected: PASS, 0 failures. The example-dataset regression test must still pass unchanged.

- [ ] **Step 5: Commit**

```bash
git add R/data_loader.R tests/testthat/test-data_loader.R
git commit -m "feat(meta): add lang_share_en to column metadata"
```

---

## Task 3: Language section on the Text analysis tab

**Files:**
- Modify: `R/mod_text_analysis.R`

**Interfaces:**
- Consumes: `language_summary()`, `detect_languages()` from Task 1
- Produces: no new exported functions; UI only

- [ ] **Step 1: Add the UI block**

In `mod_text_analysis_ui()`, insert immediately before the `shiny::h6("Keyword in context")` line:

```r
      shiny::h6("Language"),
      shiny::uiOutput(ns("lang_note")),
      shiny::fluidRow(
        shiny::column(5, DT::DTOutput(ns("lang_summary"))),
        shiny::column(7,
          shiny::checkboxInput(ns("only_non_en"),
            "Show only values not detected as English", value = TRUE),
          DT::DTOutput(ns("lang_values")))
      ),
      shiny::hr(),
```

- [ ] **Step 2: Add the server logic**

In `mod_text_analysis_server()`, insert before the `output$kwic` block:

```r
    langs_r <- shiny::reactive({
      v <- col_values(); if (is.null(v)) return(NULL)
      detect_languages(v)
    })

    output$lang_note <- shiny::renderUI({
      if (!requireNamespace("cld2", quietly = TRUE))
        return(shiny::div(class = "alert alert-warning",
          style = "padding:.3em .8em;",
          "Language detection needs the ", shiny::tags$code("cld2"),
          " package. Install it with ",
          shiny::tags$code('install.packages("cld2")'), " and restart R."))
      shiny::tags$small(shiny::tags$em(
        "Values shorter than 20 characters are reported as undetected rather ",
        "than guessed \u2014 short strings are not reliably classifiable."))
    })

    output$lang_summary <- DT::renderDT({
      v <- col_values()
      shiny::validate(shiny::need(!is.null(v), "No values."))
      s <- language_summary(v)
      s$share <- sprintf("%.1f%%", 100 * s$share)
      DT::datatable(s, rownames = FALSE, options = list(pageLength = 8, dom = "tp"))
    })

    output$lang_values <- DT::renderDT({
      v <- col_values(); l <- langs_r()
      shiny::validate(shiny::need(!is.null(v) && !is.null(l), "No values."))
      keep <- if (isTRUE(input$only_non_en)) is.na(l) | l != "en" else rep(TRUE, length(v))
      out <- tibble::tibble(value = v[keep],
                            language = ifelse(is.na(l[keep]), "(undetected)", l[keep]))
      if (nrow(out) == 0)
        return(DT::datatable(tibble::tibble(message = "Every value detected as English."),
                             rownames = FALSE, options = list(dom = "t")))
      DT::datatable(out, rownames = FALSE, options = list(pageLength = 10, dom = "tp"))
    })
```

Note: no `escape` argument, so DT's default `escape = TRUE` protects the raw `value` column.

- [ ] **Step 3: Verify it parses and the app builds**

```bash
Rscript -e "invisible(lapply(list.files('R', full.names=TRUE), parse)); invisible(parse('app.R')); cat('parse OK\n')"
```

Expected: `parse OK`

- [ ] **Step 4: Run the suite**

```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```

Expected: PASS, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add R/mod_text_analysis.R
git commit -m "feat(text-analysis): language breakdown and non-English filter"
```

---

## Task 4: Stopword tier files and helpers

**Files:**
- Create: `R/stopword_helpers.R`
- Create: `stopwords/base_stopwords.txt`, `stopwords/custom_stopwords.txt`, `stopwords/README.md`
- Modify: `.gitignore` (add `stopwords/user_stopwords.txt`)
- Test: `tests/testthat/test-stopword_helpers.R`

**Interfaces:**
- Consumes: `EN_STOPWORDS` from `R/text_helpers.R`
- Produces:
  - `STOPWORD_TIERS -> c(base = "base", project = "project", user = "user")`
  - `.stopword_paths(dir = "stopwords") -> list(base, project, user)` (file paths)
  - `.read_stopword_terms(path) -> character` (lowercased, deduped, `#` comments stripped)
  - `active_stopwords(tiers = c("base","project","user"), dir = "stopwords") -> character`
  - `add_stopword(term, tier = c("project","user"), dir = "stopwords") -> logical` (TRUE when appended, FALSE when already present)

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-stopword_helpers.R`:

```r
source(file.path("..", "..", "R", "text_helpers.R"))
source(file.path("..", "..", "R", "stopword_helpers.R"))

make_tier_dir <- function() {
  d <- withr::local_tempdir()
  writeLines(c("# base", "the", "and"), file.path(d, "base_stopwords.txt"))
  writeLines(c("# project", "beatmaker"), file.path(d, "custom_stopwords.txt"))
  d
}

test_that(".read_stopword_terms strips comments, blanks and case", {
  d <- withr::local_tempdir()
  p <- file.path(d, "x.txt")
  writeLines(c("# a comment", "", "The", "AND", "the"), p)
  expect_equal(.read_stopword_terms(p), c("the", "and"))
})

test_that(".read_stopword_terms returns empty for a missing file", {
  expect_equal(.read_stopword_terms(file.path(tempdir(), "nope.txt")), character(0))
})

test_that("active_stopwords merges the requested tiers", {
  d <- make_tier_dir()
  expect_setequal(active_stopwords(c("base", "project"), dir = d),
                  c("the", "and", "beatmaker"))
  expect_setequal(active_stopwords("base", dir = d), c("the", "and"))
})

test_that("active_stopwords falls back to EN_STOPWORDS when no tier file exists", {
  d <- withr::local_tempdir()
  expect_equal(active_stopwords(dir = d), EN_STOPWORDS)
})

test_that("add_stopword appends once and is idempotent", {
  d <- make_tier_dir()
  expect_true(add_stopword("Linguo", tier = "project", dir = d))
  expect_true("linguo" %in% active_stopwords(dir = d))
  expect_false(add_stopword("linguo", tier = "project", dir = d))
  terms <- .read_stopword_terms(file.path(d, "custom_stopwords.txt"))
  expect_equal(sum(terms == "linguo"), 1L)
})

test_that("add_stopword creates a missing tier file", {
  d <- withr::local_tempdir()
  expect_true(add_stopword("apex", tier = "user", dir = d))
  expect_true(file.exists(file.path(d, "user_stopwords.txt")))
})
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-stopword_helpers.R')"
```

Expected: FAIL — cannot open `R/stopword_helpers.R`.

- [ ] **Step 3: Implement the helper file**

Create `R/stopword_helpers.R`:

```r
# =============================================================================
# Recode Studio — Stopword tiers (pure R, no Shiny)
# =============================================================================
# Mirrors the spellcheck dictionary tier system. A token is a stopword if any
# ACTIVE tier lists it.
#
#   stopwords/base_stopwords.txt    committed, seeded from EN_STOPWORDS
#   stopwords/custom_stopwords.txt  committed, project-shared
#   stopwords/user_stopwords.txt    GITIGNORED, per-user
#
# When the stopwords/ directory is absent entirely (e.g. these helpers sourced
# from a script outside the repo), active_stopwords() falls back to the
# built-in EN_STOPWORDS so callers keep working.
# =============================================================================

STOPWORD_DIR   <- "stopwords"
STOPWORD_TIERS <- c(base = "base", project = "project", user = "user")

.stopword_paths <- function(dir = STOPWORD_DIR) {
  list(
    base    = file.path(dir, "base_stopwords.txt"),
    project = file.path(dir, "custom_stopwords.txt"),
    user    = file.path(dir, "user_stopwords.txt")
  )
}

.read_stopword_terms <- function(path) {
  if (!file.exists(path)) return(character(0))
  x <- readLines(path, warn = FALSE, encoding = "UTF-8")
  x <- x[!startsWith(trimws(x), "#")]
  x <- tolower(trimws(x))
  unique(x[nzchar(x)])
}

#' Merged stopword list across the requested tiers.
#'
#' @return Character vector; EN_STOPWORDS when no tier file exists.
active_stopwords <- function(tiers = c("base", "project", "user"),
                             dir = STOPWORD_DIR) {
  paths <- .stopword_paths(dir)
  tiers <- intersect(tiers, names(paths))
  got <- unique(unlist(lapply(tiers, function(t) .read_stopword_terms(paths[[t]])),
                       use.names = FALSE))
  if (length(got) == 0) EN_STOPWORDS else got
}

#' Append a term to a stopword tier.
#'
#' @return TRUE when appended, FALSE when the tier already had it.
add_stopword <- function(term, tier = c("project", "user"),
                         dir = STOPWORD_DIR) {
  tier <- match.arg(tier)
  term <- tolower(trimws(as.character(term)))
  if (length(term) != 1 || is.na(term) || !nzchar(term)) return(FALSE)
  path <- .stopword_paths(dir)[[tier]]
  if (term %in% .read_stopword_terms(path)) return(FALSE)
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  cat(term, "\n", sep = "", file = path, append = TRUE)
  TRUE
}
```

- [ ] **Step 4: Seed the tier files**

```bash
Rscript -e "source('R/text_helpers.R'); dir.create('stopwords', showWarnings = FALSE); writeLines(c('# Base English stopwords. Edit custom_stopwords.txt instead of this file.', EN_STOPWORDS), 'stopwords/base_stopwords.txt'); writeLines(c('# Project-shared stopwords. Committed \u2014 everyone on the team gets these.'), 'stopwords/custom_stopwords.txt')"
```

Create `stopwords/README.md`:

```markdown
# Stopword tiers

A token is treated as a stopword if **any** active tier lists it. Mirrors the
`dictionary/` tier system used by Spellcheck.

| File | Scope | Git |
|---|---|---|
| `base_stopwords.txt` | Standard English stopwords (seeded from `EN_STOPWORDS`) | committed |
| `custom_stopwords.txt` | Project-shared additions | committed |
| `user_stopwords.txt` | Personal additions | **gitignored** |

One lowercase term per line; `#` starts a comment.

Add a term for the whole team from the Text analysis tab (`+ Stop` → Project),
then `git add` and commit. Personal additions go to the user tier and are never
committed.
```

Append to `.gitignore`:

```
stopwords/user_stopwords.txt
```

- [ ] **Step 5: Run tests to verify they pass**

```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```

Expected: PASS, 0 failures.

- [ ] **Step 6: Commit**

```bash
git add R/stopword_helpers.R stopwords/ .gitignore tests/testthat/test-stopword_helpers.R
git commit -m "feat(stopwords): editable stopword tiers mirroring dictionary tiers"
```

---

## Task 5: Wire stopword tiers into the frequency views

**Files:**
- Modify: `R/text_helpers.R` (`top_tokens`, `top_ngrams`)
- Modify: `R/mod_text_analysis.R`
- Test: `tests/testthat/test-text_helpers.R`

**Interfaces:**
- Consumes: `active_stopwords()`, `add_stopword()` from Task 4
- Produces: `top_tokens(values, n, remove_stopwords, min_chars, stopwords = EN_STOPWORDS)` and `top_ngrams(values, ngram, n, remove_stopwords, min_chars, stopwords = EN_STOPWORDS)` — new trailing argument, defaulting to the current behavior so existing callers and tests are unaffected.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-text_helpers.R`:

```r
test_that("top_tokens honours a custom stopword list", {
  x <- rep("coffee beatmaker coffee", 3)
  default <- top_tokens(x, remove_stopwords = TRUE, min_chars = 1)
  expect_true("beatmaker" %in% default$token)
  custom <- top_tokens(x, remove_stopwords = TRUE, min_chars = 1,
                       stopwords = c("beatmaker"))
  expect_false("beatmaker" %in% custom$token)
  expect_true("coffee" %in% custom$token)
})

test_that("top_ngrams honours a custom stopword list", {
  x <- rep("alpha beatmaker gamma", 3)
  custom <- top_ngrams(x, ngram = 2, remove_stopwords = TRUE, min_chars = 1,
                       stopwords = c("beatmaker"))
  expect_true("alpha gamma" %in% custom$ngram)
})
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-text_helpers.R')"
```

Expected: FAIL — `unused argument (stopwords = ...)`.

- [ ] **Step 3: Implement**

In `R/text_helpers.R`, change the two signatures and the two filter lines:

```r
top_tokens <- function(values, n = 25, remove_stopwords = FALSE, min_chars = 1,
                       stopwords = EN_STOPWORDS) {
  toks <- unlist(.tokens_of(values), use.names = FALSE)
  if (min_chars > 1)    toks <- toks[nchar(toks) >= min_chars]
  if (remove_stopwords) toks <- toks[!toks %in% stopwords]
  ...
```

```r
top_ngrams <- function(values, ngram = 2, n = 25,
                       remove_stopwords = FALSE, min_chars = 1,
                       stopwords = EN_STOPWORDS) {
  per <- .tokens_of(values)
  grams <- unlist(lapply(per, function(t) {
    if (min_chars > 1)    t <- t[nchar(t) >= min_chars]
    if (remove_stopwords) t <- t[!t %in% stopwords]
    ...
```

Leave the rest of both function bodies unchanged.

- [ ] **Step 4: Wire the module**

In `R/mod_text_analysis.R`, add to the controls `fluidRow` (change the four
`shiny::column(3, ...)` widths to `shiny::column(2, ...)` and append):

```r
        shiny::column(4,
          shiny::checkboxGroupInput(ns("sw_tiers"), "Stopword tiers:",
            choices  = c("Base" = "base", "Project" = "project", "User" = "user"),
            selected = c("base", "project", "user"), inline = TRUE))
```

Add a reactive near the top of the server function:

```r
    stopwords_r <- shiny::reactive({
      active_stopwords(tiers = input$sw_tiers %||% c("base", "project", "user"))
    })
```

Pass it through both frequency outputs — in `output$tokens`:

```r
        top_tokens(v, n = input$topn %||% 25,
                   remove_stopwords = isTRUE(input$stop),
                   min_chars = input$minchars %||% 1,
                   stopwords = stopwords_r()),
```

and in `output$ngrams`:

```r
        top_ngrams(v, ngram = input$ngram %||% 2, n = input$topn %||% 25,
                   remove_stopwords = isTRUE(input$stop),
                   min_chars = input$minchars %||% 1,
                   stopwords = stopwords_r()),
```

- [ ] **Step 5: Add the "add to stopwords" control**

Replace the `output$tokens` `DT::datatable(...)` call so the table carries an
action column LAST, then render it with `escape = -ncol(df)`:

```r
    output$tokens <- DT::renderDT({
      v <- col_values()
      shiny::validate(shiny::need(!is.null(v), "No values."))
      df <- top_tokens(v, n = input$topn %||% 25,
                       remove_stopwords = isTRUE(input$stop),
                       min_chars = input$minchars %||% 1,
                       stopwords = stopwords_r())
      if (nrow(df) == 0)
        return(DT::datatable(tibble::tibble(message = "No tokens."),
                             rownames = FALSE, options = list(dom = "t")))
      df$stop <- vapply(df$token, function(tk) {
        sprintf(paste0('<button class="btn btn-xs btn-outline-secondary" ',
                       'onclick="Shiny.setInputValue(\'%s\', \'%s\', ',
                       '{priority: \'event\'})">+ Stop</button>'),
                ns("add_stop"), htmltools::htmlEscape(tk, attribute = TRUE))
      }, character(1))
      # Only the trailing `stop` column is generated markup.
      DT::datatable(df, rownames = FALSE, escape = -ncol(df),
                    options = list(pageLength = 10, dom = "tp"))
    })

    shiny::observeEvent(input$add_stop, ignoreInit = TRUE, {
      tier <- input$sw_target %||% "project"
      ok <- add_stopword(input$add_stop, tier = tier)
      shiny::showNotification(
        if (ok) sprintf("Added '%s' to the %s stopword tier.", input$add_stop, tier)
        else    sprintf("'%s' was already a stopword.", input$add_stop),
        type = if (ok) "message" else "warning", duration = 3)
      # Force the frequency views to recompute against the updated tier.
      shiny::updateCheckboxGroupInput(session, "sw_tiers",
                                      selected = input$sw_tiers)
    })
```

Add the tier target radio to the UI controls row:

```r
        shiny::column(2,
          shiny::radioButtons(ns("sw_target"), "'+ Stop' writes to:",
            choices = c("Project" = "project", "User" = "user"),
            selected = "project", inline = TRUE))
```

> `stopwords_r()` reads `input$sw_tiers`, so re-setting it in the observer is
> what invalidates the frequency tables after a write. Do not replace this with
> a manual `reactiveVal` counter — the tier list is already the dependency.

- [ ] **Step 6: Run the suite and parse check**

```bash
Rscript -e "invisible(lapply(list.files('R', full.names=TRUE), parse)); cat('parse OK\n'); testthat::test_dir('tests/testthat')"
```

Expected: `parse OK`, PASS, 0 failures.

- [ ] **Step 7: Commit**

```bash
git add R/text_helpers.R R/mod_text_analysis.R tests/testthat/test-text_helpers.R
git commit -m "feat(text-analysis): stopword tier selection and + Stop control"
```

---

## Task 6: Token vocabulary and stem grouping

**Files:**
- Create: `R/token_helpers.R`
- Test: `tests/testthat/test-token_helpers.R`

**Interfaces:**
- Consumes: `.tokens_of()`, `EN_STOPWORDS` from `R/text_helpers.R`
- Produces:
  - `token_vocabulary(values, remove_stopwords = TRUE, min_chars = 3, stopwords = EN_STOPWORDS) -> tibble(token chr, n int)` sorted by `n` descending
  - `.suffix_stem(x) -> character` — pure suffix-stripping fallback
  - `stem_groups(tokens, counts = NULL, min_size = 2) -> tibble(stem chr, token chr, n int)` — only groups with `min_size` or more distinct tokens, sorted by group total descending then `n` descending

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-token_helpers.R`:

```r
source(file.path("..", "..", "R", "text_helpers.R"))
source(file.path("..", "..", "R", "token_helpers.R"))

test_that("token_vocabulary counts tokens and honours filters", {
  x <- c("the coffee was cold", "coffee coffee tea")
  v <- token_vocabulary(x, remove_stopwords = TRUE, min_chars = 3)
  expect_true(all(c("token", "n") %in% names(v)))
  expect_equal(v$n[v$token == "coffee"], 3L)
  expect_false("the" %in% v$token)   # stopword
  expect_false("was" %in% v$token)   # stopword
})

test_that("token_vocabulary drops tokens under min_chars", {
  v <- token_vocabulary(c("ab abc abcd"), remove_stopwords = FALSE, min_chars = 3)
  expect_false("ab" %in% v$token)
  expect_true("abc" %in% v$token)
})

test_that(".suffix_stem strips common inflections", {
  expect_equal(.suffix_stem("duties"), "duty")
  expect_equal(.suffix_stem("feels"),  "feel")
  expect_equal(.suffix_stem("talked"), "talk")
  expect_equal(.suffix_stem("making"), "mak")
  expect_equal(.suffix_stem("actively"), "active")
  expect_equal(.suffix_stem("cat"), "cat")   # unchanged
})

test_that("stem_groups groups inflections and drops singletons", {
  toks   <- c("duty", "duties", "coffee")
  counts <- c(5L, 2L, 9L)
  g <- stem_groups(toks, counts)
  expect_true(all(c("stem", "token", "n") %in% names(g)))
  expect_setequal(g$token, c("duty", "duties"))
  expect_equal(length(unique(g$stem)), 1L)
  expect_false("coffee" %in% g$token)  # singleton group dropped
})

test_that("stem_groups orders by group total then count", {
  toks   <- c("talk", "talked", "duty", "duties")
  counts <- c(2L, 1L, 40L, 30L)
  g <- stem_groups(toks, counts)
  expect_equal(g$token[1], "duty")   # biggest group first, biggest member first
})

test_that("stem_groups tolerates NULL counts", {
  g <- stem_groups(c("feel", "feels"))
  expect_equal(nrow(g), 2L)
  expect_true(all(g$n == 1L))
})

test_that("stem_groups returns an empty tibble for no input", {
  g <- stem_groups(character(0))
  expect_equal(nrow(g), 0L)
  expect_true(all(c("stem", "token", "n") %in% names(g)))
})
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-token_helpers.R')"
```

Expected: FAIL — cannot open `R/token_helpers.R`.

- [ ] **Step 3: Implement**

Create `R/token_helpers.R`:

```r
# =============================================================================
# Recode Studio — Token vocabulary + stem grouping (pure R, no Shiny)
# =============================================================================
# Feeds the token-normalization UI: instead of hand-writing
#   str_replace_all(x, "duties", "duty")
# for every inflection, surface the groups and let the user pick a target.
#
# Stems come from hunspell (dictionary-backed lemmas) when the token is known,
# and from a small suffix-stripping fallback otherwise. The stem is only ever a
# GROUPING KEY - it is never written to the data, so an imperfect stem costs a
# slightly odd grouping, not a wrong recode.
# =============================================================================

#' Token frequency table for a column.
#'
#' @return tibble(token, n), sorted by n descending.
token_vocabulary <- function(values, remove_stopwords = TRUE, min_chars = 3,
                             stopwords = EN_STOPWORDS) {
  toks <- unlist(.tokens_of(values), use.names = FALSE)
  if (min_chars > 1)    toks <- toks[nchar(toks) >= min_chars]
  if (remove_stopwords) toks <- toks[!toks %in% stopwords]
  if (length(toks) == 0)
    return(tibble::tibble(token = character(0), n = integer(0)))
  tibble::tibble(token = toks) |>
    dplyr::count(token, sort = TRUE, name = "n")
}

# Suffix-stripping fallback for tokens hunspell does not know. Deliberately
# conservative and order-sensitive: longest/most specific suffix first.
.suffix_stem <- function(x) {
  x <- tolower(as.character(x))
  out <- x
  rules <- list(
    c("ies$",   "y"),
    c("ively$", "ive"),
    c("ely$",   "e"),
    c("ing$",   ""),
    c("edly$",  ""),
    c("ed$",    ""),
    c("ly$",    ""),
    c("es$",    ""),
    c("s$",     "")
  )
  for (i in seq_along(out)) {
    w <- out[i]
    if (is.na(w) || nchar(w) < 4) next
    for (r in rules) {
      if (grepl(r[1], w)) {
        cand <- sub(r[1], r[2], w)
        if (nchar(cand) >= 3) { w <- cand; break }
      }
    }
    out[i] <- w
  }
  out
}

# Dictionary lemma when hunspell knows the token, suffix fallback otherwise.
.lemma_of <- function(tokens) {
  fallback <- .suffix_stem(tokens)
  if (!requireNamespace("hunspell", quietly = TRUE)) return(fallback)
  stems <- tryCatch(hunspell::hunspell_stem(tokens),
                    error = function(e) vector("list", length(tokens)))
  out <- vapply(seq_along(tokens), function(i) {
    s <- stems[[i]]
    if (length(s) == 0 || all(is.na(s))) fallback[i] else tolower(s[[1]])
  }, character(1))
  out
}

#' Group tokens that share a stem.
#'
#' @param tokens   Character vector of distinct tokens.
#' @param counts   Optional integer counts parallel to `tokens`.
#' @param min_size Minimum distinct tokens for a group to be reported.
#' @return tibble(stem, token, n), groups ordered by total count descending,
#'   members by count descending. Empty tibble when nothing groups.
stem_groups <- function(tokens, counts = NULL, min_size = 2) {
  empty <- tibble::tibble(stem = character(0), token = character(0),
                          n = integer(0))
  tokens <- as.character(tokens)
  if (length(tokens) == 0) return(empty)
  if (is.null(counts)) counts <- rep(1L, length(tokens))
  counts <- as.integer(counts)

  out <- tibble::tibble(token = tokens, n = counts,
                        stem = .lemma_of(tokens)) |>
    dplyr::group_by(stem) |>
    dplyr::mutate(.members = dplyr::n(), .total = sum(n)) |>
    dplyr::ungroup() |>
    dplyr::filter(.members >= min_size) |>
    dplyr::arrange(dplyr::desc(.total), stem, dplyr::desc(n), token) |>
    dplyr::select(stem, token, n)

  if (nrow(out) == 0) return(empty)
  out
}
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```

Expected: PASS, 0 failures.

> If `.suffix_stem("making")` does not return `"mak"`, check rule order — `ing$`
> must be tried before `s$`. Adjust the test only if the spec's expected value
> is genuinely wrong, and say so.

- [ ] **Step 5: Commit**

```bash
git add R/token_helpers.R tests/testthat/test-token_helpers.R
git commit -m "feat(tokens): token vocabulary and stem grouping helpers"
```

---

## Task 7: The `token` match_type — matching, application, validation

**Files:**
- Modify: `R/string_helpers.R`
- Test: `tests/testthat/test-string_helpers.R`

**Interfaces:**
- Consumes: `.rule_hits()`, `apply_recodes()`, `validate_recodes()`, `.recode_cols()`
- Produces:
  - `RECODE_MATCH_TYPES` gains `"token"` (now 5 values)
  - `.token_pattern(tokens) -> character` — a single `\\b(a|b|c)\\b` alternation with each token regex-escaped
  - `.apply_token_map(values, map) -> character` — one-pass token rewrite
  - `validate_recodes()$invalid_token` — new issue tibble
  - `apply_recodes()` applies token rules after whole-cell rules, per column

**Design — read before writing code.** A `token` rule rewrites a word *inside*
a cell rather than replacing the cell. Unlike whole-cell rules, token rules must
**compose**: twenty stem rules all land on the same comment. The resolution is
one pass over an alternation of every token, with a function replacement, so
each source token is examined once and mapped once. A rule can never rewrite
another rule's output, and the single-pass invariant holds.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-string_helpers.R`:

```r
test_that("token is an allowed match_type", {
  expect_true("token" %in% RECODE_MATCH_TYPES)
})

test_that(".token_pattern escapes and anchors on word boundaries", {
  p <- .token_pattern(c("duties", "c++"))
  expect_true(grepl("^\\\\b\\(", p))
  expect_true(grepl("\\\\b$", p))
  expect_true(grepl("c\\+\\+", p, fixed = TRUE))
})

test_that(".apply_token_map rewrites whole words only, preserving the rest", {
  x <- c("the duties were made clear", "homemade duties", NA)
  out <- .apply_token_map(x, c(duties = "duty", made = "make"))
  expect_equal(out[1], "the duty were make clear")
  expect_equal(out[2], "homemade duty")   # 'homemade' untouched
  expect_true(is.na(out[3]))
})

test_that(".apply_token_map is case-insensitive on match and single-pass", {
  # 'use' -> 'utilize' must NOT re-fire on the output of 'used' -> 'use'
  out <- .apply_token_map("I used it", c(used = "use", use = "utilize"))
  expect_equal(out, "I use it")
  expect_equal(.apply_token_map("Duties here", c(duties = "duty")), "duty here")
})

test_that("apply_recodes applies token rules within cells", {
  df <- tibble::tibble(review = c("the duties were made clear",
                                  "nothing to change here"))
  rules <- empty_recodes_tibble() |>
    tibble::add_row(rule_id = "t1", variable = "review",
                    apply_to_siblings = FALSE, sibling_pattern = NA_character_,
                    match_type = "token", old_value = "duties",
                    new_value = "duty", action = "recode") |>
    tibble::add_row(rule_id = "t2", variable = "review",
                    apply_to_siblings = FALSE, sibling_pattern = NA_character_,
                    match_type = "token", old_value = "made",
                    new_value = "make", action = "recode")
  res <- apply_recodes(df, rules)
  expect_equal(res$df$review[1], "the duty were make clear")
  expect_equal(res$df$review[2], "nothing to change here")
  expect_equal(res$summary$cells_changed[res$summary$rule_id == "t1"], 1L)
  expect_equal(res$summary$cells_changed[res$summary$rule_id == "t2"], 1L)
})

test_that("token rules are order-independent", {
  df <- tibble::tibble(review = "the duties were made clear")
  mk <- function(id, old, new) tibble::tibble(
    rule_id = id, variable = "review", apply_to_siblings = FALSE,
    sibling_pattern = NA_character_, match_type = "token",
    old_value = old, new_value = new, action = "recode",
    notes = NA_character_, author = NA_character_, created_at = NA_character_,
    updated_at = NA_character_, source_dataset = NA_character_)
  a <- apply_recodes(df, dplyr::bind_rows(mk("t1", "duties", "duty"),
                                          mk("t2", "made", "make")))
  b <- apply_recodes(df, dplyr::bind_rows(mk("t2", "made", "make"),
                                          mk("t1", "duties", "duty")))
  expect_equal(a$df$review, b$df$review)
})

test_that("a whole-cell rule claims the cell before the token pass", {
  df <- tibble::tibble(review = "duties")
  rules <- empty_recodes_tibble() |>
    tibble::add_row(rule_id = "w1", variable = "review",
                    apply_to_siblings = FALSE, sibling_pattern = NA_character_,
                    match_type = "trimmed_ci", old_value = "duties",
                    new_value = "RESPONSIBILITY", action = "recode") |>
    tibble::add_row(rule_id = "t1", variable = "review",
                    apply_to_siblings = FALSE, sibling_pattern = NA_character_,
                    match_type = "token", old_value = "duties",
                    new_value = "duty", action = "recode")
  res <- apply_recodes(df, rules)
  expect_equal(res$df$review, "RESPONSIBILITY")
  expect_equal(res$summary$cells_shadowed[res$summary$rule_id == "t1"], 1L)
})

test_that("validate_recodes flags multi-word and metacharacter tokens", {
  rules <- empty_recodes_tibble() |>
    tibble::add_row(rule_id = "b1", variable = "review",
                    apply_to_siblings = FALSE, sibling_pattern = NA_character_,
                    match_type = "token", old_value = "two words",
                    new_value = "x", action = "recode")
  iss <- validate_recodes(rules)
  expect_true("invalid_token" %in% names(iss))
  expect_equal(nrow(iss$invalid_token), 1L)
})

test_that("a valid single-word token rule is not flagged", {
  rules <- empty_recodes_tibble() |>
    tibble::add_row(rule_id = "g1", variable = "review",
                    apply_to_siblings = FALSE, sibling_pattern = NA_character_,
                    match_type = "token", old_value = "duties",
                    new_value = "duty", action = "recode")
  expect_equal(nrow(validate_recodes(rules)$invalid_token), 0L)
})
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-string_helpers.R')"
```

Expected: FAIL — `"token" %in% RECODE_MATCH_TYPES` is FALSE.

- [ ] **Step 3: Add the enum and the two token primitives**

In `R/string_helpers.R`, change the enum:

```r
RECODE_MATCH_TYPES <- c("exact", "exact_ci", "trimmed_ci", "regex", "token")
```

Add after `.cells_differ()`:

```r
# --- Token rules -------------------------------------------------------------
#
# A `token` rule rewrites a WORD INSIDE a cell rather than replacing the cell.
# Unlike whole-cell rules, token rules must COMPOSE - a long comment can need
# twenty stem fixes and each must land. They are therefore applied as ONE pass
# over an alternation of every token, with a function replacement: each source
# token is examined once and mapped once, so no rule can ever rewrite another
# rule's output. That preserves the single-pass invariant instead of carving out
# an exception to it.

#' Word-boundary-anchored alternation over a set of tokens.
.token_pattern <- function(tokens) {
  esc <- stringr::str_replace_all(as.character(tokens),
                                  "([.^$|()\\[\\]{}*+?\\\\])", "\\\\\\1")
  paste0("\\b(", paste(esc, collapse = "|"), ")\\b")
}

#' Apply a named token map to a character vector in a single pass.
#'
#' @param map Named character vector: names are the tokens to match (matched
#'   case-insensitively), values are the replacements.
.apply_token_map <- function(values, map) {
  v <- as.character(values)
  if (length(v) == 0 || length(map) == 0) return(v)
  names(map) <- tolower(names(map))
  pat <- stringr::regex(.token_pattern(names(map)), ignore_case = TRUE)
  ok  <- !is.na(v)
  v[ok] <- stringr::str_replace_all(v[ok], pat, function(m) {
    r <- map[[tolower(m)]]
    if (is.null(r) || is.na(r)) m else r
  })
  v
}
```

- [ ] **Step 4: Run just the primitive tests**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-string_helpers.R')"
```

Expected: the `.token_pattern` and `.apply_token_map` tests PASS; the
`apply_recodes` / `validate_recodes` token tests still FAIL.

- [ ] **Step 5: Make `.rule_hits()` understand tokens**

In `.rule_hits()`, add a branch inside the `hits <- if (...)` chain, before the
`regex` branch:

```r
  hits <- if (match_type == "token") {
    suppressWarnings(tryCatch(
      !is.na(values) &
        grepl(.token_pattern(old_value), values, ignore.case = TRUE, perl = TRUE),
      error = function(e) rep(FALSE, n)
    ))
  } else if (match_type == "regex") {
```

This gives `validate_recodes()`'s stale check and the per-rule counts a correct
hit vector without changing how whole-cell rules behave.

- [ ] **Step 6: Add the token pass to `apply_recodes()`**

Inside `apply_recodes()`, after the existing whole-cell rule loop finishes and
before the summary is assembled, add:

```r
  # --- Token pass ------------------------------------------------------------
  # Runs AFTER all whole-cell rules so those still see original values. One
  # combined pass per column; cells already claimed by a whole-cell rule are
  # left alone (and reported as shadowed for the token rules that wanted them).
  tok_rules <- rules[!is.na(rules$match_type) & rules$match_type == "token", ,
                     drop = FALSE]
  if (nrow(tok_rules) > 0) {
    tok_cols <- unique(unlist(lapply(seq_len(nrow(tok_rules)), function(i)
      .recode_cols(df, tok_rules[i, ])), use.names = FALSE))

    for (col in tok_cols) {
      # Which token rules target this column, in rule order.
      mine <- vapply(seq_len(nrow(tok_rules)),
                     function(i) col %in% .recode_cols(df, tok_rules[i, ]),
                     logical(1))
      g <- tok_rules[mine, , drop = FALSE]
      g <- g[!is.na(g$old_value) & nzchar(g$old_value), , drop = FALSE]
      if (nrow(g) == 0) next

      # action = delete means "remove the token", i.e. replace with "".
      repl <- ifelse(!is.na(g$action) & g$action == "delete",
                     "", as.character(g$new_value))
      repl[is.na(repl)] <- ""
      map <- stats::setNames(repl, as.character(g$old_value))
      # First rule wins on a duplicated token.
      map <- map[!duplicated(names(map))]

      free   <- !claimed[[col]]
      before <- orig[[col]]
      after  <- before
      after[free] <- .apply_token_map(before[free], map)

      # Per-rule accounting against the ORIGINAL column.
      for (i in seq_len(nrow(g))) {
        rid <- g$rule_id[i]
        hit <- .rule_hits(before, g$old_value[i], "token")
        tok_matched[[rid]]  <- sum(hit)
        tok_shadowed[[rid]] <- sum(hit & claimed[[col]])
        tok_changed[[rid]]  <- sum(hit & free & .cells_differ(before, after))
      }

      touched <- free & .cells_differ(before, after)
      df[[col]][touched] <- after[touched]
      claimed[[col]] <- claimed[[col]] | touched
    }
  }
```

Declare the three accumulators next to `claimed`, before the whole-cell loop:

```r
  tok_matched <- tok_shadowed <- tok_changed <- list()
```

Then, where the summary tibble is built, fold the token counts in for rules
whose `match_type` is `"token"` — read the existing summary construction and add
the token rule's counts from `tok_matched[[rid]]` / `tok_changed[[rid]]` /
`tok_shadowed[[rid]]`, defaulting each to `0L` when the rule matched no column.

> Read the existing loop before editing. `orig` and `claimed` are already
> per-column lists keyed by column name; reuse them, do not create new ones.

- [ ] **Step 7: Add `invalid_token` to `validate_recodes()`**

Add to the issue list, alongside `invalid_regex`:

```r
  # Token rules must name a single word - whitespace or regex metacharacters
  # mean the user meant a regex or a whole-cell rule, not a token.
  tok <- rules |>
    dplyr::filter(!is.na(match_type), match_type == "token")
  issues$invalid_token <- if (nrow(tok) == 0) {
    tok[0, c("rule_id", "variable", "old_value"), drop = FALSE]
  } else {
    tok |>
      dplyr::filter(is.na(old_value) | !nzchar(old_value) |
                    grepl("\\s", old_value) |
                    grepl("[.^$|()\\[\\]{}*+?\\\\]", old_value)) |>
      dplyr::select(rule_id, variable, old_value)
  }
```

- [ ] **Step 8: Run the full suite**

```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```

Expected: PASS, 0 failures. If `test-recode_editor`-style enum tests exist that
assert `length(RECODE_MATCH_TYPES) == 4`, update them to 5 and say so.

- [ ] **Step 9: Commit**

```bash
git add R/string_helpers.R tests/testthat/test-string_helpers.R
git commit -m "feat(recodes): token match_type applied as a single composing pass"
```

---

## Task 8: Token rules in the generated R script

**Files:**
- Modify: `R/string_helpers.R` (`generate_recode_R()`)
- Modify: `R/mod_recode_editor.R` (enum help text only)
- Test: `tests/testthat/test-string_helpers.R`

**Interfaces:**
- Consumes: `.token_pattern()`, `.apply_token_map()` from Task 7
- Produces: `generate_recode_R()` emits a token block per pattern, after that pattern's `case_when` block

**This task must extend the apply↔codegen agreement test.** That test is the
guard for the whole contract; a new match type outside it is a regression
waiting to happen.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-string_helpers.R`:

```r
test_that("generated script rewrites tokens the same way apply_recodes does", {
  df <- tibble::tibble(review = c("the duties were made clear",
                                  "homemade duties", NA_character_))
  rules <- empty_recodes_tibble() |>
    tibble::add_row(rule_id = "t1", variable = "review",
                    apply_to_siblings = FALSE, sibling_pattern = NA_character_,
                    match_type = "token", old_value = "duties",
                    new_value = "duty", action = "recode") |>
    tibble::add_row(rule_id = "t2", variable = "review",
                    apply_to_siblings = FALSE, sibling_pattern = NA_character_,
                    match_type = "token", old_value = "made",
                    new_value = "make", action = "recode")
  from_apply  <- apply_recodes(df, rules)$df
  from_script <- eval_generated_recodes(df, rules)
  expect_equal(from_script, from_apply)
})

test_that("token and whole-cell rules on one column agree across engines", {
  df <- tibble::tibble(review = c("duties", "the duties were made clear",
                                  "unrelated text"))
  rules <- empty_recodes_tibble() |>
    tibble::add_row(rule_id = "w1", variable = "review",
                    apply_to_siblings = FALSE, sibling_pattern = NA_character_,
                    match_type = "trimmed_ci", old_value = "duties",
                    new_value = "RESPONSIBILITY", action = "recode") |>
    tibble::add_row(rule_id = "t1", variable = "review",
                    apply_to_siblings = FALSE, sibling_pattern = NA_character_,
                    match_type = "token", old_value = "duties",
                    new_value = "duty", action = "recode")
  expect_equal(eval_generated_recodes(df, rules), apply_recodes(df, rules)$df)
})

test_that("token rules expand across sibling columns in the script", {
  df <- tibble::tibble(note1 = "duties here", note2 = "more duties")
  rules <- empty_recodes_tibble() |>
    tibble::add_row(rule_id = "t1", variable = "note1",
                    apply_to_siblings = TRUE, sibling_pattern = "^note[0-9]+$",
                    match_type = "token", old_value = "duties",
                    new_value = "duty", action = "recode")
  expect_equal(eval_generated_recodes(df, rules), apply_recodes(df, rules)$df)
})
```

Then extend the EXISTING agreement test's rule table with two token rules over
one of its text columns, so the canonical multi-match-type fixture covers
`token` too. Do not create a parallel fixture — add to the one that is there.

- [ ] **Step 2: Run tests to verify they fail**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-string_helpers.R')"
```

Expected: FAIL — the generated script leaves token rules unapplied, so the
frames differ.

- [ ] **Step 3: Implement the codegen**

In `generate_recode_R()`, inside the per-group loop: split `g` into whole-cell
rules and token rules. Build the `case_when` block from the whole-cell rules
exactly as now (skip the block entirely when there are none), then append a
token block when token rules exist.

Add this helper above the loop:

```r
  # Emit a single-pass token rewrite for one pattern's token rules. Mirrors
  # .apply_token_map(): one alternation, one function replacement, so a rule can
  # never rewrite another rule's output.
  make_token_block <- function(gt, pattern, single_col) {
    repl <- ifelse(!is.na(gt$action) & gt$action == "delete",
                   "", as.character(gt$new_value))
    repl[is.na(repl)] <- ""
    keep <- !duplicated(as.character(gt$old_value))
    toks <- as.character(gt$old_value)[keep]
    repl <- repl[keep]

    map_lines <- paste0("  ", vapply(seq_along(toks), function(i)
      sprintf("%s = %s", deparse(toks[i]), deparse(repl[i])), character(1)),
      collapse = ",\n")

    body <- c(
      sprintf("    str_replace_all(%s,", if (single_col) "%COL%" else ".x"),
      sprintf("      regex(%s, ignore_case = TRUE),",
              deparse(.token_pattern(toks))),
      "      function(.m) .tok_map[tolower(.m)])")

    c(sprintf(".tok_map <- c(\n%s\n)", map_lines),
      "",
      if (single_col) {
        col <- sub("^\\^", "", sub("\\$$", "", pattern))
        c("df <- df |>", sprintf("  mutate(`%s` = ", col),
          gsub("%COL%", sprintf("`%s`", col), body[1], fixed = TRUE),
          body[2], body[3], "  )")
      } else {
        c("df <- df |>",
          sprintf("  mutate(across(matches(%s), function(.x)", deparse(pattern)),
          body[1], body[2], body[3], "  ))")
      },
      "")
  }
```

Call it after the `case_when` block for the group:

```r
    gt <- g[!is.na(g$match_type) & g$match_type == "token", , drop = FALSE]
    gc <- g[ is.na(g$match_type) | g$match_type != "token", , drop = FALSE]
```

— build the existing `case_when` block from `gc` (skipping it when
`nrow(gc) == 0`), then `if (nrow(gt) > 0) blocks <- c(blocks, make_token_block(gt, pattern, single_col))`,
reusing whatever the surrounding code already computes for the
single-column-vs-`across` decision.

`.tok_map` is re-assigned per block, which is intentional and safe: each block
uses it on the line immediately after. Name it distinctly per block only if a
test shows a collision.

- [ ] **Step 4: Run tests to verify they pass**

```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```

Expected: PASS, 0 failures, including the extended agreement test.

- [ ] **Step 5: Update the editor's enum hint**

In `R/mod_recode_editor.R`, find the inline text listing the allowed
`match_type` values and add `token` with a one-line gloss:
`"token (rewrite a word inside the cell, leaving the rest)"`.

- [ ] **Step 6: Commit**

```bash
git add R/string_helpers.R R/mod_recode_editor.R tests/testthat/test-string_helpers.R
git commit -m "feat(codegen): emit token rewrites and extend the agreement test"
```

---

## Task 9: Token-normalization UI

**Files:**
- Modify: `R/mod_text_analysis.R`

**Interfaces:**
- Consumes: `token_vocabulary()`, `stem_groups()` (Task 6); `cluster_strings()`, `cluster_target_decision()`, `cluster_recode_pairs()`, `recode_rule_id()`, `empty_recodes_tibble()` (existing); `active_stopwords()` (Task 4)
- Produces: emits `token` rules through `rules_proxy$add()`

`mod_text_analysis_server()` currently takes `(id, shared_state, selected_var_r)`.
Add a fourth parameter `rules_proxy` and pass it from `app.R`.

- [ ] **Step 1: Add the parameter and the app.R wiring**

```r
mod_text_analysis_server <- function(id, shared_state, selected_var_r, rules_proxy) {
```

In `app.R`, add `rules_proxy = rules_proxy` to the `mod_text_analysis_server()` call.

- [ ] **Step 2: Add the UI card**

Append inside `mod_text_analysis_ui()`'s `card_body`, after the KWIC block:

```r
      shiny::hr(),
      shiny::h6("Normalize tokens"),
      shiny::div(class = "alert alert-info", style = "padding:.4em .8em;",
        shiny::tags$small(
          "Groups words that share a stem (", shiny::tags$em("duty / duties"),
          "). Pick the form to keep and the rest become ",
          shiny::tags$strong("token rules"), " \u2014 they rewrite that word ",
          "inside each cell and leave the rest of the text alone.")),
      shiny::fluidRow(
        shiny::column(4,
          shiny::radioButtons(ns("tok_mode"), "Group by:",
            choices = c("Shared stem" = "stem", "Similarity" = "similarity"),
            selected = "stem", inline = TRUE)),
        shiny::column(4,
          shiny::numericInput(ns("tok_min_n"), "Min token count:",
            value = 2, min = 1, max = 100, step = 1)),
        shiny::column(4,
          shiny::numericInput(ns("tok_max_groups"), "Max groups shown:",
            value = 25, min = 1, max = 200, step = 5))
      ),
      shiny::uiOutput(ns("tok_conflicts")),
      shiny::uiOutput(ns("tok_groups"))
```

- [ ] **Step 3: Add the server logic**

```r
    tok_groups_r <- shiny::reactive({
      v <- col_values(); if (is.null(v)) return(NULL)
      vocab <- token_vocabulary(v, remove_stopwords = isTRUE(input$stop),
                                min_chars = input$minchars %||% 3,
                                stopwords = stopwords_r())
      vocab <- vocab[vocab$n >= (input$tok_min_n %||% 2), , drop = FALSE]
      if (nrow(vocab) == 0) return(NULL)
      g <- if ((input$tok_mode %||% "stem") == "stem") {
        stem_groups(vocab$token, vocab$n)
      } else {
        cl <- cluster_strings(vocab$token, vocab$n, threshold = 0.92,
                              algorithm = "jw", q = 2,
                              normalize = c("lower"))
        cl |>
          dplyr::group_by(cluster_id) |>
          dplyr::filter(dplyr::n() > 1) |>
          dplyr::ungroup() |>
          dplyr::transmute(stem = paste0("group ", cluster_id),
                           token = value, n = n)
      }
      if (is.null(g) || nrow(g) == 0) return(NULL)
      keep <- utils::head(unique(g$stem), input$tok_max_groups %||% 25)
      g[g$stem %in% keep, , drop = FALSE]
    })

    output$tok_groups <- shiny::renderUI({
      g <- tok_groups_r()
      if (is.null(g)) return(shiny::tags$em("No token groups at these settings."))
      stems <- unique(g$stem)
      shiny::tagList(lapply(seq_along(stems), function(k) {
        st <- stems[k]
        mem <- g[g$stem == st, , drop = FALSE]
        labs <- sprintf("%s \u2014 %d", mem$token, mem$n)
        bslib::card(
          full_screen = FALSE, fill = FALSE,
          bslib::card_header(sprintf("%s (%d forms)", st, nrow(mem))),
          bslib::card_body(
            fillable = FALSE,
            shiny::fluidRow(
              shiny::column(7,
                shiny::radioButtons(session$ns(paste0("tok_target_", k)),
                  "Keep:", choiceNames = labs, choiceValues = mem$token,
                  selected = mem$token[1])),
              shiny::column(5,
                shiny::checkboxGroupInput(session$ns(paste0("tok_excl_", k)),
                  "Exclude:", choiceNames = labs, choiceValues = mem$token))
            ),
            shiny::textInput(session$ns(paste0("tok_custom_", k)),
              "\u2026or type a different target:", value = ""),
            shiny::HTML(sprintf(
              paste0('<button class="btn btn-sm btn-primary" ',
                     'onclick="Shiny.setInputValue(\'%s\', ',
                     '{k: %d, nonce: Math.random()}, ',
                     '{priority: \'event\'})">Create token rules</button>'),
              session$ns("tok_apply"), k))
          ))
      }))
    })

    output$tok_conflicts <- shiny::renderUI({
      g <- tok_groups_r(); if (is.null(g)) return(NULL)
      stems <- unique(g$stem)
      msgs <- character(0)
      for (k in seq_along(stems)) {
        mem <- g[g$stem == stems[k], , drop = FALSE]
        d <- cluster_target_decision(
          members  = mem$token,
          radio    = input[[paste0("tok_target_", k)]],
          custom   = input[[paste0("tok_custom_", k)]],
          excluded = input[[paste0("tok_excl_", k)]] %||% character(0))
        if (!identical(d$status, "ok"))
          msgs <- c(msgs, sprintf("%s: %s", stems[k], d$message))
      }
      if (length(msgs) == 0) return(NULL)
      shiny::div(class = "alert alert-warning", style = "padding:.4em .8em;",
        shiny::tags$ul(lapply(msgs, shiny::tags$li)))
    })

    shiny::observeEvent(input$tok_apply, ignoreInit = TRUE, {
      g <- tok_groups_r(); if (is.null(g)) return(NULL)
      k <- input$tok_apply$k
      stems <- unique(g$stem)
      if (is.null(k) || k > length(stems)) return(NULL)
      mem <- g[g$stem == stems[k], , drop = FALSE]

      d <- cluster_target_decision(
        members  = mem$token,
        radio    = input[[paste0("tok_target_", k)]],
        custom   = input[[paste0("tok_custom_", k)]],
        excluded = input[[paste0("tok_excl_", k)]] %||% character(0))
      if (!identical(d$status, "ok")) {
        shiny::showNotification(d$message, type = "error", duration = NULL)
        return(NULL)
      }

      pairs <- cluster_recode_pairs(
        mem$token, d$target,
        input[[paste0("tok_excl_", k)]] %||% character(0))
      if (nrow(pairs) == 0) {
        shiny::showNotification(
          "Nothing to normalize \u2014 no non-excluded forms besides the target.",
          type = "warning", duration = 4)
        return(NULL)
      }

      v  <- selected_var_r()
      ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      new_rules <- empty_recodes_tibble() |>
        dplyr::bind_rows(tibble::tibble(
          rule_id           = vapply(pairs$old_value,
                                     function(o) recode_rule_id(v, "token", o),
                                     character(1)),
          variable          = v,
          apply_to_siblings = FALSE,
          sibling_pattern   = NA_character_,
          match_type        = "token",
          old_value         = pairs$old_value,
          new_value         = pairs$new_value,
          action            = "recode",
          notes             = sprintf("Token normalization (%s)", stems[k]),
          author            = Sys.info()[["user"]],
          created_at        = ts,
          updated_at        = ts,
          source_dataset    = shared_state$dataset_name
        ))
      rules_proxy$add(new_rules)
      shiny::showNotification(
        sprintf("Created %d token rule(s) \u2192 '%s'.", nrow(pairs), d$target),
        type = "message", duration = 3)
    })
```

- [ ] **Step 4: Parse check and suite**

```bash
Rscript -e "invisible(lapply(list.files('R', full.names=TRUE), parse)); invisible(parse('app.R')); cat('parse OK\n'); testthat::test_dir('tests/testthat')"
```

Expected: `parse OK`, PASS, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add R/mod_text_analysis.R app.R
git commit -m "feat(text-analysis): stem-group token normalization emitting token rules"
```

---

## Task 10: Manual verification of the token workflow

**Files:** none modified — this is a gate before the optional feature E.

- [ ] **Step 1: Launch and drive the app**

```r
shiny::runApp()
```

- [ ] **Step 2: Walk the workflow**

1. Data tab → **Load bundled example dataset**.
2. Variable tab → select `review`.
3. Text analysis tab → confirm the Language section renders and reports mostly `en`.
4. Set **Min token count** to 1 and confirm at least one stem group appears.
5. Pick a target, exclude a different member, click **Create token rules**.
6. Exclude the member you selected as target → confirm Apply **refuses** with a named error and writes no rules.
7. Recodes tab → confirm the new rules show `match_type = token`.
8. Preview & export → confirm the generated script contains a `.tok_map` block and `str_replace_all`, and that the diff shows partial-cell changes rather than whole-cell replacement.

- [ ] **Step 3: Record the result**

Note anything that misbehaves in the task's review notes. Do not proceed to
Task 11 with a broken token workflow — E is independent and can be dropped, but
B/C is the point of the plan.

---

## Task 11: Document clustering helpers

**Files:**
- Create: `R/doc_cluster_helpers.R`
- Test: `tests/testthat/test-doc_cluster_helpers.R`

**Interfaces:**
- Consumes: `.tokens_of()`, `EN_STOPWORDS` from `R/text_helpers.R`
- Produces:
  - `build_dtm(values, remove_stopwords = TRUE, min_chars = 3, min_docs = 1, stopwords = EN_STOPWORDS) -> Matrix` (documents × terms, dimnames set)
  - `tfidf_matrix(dtm) -> Matrix` — L2-row-normalized TF-IDF
  - `cluster_documents(tfidf, k) -> integer` — cluster id per document, named by rownames
  - `cluster_top_terms(dtm, clustering, n = 5) -> tibble(cluster int, size int, top_terms chr)`

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-doc_cluster_helpers.R`:

```r
source(file.path("..", "..", "R", "text_helpers.R"))
source(file.path("..", "..", "R", "doc_cluster_helpers.R"))

docs <- c(
  "coffee espresso latte coffee",
  "espresso coffee macchiato",
  "bicycle wheel chain bicycle",
  "wheel bicycle brake chain"
)

test_that("build_dtm has one row per document and counts terms", {
  m <- build_dtm(docs, remove_stopwords = FALSE, min_chars = 1)
  expect_equal(nrow(m), 4L)
  expect_true("coffee" %in% colnames(m))
  expect_equal(as.numeric(m["1", "coffee"]), 2)
})

test_that("build_dtm drops terms below min_docs", {
  m <- build_dtm(docs, remove_stopwords = FALSE, min_chars = 1, min_docs = 2)
  expect_true("coffee" %in% colnames(m))    # in 2 docs
  expect_false("latte" %in% colnames(m))    # in 1 doc
})

test_that("build_dtm returns an empty matrix for empty input", {
  m <- build_dtm(character(0))
  expect_equal(nrow(m), 0L)
})

test_that("tfidf rows are L2-normalized", {
  m <- tfidf_matrix(build_dtm(docs, remove_stopwords = FALSE, min_chars = 1))
  norms <- sqrt(Matrix::rowSums(m * m))
  expect_true(all(abs(norms[norms > 0] - 1) < 1e-9))
})

test_that("cluster_documents separates the two topics", {
  m <- tfidf_matrix(build_dtm(docs, remove_stopwords = FALSE, min_chars = 1))
  cl <- cluster_documents(m, k = 2)
  expect_length(cl, 4L)
  expect_equal(cl[[1]], cl[[2]])   # both coffee docs
  expect_equal(cl[[3]], cl[[4]])   # both bicycle docs
  expect_false(cl[[1]] == cl[[3]])
})

test_that("cluster_top_terms reports size and distinctive terms", {
  dtm <- build_dtm(docs, remove_stopwords = FALSE, min_chars = 1)
  cl  <- cluster_documents(tfidf_matrix(dtm), k = 2)
  s   <- cluster_top_terms(dtm, cl, n = 2)
  expect_true(all(c("cluster", "size", "top_terms") %in% names(s)))
  expect_equal(sum(s$size), 4L)
  coffee_row <- s$top_terms[s$cluster == cl[[1]]]
  expect_true(grepl("coffee", coffee_row))
})

test_that("cluster_documents errors clearly when k exceeds the document count", {
  m <- tfidf_matrix(build_dtm(docs, remove_stopwords = FALSE, min_chars = 1))
  expect_error(cluster_documents(m, k = 99), "more clusters than documents")
})
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-doc_cluster_helpers.R')"
```

Expected: FAIL — cannot open `R/doc_cluster_helpers.R`.

- [ ] **Step 3: Implement**

Create `R/doc_cluster_helpers.R`:

```r
# =============================================================================
# Recode Studio — Document clustering (pure R, no Shiny)
# =============================================================================
# Clusters ROWS (documents) by content, as opposed to mod_cluster_view which
# clusters unique VALUES by string similarity. Analysis only - produces no
# recode rules.
#
# TF-IDF + cosine + Ward linkage, all with Matrix (ships with R) and
# stats::hclust. No text-mining dependency: the heavy packages only pay for
# themselves at topic-model scale, which is explicitly out of scope.
# =============================================================================

#' Document-term matrix.
#'
#' @param min_docs Drop terms appearing in fewer than this many documents.
#' @return dgCMatrix, documents (rows) x terms (columns), dimnames set.
build_dtm <- function(values, remove_stopwords = TRUE, min_chars = 3,
                      min_docs = 1, stopwords = EN_STOPWORDS) {
  v <- as.character(values)
  per <- .tokens_of(v)
  per <- lapply(per, function(t) {
    if (min_chars > 1)    t <- t[nchar(t) >= min_chars]
    if (remove_stopwords) t <- t[!t %in% stopwords]
    t
  })
  vocab <- sort(unique(unlist(per, use.names = FALSE)))
  if (length(vocab) == 0 || length(per) == 0)
    return(Matrix::Matrix(0, nrow = length(per), ncol = 0, sparse = TRUE))

  i <- rep(seq_along(per), lengths(per))
  j <- match(unlist(per, use.names = FALSE), vocab)
  m <- Matrix::sparseMatrix(i = i, j = j, x = 1,
                            dims = c(length(per), length(vocab)),
                            dimnames = list(as.character(seq_along(per)), vocab))
  if (min_docs > 1) {
    keep <- Matrix::colSums(m > 0) >= min_docs
    m <- m[, keep, drop = FALSE]
  }
  m
}

#' L2-normalized TF-IDF over a document-term matrix.
tfidf_matrix <- function(dtm) {
  if (nrow(dtm) == 0 || ncol(dtm) == 0) return(dtm)
  df  <- Matrix::colSums(dtm > 0)
  idf <- log(nrow(dtm) / pmax(df, 1)) + 1
  m   <- dtm %*% Matrix::Diagonal(x = idf)
  dimnames(m) <- dimnames(dtm)
  norms <- sqrt(Matrix::rowSums(m * m))
  norms[norms == 0] <- 1
  m <- Matrix::Diagonal(x = 1 / norms) %*% m
  dimnames(m) <- dimnames(dtm)
  m
}

#' Hierarchical clustering of documents on cosine distance.
#'
#' @return Named integer vector: cluster id per document.
cluster_documents <- function(tfidf, k = 5) {
  n <- nrow(tfidf)
  if (n < 2) stop("Need at least two documents to cluster.")
  if (k > n) stop("Cannot ask for more clusters than documents.")
  csim <- as.matrix(tfidf %*% Matrix::t(tfidf))
  csim[csim > 1] <- 1
  cdist <- stats::as.dist(1 - csim)
  hc <- stats::hclust(d = cdist, method = "ward.D2")
  cl <- stats::cutree(hc, k)
  stats::setNames(as.integer(cl), rownames(tfidf))
}

#' Distinctive terms per cluster.
#'
#' Ranks by lift: in-cluster term share minus corpus-wide term share, so a term
#' that is common everywhere does not dominate every cluster.
cluster_top_terms <- function(dtm, clustering, n = 5) {
  ids <- sort(unique(clustering))
  p_all <- Matrix::colSums(dtm) / sum(dtm)
  rows <- lapply(ids, function(id) {
    sub <- dtm[clustering == id, , drop = FALSE]
    tot <- sum(sub)
    lift <- if (tot == 0) stats::setNames(rep(0, ncol(dtm)), colnames(dtm))
            else Matrix::colSums(sub) / tot - p_all
    top <- names(sort(lift, decreasing = TRUE))[seq_len(min(n, length(lift)))]
    tibble::tibble(cluster = as.integer(id),
                   size = sum(clustering == id),
                   top_terms = paste(top[!is.na(top)], collapse = ", "))
  })
  dplyr::bind_rows(rows)
}
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```

Expected: PASS, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add R/doc_cluster_helpers.R tests/testthat/test-doc_cluster_helpers.R
git commit -m "feat(docs): TF-IDF document clustering helpers"
```

---

## Task 12: Document clustering UI

**Files:**
- Modify: `R/mod_text_analysis.R`

**Interfaces:**
- Consumes: `build_dtm()`, `tfidf_matrix()`, `cluster_documents()`, `cluster_top_terms()` from Task 11

- [ ] **Step 1: Add the UI block**

Append inside `mod_text_analysis_ui()`'s `card_body`:

```r
      shiny::hr(),
      shiny::h6("Document clusters"),
      shiny::tags$small(shiny::tags$em(
        "Groups whole values by shared vocabulary (TF-IDF + cosine). ",
        "Analysis only \u2014 this produces no recode rules.")),
      shiny::fluidRow(
        shiny::column(4,
          shiny::sliderInput(ns("dc_k"), "Number of clusters:",
            min = 2, max = 20, value = 5, step = 1)),
        shiny::column(4,
          shiny::numericInput(ns("dc_min_docs"), "Drop terms in fewer than N values:",
            value = 2, min = 1, max = 50, step = 1)),
        shiny::column(4,
          shiny::numericInput(ns("dc_top"), "Top terms per cluster:",
            value = 5, min = 1, max = 20, step = 1))
      ),
      DT::DTOutput(ns("dc_summary")),
      shiny::br(),
      shiny::uiOutput(ns("dc_pick")),
      DT::DTOutput(ns("dc_docs"))
```

- [ ] **Step 2: Add the server logic**

```r
    # Cosine similarity is O(n^2) in documents; refuse rather than hang.
    DC_MAX_DOCS <- 2000L

    dc_r <- shiny::reactive({
      v <- col_values(); if (is.null(v) || length(v) < 2) return(NULL)
      if (length(v) > DC_MAX_DOCS) return(list(too_big = length(v)))
      dtm <- build_dtm(v, remove_stopwords = isTRUE(input$stop),
                       min_chars = input$minchars %||% 3,
                       min_docs = input$dc_min_docs %||% 2,
                       stopwords = stopwords_r())
      if (nrow(dtm) < 2 || ncol(dtm) == 0) return(NULL)
      k <- min(input$dc_k %||% 5, nrow(dtm))
      cl <- cluster_documents(tfidf_matrix(dtm), k = k)
      list(values = v, dtm = dtm, clustering = cl)
    })

    output$dc_summary <- DT::renderDT({
      d <- dc_r()
      shiny::validate(shiny::need(!is.null(d), "Not enough text to cluster."))
      if (!is.null(d$too_big))
        shiny::validate(shiny::need(FALSE, sprintf(
          "Too many values to cluster (%d; limit %d). Filter the data first.",
          d$too_big, DC_MAX_DOCS)))
      DT::datatable(cluster_top_terms(d$dtm, d$clustering, n = input$dc_top %||% 5),
                    rownames = FALSE, options = list(pageLength = 10, dom = "tp"))
    })

    output$dc_pick <- shiny::renderUI({
      d <- dc_r(); if (is.null(d) || !is.null(d$too_big)) return(NULL)
      shiny::selectInput(session$ns("dc_show"), "Show values in cluster:",
                         choices = sort(unique(d$clustering)))
    })

    output$dc_docs <- DT::renderDT({
      d <- dc_r()
      shiny::validate(shiny::need(!is.null(d) && is.null(d$too_big), ""))
      sel <- suppressWarnings(as.integer(input$dc_show))
      shiny::validate(shiny::need(!is.na(sel), "Pick a cluster."))
      DT::datatable(tibble::tibble(value = d$values[d$clustering == sel]),
                    rownames = FALSE, options = list(pageLength = 10, dom = "tp"))
    })
```

- [ ] **Step 3: Parse check and suite**

```bash
Rscript -e "invisible(lapply(list.files('R', full.names=TRUE), parse)); invisible(parse('app.R')); cat('parse OK\n'); testthat::test_dir('tests/testthat')"
```

Expected: `parse OK`, PASS, 0 failures.

- [ ] **Step 4: Commit**

```bash
git add R/mod_text_analysis.R
git commit -m "feat(text-analysis): document clustering with per-cluster top terms"
```

---

## Task 13: Documentation

**Files:**
- Modify: `CLAUDE.md`, `README.md`

- [ ] **Step 1: Update CLAUDE.md**

- Architecture tree: add `stopword_helpers.R`, `token_helpers.R`, `doc_cluster_helpers.R`, and the `stopwords/` directory.
- "Generated R shape" table: add the `token` row — `str_replace_all(<col>, regex("\\b(a|b)\\b", ignore_case = TRUE), function(.m) .tok_map[tolower(.m)])`.
- "The single-pass invariant" section: add the paragraph explaining that token rules compose *within* a cell but are still single-pass across rules, that the token pass runs after whole-cell rules, and that the agreement test now covers `token`.
- "match_type semantics": document `token` — word-boundary anchored, case-insensitive, replaces only the token, and `validate_recodes()$invalid_token` rejects multi-word or metacharacter tokens.
- New "Stopwords" section mirroring the "Dictionaries" section.
- Testing table: add the three new test files with their assertion counts, and update the total.
- Open/TODO: strike the "Peer code review" and "language detection" items, replacing them with a line recording what the reference script turned out to be (`ext/tokenization_reference.Rmd`, tokenization/normalization pipeline) and that both are now implemented.
- Gotchas: add "`cld2` is an OPTIONAL runtime dependency — `detect_languages()` returns all-NA when it is missing, and the UI shows an install hint. Don't make it a hard requirement."

- [ ] **Step 2: Update README.md**

- Install line: add `cld2`.
- "How it works": extend the Text analysis bullet with language detection, token normalization, and document clusters.
- "The recode CSV" match-type list: add `token`.
- "Project layout": add the three new helper files and `stopwords/`.
- New "Stopword tiers" section next to "Dictionary system".

- [ ] **Step 3: Final full verification**

```bash
Rscript -e "invisible(lapply(list.files('R', full.names=TRUE), parse)); invisible(parse('app.R')); cat('parse OK\n'); testthat::test_dir('tests/testthat')"
```

Expected: `parse OK`, `[ FAIL 0 | WARN 0 | SKIP 0 | PASS <n> ]` with `<n>` well above 383.

- [ ] **Step 4: Commit**

```bash
git add CLAUDE.md README.md
git commit -m "docs: token normalization, stopword tiers, language detection, doc clusters"
```

---

## Self-review notes

**Spec coverage:** A → Tasks 1-3. D → Tasks 4-5. C → Tasks 6, 9. B → Tasks 7-8. E → Tasks 11-12. Manual gate → Task 10. Docs → Task 13.

**Known risks, called out rather than hidden:**

1. **Task 7 Step 6 is the hardest edit in the plan.** It modifies `apply_recodes()`, whose correctness is load-bearing and was only just repaired. The plan deliberately says "read the existing loop before editing" rather than showing a full replacement, because the surrounding variable names must be matched exactly. If the summary-assembly folding proves awkward, the fallback is to build the token summary rows separately and `dplyr::rows_update()` them into the existing summary — but try the direct route first.

2. **Task 8's `make_token_block()` string assembly is fiddly.** The `%COL%` placeholder for the single-column branch is a shortcut; if it fights the surrounding code, build the two branches independently instead. The agreement test is the arbiter — if it passes, the emitted code is right.

3. **`.suffix_stem()` expected values in Task 6 are asserted, not derived.** `"making" -> "mak"` is what the listed rule order produces. If hunspell is available, `.lemma_of()` will usually override it with `"make"`, so the fallback's exact output rarely surfaces. Tests target `.suffix_stem()` directly to keep it pinned.

4. **Feature E's `DC_MAX_DOCS <- 2000` is a guess.** Cosine similarity builds an n×n dense matrix — 2000 documents is ~32 MB, which is fine; 10000 would be ~800 MB, which is not. The limit is enforced with an explicit message rather than silent truncation.
