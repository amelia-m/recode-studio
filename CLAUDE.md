# CLAUDE.md

Guidance for Claude Code (and other Claude instances) working in this repo.
IMPORTANT: these instructions override default behavior — follow them.

## Project

**Recode Studio** — a dataset-agnostic R/Shiny app for cleaning messy string
variables (typos, capitalization noise, duplicate tokens, near-duplicate
variants) without writing R by hand. Target user is spreadsheet-fluent but NOT
a fluent R coder.

Load any CSV/Excel → pick a text column → browse values alphabetically or as
string-similarity clusters → spellcheck → build recode rules (with optional
sibling-column propagation) → export a recode CSV + a runnable `dplyr` script.
**The app never modifies the source data file.** It only exports a CSV + R.

### Origin

Ported and generalized from the **upstream** pipeline's "Recode Studio" Shiny
section (`the upstream data pipeline`). That version was agency/health
specific (quarter dropdown reading `merged/dataset_wide_*.csv`, agency column
prefixes, a bundled medical dictionary, a multi-section shell). This repo is
the standalone, domain-neutral spin-off. If you change shared logic here,
the upstream copy does NOT auto-update (and vice-versa) — they diverged on purpose.

## Running

```r
# from the repo root
shiny::runApp()
```

On the **Data** tab, "Load bundled example dataset" loads
`inst/extdata/example_messy.csv` — a benign café-orders dataset (30 rows, 8
columns) that exercises every feature: typo clusters (cappuccino/espresso
variants), a `drink1`/`drink2` sibling family, a multi-word `notes` column with
reordered + duplicate-token values (for the normalize/cosine options), plus
excluded numeric/date/small-choice columns (`order_id`, `price`, `order_date`,
`size`). Deliberately non-sensitive.

## Architecture

Single-purpose Shiny app (no multi-section shell — that was upstream-specific).

```
app.R                    # entry: sources R/, builds ui + server, shinyApp()
R/
  string_helpers.R       # PURE R core (no Shiny). cluster/validate/apply/codegen
  text_helpers.R         # PURE R. long-text: tokenize, classify_text_length,
                         #   top_tokens / top_ngrams / kwic, count_sentences
  data_loader.R          # read CSV/Excel + build per-column metadata
                         #   (incl. is_long_text / text_kind via text_helpers)
  ui_helpers.R           # %||%, warning_banner(), badge()
  mod_data_input.R       # upload / example loader (ONLY module that WRITES state)
  mod_variable_picker.R  # DT table of columns; returns selected variable reactive
  mod_value_table.R      # frequency table + "create rule from selected rows"
  mod_cluster_view.R     # similarity clusters; user-chosen target + algo/normalize
  mod_spellcheck_view.R  # hunspell flags + clickable suggestions + dictionaries
  mod_text_analysis.R    # long-text columns: length dist, token/n-gram freq, KWIC
  mod_recode_editor.R    # editable DT of rules + Validate
  mod_preview_export.R   # before/after diff + downloads + copy-to-clipboard
  mod_import_recodes.R   # merge an external recodes CSV (conflict resolution)
dictionary/
  seed_terms.txt         # tier 1 (committed, ships EMPTY — no assumed domain)
  custom_terms.txt       # tier 2 (committed, project-shared additions)
  user_terms.txt         # tier 3 (GITIGNORED, per-user additions)
  disciplines/           # optional domain word lists; bundled: medical.txt
inst/extdata/example_messy.csv
tests/testthat/test-string_helpers.R
```

### State + data flow

- `server()` owns `shared_state <- reactiveValues(data, meta, dataset_name)`.
  **Only `mod_data_input` writes it.** Every other module reads it.
- `rules_proxy <- list(get = reactive, add = fn, set = fn)` is the single
  mutation channel for the in-session rule set (`rv$rules`). Modules that
  create/edit rules take `rules_proxy`. `add()` treats a colliding `rule_id`
  as an update (drops the old, binds the new).
- `selected_var_r` (reactive, from the variable picker) and `unique_values_r`
  (reactive `tibble(value, n)` for the selected column) are computed once in
  `server()` and passed down to the value/cluster/spellcheck modules. Don't
  recompute uniques per module.

### Recode CSV schema (`recodes_master.csv`)

Columns: `rule_id`, `variable`, `apply_to_siblings` (lgl), `sibling_pattern`
(regex or NA), `match_type` (`exact`/`exact_ci`/`trimmed_ci` default/`regex`),
`old_value`, `new_value`, `action` (`recode`/`delete`), `notes`, `author`,
`created_at`, `updated_at`, `source_dataset`.

- Unique key: `(variable, match_type, old_value)`. `rule_id` is a stable
  12-char hash of those three (`recode_rule_id()`).
- NA encoding: `new_value = NA` round-trips as the literal string `<NA>`
  (reader/writer use `na = "<NA>"`). Empty string stays empty. This survives
  Excel edits. `action = delete` sets cells to NA regardless of `new_value`.
- NOTE: schema field is `source_dataset` here; the upstream copy calls it
  `source_quarter`. Don't cross them.

### Generated R shape

`generate_recode_R(rules, dataset_id)` emits one `dplyr::case_when()` block per
(effective_pattern, match_type) group. Single-column rules → plain `mutate()`;
sibling rules → `mutate(across(matches("<pattern>"), function(.x) case_when(...)))`.
`trimmed_ci` wraps the LHS in `str_squish(tolower(...))`. `regex` rules emit
`str_detect(<col>, "<pattern>") ~ <new>` arms instead of `==`. Always ends each
arm set with `.default = <col>` so unmatched values pass through.

### match_type semantics

`exact` / `exact_ci` / `trimmed_ci` compare for equality after
`normalize_value()`. `regex` matches `old_value` as an unanchored `grepl`
pattern against the RAW value and replaces the whole cell (no partial sub /
backreferences). All matching goes through the `.rule_hits()` helper in
`string_helpers.R` — used by both `apply_recodes()` and the stale check in
`validate_recodes()`, so they can never diverge. Invalid regex patterns are
caught (all-FALSE, no error) and surfaced by `validate_recodes()$invalid_regex`.

## Conventions

- R + tidyverse. Native pipe `|>`, never `%>%`.
- 2-space indent.
- `string_helpers.R` and `data_loader.R` stay Shiny-free so they're unit-testable
  and reusable from scripts. Shiny calls live only in `mod_*`, `app.R`.
- Module files define `mod_<name>_ui(id)` + `mod_<name>_server(id, ...)`.

## Gotchas (learned the hard way — don't reintroduce)

- **`case_when`, not `case_match`.** `dplyr::case_match()` is deprecated as of
  dplyr 1.2.0 and prints a warning at runtime. The generator uses `case_when`.
- **`isTRUE()` is NOT vectorised.** In `generate_recode_R()` guard
  `apply_to_siblings` with `!is.na(x) & x` over a vector, not `isTRUE(x)`.
  In `apply_recodes()` / `validate_recodes()` the loop is row-at-a-time so
  `isTRUE()` is technically safe, but use `!is.na(x) && x` there too to
  match the documented pattern and survive future vectorization.
- **Sibling regex escaping uses stringr (ICU), not base `gsub` (TRE).** An
  earlier base-`gsub` char class with `{}` threw "Invalid contents of {}" in the
  TRE engine and broke the clusters tab. `suggest_sibling_pattern()` now escapes
  via `stringr::str_replace_all`. Keep it that way.
- **bslib cards must be `fill = FALSE` + `card_body(fillable = FALSE)`** here.
  Default flex-fill collapses stacked cards / overlaps DT + inputs (hit this on
  the clusters and spellcheck tabs). Every card in this app sets both.
- **Inline DT action buttons** use raw HTML + `Shiny.setInputValue('<ns-id>',
  payload, {priority:'event'})` with `escape = FALSE`. Multi-field payloads are
  JSON-encoded and parsed with `jsonlite::fromJSON` server-side (see spellcheck
  suggestion/custom-correction handlers).
- **Free-text classifier** (`build_meta`) excludes dates, numbers, and small
  choice sets: requires median length > 3, NA share < 60%, > 10 unique values,
  and not date-like / numeric-like (80% regex thresholds). Don't loosen without
  re-checking the example dataset still classifies cause/city as text and
  id/count/status/visit_date as non-candidates.

## Dictionaries

Hunspell `en_US` + three supplementary tiers (seed/custom/user) + any selected
**discipline** dictionaries from `dictionary/disciplines/*.txt`. A word is OK if
any source accepts it. The Spellcheck tab can select multiple disciplines and
import a new one (uploaded `.txt` is copied into the folder and auto-selected;
the user must `git add`/commit it to share with the team). `medical.txt` ships bundled. `seed_terms.txt` is intentionally
empty — this is a general tool, no assumed vocabulary.

## Testing

```r
testthat::test_dir("tests/testthat")
```

`test-string_helpers.R` covers the pure-R core (schema, normalize, cluster,
CSV round-trip, validate incl. regex/enum, apply incl. regex, codegen).
`test-data_loader.R` covers metadata + classifiers (`build_meta`,
`column_group`, `suggest_sibling_pattern` incl. metachar escaping, date/numeric
detection, `read_dataset`). ~77 assertions. CI runs them on every push/PR via
`.github/workflows/tests.yml` (r-lib/actions + DESCRIPTION-driven deps).

**Dependencies:** this repo uses `renv` (initialized 2026-06; `renv.lock` pins
101 packages on R 4.5.2). `.Rprofile` activates renv on session start, so a
bare `Rscript` from the repo root uses the project library — no `R_LIBS`
workaround. On a fresh clone, run `renv::restore()` first. `DESCRIPTION`
(Imports) and the README install line list the direct deps. `writexl` is NOT a
dependency — exports are CSV + R script only.

## Environment

- Windows. R 4.5.2 at `C:\Program Files\R\R-4.5.2\bin\Rscript.exe`.
- IDE: Positron (restart R with Ctrl+Shift+0, NOT the VS Code "R: Restart R").
- Git default branch: `main`.
- GitHub remote: https://github.com/amelia-m/recode-studio (public, pushed 2026-06-08).

## Open / TODO

- Optional: expand the free-text heuristic to let the user override a column's
  candidate flag manually.
- Optional: README screenshot / demo GIF (capture from a running app).

Note: `match_type` / `action` are constrained — single-sourced in
`RECODE_MATCH_TYPES` / `RECODE_ACTIONS` (string_helpers.R). The editor rejects
out-of-range edits live; `validate_recodes()$invalid_enum` catches bad enums in
imported CSVs.
