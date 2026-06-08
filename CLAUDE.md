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
`inst/extdata/example_messy.csv` (30 rows, deliberate asphyxiation/gunshot
typo families + a `cause1`/`cause2` sibling pair) so the app is testable out
of the box.

## Architecture

Single-purpose Shiny app (no multi-section shell — that was upstream-specific).

```
app.R                    # entry: sources R/, builds ui + server, shinyApp()
R/
  string_helpers.R       # PURE R core (no Shiny). cluster/validate/apply/codegen
  data_loader.R          # read CSV/Excel + build per-column metadata
  ui_helpers.R           # %||%, warning_banner(), badge()
  mod_data_input.R       # upload / example loader (ONLY module that WRITES state)
  mod_variable_picker.R  # DT table of columns; returns selected variable reactive
  mod_value_table.R      # frequency table + "create rule from selected rows"
  mod_cluster_view.R     # similarity clusters + "recode all to modal"
  mod_spellcheck_view.R  # hunspell flags + clickable suggestions + dictionaries
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
`trimmed_ci` wraps the LHS in `str_squish(tolower(...))`. Always ends each arm
set with `.default = <col>` so unmatched values pass through.

## Conventions

- R + tidyverse. Native pipe `|>`, never `%>%`.
- 2-space indent.
- `string_helpers.R` and `data_loader.R` stay Shiny-free so they're unit-testable
  and reusable from scripts. Shiny calls live only in `mod_*`, `app.R`.
- Module files define `mod_<name>_ui(id)` + `mod_<name>_server(id, ...)`.

## Gotchas (learned the hard way — don't reintroduce)

- **`case_when`, not `case_match`.** `dplyr::case_match()` is deprecated as of
  dplyr 1.2.0 and prints a warning at runtime. The generator uses `case_when`.
- **`isTRUE()` is NOT vectorised.** In `generate_recode_R()` / `apply_recodes()`
  guard `apply_to_siblings` with `!is.na(x) & x` over a vector, not `isTRUE(x)`,
  or sibling expansion silently breaks for multi-row rule sets.
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
import a new one (uploaded `.txt` is copied into the folder, committed,
auto-selected). `medical.txt` ships bundled. `seed_terms.txt` is intentionally
empty — this is a general tool, no assumed vocabulary.

## Testing

```r
testthat::test_dir("tests/testthat")
```

Covers the pure-R core only (schema, normalize, cluster, CSV round-trip,
validate, apply, codegen). 31 assertions.

**Dependency note:** this repo has NO `renv` yet, so a bare `Rscript` uses the
system library, which on this machine is missing `stringdist`/`igraph`/
`hunspell`/etc. For verification, point `R_LIBS` at the upstream project's renv
library:

```powershell
$env:R_LIBS = "renv/library"
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" -e "testthat::test_dir('tests/testthat')"
```

A real fix is `renv::init()` here (TODO, not done yet). Dependencies are listed
in `DESCRIPTION` (Imports) and the README install line.

## Environment

- Windows. R 4.5.2 at `C:\Program Files\R\R-4.5.2\bin\Rscript.exe`.
- IDE: Positron (restart R with Ctrl+Shift+0, NOT the VS Code "R: Restart R").
- Git default branch: `main`. Two commits so far (initial + discipline dicts).
- No GitHub remote yet — push/visibility is the user's decision (name decided:
  "recode-studio").

## Open / TODO

- `renv::init()` for reproducible deps.
- GitHub remote + push (needs `gh` auth + visibility decision).
- Optional: expand the free-text heuristic to let the user override a column's
  candidate flag manually.
