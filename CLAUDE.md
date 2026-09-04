# CLAUDE.md

Guidance for Claude Code (and other AI agents) working in this repo.
IMPORTANT: these instructions override default behavior — follow them.
All agents must adhere to the **Clanker Constitution** in `.agents/constitution.md` (see `AGENTS.md`).

## Project

**Recode Studio** — a dataset-agnostic R/Shiny app for cleaning messy string
variables (typos, capitalization noise, duplicate tokens, near-duplicate
variants) without writing R by hand. Target user is spreadsheet-fluent but NOT
a fluent R coder.

Load any CSV/Excel → pick a text column → browse values alphabetically or as
string-similarity clusters → spellcheck → build recode rules (with optional
sibling-column propagation) → optionally derive new columns via
recategorization → export a recode CSV + a runnable `dplyr` script.
**The app never modifies the source data file.** It only exports a CSV + R.

### Origin

Ported and generalized from an earlier data pipeline's "Recode Studio" Shiny
section for a state agency. That version was agency-specific (quarter dropdown
reading merged CSVs, agency column prefixes, a bundled medical dictionary, a
multi-section shell). This repo is the standalone, domain-neutral spin-off. If you
change shared logic here, the upstream copy does NOT auto-update (and vice-versa) —
they diverged on purpose.

## Running

```r
# from the repo root
shiny::runApp()
```

On the **Data** tab, "Load bundled example dataset" loads
`inst/extdata/example_messy.csv` — a benign café-orders dataset (30 rows, 9
columns) that exercises every feature: typo clusters (cappuccino/espresso
variants), a `drink1`/`drink2` sibling family, a multi-word `notes` column with
reordered + duplicate-token values (for the normalize/cosine options), a
sentence-length `review` column (for long-text detection / Text analysis), plus
excluded numeric/date/small-choice columns (`order_id`, `price`, `order_date`,
`size`). Deliberately non-sensitive.

Free-text candidates in the example are exactly `drink1`, `drink2`, `city`,
`notes`, `review`. Any classifier change must preserve that — there is a
regression test pinning it in `test-data_loader.R`.

## Architecture

Single-purpose Shiny app (no multi-section shell — that was agency-specific).

```
app.R                    # entry: sources R/, builds ui + server, shinyApp()
R/
  string_helpers.R       # PURE R core (no Shiny). cluster/validate/apply/codegen
                         #   + match_taxonomy() (fuzzy match a column against an
                         #   approved standard list)
  text_helpers.R         # PURE R. long-text: tokenize, classify_text_length,
                         #   top_tokens / top_ngrams / kwic, count_sentences
  recat_helpers.R        # PURE R. recategorization: match/apply/codegen/IO
  data_loader.R          # read CSV/Excel + build per-column metadata
                         #   (incl. is_long_text / text_kind via text_helpers)
  ui_helpers.R           # %||%, warning_banner(), badge()
  mod_data_input.R       # upload / example loader (ONLY module that WRITES state)
  mod_variable_picker.R  # DT table of columns; returns selected variable reactive
  mod_value_table.R      # frequency table + "create rule from selected rows"
  mod_cluster_view.R     # TWO sub-tabs: "Similarity Clustering" (target +
                         #   exclude + conflict refusal + algorithm guide) and
                         #   "Reference Taxonomy Matcher" (match a column against
                         #   a standard list, then emit recode rules)
                         #   also defines PURE cluster_target_decision() +
                         #   cluster_recode_pairs() and BUILTIN_TAXONOMIES
  mod_spellcheck_view.R  # hunspell flags + clickable suggestions + dictionaries
  mod_text_analysis.R    # long-text columns: length dist, token/n-gram freq, KWIC
  mod_recode_editor.R    # editable DT of rules + Validate
  mod_recategorize.R     # derive a new column from cross-variable term logic
  mod_preview_export.R   # before/after diff + downloads + copy-to-clipboard
  mod_import_recodes.R   # merge an external recodes CSV (conflict resolution)
dictionary/
  seed_terms.txt         # tier 1 (committed, ships EMPTY — no assumed domain)
  custom_terms.txt       # tier 2 (committed, project-shared additions)
  user_terms.txt         # tier 3 (GITIGNORED, per-user additions)
  disciplines/           # optional domain word lists; bundled: medical.txt,
                         #   public_health.txt, public_policy.txt, education.txt
inst/extdata/example_messy.csv
tests/testthat/        # test-string_helpers, -data_loader, -text_helpers,
                       #   -recat_helpers, -cluster_target
```

UI tab order: 1 Data, 2 Variable, 3 Browse values, 4 Clusters, 5 Spellcheck,
6 Text analysis, 7 Recodes, 8 Recategorize, 9 Preview & export. Tab 9 hosts
BOTH `mod_preview_export` and `mod_import_recodes` — import has no tab of its
own. Tab 4 is itself split into two sub-tabs (Similarity Clustering /
Reference Taxonomy Matcher), both served by `mod_cluster_view`.

### State + data flow

- `server()` owns `shared_state <- reactiveValues(data, meta, dataset_name)`.
  **Only `mod_data_input` writes it.** Every other module reads it.
- `rules_proxy <- list(get = reactive, add = fn, set = fn)` is the single
  mutation channel for the in-session rule set (`rv$rules`). Modules that
  create/edit rules take `rules_proxy`. `add()` treats a colliding `rule_id`
  as an update (drops the old, binds the new).
- `recat_proxy` is the SEPARATE, parallel channel for `rv$recat` (keyed on
  `recat_id`). Recategorization rules never pass through `rules_proxy` — the
  two rule sets export to different files and apply in different passes.
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
- NOTE: schema field is `source_dataset` here; the upstream pipeline copy calls it
  `source_quarter`. Don't cross them. `readr` does NOT error on the mismatch —
  it emits a console-only warning and passes the foreign column through, and a
  naive `bind_rows` then yields a 14-column frame with split provenance.
  `mod_import_recodes` guards this: `.check_incoming_recodes()` renames a lone
  `source_quarter`, refuses anything else off-schema, and forces the merged
  frame back to `names(empty_recodes_tibble())`.

### Generated R shape

`generate_recode_R(rules, dataset_id)` emits **one `dplyr::case_when()` block
per `effective_pattern`** — all match types share the block. Single-column
rules → plain `mutate()`; sibling rules →
`mutate(across(matches("<pattern>"), function(.x) case_when(...)))`.

Arm LHS by match type — **both sides are normalized identically**:

| match_type | emitted arm |
|---|---|
| `exact` | `<col> == "<v>"` |
| `exact_ci` | `tolower(<col>) == "<tolower v>"` |
| `trimmed_ci` | `str_squish(tolower(<col>)) == "<squished lower v>"` |
| `regex` | `str_detect(<col>, "<pattern>")` |
| `old_value` is NA | `is.na(<col>)` |
| unusable match_type | `FALSE` |

Always ends each arm set with `.default = <col>` so unmatched values pass
through. Prepends a WARNING comment block when any column is reached by more
than one pattern (those blocks run in sequence and can re-recode).

### The single-pass invariant (do not break this)

`apply_recodes()` and `generate_recode_R()` MUST produce identical output for
the same rules and data. Both are single-pass, first-match-wins:

- `apply_recodes()` snapshots each targeted column into `orig` and carries a
  per-column `claimed` mask. Every rule matches against `orig`, never against
  another rule's output.
- The generator's one-block-per-pattern grouping is what preserves this in the
  script. Grouping by `(pattern, match_type)` instead emits sequential blocks
  where a later block re-recodes an earlier one's output — that was a real bug
  (an `exact` rule silently fully undone) and the two engines disagreed.

`test-string_helpers.R` has an **agreement test** that evaluates the generated
script and compares it cell-for-cell against `apply_recodes()` over a synthetic
frame covering all four match types, sibling expansion, a chained-rule pair and
an NA target. Keep it passing; it is the guard for all of the above.

`apply_recodes()$summary` carries `cells_matched` (cells the rule hit),
`cells_changed` (cells whose value actually differs — a no-op rule reports 0),
and `cells_shadowed` (hit but already claimed by an earlier rule).

### Recategorization (`recat_<dataset>.csv`)

Derives a NEW column from term logic across several existing columns. It never
rewrites a source cell — that is the whole distinction from recoding.

A row matches when, across the rule's resolved columns, **ANY** column contains
**ANY** include term (OR) **AND NONE** contains any exclude term (AND-NOT,
rule-wide). Zero include terms matches nothing (blanket-rule guard). Rules
sharing an `out_col` run in ascending `priority` (NA last, then row order);
first match wins; assigned cells are never overwritten; unmatched stay NA.

Schema: `recat_id`, `out_col`, `category`, `vars` (`;`-joined),
`sibling_pattern`, `include_terms` (`;`-joined), `exclude_terms` (`;`-joined),
`match_type` (`literal`/`regex`), `priority`, `notes`, `author`, `created_at`,
`source_dataset`. Same `<NA>` encoding as recodes. No `updated_at`.

`match_type = literal` is case-insensitive substring
(`str_detect(tolower(x), fixed(tolower(t)))`); `regex` is case-insensitive
`stringr::regex`. Enums single-sourced in `RECAT_MATCH_TYPES`; delimiter in
`RECAT_LIST_SEP`.

**Ordering trap:** recodes run FIRST, recat SECOND. The Recategorize tab's
match-count preview is therefore computed on the RECODED frame (wired via
`recode_basis_r` in `app.R`), not the raw one — previewing on raw values
over-reports. Don't "simplify" that back to the raw frame.

### match_type semantics

`exact` / `exact_ci` / `trimmed_ci` compare for equality after
`normalize_value()`. `regex` matches `old_value` as an unanchored `grepl`
pattern against the RAW value and replaces the whole cell (no partial sub /
backreferences). All matching goes through the `.rule_hits()` helper in
`string_helpers.R` — used by both `apply_recodes()` and the stale check in
`validate_recodes()`, so they can never diverge. Invalid regex patterns are
caught (all-FALSE, no error) and surfaced by `validate_recodes()$invalid_regex`.

`.rule_hits()` must always return a plain logical with **no NA** — it guards
zero-length input, unusable `match_type`, returns `is.na(values)` for an NA
`old_value`, and scrubs residual NAs to FALSE. Callers do `if (any(hit))`, so
a single NA in that vector throws "missing value where TRUE/FALSE needed" and
aborts Apply & Export. Don't remove the scrub.

## Conventions

- R + tidyverse. Native pipe `|>`, never `%>%`.
- 2-space indent.
- `string_helpers.R`, `text_helpers.R`, `recat_helpers.R` and `data_loader.R`
  stay Shiny-free so they're unit-testable and reusable from scripts. Shiny
  calls live only in `mod_*`, `app.R`. (Consequence: a pure helper in those
  files cannot use `%||%`, which lives in `ui_helpers.R`.)
- Module files define `mod_<name>_ui(id)` + `mod_<name>_server(id, ...)`.
- **Markdown links:** Always use standard relative paths (e.g. `[README](README.md)`, `[.agents/constitution.md](.agents/constitution.md)`) in `.md` files. Never commit `file:///` URIs or host-specific absolute paths.

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
  payload, {priority:'event'})`. Multi-field payloads are JSON-encoded and
  parsed with `jsonlite::fromJSON` server-side (see spellcheck
  suggestion/custom-correction handlers).
- **NEVER `escape = FALSE` on a DT holding dataset content.** This app ingests
  arbitrary third-party CSV/Excel, so untrusted input is the PRIMARY path;
  `escape = FALSE` unescapes EVERY column, and a cell containing
  `<img src=x onerror=...>` then executes on render. Use
  `escape = -ncol(df)` with the generated-markup column placed LAST (the
  pattern in `mod_recode_editor`, `mod_spellcheck_view`, `mod_recategorize`),
  or a name-based index. When adding a column to one of those tables, keep the
  action/delete column last or the escape index silently points at user data.
- **Free-text classifier** (`build_meta`) excludes dates, numbers, and small
  choice sets: name must not match `(^|_)id$`, then median length > 3, NA share
  < 60%, > 10 unique values, and not date-like / numeric-like (80% regex
  thresholds). The cheap name check runs FIRST. Don't loosen without re-checking
  the example dataset still classifies drink1/drink2/city/notes/review as text
  and order_id/size/price/order_date as non-candidates.
  Deliberate divergence from upstream: we do NOT exclude `label$`. That was an
  agency coded-value-twin convention, not a general one — in a domain-neutral tool
  `product_label` / `warning_label` is plausibly real free text, and there is no
  manual override yet. Pinned by a test; don't "fix" it.
- **The two fingerprint algorithms are complements, not variants.**
  `.fingerprint_key()` (key collision) sorts the deduped TOKEN set, so it is
  word-order invariant but blind to where the spaces fall.
  `.ngram_fingerprint_key()` strips whitespace entirely and n-grams the WHOLE
  string, so it collapses `Wal Mart`/`WalMart` and `e-mail`/`email` — which key
  collision cannot — but at q >= 2 it does NOT group word-order swaps. At q = 1
  the key degenerates to the distinct-character set (`Krzysztof`/`Kryzysztof`).
  An earlier version n-grammed per token, which made it a fuzzier restatement of
  key collision and contradicted both its own docstring and the in-app guide.
  A test in `test-string_helpers.R` pins the complementary behaviour; don't
  "simplify" it back.
- **`shiny::showNotification(type=)` is `match.arg`'d** against
  `c("default", "message", "warning", "error")`. Anything else (`"information"`,
  `"success"`, `"info"`) throws at runtime, in a handler that may only fire on a
  rare branch. Was a live bug in the taxonomy apply handler.
- **Don't pre-format a numeric DT column with `sprintf()`.** It becomes a
  string and DT then sorts it lexicographically — `"9.0%"` above `"85.0%"` above
  `"100.0%"`. Keep the column numeric and use `DT::formatPercentage()` /
  `formatRound()`. Was a live bug in the taxonomy match table.

## Dictionaries

Hunspell `en_US` + three supplementary tiers (seed/custom/user) + any selected
**discipline** dictionaries from `dictionary/disciplines/*.txt`. A word is OK if
any source accepts it. The Spellcheck tab can select multiple disciplines and
import a new one (uploaded `.txt` is copied into the folder and auto-selected;
the user must `git add`/commit it to share with the team). `medical.txt`,
`public_health.txt`, `public_policy.txt` and `education.txt` ship bundled.
`seed_terms.txt` is intentionally empty — this is a general tool, no assumed
vocabulary.

## Testing

```r
testthat::test_dir("tests/testthat")
```

Run a single file with
`testthat::test_file("tests/testthat/test-string_helpers.R")`.

| file | covers | assertions |
|---|---|---|
| `test-string_helpers.R` | schema, normalize, cluster (incl. fingerprint / n-gram fingerprint), taxonomy matching, CSV round-trip, validate (regex/enum/stale), apply (siblings/delete/regex/NA), codegen, **apply↔codegen agreement**, rule-order independence | 154 |
| `test-recat_helpers.R` | recat match/apply/priority/codegen/IO + the three worked examples end-to-end against the bundled CSV | 144 |
| `test-data_loader.R` | metadata + classifiers (`build_meta`, `column_group`, `suggest_sibling_pattern` incl. metachar escaping, date/numeric/id detection, `read_dataset`) + example-dataset regression | 53 |
| `test-cluster_target.R` | `cluster_target_decision()`, exclusion pair-building, `testServer` wiring for the Apply refusal | 50 |
| `test-text_helpers.R` | sentences, length classification, tokens, n-grams, KWIC | 20 |

421 total. Tests `source()` the pure-R files by relative path — no `mod_*.R`
except `mod_cluster_view.R` (which is Shiny-free at load time because every
`shiny::` call sits inside a function body). CI runs them on every push/PR via
`.github/workflows/tests.yml` (r-lib/actions + DESCRIPTION-driven deps).

CI sets `RENV_CONFIG_AUTOLOADER_ENABLED: "false"` at job level, and it must stay.
`setup-r-dependencies` runs `Rscript` without `--vanilla`, so `.Rprofile` would
activate renv, set `RENV_PROJECT`, and make `.libPaths()[1]` the renv library
(`~/.cache/R/renv/library/<project>-<hash>/...`). The action branches on
`RENV_PROJECT` and installs there, but points `actions/cache` at
`$R_LIBS_USER/*` plus a repo-relative `renv/library` — and this project's renv
library is in neither place. Both cache paths are then empty, so the run logs
"Path(s) specified in the action for caching do(es) not exist", saves nothing,
and rebuilds every package from source (6-9 min instead of ~1). CI resolves deps
from DESCRIPTION by design, so renv has no role there.

**Dependencies:** this repo uses `renv` (renv 1.2.4; `renv.lock` pins
105 packages on R 4.6.1). `.Rprofile` activates renv on session start, so a
bare `Rscript` from the repo root uses the project library — no `R_LIBS`
workaround. On a fresh clone, run `renv::restore()` first. `DESCRIPTION`
(Imports) and the README install line list the direct deps — keep all three in
sync; `ggplot2` and `htmltools` were used for months without being declared,
which silently broke non-renv installs and CI dep resolution. `writexl` is NOT
a dependency — exports are CSV + R script only.

## Environment

- Windows (R 4.6.1+).
- IDE: Positron (restart R with Ctrl+Shift+0, NOT the VS Code "R: Restart R").
- Git default branch: `main`.
- GitHub remote: https://github.com/amelia-m/recode-studio (public, pushed 2026-06-08).

## Open / TODO

- **Language detection** — add an option to detect the language of a text
  column's values (e.g. flag non-English entries, or per-value language tags).
  Most relevant to the Text analysis tab / long-text columns. Candidate
  approaches: `cld2`/`cld3` (compiled, accurate) or `textcat` (pure-R, heavier).
- **Peer tokenization review** — Tokenization/cleaning reference script identified and
  archived at `ext/tokenization_reference.Rmd` (see spec in `docs/superpowers/specs/2026-08-13-text-normalization.md`).
- **Report `escape = FALSE` upstream** — the DT escaping defect fixed
  here also exists in upstream's `mod_recode_editor.R` and `mod_spellcheck_view.R`.
- Optional: `mod_import_recodes`-style import for recat CSVs. `read_recat()`
  exists and is tested, but nothing in the UI calls it.
- Optional: harden `read_recodes()` itself with a required-column check —
  currently the schema guard lives only in `mod_import_recodes`, so other
  callers still get `readr`'s console-only warning.
- Optional: expand the free-text heuristic to let the user override a column's
  candidate flag manually.
- Optional: README screenshot / demo GIF (capture from a running app).
- Optional: `mod_preview_export` labels `cells_changed` as "Cells affected".
  That column now means *cells actually rewritten*; `cells_matched` carries the
  older meaning. Relabel or switch.

### Features deliberately NOT ported from upstream

Don't re-add these without a reason — they are quarter-ordered, network-path,
or agency-coupled and do not generalize: raw-vs-pre-cleaned review basis
(`preclean_for_review`, `rebase_rules_onto_raw`, `prior_quarter_recodes`),
per-variable review-status store, value-level flagging to a shared queue,
"save to network folder" export targets, `recode_script_candidates()` filename
contract, the place-name-seeded 4th dictionary tier, and
`LAUNCH_RECODE_STUDIO.R`. Also: `suggest_sibling_pattern()` here escapes regex
metacharacters and upstream's does not — this repo is AHEAD; don't sync it
backwards.

Note: `match_type` / `action` are constrained — single-sourced in
`RECODE_MATCH_TYPES` / `RECODE_ACTIONS` (string_helpers.R), and recat's in
`RECAT_MATCH_TYPES` (recat_helpers.R). The editor rejects out-of-range edits
live; `validate_recodes()$invalid_enum` catches bad enums in imported CSVs.
