# CLAUDE.md

Guidance for Claude Code (and other AI agents) working in this repo.
IMPORTANT: these instructions override default behavior. Follow them.
All agents must adhere to the **Clanker Constitution** in `.agents/constitution.md` (see `AGENTS.md`).

## Project

**Recode Studio** is a dataset-agnostic R/Shiny app for cleaning messy string
variables (typos, capitalization noise, duplicate tokens, near-duplicate
variants) without writing R by hand. Target user is spreadsheet-fluent but NOT
a fluent R coder.

Load any CSV/Excel → pick a text column → optionally normalize whole columns
(case / whitespace) → browse values alphabetically or as string-similarity
clusters → spellcheck → build recode rules (with optional sibling-column
propagation) → optionally derive new columns via recategorization → export the
rule CSVs + a runnable `dplyr` script, and optionally a zip holding a cleaned
copy of the data.
**The app never modifies the source data file.** It exports rule files, an R
script and, on request, a cleaned COPY of the data as a separate download.

### Origin

Ported and generalized from an earlier data pipeline's "Recode Studio" Shiny
section for a state agency. That version was agency-specific (quarter dropdown
reading merged CSVs, agency column prefixes, a bundled medical dictionary, a
multi-section shell). This repo is the standalone, domain-neutral spin-off. If
you change shared logic here, the upstream copy does NOT auto-update (and
vice versa): they diverged on purpose. What was ported back from upstream, what
was declined, and defects present in both are logged in
[docs/upstream-port-candidates.md](docs/upstream-port-candidates.md).

## Running

```r
# from the repo root
shiny::runApp()
```

On the **Data** tab, "Load bundled example dataset" loads
`inst/extdata/example_messy.csv`, a benign café-orders dataset (30 rows, 9
columns) that exercises every feature: typo clusters (cappuccino/espresso
variants), a `drink1`/`drink2` sibling family, a multi-word `notes` column with
reordered + duplicate-token values (for the normalize/cosine options), a
sentence-length `review` column (for long-text detection / Text analysis), plus
excluded numeric/date/small-choice columns (`order_id`, `price`, `order_date`,
`size`). Deliberately non-sensitive.

Free-text candidates in the example are exactly `drink1`, `drink2`, `city`,
`notes`, `review`. Any classifier change must preserve that: there is a
regression test pinning it in `test-data_loader.R`.

## Architecture

Single-purpose Shiny app (no multi-section shell; that was agency-specific).

```
app.R                    # entry: sources R/, builds ui + server, shinyApp()
R/
  string_helpers.R       # PURE R core (no Shiny). cluster/validate/apply/codegen
                         #   + match_taxonomy() + rule CSV IO + merge_recode_rules()
                         #   + the column-normalization engine (apply, preview,
                         #   validate, IO, norm_spec stamp, codegen blocks)
  text_helpers.R         # PURE R. long-text: tokenize, classify_text_length,
                         #   top_tokens / top_ngrams / kwic, count_sentences
  recat_helpers.R        # PURE R. recategorization: match/apply/codegen/IO
  rule_warnings.R        # PURE R. rules that quietly do less than expected
  export_bundle.R        # PURE R. cleaned-data zip: pipeline, provenance, script
  data_loader.R          # read CSV/Excel + per-column metadata + source_file_meta()
  rule_proxies.R         # make_rules_proxy / make_norms_proxy / make_recat_proxy
                         #   (Shiny only inside the returned closures; testable)
  ui_helpers.R           # %||%, warning_banner(), badge(), help layer
                         #   (help_line / help_btn / guide_note / card_header_help),
                         #   empty_state(), dt_truncate_defs() + css + hint
  mod_data_input.R       # upload / example loader (ONLY module that WRITES state)
  mod_variable_picker.R  # tab-2 DT picker + "Recoding:" banner, synced via
                         #   variable_selection_server()
  mod_normalize.R        # whole-column trim/squish/case rules + before/after preview
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
  mod_preview_export.R   # counts, rule warnings, diff, downloads, cleaned-data zip
  mod_import_recodes.R   # merge external recodes / normalizations CSVs
dictionary/
  seed_terms.txt         # tier 1 (committed, ships EMPTY: no assumed domain)
  custom_terms.txt       # tier 2 (committed, project-shared additions)
  user_terms.txt         # tier 3 (GITIGNORED, per-user additions)
  disciplines/           # optional domain word lists; bundled: medical.txt,
                         #   public_health.txt, public_policy.txt, education.txt
inst/extdata/example_messy.csv
tests/testthat/          # see Testing
```

UI tab order (verify against `app.R`): 1 Data, 2 Variable, 3 Normalize,
4 Browse values, 5 Clusters, 6 Spellcheck, 7 Text analysis, 8 Recodes,
9 Recategorize, 10 Preview & export. Tab 10 hosts BOTH `mod_preview_export`
(including the "Download cleaned data" card) and `mod_import_recodes`; import
has no tab of its own. Tab 5 is itself split into two sub-tabs (Similarity
Clustering / Reference Taxonomy Matcher), both served by `mod_cluster_view`.
The "Recoding:" variable banner sits above the tab set.

### State + data flow

- `server()` owns `shared_state <- reactiveValues(data, meta, dataset_name,
  source_meta)`. **Only `mod_data_input` writes it.** Every other module reads
  it. `source_meta` (`source_file_meta()`: file base name, sheet, size,
  SHA-256, `loaded_at`) is computed at LOAD time because an upload's temp file
  does not outlast the upload; only the export bundle reads it.
- **Variable selection:** `variable_selection_server()` owns one
  `current_var` reactiveVal. The tab-2 table and the banner selectize both sync
  to it, each hop guarded with "only if it differs" so they cannot ping-pong.
  Every per-variable module reads the returned `selected_var_r`.
- Three rule stores in `rv`, each behind a `list(get, add, set)` proxy from
  `R/rule_proxies.R`. Modules never touch `rv` directly.
  - `rules_proxy` (`rv$rules`, recodes). `add()` merges IN PLACE via
    `merge_recode_rules()`: a colliding `rule_id` replaces the row where it
    stands, new ids append. `add()` also stamps each incoming rule's
    `norm_spec` with the normalization in force for its columns. That is the
    ONLY place stamping happens. The Recodes editor and import use `set()`,
    which never stamps, so a rule keeps the stamp it was authored under.
  - `norms_proxy` (`rv$norms`, normalizations). One rule per target column:
    `add()` upserts in place, and also replaces a rule for the same
    `variable` whose sibling choice changed.
  - `recat_proxy` (`rv$recat`, keyed on `recat_id`), in-place merge like
    recodes. Recategorization rules never pass through `rules_proxy`: the rule
    sets export to different files and apply in different passes.
- `normalized_r` (raw data with normalizations applied) and `unique_values_r`
  (reactive `tibble(value, n)` of the selected column AFTER normalization) are
  computed once in `server()` and passed to the value/cluster/spellcheck
  modules. Don't recompute uniques per module.

### Order of operations (do not break this)

**Normalize, then recode, then recategorize**, everywhere: Browse values,
Clusters and Spellcheck show normalized values (so rules are minted against
them); `recode_basis_r` in `app.R` feeds the Recategorize preview the
normalized-then-recoded frame; Preview & export counts recodes on normalized
data; the generated script, `build_cleaned_dataset()` and the bundle script all
run the same three steps. Previewing recat on raw values over-reports. Don't
"simplify" any of these back to the raw frame.

**Exception, deliberate:** Text analysis reads RAW `shared_state$data`. It is
descriptive and should describe the file as delivered.

Consequence: normalizing a column can kill an `exact` or `regex` recode written
against the values as delivered. `norm_spec` + the drift warning exist to catch
that.

### Recode CSV schema (`recodes_master.csv`)

14 columns: `rule_id`, `variable`, `apply_to_siblings` (lgl), `sibling_pattern`
(regex or NA), `match_type` (`exact`/`exact_ci`/`trimmed_ci` default/`regex`),
`old_value`, `new_value`, `action` (`recode`/`delete`), `notes`, `author`,
`created_at`, `updated_at`, `source_dataset`, `norm_spec`.

- Unique key: `(variable, match_type, old_value)`. `rule_id` is a stable
  12-char hash of those three (`recode_rule_id()`). `norm_spec` is NOT part of
  the key.
- `norm_spec`: canonical, human-readable stamp of the normalization covering
  the rule's columns when it was added, e.g. `city=squish;titlecase` or
  `drink1=uppercase|drink2=uppercase`. `""` means no normalization applied;
  NA means never stamped (a file from before the column existed).
- NA encoding: `new_value = NA` round-trips as the literal string `<NA>`
  (reader/writer use `na = "<NA>"`). Empty string stays empty. This survives
  Excel edits. `action = delete` sets cells to NA regardless of `new_value`.
- `read_recodes(path, conform = TRUE)` reads all-character and conforms: every
  schema column present, in order, typed (`apply_to_siblings` coerced), missing
  ones added as NA, unknown extras KEPT after them. Older 13-column files read
  with an NA `norm_spec`. `write_recodes()` emits exactly the 14 schema
  columns. `conform = FALSE` is the raw read the import guard needs.
- NOTE: the provenance field is `source_dataset` here; the upstream pipeline
  copy calls it `source_quarter`. Don't cross them. `mod_import_recodes`
  guards this: `.check_incoming_recodes()` (on the raw read) renames a lone
  `source_quarter`, tolerates a missing `norm_spec` (added as NA and the user
  is told) and keeps it when present, refuses any other missing or unknown
  column, and forces the merged frame back to `names(empty_recodes_tibble())`.
  `merge_incoming_rules()` keeps colliding rules in `cur`'s position.

### Normalizations (`normalizations_master.csv`)

Whole-column text transforms, a separate rule type (not a recode row: a
column-wide transform claims no cells and would collide on `rule_id`). Ops, in
canonical applied order, single-sourced in `NORMALIZE_OPS`: `trim`, `squish`,
`lowercase`, `uppercase`, `titlecase`. At most one case op per rule. Ops
commute, so a rule has no ordering semantics. LOSSY: title case turns McKenzie
into Mckenzie; `normalization_preview()` shows the pairs before a rule is
added.

Schema: `norm_id` (hash of the TARGET, `variable` + `sibling_pattern`, never
the ops), `variable`, `apply_to_siblings`, `sibling_pattern`, `ops`
(`;`-joined), `notes`, `author`, `created_at`, `updated_at`,
`source_dataset`. Blanks are written as empty cells and both `""` and `<NA>`
read back as NA. Imported through the same card as recodes
(`.check_incoming_normalizations()`, keyed on `norm_id`).

These are unrelated to the Clusters tab's "normalize before clustering"
options, which only change how values are grouped and never rewrite data.

### Generated R shape

`generate_recode_R(rules, dataset_id, source_csv_path = NULL, normalizations =
NULL)` emits the normalization blocks FIRST (one `mutate()` per rule, or
`mutate(across(matches(<pattern>, ignore.case = FALSE), ...))` for a family),
then **one `dplyr::case_when()` block per `effective_pattern`**; all match
types share the block. Single-column rules → plain `mutate()`; sibling rules →
`mutate(across(matches("<pattern>", ignore.case = FALSE), function(.x)
case_when(...)))`.

Arm LHS by match type (**both sides are normalized identically**):

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
  where a later block re-recodes an earlier one's output. That was a real bug
  (an `exact` rule silently fully undone) and the two engines disagreed.
- Because row order decides which rule claims a cell, every merge path keeps
  rows IN PLACE: `merge_recode_rules()`, `merge_recat_rules()`, the proxies'
  `add()`, and `merge_incoming_rules()` on import. The older drop-and-append
  moved an edited rule (even a notes-only edit) to the end and could change
  output. Don't reintroduce it.

Guards, keep them passing:

- `test-string_helpers.R` has two **agreement tests** that evaluate the
  generated script and compare it cell-for-cell against the app: one against
  `apply_recodes()` over a synthetic frame covering all four match types,
  sibling expansion, a chained-rule pair and an NA target; one against
  normalize-then-recode.
- `test-export_bundle.R` runs the bundle's `clean_<id>.R` on the original file
  and requires a **byte-identical** CSV to the bundle's cleaned CSV.

`apply_recodes()$summary` carries `cells_matched` (cells the rule hit),
`cells_changed` (cells whose value actually differs; a no-op rule reports 0),
and `cells_shadowed` (hit but already claimed by an earlier rule). Preview &
export shows all three with definitions.

### Rule warnings (`R/rule_warnings.R`)

Plain-language reports, never changes. Kinds: `normalize_no_target`,
`normalize_no_change`, `recode_no_match` (uses `cells_matched`, so a fully
shadowed rule is not reported), `norm_spec_drift` (stamp differs from the
current normalization), `norm_spec_unstamped` (NA stamp on a column that is now
normalized), `recat_terms_recoded_away` (a recat rule matches fewer rows after
the recodes). Shown in the yellow box on Preview & export and written into the
bundle's provenance. `collect_rule_warnings()` applies each step once; Preview
& export and `build_cleaned_dataset()` assemble the same result from
intermediates they already hold (a test pins the equality).

### Cleaned data export bundle (`R/export_bundle.R`)

"Download cleaned data (zip)" on Preview & export. `export_bundle_zip()` builds
everything in a fresh `tempfile()` directory and zips it with `zip::zip()`.
Contents:

- `<id>_cleaned.csv`: normalize → recode → recat. Missing values written as
  EMPTY cells, UTF-8, no BOM (`BUNDLE_CSV_NA`). Consequence: an empty string
  and a missing value are indistinguishable in this file. The rule files keep
  their own `<NA>` convention.
- `normalizations_master.csv`, `recodes_master.csv`, `recat_<id>.csv`: always
  present, even with zero rules.
- `clean_<id>.R`: ONE script that reads the original file the way
  `read_dataset()` does (all text), runs the `generate_recode_R()` body
  (normalize + recode) and the `generate_recat_R()` body verbatim, writes
  `<id>_cleaned_reproduced.csv`, and warns on a SHA-256 mismatch with the
  recorded input.
- `provenance.txt` (for people) and `provenance.json` (for tools): base file
  name, sheet, size, SHA-256, load and export times, app version, rows/columns
  before and after, added columns, rule counts, cells changed, warnings,
  file manifest. Only the base name is ever recorded, never a folder.

Row-count invariant: the pipeline never adds or drops rows.
`build_cleaned_dataset()` stops after any step that changes `nrow`, and
`build_provenance()` stops if cleaned and raw row counts differ.

### Recategorization (`recat_<dataset>.csv`)

Derives a NEW column from term logic across several existing columns. It never
rewrites a source cell; that is the whole distinction from recoding.

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

### match_type semantics

`exact` / `exact_ci` / `trimmed_ci` compare for equality after
`normalize_value()`. `regex` matches `old_value` as an unanchored `grepl`
pattern against the value (after column normalizations) and replaces the whole
cell (no partial sub / backreferences). All matching goes through the
`.rule_hits()` helper in `string_helpers.R`, used by both `apply_recodes()` and
the stale check in `validate_recodes()`, so they can never diverge. Invalid
regex patterns are caught (all-FALSE, no error) and surfaced by
`validate_recodes()$invalid_regex`.

`.rule_hits()` must always return a plain logical with **no NA**: it guards
zero-length input, unusable `match_type`, returns `is.na(values)` for an NA
`old_value`, and scrubs residual NAs to FALSE. Callers do `if (any(hit))`, so
a single NA in that vector throws "missing value where TRUE/FALSE needed" and
aborts Apply & Export. Don't remove the scrub.

`match_type` / `action` are constrained, single-sourced in
`RECODE_MATCH_TYPES` / `RECODE_ACTIONS` (string_helpers.R), and recat's in
`RECAT_MATCH_TYPES` (recat_helpers.R). The editor rejects out-of-range edits
live; `validate_recodes()$invalid_enum` catches bad enums in imported CSVs.

## Conventions

- R + tidyverse. Native pipe `|>`, never `%>%`.
- 2-space indent.
- `string_helpers.R`, `text_helpers.R`, `recat_helpers.R`, `rule_warnings.R`,
  `export_bundle.R` and `data_loader.R` stay Shiny-free so they're
  unit-testable and reusable from scripts. Shiny calls live only in `mod_*`,
  `rule_proxies.R` closures, `ui_helpers.R` and `app.R`. (Consequence: a pure
  helper in those files cannot use `%||%`, which lives in `ui_helpers.R`.)
- Module files define `mod_<name>_ui(id)` + `mod_<name>_server(id, ...)`.
- In-app help uses the three helpers in `ui_helpers.R`, smallest first:
  `help_line()` (one muted line), `help_btn()` / `card_header_help()` (a "?"
  popover in a card header), `guide_note()` (collapsed "how this works" block).
- **No em dashes** (U+2014) in code, comments, docs or UI strings. Use a spaced
  hyphen, comma, colon, or split the sentence. Guard tests enforce this.
- **Markdown links:** Always use standard relative paths (e.g. `[README](README.md)`, `[.agents/constitution.md](.agents/constitution.md)`) in `.md` files. Never commit `file:///` URIs or host-specific absolute paths.
- **Editor spellcheck is `cspell.json` + `.cspell/example-typos.txt`.** Run it
  with `npx cspell lint "**/*.{R,Rmd,md,json}"`; keep it clean. Four
  exclusions are deliberate, don't "tidy" them away:
  - `inst/extdata/*.csv` is ignored wholesale. Its misspellings ARE the fixture
    for the classifier regression test - a spellcheck "fix" there breaks
    `test-data_loader.R`.
  - `.cspell/example-typos.txt` holds the deliberate misspellings that appear
    in docstrings, the README worked examples and the test fixtures. Read its
    header before touching it; several entries are load-bearing.
  - `ext/**` and `docs/superpowers/**` are archived third-party reference and
    plan docs. Their vocabulary is not ours to allowlist.
  - `dictionary/disciplines/*.txt` are wired in via an `overrides` block scoped
    to `dictionary/**`, NOT enabled repo-wide. They exist so those word lists
    don't self-flag; enabling them globally would silently accept ~400
    clinical terms in R source. They are runtime data dictionaries for the
    app's hunspell tiers, not source vocabulary - different job.

  `addWords` is `false` on every definition so the editor's "Add word" action
  can't write into the app's runtime dictionaries; those are populated only by
  the app's own "Add to dictionary -> Project" button.

## Gotchas (learned the hard way, don't reintroduce)

- **`case_when`, not `case_match`.** `dplyr::case_match()` is deprecated as of
  dplyr 1.2.0 and prints a warning at runtime. The generator uses `case_when`.
- **tidyselect `matches()` ignores case by default.** `apply_recodes()` and
  `apply_normalizations()` resolve sibling patterns with case-sensitive
  `grep()`, so every generated `matches()` passes `ignore.case = FALSE`.
  Without it `^drink[0-9]+$` also rewrote a `Drink3` column in the script only.
- **`isTRUE()` is NOT vectorised.** In `generate_recode_R()` guard
  `apply_to_siblings` with `!is.na(x) & x` over a vector, not `isTRUE(x)`.
  In `apply_recodes()` / `validate_recodes()` the loop is row-at-a-time so
  `isTRUE()` is technically safe, but use `!is.na(x) && x` there too to
  match the documented pattern and survive future vectorization.
- **Sibling regex escaping uses stringr (ICU), not base `gsub` (TRE).** An
  earlier base-`gsub` char class with `{}` threw "Invalid contents of {}" in the
  TRE engine and broke the clusters tab. `suggest_sibling_pattern()` now escapes
  via `stringr::str_replace_all`. Keep it that way.
- **`zip::zip()`, never `utils::zip()`.** `utils::zip()` shells out to an
  external zip program, which exists on a developer machine only because Rtools
  is installed. End users don't have one.
- **Don't name a local helper after a dplyr export used inside a data mask.**
  A helper called inside a dplyr verb is shadowed by a dplyr function of the
  same name (`pick`, `across`, `where`, ...). Upstream hit this as a render
  failure; `preview_diff_table()` uses base subsetting for that reason.
- **bslib cards must be `fill = FALSE` + `card_body(fillable = FALSE)`** here.
  Default flex-fill collapses stacked cards / overlaps DT + inputs (hit this on
  the clusters and spellcheck tabs). Every card in this app sets both.
- **Empty states are plain text, not a one-column datatable.** Use
  `empty_state()` in a `uiOutput` and return NULL from `renderDT` in the same
  state. A `datatable(tibble(message = ...))` draws a literal "message" header
  with a sort arrow.
- **`.shiny-input-container` is 300px wide** (shiny.css). A hand-built
  radio/checkbox group wrapping a table needs an inline `style = "width:100%"`
  on the wrapper, or `width: 100%` inside it means 100% of 300px (see
  `cluster_member_table()`).
- **DT column widths jump on sort** unless the table has
  `table-layout: fixed` with declared widths; `autoWidth = FALSE` alone does
  not stop the browser laying out from the current page (see
  `mod_value_table.R`). Long cells use `dt_truncate_defs()`.
- **An update sent to an input `renderUI` hasn't created yet is dropped, not
  queued.** That is why the "Recoding:" banner selectize is built statically.
  The same applies to a DT proxy command sent to a table not yet drawn, which
  is why the tab-2 picker holds a pending selection until the table confirms it.
- **Inline DT action buttons** use raw HTML + `Shiny.setInputValue('<ns-id>',
  payload, {priority:'event'})`. Multi-field payloads are JSON-encoded and
  parsed with `jsonlite::fromJSON` server-side (see spellcheck
  suggestion/custom-correction handlers).
- **NEVER `escape = FALSE` on a DT holding dataset content.** This app ingests
  arbitrary third-party CSV/Excel, so untrusted input is the PRIMARY path;
  `escape = FALSE` unescapes EVERY column, and a cell containing
  `<img src=x onerror=...>` then executes on render. Use
  `escape = -ncol(df)` with the generated-markup column placed LAST (the
  pattern in `mod_recode_editor`, `mod_spellcheck_view`, `mod_recategorize`,
  `mod_normalize`), or a name-based index. When adding a column to one of
  those tables, keep the action/delete column last or the escape index
  silently points at user data.
- **Free-text classifier** (`build_meta`) excludes dates, numbers, and small
  choice sets: name must not match `(^|_)id$`, then median length > 3, NA share
  < 60%, > 10 unique values, and not date-like / numeric-like (80% regex
  thresholds). The cheap name check runs FIRST. Don't loosen without re-checking
  the example dataset still classifies drink1/drink2/city/notes/review as text
  and order_id/size/price/order_date as non-candidates.
  Deliberate divergence from upstream: we do NOT exclude `label$`. That was an
  agency coded-value-twin convention, not a general one: in a domain-neutral
  tool `product_label` / `warning_label` is plausibly real free text, and there
  is no manual override yet. Pinned by a test; don't "fix" it.
- **The two fingerprint algorithms are complements, not variants.**
  `.fingerprint_key()` (key collision) sorts the deduped TOKEN set, so it is
  word-order invariant but blind to where the spaces fall.
  `.ngram_fingerprint_key()` strips whitespace entirely and n-grams the WHOLE
  string, so it collapses `Wal Mart`/`WalMart` and `e-mail`/`email` (which key
  collision cannot) but at q >= 2 it does NOT group word-order swaps. At q = 1
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
  string and DT then sorts it lexicographically (`"9.0%"` above `"85.0%"` above
  `"100.0%"`). Keep the column numeric and use `DT::formatPercentage()` /
  `formatRound()`. Was a live bug in the taxonomy match table.
- **Hunspell `.dic` artifacts are stripped by the dictionary loader.**
  `.read_dict_terms()` drops affix flags after `/`, the numeric count line and
  multi-word lines, because the Spellcheck tab imports any `.txt`.
- **Known-harmless console noise**, don't chase it: httpuv
  `wsconn_address ... not found` on session close; a jsonlite `keep_vec_names`
  notice triggered by DT; bslib's bootstrap JS copy failing with
  "Permission denied" when antivirus holds the file.

## Dictionaries

Hunspell `en_US` + three supplementary tiers (seed/custom/user) + any selected
**discipline** dictionaries from `dictionary/disciplines/*.txt`. A word is OK if
any source accepts it. The Spellcheck tab can select multiple disciplines and
import a new one (uploaded `.txt` is copied into the folder and auto-selected;
the user must `git add`/commit it to share with the team). `medical.txt`,
`public_health.txt`, `public_policy.txt` and `education.txt` ship bundled.
`seed_terms.txt` is intentionally empty: this is a general tool, no assumed
vocabulary.

## Testing

```r
testthat::test_dir("tests/testthat")
```

Run a single file with
`testthat::test_file("tests/testthat/test-string_helpers.R")`.

| file | covers | assertions |
|---|---|---|
| `test-string_helpers.R` | schema, normalize_value, cluster (incl. fingerprint / n-gram fingerprint), taxonomy matching, CSV round-trip + conform-on-read, merge in place, `.rule_hits` truth table, validate (regex/enum/stale), apply (siblings/delete/regex/NA), codegen incl. `ignore.case = FALSE`, **both agreement tests**, rule-order independence | 233 |
| `test-normalizations.R` | normalization ops, id, apply, preview, validate, CSV IO (incl. 13-column recodes files), `norm_spec` stamp (on rules proxy `add()` only), codegen, norms proxy upsert, Normalize tab | 166 |
| `test-recat_helpers.R` | recat match/apply/priority/codegen/IO + the three worked examples end-to-end against the bundled CSV | 148 |
| `test-export_bundle.R` | pipeline order, row-count stop, `source_file_meta`, provenance, files written, empty-cell NA round-trip, **script reproduces a byte-identical CSV**, SHA-256 warning, `zip::zip`, download wiring | 130 |
| `test-rule_warnings.R` | every warning kind, `collect_rule_warnings()` | 81 |
| `test-cluster_target.R` | `cluster_target_decision()`, exclusion pair-building, single-table cluster cards, `testServer` wiring for the Apply refusal | 72 |
| `test-import_recodes.R` | import schema guard (`source_quarter` rename, optional `norm_spec`, refusals), in-place `merge_incoming_rules()`, normalizations import | 59 |
| `test-data_loader.R` | metadata + classifiers (`build_meta`, `column_group`, `suggest_sibling_pattern` incl. metachar escaping, date/numeric/id detection, `read_dataset`) + example-dataset regression | 53 |
| `test-dt_truncate.R` | `dt_truncate_defs()` / css / hint | 40 |
| `test-recategorize_module.R` | Recategorize preview on the recoded frame, picker seeding | 29 |
| `test-variable_picker.R` | banner choices, `current_var` two-way sync without ping-pong, filter lifted for a hidden column | 29 |
| `test-preview_export.R` | `preview_diff_table()` counts, incl. rules with no data loaded | 26 |
| `test-guide_helpers.R` | help layer: accessibility attributes, collapsed by default, escaping | 24 |
| `test-preview_rule_warnings.R` | "did less than expected" panel and its server wiring | 24 |
| `test-text_helpers.R` | sentences, length classification, tokens, n-grams, KWIC | 20 |
| `test-app_wiring.R` | rules / recat proxies: `add()` merges in place and conforms partial rows, `set()` replaces | 16 |
| `test-spellcheck_dict.R` | `.read_dict_terms()` strips `.dic` artifacts | 11 |
| `test-em_dash_guard.R` | no em dash or em dash entity in any project text file (skips `renv/`, `ext/`, `inst/extdata/`) | 6 |
| `test-all-files-parse.R` | every R file in the project parses | 2 |

1169 total.

Tests `source()` the R files they need by relative path. Pure files are always
safe; the `mod_*.R` files that tests source (cluster view, data input, import,
normalize, preview & export, recategorize, spellcheck, variable picker) are
Shiny-free at load time because every `shiny::` call sits inside a function
body, and their server wiring is exercised with `shiny::testServer()`. Keep
new modules that way. CI runs the suite on every push/PR via
`.github/workflows/tests.yml` (r-lib/actions + DESCRIPTION-driven deps).

CI sets `RENV_CONFIG_AUTOLOADER_ENABLED: "false"` at job level, and it must stay.
`setup-r-dependencies` runs `Rscript` without `--vanilla`, so `.Rprofile` would
activate renv, set `RENV_PROJECT`, and make `.libPaths()[1]` the renv library
(`~/.cache/R/renv/library/<project>-<hash>/...`). The action branches on
`RENV_PROJECT` and installs there, but points `actions/cache` at
`$R_LIBS_USER/*` plus a repo-relative `renv/library`, and this project's renv
library is in neither place. Both cache paths are then empty, so the run logs
"Path(s) specified in the action for caching do(es) not exist", saves nothing,
and rebuilds every package from source (6-9 min instead of ~1). CI resolves deps
from DESCRIPTION by design, so renv has no role there.

**Dependencies:** this repo uses `renv` (renv 1.2.4; `renv.lock` pins
106 packages on R 4.6.1). `.Rprofile` activates renv on session start, so a
bare `Rscript` from the repo root uses the project library, no `R_LIBS`
workaround. On a fresh clone, run `renv::restore()` first. `DESCRIPTION`
(Imports) and the README install line list the direct deps: keep all three in
sync. `ggplot2` and `htmltools` were used for months without being declared,
which silently broke non-renv installs and CI dep resolution. `digest`
(SHA-256 of the loaded file) and `zip` (the bundle) are direct dependencies.
`writexl` is NOT a dependency: every export is CSV, R or text.

## Environment

- Windows (R 4.6.1+).
- IDE: Positron (restart R with Ctrl+Shift+0, NOT the VS Code "R: Restart R").
- Git default branch: `main`.
- GitHub remote: https://github.com/amelia-m/recode-studio (public, pushed 2026-06-08).

## Open / TODO

- **Language detection:** add an option to detect the language of a text
  column's values (e.g. flag non-English entries, or per-value language tags).
  Most relevant to the Text analysis tab / long-text columns. Candidate
  approaches: `cld2`/`cld3` (compiled, accurate) or `textcat` (pure-R, heavier).
- **Peer tokenization review:** a tokenization/cleaning reference script is
  archived at `ext/tokenization_reference.Rmd` (see the spec in
  `docs/superpowers/specs/2026-08-13-text-normalization.md`).
- **Report `escape = FALSE` upstream:** the DT escaping defect fixed here still
  exists in four upstream tables (recode editor, spellcheck, recategorize,
  normalize).
- Optional: `mod_import_recodes`-style import for recat CSVs. `read_recat()`
  exists and is tested, but nothing in the UI calls it.
- Optional: expand the free-text heuristic to let the user override a column's
  candidate flag manually.
- Optional: README screenshot / demo GIF (capture from a running app).
- Defects present in both this repo and upstream, loose ends from the
  September sync, and open decisions are tracked in
  [docs/upstream-port-candidates.md](docs/upstream-port-candidates.md)
  (sections 4 and 6). Don't duplicate them here.

### Features deliberately NOT ported from upstream

Don't re-add these without a reason. They are quarter-ordered, network-path,
or agency-coupled and do not generalize: raw-vs-pre-cleaned review basis
(`preclean_for_review`, `rebase_rules_onto_raw`, `prior_quarter_recodes`),
per-variable review-status store, value-level flagging to a shared queue,
"save to network folder" export targets, `recode_script_candidates()` filename
contract, the place-name-seeded 4th dictionary tier, and
`LAUNCH_RECODE_STUDIO.R`.

Also declined in the September sync: upstream's layout rework (numbered left
rail, collapsible settings, warnings sidebar; skipped for now, not rejected),
its rename of `recodes_master.csv` to `recode_rules.csv` (churn, no behaviour
change), and its removal of the per-user dictionary tier (fits a shared team
review, not a general tool).

`suggest_sibling_pattern()` here escapes regex metacharacters and upstream's
does not: this repo is AHEAD; don't sync it backwards.
