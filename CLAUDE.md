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
section, built for one organization's recurring data deliveries. That version
was tied to its own setting (a reporting-period dropdown reading merged CSVs,
source-specific column prefixes, a bundled medical dictionary, a multi-section
shell). This repo is the standalone, domain-neutral spin-off. If
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
                         #   + regex_compiles(), clean_sibling_pattern(),
                         #   recode_key_id()
  text_helpers.R         # PURE R. long-text: tokenize, classify_text_length,
                         #   top_tokens / top_ngrams / kwic, count_sentences,
                         #   detect_languages() / language_summary() (cld2)
  stopword_helpers.R     # PURE R. stopword tiers: active_stopwords(),
                         #   add_stopword() (verified write)
  recat_helpers.R        # PURE R. recategorization: match/apply/validate/
                         #   codegen/IO
  rule_warnings.R        # PURE R. rules that quietly do less than expected,
                         #   each with a severity (warning / note)
  export_bundle.R        # PURE R. cleaned-data zip: pipeline, provenance, script
  data_loader.R          # read CSV/Excel + per-column metadata + source_file_meta()
  rule_proxies.R         # make_rules_proxy / make_norms_proxy / make_recat_proxy
                         #   (Shiny only inside the returned closures; testable)
  ui_helpers.R           # %||%, warning_banner(), badge(), help layer
                         #   (help_line / help_btn / guide_note / card_header_help),
                         #   empty_state(), dt_truncate_defs() + css + hint,
                         #   issue_class_title() (readable fallback title for
                         #   an unknown validation class, never an error)
  mod_data_input.R       # upload / example loader (ONLY module that WRITES state)
  mod_variable_picker.R  # tab-2 DT picker + "Recoding:" banner, synced via
                         #   variable_selection_server()
  mod_normalize.R        # whole-column trim/squish/case rules + before/after preview
  mod_value_table.R      # frequency table + "create rule from selected rows"
                         #   also PURE value_frequency_table() (feeds
                         #   unique_values_r) + dt_empty_value_defs()/_css()
  mod_cluster_view.R     # TWO sub-tabs: "Similarity Clustering" (target +
                         #   exclude + conflict refusal + algorithm guide) and
                         #   "Reference Taxonomy Matcher" (match a column
                         #   against a standard list: three in
                         #   BUILTIN_TAXONOMIES, or an uploaded one, then
                         #   emit recode rules)
                         #   also defines PURE cluster_target_decision() +
                         #   cluster_recode_pairs(), cluster_drop_blank(),
                         #   cluster_cap_values(), cluster_cards_to_show(),
                         #   PAIRWISE_CLUSTER_CAP / MAX_CLUSTER_CARDS and
                         #   BUILTIN_TAXONOMIES
  mod_spellcheck_view.R  # hunspell flags + clickable suggestions + dictionaries
                         #   (.js_value()/.attr_value() action-cell escaping,
                         #   .copy_file_verified(), .spell_drop_blank(),
                         #   PURE spell_replace_token() + shared tokenizer
                         #   .spell_tokens(), .hunspell_loader())
  mod_text_analysis.R    # long-text columns: length dist, token/n-gram freq
                         #   (stopword tiers, "+ Stop"), word cloud, Language,
                         #   KWIC
  mod_recode_editor.R    # editable DT of rules + Validate; PURE
                         #   apply_recode_edit() handles one cell edit,
                         #   PURE recode_validation_ui() renders Validate
  mod_recategorize.R     # derive a new column from cross-variable term logic;
                         #   PURE recat_add_refusal() + recat_issues_ui()
  mod_preview_export.R   # counts, rule warnings + notes, diff, downloads,
                         #   cleaned-data zip
  mod_import_recodes.R   # merge external recodes / normalizations CSVs;
                         #   PURE import_recodes_summary() + preview UI
dictionary/
  seed_terms.txt         # tier 1 (committed, ships EMPTY: no assumed domain)
  custom_terms.txt       # tier 2 (committed, project-shared additions)
  user_terms.txt         # tier 3 (GITIGNORED, per-user additions)
  disciplines/           # optional domain word lists; bundled: medical.txt,
                         #   public_health.txt, public_policy.txt, education.txt
stopwords/               # Text analysis stopword tiers (see Stopwords)
  base_stopwords.txt     # base (committed, seeded from EN_STOPWORDS)
  custom_stopwords.txt   # project (committed, shared additions)
  user_stopwords.txt     # user (GITIGNORED, per-user additions)
inst/extdata/
  example_messy.csv      # the Data tab's "load bundled example" (30 rows)
  example_survey.csv     # survey-shaped fixture, used by the vignettes only
                         #   (25 rows: respondent_id, department,
                         #   feedback_theme1/2, training_need,
                         #   experience_rating, submission_date,
                         #   detailed_comments)
vignettes/               # getting-started, long-text-analysis,
                         #   recategorization (knitr; DESCRIPTION declares
                         #   VignetteBuilder). They run real code against the
                         #   bundled CSVs, so a behaviour change can break
                         #   them. Nothing in the test suite covers them.
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
  modules. Don't recompute uniques per module. `unique_values_r` is built by
  `value_frequency_table()` (`R/mod_value_table.R`): it KEEPS the empty
  string `""` (so Browse values can mint a rule for empty cells) and drops NA.
  Clusters, the taxonomy matcher and Spellcheck drop blanks themselves
  (`cluster_drop_blank()`, `.spell_drop_blank()`).
- **Loaded blanks are NA, not `""`.** `read_dataset()` overrides neither
  reader's `na` nor `trim_ws`, so their own defaults decide, and the two
  readers do NOT agree.
  readr's are `na = c("", "NA")` and `trim_ws = TRUE`, so empty, quoted-blank
  and whitespace-only CSV cells arrive as NA, and so does a cell holding the
  literal text `NA`.
  readxl's are `na = ""` and `trim_ws = TRUE`, so blank and whitespace-only
  Excel cells also arrive as NA, but a cell holding the literal text `NA`
  arrives as the STRING `"NA"`.
  Either way a loaded file never contains `""`; the `""` handling matters mainly
  for values a normalization squishes to `""` and data arriving other ways.
  NA cells cannot currently be targeted from Browse values (open question in
  `docs/upstream-port-candidates.md`).

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
- **Import finds recode conflicts by KEY, not stored id.** A stored `rule_id`
  can be stale (saved before a key edit, or by another tool), so the recodes
  import passes `key_fn = recode_key_id` to `merge_incoming_rules()`, and
  `import_recodes_summary()` counts conflicts the same way (including how many
  carry a different id on each side). The winner keeps the id it carries; ids
  are never rewritten (Validate's `id_mismatch` lists them). An incoming
  sibling rule whose pattern does not compile is accepted and listed as "will
  not apply". Normalizations import still merges on `norm_id`.

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
types share the block. The emitted SHAPE follows the pattern, not the sibling
flag: a group whose pattern names exactly one syntactic column (a plain rule's
escaped `^<name>$`, or a sibling pattern the user typed that happens to be
`^<syntactic name>$`) becomes a single-column `mutate()` with the column
written by `.r_col_token()`; every other group becomes
`mutate(across(matches("<pattern>", ignore.case = FALSE), function(.x)
case_when(...)))`.

A PLAIN (non-sibling) rule's `effective_pattern` is `^<name>$` with the name
regex-escaped by `.escape_regex()` (stringr/ICU), so it groups and
overlap-checks against exactly its own column (`q1.a` no longer reaches
`q1_a`, and the overlap WARNING no longer false-fires for that pair). The
block is then emitted as a single-column `mutate()` with the column written by
`.r_col_token()`, backticked when non-syntactic (`price ($)`, `my col`), so no
regex engine is involved in reaching it. Sibling patterns are the user's own
regexes and are used as typed, after `clean_sibling_pattern()` (a blank
pattern is NA, i.e. no pattern). `.is_plain_rule()` is the vectorised test.

A sibling pattern that does not compile (`regex_compiles()`) gets NO block:
the generator emits a `# SKIPPED: ...` comment naming the pattern and rule
ids, matching the app, where such a rule reaches no column. The overlap check
ignores skipped patterns. Normalization blocks and recat arms do the same
(`# SKIPPED` comment instead of code).

Arm LHS by match type (**both sides are normalized identically**):

| match_type | emitted arm |
|---|---|
| `exact` | `<col> == "<v>"` |
| `exact_ci` | `tolower(<col>) == "<tolower v>"` |
| `trimmed_ci` | `str_squish(tolower(<col>)) == "<squished lower v>"` |
| `regex` | `grepl("<pattern>", <col>)` |
| `old_value` is NA | `is.na(<col>)` |
| unusable match_type, or `regex` that is `""` or does not compile | `rep_len(FALSE, length(<col>))` |

The rows are the match-type cases, not a precedence list.
The code tests in this order, in `generate_recode_R()` and `.rule_hits()`
alike: unusable `match_type` first (never-matching arm), then an NA
`old_value` (`is.na()`, which therefore wins over `regex`), then a `regex`
pattern that is `""` or does not compile (never-matching arm), then `regex`,
then the three equality forms.

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
  sibling expansion, a chained-rule pair, an NA target and awkward column
  names (`q1.a` beside `q1_a`, `price ($)`, `my col`); one against
  normalize-then-recode.
- `test-export_bundle.R` runs the bundle's `clean_<id>.R` on the original file
  and requires a **byte-identical** CSV to the bundle's cleaned CSV,
  including a frame with those awkward column names.

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
the recodes). A `recode_no_match` for a rule with an invalid sibling pattern
says the rule was skipped for that reason. `collect_rule_warnings()` applies
each step once; Preview & export and `build_cleaned_dataset()` assemble the
same result from intermediates they already hold (a test pins the equality).

**Severity.** Every row has `severity`, `"warning"` or `"note"`
(`RULE_WARNING_SEVERITIES`). Only the two `norm_spec_*` kinds can be a note:
when `.stamp_change_is_neutral()` finds that every op differing between the
stamped and current normalization is one the rule's match type already
ignores (`.NEUTRALIZED_OPS`: `trimmed_ci` ignores trim, squish and case;
`exact_ci` ignores case; `exact` and `regex` ignore nothing), or the rule
targets NA cells. An NA stamp is neutral only if the match type neutralizes
every op. Preview & export puts warnings in the yellow box and notes in a
collapsed `guide_note()` block that does not replace the all-clear line
(`rule_warnings_ui()`; a missing or unknown severity is treated as a warning).
Provenance lists notes after warnings, marked `(note)`, and the JSON carries
`severity`.

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
`stringr::regex` (ICU, in the app AND the script, so terms agree; a term that
does not compile matches nothing and is emitted as `FALSE`). Sibling patterns
resolve with base `grep()` (TRE), like recodes. Enums single-sourced in
`RECAT_MATCH_TYPES`; delimiter in `RECAT_LIST_SEP`.

`.recat_match_type()` reads the type ignoring case and surrounding spaces
(`" Regex "` is `regex`); NULL, NA and blank mean `literal`; anything else is
UNRECOGNISED (NA) and the rule matches nothing. `.recat_skip_reason()` names
why a rule is skipped (bad match type or a sibling pattern that does not
compile); a skipped rule resolves to no column and gets a `# SKIPPED` comment
instead of an arm. `recat_helpers.R` must stay sourceable alone, so it keeps
private mirrors of `clean_sibling_pattern()` / `regex_compiles()`
(`.recat_clean_pattern()`, `.recat_tre_compiles()`); keep them in step.

`validate_recat(rules, data = NULL)` classes, each a tibble of `recat_id`,
`out_col`, `category`, `value`, `detail`: `invalid_match_type`,
`invalid_regex` (ICU), `no_include_terms`, `invalid_sibling_pattern`,
`duplicate_ids`, and with `data`: `missing_columns`, `no_columns`. The
Recategorize tab shows them in `recat_issues_ui()`, and Add refuses a rule
carrying any of the first four (`recat_add_refusal()`, `.RECAT_REFUSE_CLASSES`),
since as written it would match nothing.

### match_type semantics

`exact` / `exact_ci` / `trimmed_ci` compare for equality after
`normalize_value()`. `regex` matches `old_value` as an unanchored `grepl`
pattern against the value (after column normalizations) and replaces the whole
cell (no partial sub / backreferences). All matching goes through the
`.rule_hits()` helper in `string_helpers.R`, used by both `apply_recodes()` and
the stale check in `validate_recodes()`, so they can never diverge. Invalid
regex patterns are caught (all-FALSE, no error) and surfaced by
`validate_recodes()$invalid_regex`.

**One regex engine: base R's default (TRE), everywhere recodes and
normalizations touch a regex.** The app matches `regex` rules with `grepl()`
and resolves sibling patterns with `grep()`; the script emits `grepl()` arms
and tidyselect `matches()`, which is itself base `grep()` with `perl = FALSE`.
`regex_compiles()` is the single "is this usable" test for validation, codegen
and UI entry. (The script used to emit `str_detect()`, ICU, and patterns like
`\p{Lu}` or `a{,2}` disagreed; fixed.) The exception is recat regex TERMS,
which are ICU on both sides (see Recategorization).

Sibling patterns: `clean_sibling_pattern()` turns `""` / whitespace-only into
NA (no pattern: the rule targets its own `variable`), on read and wherever a
pattern is resolved. A non-blank pattern that does not compile makes the rule
reach NO column in the app and no block in the script; it never throws.

Empty `old_value` (`""`): under `exact` and `exact_ci` it matches only empty
cells; under `trimmed_ci` it also matches whitespace-only cells (they squish
to `""`). Under `regex` an EMPTY pattern matches NOTHING, in the app and the
script (base `grepl("")` would hit every non-missing cell), and
`validate_recodes()$invalid_regex` reports it.
Use `^$` to target empty cells with a regex. A rule minted from Browse values'
"(empty)" row starts with a blank `new_value`, which Validate flags until the
user fills it in or sets `action = delete`.

`.rule_hits()` must always return a plain logical with **no NA**: it guards
zero-length input, unusable `match_type`, returns `is.na(values)` for an NA
`old_value`, and scrubs residual NAs to FALSE. Callers do `if (any(hit))`, so
a single NA in that vector throws "missing value where TRUE/FALSE needed" and
aborts Apply & Export. Don't remove the scrub.

`match_type` / `action` are constrained, single-sourced in
`RECODE_MATCH_TYPES` / `RECODE_ACTIONS` (string_helpers.R), and recat's in
`RECAT_MATCH_TYPES` (recat_helpers.R). The editor rejects out-of-range edits
live; `validate_recodes()$invalid_enum` catches bad enums in imported CSVs.

### Validate classes

All validators report and never change a rule.

- `validate_recodes(rules, data = NULL)`: `duplicate_keys`, `invalid_enum`,
  `invalid_regex` (does not compile, or empty), `invalid_sibling_pattern`
  (rule is skipped), `id_mismatch` (stored `rule_id` differs from
  `recode_key_id()`; ids are NOT rewritten), `spellcheck_lost_words` (notes
  start with "Spellcheck" and `new_value` has fewer words than `old_value`:
  made by the old Spellcheck that replaced the whole value with the word),
  `blank_new_value`, `rule_chains`, and with `data`: `stale`.
- `validate_normalizations(rules, data = NULL)`: `no_ops`, `invalid_ops`,
  `conflicting_case`, `duplicate_targets`, `invalid_sibling_pattern`, and with
  `data`: `missing_column`, `overlapping_targets`.
- `validate_recat()`: see Recategorization.

Renderers: `recode_validation_ui()` (Recodes Validate button),
`normalization_issues_ui()` (Normalize), `recat_issues_ui()` (Recategorize).
Each looks titles up through `issue_class_title()`, so a class added to a
validator later still renders under a readable fallback title instead of
crashing the panel ("subscript out of bounds" on `titles[[nm]]` was a real
crash on the Normalize tab).

### Recodes editor (`R/mod_recode_editor.R`)

Every cell edit goes through the pure `apply_recode_edit()`, which returns
`status` `ok` / `refused` / `ignored`:

- An edit to `old_value` or `match_type` RECOMPUTES `rule_id`. If the new id
  belongs to ANOTHER rule the edit is refused with an error notification
  naming that rule, and the rule set is unchanged. (A stale id used to let a
  later rule for the original value silently overwrite the edited one in
  `add()`.)
- Invalid `match_type` / `action` values are refused the same way.
- A `sibling_pattern` edit goes through `clean_sibling_pattern()` (a cleared
  cell is stored as NA, never `""`), and a pattern that does not compile is
  refused. Normalize and Recategorize refuse a bad typed pattern at entry too.
- A rule made by Spellcheck (`.is_spellcheck_rule()`: notes start with
  "Spellcheck") cannot be switched to `regex`: its `old_value` is literal text
  and a regex would replace every matching cell whole.
- Saves use `set()`, never `add()`, so `norm_spec` is not restamped. That is
  correct for key edits: the stamp describes the rule's COLUMNS, which an
  `old_value` / `match_type` edit does not change. Editing
  `apply_to_siblings` / `sibling_pattern` DOES change the columns but keeps
  the old stamp, which can surface as a `norm_spec_drift` warning (errs
  toward warning).
- A refused edit is reverted in the browser with `DT::replaceData()` (see
  Gotchas). `""` in `old_value` / `new_value` shows as "(empty)" via
  `dt_empty_value_defs()`.

### Clusters and Recategorize preview

- Clustering settings (algorithm, threshold, q, normalizers) are debounced
  500 ms, and `clusters_r()` uses `bindCache(cluster_input_r(), cache =
  "session")`; the threshold is blanked out of the key for non-pairwise
  algorithms. Session-scoped on purpose: one user's values never sit in a
  cache another session can hit.
- Pairwise algorithms (`jw`, `osa`, `lv`, `lcs`, `cosine`, `jaccard`) cluster
  at most `PAIRWISE_CLUSTER_CAP = 1000` distinct values (most frequent kept),
  with a `help_line()` pointing to key collision, n-gram fingerprint, soundex
  and metaphone, which are linear and never capped. Measured once in September
  2026 on the development laptop, not re-measured since: 1000 values about 1 s
  (edit-distance) to 2.5 s (cosine/jaccard); 2000 about 3 s to 8 s. The cap
  counts VALUES, so long values can cost far more than these figures suggest.
- At most `MAX_CLUSTER_CARDS = 50` cards, largest first, with a "Showing N of
  M" line. The server takes `debounce_ms`, `pairwise_cap`, `max_cards`
  arguments so tests can use small values.
- Blank values (NA, `""`, whitespace-only) are dropped before clustering, the
  taxonomy matcher and Spellcheck.
- The Recategorize preview is a snapshot. It records the form (parsed terms,
  not raw text; notes excluded) when computed, and while the form differs the
  result is greyed out under an "Out of date" line; the next Preview clears
  it. It does NOT track later recode-rule changes.

### Spellcheck (`R/mod_spellcheck_view.R`)

- The flagger and the correction share one tokenizer, `.spell_tokens()`
  (`SPELL_TOKEN_RUN`: lowercase runs of ASCII letters and apostrophes).
  Single-character tokens are dropped before checking (`nchar(toks) > 1`), and
  only the FIRST unrecognized token per value is flagged.
- A suggestion (or the "Other" box) makes a rule that replaces ONLY the
  flagged word: `spell_replace_token()` replaces every whole-word,
  case-insensitive occurrence, keeps surrounding quote marks, and mirrors ALL
  CAPS / Title Case from the original. A correction that leaves the value
  unchanged creates no rule. Notes read `Spellcheck: token 'x' -> 'y'`.
- The rule is a whole-cell `exact` rule on the value as displayed (so a
  differently cased copy elsewhere keeps its own casing) and the editor
  refuses switching it to `regex`.
- en_US only, no fallback (hunspell's "default" IS en_US, so the old retry
  loaded the same file). `.build_hunspell()` takes a `loader` (default
  `.hunspell_loader()`) so tests can inject a failure. After one failure the
  tab says spellcheck is unavailable and makes no further attempt this
  session.

### Text analysis (`R/mod_text_analysis.R`)

Reads RAW data (see Order of operations). Creates no rules; the only thing it
writes is the stopword tier files.

- **Language:** `detect_languages()` wraps `cld2::detect_language()`; values
  under `LANG_MIN_CHARS = 20` characters are NA ("(undetected)" in
  `summarise_languages()`). `cld2` is a declared dependency but OPTIONAL at
  runtime: `lang_detection_available()` gates it, detection returns all-NA
  without it, and the tab shows an install hint instead. The section shows a
  breakdown table and the values not detected as English.
  `build_meta()` adds `lang_share_en` (share of up to `LANG_SAMPLE_N = 200`
  sampled values detected as English) for free-text candidates; it is
  reported only and never affects the candidate flag.
- **Stopwords:** `top_tokens()` / `top_ngrams()` take a `stopwords` argument
  (default `EN_STOPWORDS`); the tab passes `active_stopwords()` for the ticked
  tiers, and the word cloud reuses the same token table. Stopwords are removed
  before n-grams are formed. "+ Stop" (shown only while stopword removal is on)
  calls `add_stopword()` and bumps a version reactive so every view
  recomputes. See Stopwords below.

## Conventions

- R + tidyverse. Native pipe `|>`, never `%>%`.
- 2-space indent.
- `string_helpers.R`, `text_helpers.R`, `stopword_helpers.R`,
  `recat_helpers.R`, `rule_warnings.R`, `export_bundle.R` and `data_loader.R`
  stay Shiny-free so they're unit-testable and reusable from scripts. Shiny
  calls live only in `mod_*`,
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
  dplyr 1.2.0 and prints a warning at runtime (re-measured on dplyr 1.2.1).
  Its deprecation message points to `recode_values()`; do NOT follow it. The
  generator uses `case_when`, which is not deprecated, works with the
  `.default` passthrough the single-pass invariant needs, and does not raise
  the generated script's dplyr floor.
- **tidyselect `matches()` ignores case by default.** `apply_recodes()` and
  `apply_normalizations()` resolve sibling patterns with case-sensitive
  `grep()`, so every generated `matches()` passes `ignore.case = FALSE`.
  Without it `^drink[0-9]+$` also rewrote a `Drink3` column in the script only.
  (Engine is not an issue: `matches()` is base `grep()`, TRE, like the app.)
- **Clean a sibling pattern before testing it.** Call
  `clean_sibling_pattern()` first, then `regex_compiles()`. `regex_compiles()`
  is FALSE for `""` but TRUE for `" "` (a valid regex matching a space), so
  testing a raw pattern lets a whitespace-only one through as a real pattern.
  A blank must never reach resolution as `""`: base `grep("", names)` selects
  EVERY column in the app, while tidyselect's `matches("")` refuses it outright
  (its `check_match()` rejects an empty string) and the script errors.
- **Every regex a user can type is compile-checked before use.** Validate,
  apply and codegen test with `regex_compiles()` (recat terms:
  `.recat_icu_compiles()`) and skip or report; the UI refuses the pattern at
  entry. A bad sibling pattern used to throw in validate, apply and codegen
  alike.
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
- **Never quote data into inline JS with `'%s'`.** Escaping only `'` left
  `"`, `<` and `\` free to break the attribute. JSON-encode each value
  (`.js_value()`), build the handler, then attribute-escape the WHOLE
  attribute (`.attr_value()`); the browser decodes entities before the JS is
  parsed. See `.render_action_cell()` in `mod_spellcheck_view.R`.
- **Assigning an identical value to a `reactiveValues` entry does not
  invalidate.** The old `rules_proxy$set(r)` "revert" after a refused edit
  never redrew, so the typed value stayed on screen. Revert with
  `DT::replaceData(proxy, ...)` instead (`revert_table()` in
  `mod_recode_editor.R`).
- **A bare scalar `FALSE` arm breaks `case_when()`** when it is the only arm:
  "Can't recycle `.default` (size n) to size 1". An arm that matches nothing
  is emitted as `rep_len(FALSE, length(<col>))`.
- **`file.copy()` can fail silently on Windows** (paths with spaces are one
  trigger) and its return value is not trustworthy. Dictionary import uses
  `.copy_file_verified()`: `readBin` / `writeBin`, then reads the destination
  back and requires identical bytes.
- **NEVER `escape = FALSE` on a DT holding dataset content.** This app ingests
  arbitrary third-party CSV/Excel, so untrusted input is the PRIMARY path;
  `escape = FALSE` unescapes EVERY column, and a cell containing
  `<img src=x onerror=...>` then executes on render. Use
  `escape = -ncol(df)` with the generated-markup column placed LAST (the
  pattern in `mod_recode_editor`, `mod_spellcheck_view`, `mod_recategorize`,
  `mod_normalize` and `mod_text_analysis`), or a name-based index. When adding a
  column to one of
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
- **Look titles up with `issue_class_title()`, never `titles[[nm]]`.** A class
  a validator gains later otherwise throws "subscript out of bounds" and takes
  the whole panel down.
- **Set `RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE` for local `Rscript` runs.**
  renv's startup synchronized check can take minutes on every `Rscript` from
  the repo root; the variable skips it and changes nothing else.
- **Positron's "Install <pkg>" button in the error pane bypasses renv.** It
  runs `install.packages()` into `.libPaths()[1]`, which under an active renv
  project IS the project library, so the package appears, the app starts, and
  the reason restore left it unusable can no longer be seen. Seen 2026-09-04
  on a fresh clone: `renv::restore()` reported shiny installed, `runApp()`
  said "no package called 'shiny'", the button was clicked, and afterwards
  every package in the library had a DESCRIPTION and `renv::repair()` found
  nothing. Assumed cause: a broken link from the renv cache for that one
  package; not checked, because the button overwrote the evidence. The check
  that would settle it next time, BEFORE clicking anything: list the project
  library and look for package directories with no DESCRIPTION, then
  `renv::repair()`. The README says this under Quick start. Also harmless:
  `renv::status()` listing `renv` itself as used but not installed (it loads
  from its own sandbox, not the project library).
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

## Stopwords

Mirrors the dictionary tiers, for the Text analysis tab only. A token is a
stopword if any TICKED tier lists it (`active_stopwords(tiers)`; no tier
ticked means no stopwords):

- `stopwords/base_stopwords.txt`: base, committed, seeded from `EN_STOPWORDS`
  (a test pins them equal). Edited by hand only; the app never writes it. If
  the file is missing the base tier falls back to `EN_STOPWORDS`.
- `stopwords/custom_stopwords.txt`: project, committed. "+ Stop" with target
  Project appends here; commit it to share.
- `stopwords/user_stopwords.txt`: user, GITIGNORED. "+ Stop" with target User.

Files read like the dictionary loader: lowercase, `#` comments, blank,
multi-word and numeric lines and `/` affix flags dropped. `add_stopword()`
refuses the base tier and unusable terms, and verifies the write by reading
the file back (Windows writes can fail silently), taking a `writer` argument
so tests can inject a failure. Stopwords change only what Text analysis
counts, never data, rules or exports. See `stopwords/README.md`.

## Testing

```r
testthat::test_dir("tests/testthat")
```

Run a single file with
`testthat::test_file("tests/testthat/test-string_helpers.R")`.

| file | covers | assertions |
|---|---|---|
| `test-string_helpers.R` | schema, normalize_value, cluster (incl. fingerprint / n-gram fingerprint), taxonomy matching, CSV round-trip + conform-on-read, merge in place, `.rule_hits` truth table (incl. `""` per match type, empty regex), validate (regex/enum/stale), apply (siblings/delete/regex/NA), codegen incl. `ignore.case = FALSE`, `grepl()` regex arms, escaped plain-rule patterns, backticked names, `rep_len(FALSE, ...)` arms, **both agreement tests** (incl. awkward column names), rule-order independence | 293 |
| `test-spellcheck_view.R` | action-cell escaping (`.js_value()` / `.attr_value()`) with awkward characters round-tripping to the created rule, action column last, `.spell_drop_blank()` and the all-blank empty state, `spell_replace_token()` (other words, punctuation, every occurrence, whole words only, case mirroring, quote marks, no-op), shared tokenizer, `exact` rules leave other casings alone, no hunspell fallback and no retry | 235 |
| `test-normalizations.R` | normalization ops, id, apply, preview, validate, CSV IO (incl. 13-column recodes files), `norm_spec` stamp (on rules proxy `add()` only), codegen, norms proxy upsert, Normalize tab | 175 |
| `test-export_bundle.R` | pipeline order, row-count stop, `source_file_meta`, provenance (incl. severity, notes marked), files written, empty-cell NA round-trip, **script reproduces a byte-identical CSV** (incl. awkward column names), bundle script skips unusable rules like the app, SHA-256 warning, `zip::zip`, download wiring | 164 |
| `test-rule_checks.R` | `regex_compiles()`, `clean_sibling_pattern()` (and recat's private mirrors agreeing), bad sibling patterns reported and skipped for recodes / normalizations / recat in app and script, blank pattern means own column, recat match type reading and app/script agreement, bad recat regex terms, `validate_recat()`, `id_mismatch`, `recode_key_id()`, `spellcheck_lost_words`, warning vs note severity, tidyselect `matches()` uses base `grep()` (TRE) | 163 |
| `test-recat_helpers.R` | recat match/apply/priority/codegen/IO + the three worked examples end-to-end against the bundled CSV | 148 |
| `test-cluster_target.R` | `cluster_target_decision()`, exclusion pair-building, single-table cluster cards, `testServer` wiring for the Apply refusal, blank dropping (clusters and taxonomy matcher), pairwise cap + note, card cap, debounce, session cache | 141 |
| `test-ui_rule_checks.R` | `issue_class_title()`, Normalize panel and tab with invalid / unknown classes and refused typed patterns, cleared pattern stored as NA, `recode_validation_ui()` (new classes, plurals, escaping, unknown class), editor sibling-pattern and Spellcheck-to-regex refusals, `recat_add_refusal()` / `recat_issues_ui()`, import merge by key and `import_recodes_summary()` | 112 |
| `test-rule_warnings.R` | every warning kind, `collect_rule_warnings()` | 82 |
| `test-data_loader.R` | metadata + classifiers (`build_meta`, `column_group`, `suggest_sibling_pattern` incl. metachar escaping, date/numeric/id detection, `read_dataset`), `lang_share_en` (candidates only, NA without cld2) + example-dataset regression | 67 |
| `test-text_analysis_module.R` | "+ Stop" button escaping, tier selection feeding Top words / phrases / cloud, "+ Stop" targets and failed writes, Language section (breakdown, non-English filter, escaping, install hint without cld2), `lang_values_table()` | 66 |
| `test-empty_values.R` | `value_frequency_table()` keeps `""` and drops NA, `""` matching per match type, empty-value agreement between app and script per match type, validate, `""` vs `<NA>` CSV round-trip, the "(empty)" label, Browse values minting a `""` rule | 65 |
| `test-import_recodes.R` | import schema guard (`source_quarter` rename, optional `norm_spec`, refusals), in-place `merge_incoming_rules()`, normalizations import | 59 |
| `test-recategorize_module.R` | Recategorize preview on the recoded frame, picker seeding, "Out of date" marker and what does and does not trigger it | 58 |
| `test-text_helpers.R` | sentences, length classification, tokens, n-grams, KWIC, custom `stopwords` argument, `detect_languages()` / `language_summary()` (incl. no cld2) | 53 |
| `test-stopword_helpers.R` | tier paths, `.read_stopword_terms()` cleaning, `active_stopwords()` merge and EN_STOPWORDS fallback, `add_stopword()` (idempotent, tier, newline, refusals, loud and silent write failures, non-ASCII), committed base tier equals EN_STOPWORDS, user tier gitignored | 47 |
| `test-preview_rule_warnings.R` | "did less than expected" panel and its server wiring, notes block apart from warnings, missing / unknown severity counts as warning | 46 |
| `test-recode_editor.R` | `apply_recode_edit()`: `rule_id` recomputed on key edits, clash refusal naming the other rule, enum refusal, locked columns, `norm_spec` untouched, position kept; `testServer` wiring | 46 |
| `test-dt_truncate.R` | `dt_truncate_defs()` / css / hint | 40 |
| `test-spellcheck_dict.R` | `.read_dict_terms()` strips `.dic` artifacts; `.copy_file_verified()` and the import handler, incl. silent and loud write failures | 40 |
| `test-variable_picker.R` | banner choices, `current_var` two-way sync without ping-pong, filter lifted for a hidden column | 29 |
| `test-preview_export.R` | `preview_diff_table()` counts, incl. rules with no data loaded | 26 |
| `test-guide_helpers.R` | help layer: accessibility attributes, collapsed by default, escaping | 24 |
| `test-app_wiring.R` | rules / recat proxies: `add()` merges in place and conforms partial rows, `set()` replaces | 16 |
| `test-escape_guard.R` | no bare `escape = FALSE` (or `= F`) in any app R source, read from parse tokens so comments and strings don't count | 11 |
| `test-em_dash_guard.R` | no em dash or em dash entity in any project text file (skips `renv/`, `ext/`, `inst/extdata/`) | 6 |
| `test-all-files-parse.R` | every R file in the project parses | 2 |

2214 total (27 files).

Tests `source()` the R files they need by relative path. Pure files are always
safe; the `mod_*.R` files that tests source (cluster view, data input, import,
normalize, preview & export, recategorize, recode editor, spellcheck, text
analysis, value table, variable picker) are Shiny-free at load time because
every `shiny::` call sits inside a function body, and their server wiring is
exercised with `shiny::testServer()`. Keep new modules that way. For local
runs set `RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE` (see Gotchas).
CI runs the suite via `.github/workflows/tests.yml` (r-lib/actions +
DESCRIPTION-driven deps) on pushes to `main` and on pull requests targeting
`main` ONLY: a push to a feature branch with no open PR runs nothing, so the
local suite is the only evidence until the PR exists.

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
107 packages on R 4.6.1). `.Rprofile` activates renv on session start, so a
bare `Rscript` from the repo root uses the project library, no `R_LIBS`
workaround. On a fresh clone, run `renv::restore()` first. `DESCRIPTION`
(Imports) and the README install line list the direct deps: keep all three in
sync. `ggplot2` and `htmltools` were used for months without being declared,
which silently broke non-renv installs and CI dep resolution. `digest`
(SHA-256 of the loaded file) and `zip` (the bundle) are direct dependencies.
So is `cld2` (language detection), declared in Imports and pinned, but the app
must keep working without it: every call goes through
`lang_detection_available()`. `writexl` is NOT a dependency: every export is
CSV, R or text.

## Environment

- Windows (R 4.6.1+).
- IDE: Positron (restart R with Ctrl+Shift+0, NOT the VS Code "R: Restart R").
- Git default branch: `main`.
- GitHub remote: https://github.com/amelia-m/recode-studio (public, pushed 2026-06-08).

## Open / TODO

- **Peer tokenization review:** a tokenization/cleaning reference script is
  archived at `ext/tokenization_reference.Rmd` (see the spec in
  `docs/superpowers/specs/2026-08-13-text-normalization.md`).
- **Word-level fixes:** Spellcheck still flags only the first unrecognized
  word per value and makes whole-cell rules. Word-level rules are specified in
  [docs/superpowers/specs/2026-09-16-word-fixes.md](docs/superpowers/specs/2026-09-16-word-fixes.md),
  which has open questions for the owner.
- Optional: `mod_import_recodes`-style import for recat CSVs. `read_recat()`
  exists and is tested, but nothing in the UI calls it.
- Optional: expand the free-text heuristic to let the user override a column's
  candidate flag manually.
- Optional: README screenshot / demo GIF (capture from a running app).
- **For Amelia:** the coworker whose first-run restore failed on 2026-09-04
  (data.table / bit64 binaries gone from CRAN for R 4.6) has a library that
  matches the bumped lockfile only once she pulls `main` at or after commit
  `6686cd0`. Until then her `renv::status()` reports those packages out of
  sync, and she must not run `renv::snapshot()`. Ask her to `git pull` and
  run `renv::status()` once; "No issues found" closes this item.
- Defects present in both this repo and upstream, loose ends from the
  September sync, and open decisions are tracked in
  [docs/upstream-port-candidates.md](docs/upstream-port-candidates.md)
  (sections 4 and 6), with the questions still waiting for the owner in
  section 8. Don't duplicate them here.

### Features deliberately NOT ported from upstream

Don't re-add these without a reason. They assume a recurring delivery cycle, a
shared network path, or one organization's conventions, and do not generalize:
raw-vs-pre-cleaned review basis (`preclean_for_review`,
`rebase_rules_onto_raw`, and upstream's carry-forward of the previous period's
recodes, `prior_quarter_recodes` there),
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
