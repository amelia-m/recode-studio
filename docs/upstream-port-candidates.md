# Upstream port candidates

Where this repo is ahead of the upstream Recode Studio it was spun off from, and
what might be worth porting back later. Nothing here has been ported upstream.

Compiled 2026-09-15 from a side-by-side review of both codebases. File paths for
upstream are relative to its repo root; paths without a prefix are this repo.

Also recorded here: defects present in BOTH codebases, and decisions still open
from the same review.

## 1. Bugs fixed here that upstream still has

Rank these above any feature.

1. **Dataset text rendered as HTML in DT tables.** Upstream still uses
   `escape = FALSE` in `modules/mod_recode_editor.R`, `mod_spellcheck_view.R`,
   `mod_recategorize.R`, and in a newer `mod_normalize.R` (the same defect class
   reintroduced after the original fix). A cell holding
   `<img src=x onerror=...>` executes on render. Fix pattern here:
   `escape = -ncol(df)` with the generated-markup column last
   (`R/mod_recode_editor.R`, `R/mod_spellcheck_view.R`, `R/mod_recategorize.R`).
2. **Recat crashes on an NA `match_type`.** Upstream's recat matcher and codegen
   default the type with `%||%`, which only catches NULL, so an NA reaches
   `if (match_type == "regex")` and throws "missing value where TRUE/FALSE
   needed". A rule saved with a blank type round-trips as `<NA>` and would abort
   upstream's export. Fix here: `.recat_match_type()` in `R/recat_helpers.R`
   maps NA and "" to `literal`.
3. **Recat script fails to parse on non-syntactic names.** Upstream emits bare
   column names (`str_detect(tolower(drink 1), ...)`) and a raw-quoted `out_col`.
   Latent upstream because its columns are snake_case. Fix here: backticked
   names and `deparse()` in `R/recat_helpers.R`.
4. **Import has no schema check.** Upstream binds whatever `read_recodes()`
   returns; readr only warns on the console and `bind_rows` unions the column
   sets, corrupting the rule file's schema. Fix here:
   `.check_incoming_recodes()` in `R/mod_import_recodes.R`. When porting,
   reverse the tolerated rename direction and require upstream's extra column.
5. **Spellcheck tab dies when the en_US dictionary is missing.** Upstream calls
   `hunspell::dictionary("en_US")` with no fallback. Fix here: nested
   `tryCatch` with a default-dictionary fallback and a notification in
   `R/mod_spellcheck_view.R`.
6. **An NA keep target recodes a whole cluster to NA.** Upstream
   `cluster_recode_pairs()` returns every member mapped to NA when `keep` is NA.
   Latent, since the decision helper never returns ok with an NA target. Guard
   here: `is.na(keep[1])` in `R/mod_cluster_view.R`.
7. **Sibling pattern not escaped.** Upstream builds `^<stem>[0-9]+$` without
   escaping, so a stem `a.b` also matches `axb3` and a stem with `(` errors.
   Latent upstream. Fix here: `suggest_sibling_pattern()` in `R/data_loader.R`.
8. **Generated script matches sibling columns case-insensitively.**
   tidyselect `matches()` ignores case by default, while `apply_recodes()`
   uses case-sensitive `grep()`, so a sibling pattern like `^drink[0-9]+$`
   also rewrites a `Drink3` column in the script but not in the app. Found and
   fixed here (`ignore.case = FALSE`) while porting normalizations. Upstream's
   generator uses the same construct; not yet confirmed there by running it.

Fixed here on 2026-09-16 (commits "Fix codegen for awkward column names, stale
rule ids, empty values" and "Fix cluster recomputation, stale recat preview,
spellcheck escaping"). Items 9 and 12 through 15 were established as upstream
defects in section 4; the rest are marked where upstream was not checked.

9. **Plain-rule codegen does not escape the column name.** Upstream still has
   it (section 4, item 1). Fix here: `.escape_regex()` for the grouping
   pattern and a backticked single-column `mutate()` via `.r_col_token()` in
   `generate_recode_R()` (`R/string_helpers.R`).
10. **A scalar `FALSE` arm breaks `case_when()`.** When it is the only arm the
    script fails with "Can't recycle `.default`". Fix here: emit
    `rep_len(FALSE, length(<col>))`. Upstream not checked.
11. **An empty regex pattern wipes a column in the app.** Base `grepl("")`
    matches every non-missing cell while the script's `str_detect()` refuses
    the pattern. Fix here: an empty pattern matches nothing in both, and
    `validate_recodes()` reports it as `invalid_regex`. Upstream not checked.
12. **Editing `old_value` or `match_type` keeps a stale `rule_id`.** Upstream
    still has it (section 4, item 2). Fix here: `apply_recode_edit()` in
    `R/mod_recode_editor.R`.
13. **Cluster recomputation is neither debounced nor capped.** Upstream still
    has it (section 4, item 3). Fix here: debounce, session `bindCache()`,
    `PAIRWISE_CLUSTER_CAP`, `MAX_CLUSTER_CARDS` in `R/mod_cluster_view.R`.
14. **Recat preview goes stale silently.** Upstream still has it (section 4,
    item 4). Fix here: form snapshot and "Out of date" marker in
    `R/mod_recategorize.R`.
15. **Spellcheck action-cell attribute escaping.** Upstream still has it
    (section 4, item 5). Fix here: `.js_value()` then `.attr_value()` in
    `R/mod_spellcheck_view.R`.
16. **A refused Recodes edit does not redraw.** Re-setting unchanged rules
    into `reactiveValues` does not invalidate, so the rejected value stays on
    screen (this also affected the invalid-enum rejection). Fix here:
    `DT::replaceData()`. Upstream not checked.
17. **Spellcheck replaced the whole value with the suggested word.** The rule
    made from a suggestion had `old_value` = the whole value and `new_value` =
    the corrected WORD, so `iced coffe` became `coffee`. Found 2026-09-16; a
    fix that corrects only the flagged word inside the value is in progress
    here. Upstream's spellcheck builds rules the same way; not checked by
    running it.

## 2. Features and robustness here that upstream lacks

- **Key-collision and n-gram fingerprint clustering** (`R/string_helpers.R`).
  Linear time; word-order invariant and whitespace invariant respectively.
  Suited to short free-text fields with inconsistent spacing or word order.
- **Reference Taxonomy Matcher** (`match_taxonomy()`, second sub-tab of
  Clusters). Match a column against an approved list and emit rules. An
  in-state county or state list would be the obvious built-in upstream.
- **Algorithm guidance** in the cluster view: which algorithms ignore the
  threshold, plus a reference accordion.
- **Discipline dictionaries** with multi-select and import on the Spellcheck
  tab.
- **Text analysis tab** (`R/text_helpers.R`, `R/mod_text_analysis.R`): long-text
  detection, length distribution, top words and phrases, keyword in context,
  word cloud. Fits upstream's narrative columns.
- **Recat rules owned by the server** (`recat_proxy` in `app.R`) instead of a
  module-local reactiveVal, which blocks import, cross-module warnings and tests.
- **`build_meta()` details:** the identifier-name guard is case-insensitive
  (`Case_ID` excluded), and empty strings count as missing in `na_share`.
  Small real-world effect upstream because its data is lowercase with blanks
  read as NA.
- **Test coverage upstream lacks for behaviour it does have:** codegen with
  quotes and backslashes, invalid enums, regex apply/delete/invalid pattern,
  summary column names, one `case_when` block for mixed match types and its
  ordering, the overlap WARNING comment, `.rule_hits` with NA and unusable
  types, `is.na()` and `FALSE` codegen arms, recat invalid regex, zero include
  terms, nonexistent columns, sibling expansion, NA priority ordering,
  non-syntactic source columns, `case_when` not `case_match`. See
  `tests/testthat/test-string_helpers.R`, `test-recat_helpers.R`,
  `test-cluster_target.R`.

## 3. Upstream doc drift noticed in passing

- Its recode rules README says `old_value` "cannot be NA", which contradicts
  NA-targeting rules.
- The same README says the app writes via temp file plus rename;
  `write_recodes()` calls `readr::write_csv` directly.

## 4. Defects found in both codebases

Not ports in either direction. Recorded so they are not lost. Items 1 through
5 were fixed here on 2026-09-16 and STILL EXIST UPSTREAM (see section 1, items
9 and 12 through 15). Item 6 was this repo only and is fixed here.

1. **Regex metacharacters in a variable name break apply/codegen agreement.**
   `generate_recode_R()` built `paste0("^", variable, "$")` unescaped while
   `apply_recodes()` targets the name exactly. A rule on `q1.a` also recoded
   `q1_a` in the script; a name containing `(` made the script error.
   **Fixed here 2026-09-16:** the pattern is escaped with `.escape_regex()`
   and the block is a backticked single-column `mutate()`; the agreement and
   bundle tests now cover `q1.a`/`q1_a`, `price ($)` and `my col`. Still
   upstream.
2. **Editing a rule's `old_value` or `match_type` keeps a stale `rule_id`.**
   `recode_rule_id()` hashes `(variable, match_type, old_value)`. A later rule
   minted for the original value collides and `rules_proxy$add()` silently
   drops the edited rule. **Fixed here 2026-09-16:** `apply_recode_edit()`
   recomputes the id and refuses an edit whose new id belongs to another
   rule, naming it. Still upstream.
3. **No debounce or cap on cluster recomputation.** Every threshold, q-gram or
   normalizer change reruns the quadratic clustering and rebuilds every card
   through `renderUI`. **Fixed here 2026-09-16:** 500 ms debounce, per-session
   `bindCache()`, pairwise algorithms capped at 1000 values with a pointer to
   the linear ones, at most 50 cards. Still upstream.
4. **Recat preview goes stale.** It is a snapshot assigned inside
   `observeEvent` and does not update or grey out after the form changes.
   **Fixed here 2026-09-16:** the form is recorded with the preview, which is
   greyed out under an "Out of date" line while the form differs. Still
   upstream.
5. **Spellcheck suggestion escaping** in the action cell only escapes `'`
   inside the `onclick` attribute. Low risk. **Fixed here 2026-09-16:** values
   are JSON-encoded (`.js_value()`) and the whole attribute is escaped
   (`.attr_value()`). Still upstream.

This repo only:

6. **Dictionary import reports success when the copy failed.** The upload
   handler wrapped `file.copy()` in a `tryCatch` that always returned TRUE,
   and `file.copy()` can fail silently on Windows. **Fixed here 2026-09-16:**
   `.copy_file_verified()` copies with `readBin` / `writeBin` and compares the
   bytes before reporting success.
7. ~~**The taxonomy matcher's empty state** was a one-column DT with a
   "Message" header.~~ Fixed 2026-09-15: now `empty_state()`.

## 5. Decisions from the September 2026 upstream sync

- **Git history is not being rewritten** (decided 2026-09-15). Earlier commits
  carry test fixtures that combined real US place names with violent-death
  vocabulary. There is no agreement prohibiting disclosure of the partner, so
  fixing the current tree is enough. The published history has no program
  names, paths, usernames or person names.
- **Column normalizations: port.** Whole-column trim/squish/case rules that
  run before recodes, with the `norm_spec` column in the rule CSV.
- **Upstream layout rework (numbered left rail, collapsible settings, warnings
  sidebar): skipped for now.**
- **Cleaned data export: add it.** This changes the app's promise that it
  only exports rules and a script. The export bundles the cleaned data, a
  provenance record (original data file name, row and column counts and
  similar), and the rules that were applied.
- **Not ported, deliberately:** upstream's rename of `recodes_master.csv` to
  `recode_rules.csv` (churn for no behaviour change), and its removal of the
  per-user dictionary tier (fits a shared team review, not a general tool).

## 6. Open questions and follow-ups

1. **cspell could not be run.** It is not installed on the development machine
   and no agent was allowed to install it, so the repo's "cspell is clean" rule
   is unverified for everything added in September. New test fixtures
   deliberately use misspellings that may need entries in
   `.cspell/example-typos.txt`. Needs a decision on installing cspell (via npx
   or globally) to check.
2. ~~**Should the shared defects in section 4 be fixed in this repo?**~~
   Answered yes, and done 2026-09-16: section 4 items 1 through 6 are fixed
   here. Items 1 through 5 still exist upstream.
3. **Report the escape defect upstream.** Upstream still renders dataset text
   as HTML in four tables; see section 1, item 1.
4. **Text normalization plan** (`docs/superpowers/plans/2026-08-13-text-normalization.md`):
   the execution approach (subagent-driven or inline) was never chosen, and the
   plan has not been built. It overlaps partly with the column normalizations
   port above; reconcile before building either further.
5. **Empty string values.** Upstream keeps `""` in the unique-values list;
   this repo dropped them, so an empty string could not be targeted from
   Browse values. The owner wanted that fixed, and it is done (2026-09-16):
   `value_frequency_table()` keeps `""`, shown as "(empty)" in Browse values
   and the Recodes table. Finding: `read_dataset()` uses readr's defaults, so
   empty, quoted-blank and whitespace-only CSV cells load as NA, never as
   `""`. The fix therefore matters mainly for values a normalization squishes
   to `""` and for data arriving other ways.
   **Still open:** should Browse values also offer a "(missing)" row that
   creates an NA-targeting rule? Loaded blanks are NA, and NA cells cannot
   currently be targeted from Browse values (only through a rules CSV
   holding `<NA>` in `old_value`).
6. **Loose ends found during the normalizations port** (not fixed):
   - A recode or normalization rule on a column missing from the data makes
     the generated script error (the recode case predates the port).
   - Sibling patterns in the generated script go through tidyselect's regex
     engine, which may differ from base `grep()` on unusual patterns.
   - An imported normalization with an invalid regex makes
     `apply_normalizations()` error; the app falls back to raw values, but
     `validate_normalizations()` has no invalid-regex issue class.
   - (Resolved as intended, 2026-09-16.) Editing `old_value` or
     `match_type` keeps the `norm_spec` stamp, which is correct: the stamp
     describes the normalization of the rule's COLUMNS, and those edits do
     not change the columns. Editing `apply_to_siblings` or
     `sibling_pattern` does change them but also keeps the old stamp; that
     can surface as a `norm_spec_drift` warning, which errs toward warning.
   - Re-selecting an existing value on Browse values resets that rule's
     `new_value` to the old value (in place, but it overwrites the edit).
   - The Variable tab's empty state is still a one-column "message" table.

   Loose ends found during the 2026-09-16 defect fixes (not fixed):
   - An invalid sibling regex makes `apply_recodes()` and the overlap check
     error, rather than being caught and reported.
   - Two rules whose `old_value` differs only in whitespace get different
     `rule_id`s even under `trimmed_ci`, where they match the same cells.
   - Regex matching uses base R's TRE engine in the app (`grepl`) and ICU in
     the generated script (`str_detect`), so unusual patterns could disagree.
   - The Recategorize "Out of date" marker tracks the form only, not later
     changes to the recode rules the preview was computed on.
   - Applied clusters stay on screen, because recodes do not feed back into
     clustering, so the "Showing N of M" count never shrinks as you work.
   - Importing a discipline dictionary with a bundled name (for example
     `medical.txt`) overwrites the bundled file.
   - Key-collision clustering took about 6 s for 3000 random values. It is
     linear and deliberately uncapped, so very large columns are still slow.
   - Spellcheck flags only the FIRST unrecognized word per value, and it reads
     data before recodes, so after a fix the same value is still flagged on the
     same word and a second typo in it can never be fixed from that tab.
     Addressed by the word-fixes spec
     ([2026-09-16-word-fixes.md](superpowers/specs/2026-09-16-word-fixes.md)).
   - A Spellcheck rule is a whole-cell `trimmed_ci` rule, so a differently
     cased copy of the same value elsewhere in the column takes the clicked
     value's casing.
   - renv's startup synchronized check can take minutes on every `Rscript`
     run from the repo root; `RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE` in the
     environment skips it for local test runs. Possibly worth a note in
     CLAUDE.md.
7. **Docs updated for the September sync** (2026-09-15): CLAUDE.md and
   README.md now cover normalizations, the rule warnings, the variable banner
   and the cleaned data export bundle. Updated again 2026-09-16 for the
   defect fixes above.
