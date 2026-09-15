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

## 4. Defects in both codebases, fixed in neither

Not ports in either direction. Recorded so they are not lost.

1. **Regex metacharacters in a variable name break apply/codegen agreement.**
   `generate_recode_R()` builds `paste0("^", variable, "$")` unescaped while
   `apply_recodes()` targets the name exactly. A rule on `q1.a` also recodes
   `q1_a` in the script; a name containing `(` makes the script error. Not
   covered by the agreement test. Fix: escape the variable, or emit a
   backticked plain `mutate()` for single-column rules.
2. **Editing a rule's `old_value` or `match_type` keeps a stale `rule_id`.**
   `recode_rule_id()` hashes `(variable, match_type, old_value)`. A later rule
   minted for the original value collides and `rules_proxy$add()` silently
   drops the edited rule. Fix: recompute the id on edits to key columns and warn
   on collision.
3. **No debounce or cap on cluster recomputation.** Every threshold, q-gram or
   normalizer change reruns the quadratic clustering and rebuilds every card
   through `renderUI`.
4. **Recat preview goes stale.** It is a snapshot assigned inside
   `observeEvent` and does not update or grey out after the form changes.
5. **Spellcheck suggestion escaping** in the action cell only escapes `'`
   inside the `onclick` attribute. Low risk.

This repo only:

6. **Dictionary import reports success when the copy failed.** The upload
   handler wraps `file.copy()` in a `tryCatch` that always returns TRUE, and
   `file.copy()` can fail silently on Windows. Check `file.exists()` on the
   destination instead.
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
2. **Should the shared defects in section 4 be fixed in this repo?** They are
   not upstream ports, so the sync did not touch them. Item 1 (regex
   metacharacters in a variable name breaking apply/codegen agreement) and
   item 2 (stale `rule_id` after editing a key column) are silent
   wrong-output bugs.
3. **Report the escape defect upstream.** Upstream still renders dataset text
   as HTML in four tables; see section 1, item 1.
4. **Text normalization plan** (`docs/superpowers/plans/2026-08-13-text-normalization.md`):
   the execution approach (subagent-driven or inline) was never chosen, and the
   plan has not been built. It overlaps partly with the column normalizations
   port above; reconcile before building either further.
5. **Empty string values.** Upstream keeps `""` in the unique-values list; this
   repo drops them, so an empty string cannot be targeted from Browse values.
   Unconfirmed whether that difference is deliberate.
6. **Loose ends found during the normalizations port** (not fixed):
   - A recode or normalization rule on a column missing from the data makes
     the generated script error (the recode case predates the port).
   - Sibling patterns in the generated script go through tidyselect's regex
     engine, which may differ from base `grep()` on unusual patterns.
   - An imported normalization with an invalid regex makes
     `apply_normalizations()` error; the app falls back to raw values, but
     `validate_normalizations()` has no invalid-regex issue class.
   - Editing `old_value` in the Recodes table keeps the old `norm_spec` stamp.
   - Re-selecting an existing value on Browse values resets that rule's
     `new_value` to the old value (in place, but it overwrites the edit).
   - The Variable tab's empty state is still a one-column "message" table.
7. **Docs updated for the September sync** (2026-09-15): CLAUDE.md and
   README.md now cover normalizations, the rule warnings, the variable banner
   and the cleaned data export bundle.
