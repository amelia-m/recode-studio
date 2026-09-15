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

## 2. Features and robustness here that upstream lacks

- **Key-collision and n-gram fingerprint clustering** (`R/string_helpers.R`).
  Linear time; word-order invariant and whitespace invariant respectively.
  Suited to free-text place and cause fields.
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
