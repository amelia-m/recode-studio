# Word fixes (token maps) and word grouping - design spec

**Date:** 2026-09-16
**Supersedes:** features B and C of
[2026-08-13-text-normalization.md](2026-08-13-text-normalization.md). Features
A (language detection) and D (stopword tiers) of that spec are being built as
written; feature E (row clustering by vocabulary) is deferred.
**Status:** draft for review. No plan yet.

## Problem

Every rule the app can make replaces a WHOLE cell. That is right for short
labels (`capuccino` -> `cappuccino`) and wrong for anything with more than one
word:

- **Long text is uncleanable.** In a free-text column almost every value is
  unique, so fixing one misspelled word means one rule per distinct sentence.
- **Spellcheck was destroying data.** It flags one word in a value, but the
  rule it made replaced the whole value with the suggested word
  (`iced coffe` -> `coffee`). A minimal fix is in progress (the rule now
  rewrites the whole value with only that word corrected), but it still needs
  one rule per distinct value.
- **Inflection and typo variants of the same word** ("duty / duties",
  "choclate / chocolate") can only be merged by hand, sentence by sentence.

The reference tokenization script in `ext/` shows the manual version of this:
twenty hand-written `str_replace_all()` calls, with no word boundaries, so
`made -> make` also turns "homemade" into "homemake".

## Scope

| id | feature |
|---|---|
| B | **Word fixes:** a rule type that replaces one word everywhere it appears in a column, leaving the rest of each cell alone |
| C | **Word grouping:** a suggestion screen that groups a column's words by shared stem or by similarity and turns a chosen grouping into word fixes |

C depends on B. B is useful on its own (typed by hand, and from Spellcheck).

## Why a separate rule type, not a recode match type

The August spec made word fixes a fifth recode `match_type` (`token`). Since
then, column normalizations were ported as their own rule type, for a reason
that applies equally here: a transform that rewrites part of a cell does not
"claim" the cell, so it does not fit the recode model (one rule per cell,
first match wins, `rule_id` keyed on the value). Forcing it in needed special
cases for claiming, shadowing and counting.

A word fix has the same shape as a normalization: a column-level transform
that runs before recodes. So B becomes a third rule type in the normalize step,
alongside normalizations, and recodes keep their current, simple semantics.

## Order of operations

**normalize ops -> word fixes -> recode -> recategorize.**

- Word fixes run after the whole-column ops, so a lowercased column needs only
  lowercase word fixes, and whitespace is already tidy.
- They run before recodes, so Browse values, Clusters and Spellcheck see the
  corrected text, and whole-cell recodes are authored against it.
- Text analysis keeps reading raw data (unchanged, deliberate).

This order is enforced everywhere the current order is: `unique_values_r`,
`recode_basis_r`, Preview & export, `generate_recode_R()`,
`build_cleaned_dataset()` and the bundle script.

## B: word fixes

### Semantics

- `from_word` is a single word: letters, digits and apostrophes, the same
  character class the app's tokenizer uses. It is matched as a whole word,
  case-insensitively.
- Word boundaries are explicit lookarounds, `(?<![[:alnum:]'])` and
  `(?![[:alnum:]'])`, not `\b`, so apostrophes behave the same as in the
  tokenizer. Matching uses stringr (ICU) in BOTH the app and the script, so
  the two cannot disagree the way TRE and ICU do elsewhere.
- `to_word` is any non-missing text (it may contain spaces or punctuation:
  `dont -> don't`, `w -> with`).
- **Single pass.** All word fixes for a column are applied in one
  `str_replace_all()` over an alternation of every `from_word`, with a
  function replacement looking the match up in a map. Each word in the
  original text is examined once and mapped once, so fixes never chain
  (`used -> use` plus `use -> utilize` turns "used" into "use", not
  "utilize"). Validation warns when a `to_word` is another fix's `from_word`,
  because the user may expect chaining.
- **Casing (open question 2):** proposed default is to mirror the matched
  word's pattern: all caps stays all caps, Title Case stays Title Case,
  otherwise use `to_word` as written. This matches the Spellcheck fix.

### Schema: `word_fixes_master.csv`

`wfix_id` (hash of the target + `from_word`, lowercased), `variable`,
`apply_to_siblings`, `sibling_pattern`, `from_word`, `to_word`, `notes`,
`author`, `created_at`, `updated_at`, `source_dataset`. Same blank conventions
as `normalizations_master.csv`.

One `from_word` per target. Adding a fix for an existing `from_word` updates it
in place.

### Engine (pure, in `R/string_helpers.R` next to normalizations)

`empty_word_fixes_tibble()`, `word_fix_id()`, `apply_word_fixes(df, fixes)`
returning `list(df, summary)` with cells changed and occurrences replaced per
fix, `word_fix_preview(df, fixes)` (before/after pairs with counts),
`validate_word_fixes()` (not a single word, duplicate `from_word`, missing
column, chain warning, a fix that changes nothing), `read_word_fixes()` /
`write_word_fixes()`, `merge_word_fixes()` (in place).

### Codegen and the agreement guarantee

`generate_recode_R()` gains `word_fixes = NULL` and emits, after the
normalization blocks and before the recode blocks, one block per target: a
named map vector and a single `str_replace_all()` with a small case-mirroring
helper emitted once at the top of the script. Non-syntactic names are
backticked; sibling families use `matches(..., ignore.case = FALSE)`.

Both agreement tests extend to normalize -> word fixes -> recode, and the
bundle reproduction test gains word fixes. These are the acceptance gate.

### State, stamping and warnings

- `rv$wfixes` behind `make_word_fixes_proxy()`; in-place merge.
- **`norm_spec` stamp (open question 3):** proposed to leave word fixes OUT of
  the stamp. Including them would mark every recode as drifted each time any
  word fix is added. `recode_no_match` already catches recodes a word fix
  makes dead.
- New warnings: `word_fix_no_change`, `word_fix_no_target`, `word_fix_chain`.

### UI

- **Where (open question 4):** proposed as a "Word fixes" section on the
  Normalize tab (no renumbering), since both are column-level transforms that
  run before recodes.
- Current column's fixes in a table (remove button last, safe escaping), an
  add form (`from_word`, `to_word`, siblings choice), and a keyword-in-context
  preview of the cells that would change, before and after.
- **Spellcheck:** each flagged word offers "Fix this word everywhere in the
  column" (a word fix) next to the existing "Fix this value only" (a recode).
  The default choice follows the column kind: long text defaults to the word
  fix. (Open question 5.)
- Import and export: `word_fixes_master.csv` in the Preview & export downloads,
  the import card, and the cleaned data bundle; the bundle script applies them.

## C: word grouping

- On the Normalize tab's Word fixes section, a "Suggest word fixes" panel.
- Source: the column's word vocabulary after normalize ops, excluding the
  active stopwords (feature D) and words shorter than a minimum length.
- Grouping modes:
  - **Shared stem:** `hunspell::hunspell_stem()` where the dictionary knows the
    word, with a conservative suffix-stripping fallback. The stem is only a
    grouping key; it is never written to data.
  - **Similarity:** the existing cluster algorithms run over words instead of
    cell values (for typos: `choclate` / `chocolate`), subject to the same
    pairwise cap and card cap as Clusters.
- Each group reuses the cluster card table (Keep, n, Exclude, Word) and
  `cluster_target_decision()`, so the keep/exclude conflict refusal comes for
  free. Apply turns the group into word fixes.
- Groups for words that already have a fix are marked, not hidden.

## Open questions

1. **Deleting a word:** allow an empty `to_word` (remove the word, then
   collapse the double space it leaves)? Proposed: not in v1.
2. **Casing:** mirror the matched word's case pattern (proposed), or always
   use `to_word` as typed?
3. **Stamp:** leave word fixes out of `norm_spec` (proposed)?
4. **Placement:** a section on the Normalize tab (proposed), or a separate
   "Word fixes" tab (which renumbers the tabs again)?
5. **Spellcheck default:** word fix for long-text columns and recode for short
   ones (proposed), or always ask?

## Non-goals (v1)

Multi-word phrases as `from_word`, regex word fixes, per-row edits, stemming the
stored data, row clustering by vocabulary (feature E).
