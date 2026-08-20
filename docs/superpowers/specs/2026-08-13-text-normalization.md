# Text normalization & corpus analysis — design spec

**Date:** 2026-08-13
**Origin:** `ext/tokenization_reference.Rmd` + accompanying note
on tokenization / normalization (conflation → stemming → lemmatization).

## Problem

The reference Rmd hand-writes ~20 normalization rules against a Reddit comment corpus:

```r
clean = str_replace_all(clean, "duties", "duty"),
clean = str_replace_all(clean, "feels",  "feel"),
clean = str_replace_all(clean, "talked", "talk"),
```

This is Recode Studio's job, done by hand, because the app cannot express it:

1. **Recode rules replace the whole cell.** The reference pipeline needs to rewrite a *token
   inside* a sentence. No existing `match_type` does that — `regex` matches
   unanchored but still replaces the entire cell.
2. **Nothing suggests the pairs.** He derived `duties → duty` by eye. The
   cluster machinery already solves that shape; it is just pointed at unique
   cell values rather than at tokens.

His hand-rolled version is also unsafe: `str_replace_all` has no word
boundaries, so `"made" → "make"` also rewrites *homemade* → *homemake*, and
`"used" → "use"` turns *unused* into *unuse*.

Two further capabilities in his script are missing here: per-value language
detection (`cld2::detect_language`, line 51) and corpus-level document
clustering (TF-IDF + `hclust`, lines 265-335).

## Scope

Five features, sequenced so later ones can be dropped without stranding
earlier ones.

| id | feature | depends on |
|---|---|---|
| A | Language detection per value | — |
| D | Custom stopword tiers | — |
| C | Token / stem clustering | D |
| B | Token-level recode rules | C |
| E | Document TF-IDF clustering | D |

E is built **inside the Text analysis tab**, not as a separate tool: it needs
the loader, variable picker, tokenizer and D's stopword tiers, and duplicating
those to split it out buys nothing. If it later grows into topic modelling
(`textmineR`'s actual purpose — the reference script stops just short of it), that is
the point where it earns its own tool.

## Dependencies

**One new package: `cld2`** (feature A). Compiled, on CRAN with Windows
binaries, ~1 MB.

Explicitly NOT taken, with reasons:

- `SnowballC` — `hunspell::hunspell_stem()` already returns dictionary-backed
  stems and `hunspell` is already a dependency. Snowball stems more
  aggressively but produces non-words (`operational` → `oper`); dictionary
  lemmas are the better fit for a user-facing "pick the target" UI.
- `textmineR` / `tm` — feature E needs a term-document matrix, TF-IDF, and
  `hclust`. `Matrix` ships with R, `stats::hclust`/`cutree` are base. The heavy
  dependency only pays for itself at topic-model scale.
- `stopwords` — `EN_STOPWORDS` already exists in `text_helpers.R`; feature D
  layers user-editable tiers on top of it.
- `wordcloud` / `ggwordcloud` — a ranked frequency table is more useful than a
  word cloud and the app already renders those.

## Feature specs

### A — Language detection

`detect_languages(values)` wraps `cld2::detect_language()` and returns an
ISO-639-1 code or `NA` per value. Surfaced in two places:

- **Text analysis tab** — a language breakdown table (code, n, share) plus a
  "Show only non-English values" toggle over a values table.
- **`build_meta()`** — a new `lang_share_en` column (share of non-missing
  values detected as English; `NA` when detection is unavailable).

`cld2` is treated as OPTIONAL at runtime: if `requireNamespace("cld2")` fails,
`detect_languages()` returns all-`NA` and the UI shows an install hint rather
than erroring. This keeps the tab alive on installs that skip the compiled
package.

Detection is unreliable on very short strings; values under 20 characters are
reported as `NA` rather than guessed.

### D — Custom stopword tiers

Mirrors the existing dictionary tier system:

```
stopwords/
  base_stopwords.txt      committed, seeded from EN_STOPWORDS
  custom_stopwords.txt    committed, project-shared
  user_stopwords.txt      GITIGNORED, per-user
```

A token is a stopword if any active tier lists it. The Text analysis tab gains
a control to add the selected token to a chosen tier (project or user),
matching the Spellcheck tab's `+ Dict` pattern.

`EN_STOPWORDS` stays in `text_helpers.R` as the seed content and the fallback
when the `stopwords/` directory is absent, so the pure-R helpers keep working
when sourced from a script outside the repo.

### C — Token / stem clustering

Runs the *existing* `cluster_strings()` machinery over the token vocabulary of
a long-text column instead of over unique cell values, plus a stem-based
grouping mode.

`token_vocabulary(values, remove_stopwords, min_chars)` → `tibble(token, n)`.

`stem_groups(tokens)` → `tibble(token, stem, n)` using `hunspell_stem()`, with
a pure suffix-stripping fallback (`.suffix_stem()`) for tokens the dictionary
does not know. Groups of size 1 are dropped.

The UI reuses the cluster-card pattern from `mod_cluster_view`: one card per
group, radio to pick the target token, per-member exclude, and the same
conflict refusal from `cluster_target_decision()`. Applying a group emits
**token rules** (feature B).

Mode selector: **stem groups** (dictionary lemma) or **similarity** (the
existing 8 algorithms over tokens, for typos rather than inflections).

### B — Token-level recode rules

A fifth `match_type`: **`token`**.

Semantics: `old_value` is a single token, matched **word-boundary anchored and
case-insensitively** inside the cell; `new_value` replaces just that token. The
rest of the cell — punctuation, capitalization of other words, whitespace — is
preserved.

**The composition problem.** `apply_recodes()` assigns a scalar to hit cells
(`df[[col]][hit] <- new_val`) and carries a `claimed` mask so the first
matching rule wins. That is correct for whole-cell rules and wrong for tokens:
The reference pipeline applies twenty stem rules to the *same* comment, and each must land.

**Resolution — one token pass, not twenty.** All `token` rules targeting a
column are collected into a single lookup table and applied in ONE
`str_replace_all()` over an alternation of every token, with a function
replacement:

```r
.tok_map <- c("duties" = "duty", "feels" = "feel", "talked" = "talk")
str_replace_all(x, regex("\\b(duties|feels|talked)\\b", ignore_case = TRUE),
                function(m) .tok_map[[tolower(m)]])
```

This is genuinely single-pass — each source token is examined once and mapped
once, so a rule can never rewrite another rule's output — while still letting
every rule affect the same cell. It preserves the invariant rather than
carving out an exception to it.

Ordering: the token pass runs **after** all whole-cell rules on that column, so
whole-cell rules still see original values. Both engines do this identically.

`claimed` semantics: a cell rewritten by a whole-cell rule is claimed and the
token pass skips it. A cell touched only by the token pass is claimed
afterwards. `cells_matched` / `cells_changed` / `cells_shadowed` are reported
per token rule by counting cells whose specific token was present.

Validation: a `token` rule whose `old_value` contains whitespace or regex
metacharacters is rejected by `validate_recodes()` as `invalid_token` — tokens
are single words by construction.

**The agreement test must be extended to cover token rules.** That test is the
guard for the whole apply↔codegen contract; a new match type that is not in it
is a regression waiting to happen.

### E — Document clustering

Operates on rows (documents) rather than values.

- `build_dtm(values, remove_stopwords, min_chars, min_docs)` → term-document
  matrix (`Matrix::sparseMatrix`), terms appearing in fewer than `min_docs`
  documents dropped (reference script uses `> 3`).
- `tfidf_matrix(dtm)` → L2-normalized TF-IDF.
- `cluster_documents(tfidf, k)` → `hclust` on cosine distance with
  `method = "ward.D2"` (reference choice), cut to `k` clusters.
- `cluster_top_terms(dtm, clustering, n)` → per-cluster distinctive terms by
  the lift measure used in the reference script: in-cluster term share minus corpus term share.

UI: a `k` slider, a cluster summary table (cluster, size, top terms), and a
per-cluster document viewer. Analysis output only — no rules, no export.
Guarded by a row-count ceiling with an explicit message, since the cosine
similarity step is O(n²) in documents.

## Non-goals

- Topic modelling (LDA). The growth path, not this work.
- Stemming the stored data. Recode Studio never modifies source data; token
  rules are exported as script + CSV like every other rule.
- Sentiment analysis. The reference script has it commented out; no user asked.
- Language *translation* — detection only.
