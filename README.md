# Recode Studio

[![tests](https://github.com/amelia-m/recode-studio/actions/workflows/tests.yml/badge.svg)](https://github.com/amelia-m/recode-studio/actions/workflows/tests.yml)

A dataset-agnostic [Shiny](https://shiny.posit.co/) app for cleaning messy
string variables — typos, capitalization noise, duplicate tokens, and
near-duplicate variants.

It serves two kinds of users:

- **Spreadsheet-comfortable, not-fluent-in-R** reviewers get a point-and-click
  way to clean values with no code at all.
- **R users** get a fast, repeatable way to *generate and update recoding
  syntax* — let the app find variant clusters and spellcheck flags, then export
  a tidy `dplyr` script and a re-importable rule CSV instead of hand-writing and
  re-editing `case_when()` blocks every time the data changes.

Load any CSV or Excel file, pick a text column, and Recode Studio helps you:

- **Browse** unique values alphabetically or grouped by string similarity.
- **Cluster** with 10 algorithms (Jaro-Winkler, Levenshtein, OSA, Cosine, Jaccard, linear $O(N)$ Key-Collision Fingerprint, N-gram Fingerprint, Soundex, Metaphone) with built-in pros/cons reference guides.
- **Match Taxonomies** against uploaded reference standard lists (e.g. ISO codes, state names, customer taxonomies).
- **Spot** likely typos via Hunspell spellcheck and rare-member-in-cluster flags.
- **Build** "old value → new value" recode rules, with optional propagation to sibling columns (e.g. `cause1`, `cause2`, ...).
- **Recategorize** across multiple variables with conditional include/exclude rule priorities.
- **Preview** exactly which cells change before committing.
- **Export** a canonical recode CSV plus a copyable R script.
- **Re-import** a previous recode CSV (even one edited in Excel) and merge it with new work, so you never start from scratch on a new batch of data.

The app **never modifies your data file.** It produces a recode CSV and an R
script that you run yourself.

## Before you get started

New to R or GitHub? This section walks you through the one-time setup. If you
already use R and Git, skip to [Quick start](#quick-start).

### 1. Install R (required)

Download and install **R** from [cran.r-project.org](https://cran.r-project.org/).
That's the engine the app runs on.

Optional but recommended — an editor that makes running R pleasant:
[**Positron**](https://positron.posit.co/) or
[**RStudio**](https://posit.co/download/rstudio-desktop/). Either lets you open
this project and click "Run" instead of typing at a console.

### 2. Get the code

You need a copy of this project on your computer. Two ways:

**Option A — Download a ZIP (easiest, no Git needed):**

1. Go to the project page:
   <https://github.com/amelia-m/recode-studio>
2. Click the green **`< > Code`** button, then **Download ZIP**.
3. Unzip it somewhere you'll remember (e.g. your Documents folder). You'll get
   a folder called `recode-studio`.

The downside: to get later updates you re-download and replace the folder.

**Option B — Clone with Git (better for staying up to date):**

[Git](https://git-scm.com/downloads) is a tool for copying and updating code.
Once it's installed, open a terminal (Command Prompt / PowerShell on Windows,
Terminal on Mac) and run:

```bash
git clone https://github.com/amelia-m/recode-studio.git
```

This creates a `recode-studio` folder. Later, to pull in updates, run
`git pull` from inside that folder — no re-downloading.

### 3. Open the project

Opening the project points your R session at the `recode-studio` folder, so the
commands in the next section "just work". Steps differ slightly by editor.

**RStudio:**

1. **File → Open Project…**
2. Browse into the `recode-studio` folder and pick **`recode-studio.Rproj`**.
3. The **Console** is the bottom-left pane — that's where you'll type commands.
4. Easiest way to launch the app: open `app.R` (click it in the **Files** pane,
   bottom-right) and click the **Run App** button at the top of the editor.

**Positron:**

1. **File → Open Folder…** and pick the `recode-studio` folder.
2. Positron starts an **R** session automatically; the **Console** tab is at the
   bottom — that's where you'll type commands. (If no R console appears, click
   the interpreter picker in the top-right and choose your R version, or open
   the Command Palette with **Ctrl/Cmd+Shift+P** → "Interpreter: Select".)
3. Easiest way to launch the app: open `app.R` and click the **Run App** /
   **▶** button at the top of the editor.

In either editor you can skip the button and just type the commands from
[Quick start](#quick-start) into the Console.

> **Tip:** "from the repo root" means your R session's working directory is the
> `recode-studio` folder. Opening the project / folder (above) handles that for
> you. Confirm with `getwd()` — it should end in `/recode-studio`.

## Quick start

This project uses [`renv`](https://rstudio.github.io/renv/) for reproducible
dependencies. On a fresh clone:

```r
renv::restore()   # installs the pinned package versions from renv.lock
shiny::runApp()   # run from the repo root
```

Without `renv`, install the direct dependencies manually:

```r
install.packages(c(
  "shiny", "bslib", "DT", "dplyr", "stringr", "readr", "tibble", "purrr",
  "tidyr", "ggplot2", "htmltools", "stringdist", "igraph", "phonics",
  "hunspell", "readxl", "clipr", "jsonlite", "rlang", "wordcloud2"
))
shiny::runApp()
```

Then on the **Data** tab, click **Load bundled example dataset** to try it
immediately, or upload your own CSV/Excel.

## How it works

1. **Data** — upload a CSV/Excel (or load the example). Every column is read as
   text. The app flags which columns look like free text (excluding dates,
   numbers, and small choice sets like Yes/No), and tags each as **short** (a
   few words / labels) or **long** (sentences / paragraphs).
2. **Variable** — a sortable, filterable table of columns (with a `kind` column:
   short / long). Click one to work on.
3. **Browse values** — frequency table of unique values; rows with duplicated
   adjacent tokens (e.g. `"oat milk oat milk"`) are highlighted.
4. **Clusters** — values grouped by similarity. Choose the metric (edit-distance:
   Jaro-Winkler, OSA, Levenshtein, longest-common-substring; token overlap:
   cosine, Jaccard q-gram; phonetic: Soundex, Metaphone), the q-gram size, and
   which normalizations to apply before clustering (lowercase, strip
   punctuation, collapse whitespace, dedupe adjacent words, ignore word order).
   Each cluster proposes the most common spelling as canonical; pick a different
   member or type your own target, then recode the rest to it — optionally
   across sibling columns. (Best for **short** columns.)
5. **Spellcheck** — Hunspell flags + clickable suggestions, a free-form
   correction box, selectable discipline dictionaries, and an "add to
   dictionary" button. (Best for **short** columns.)
6. **Text analysis** — for **long** (sentence/paragraph) columns where
   clustering and spellcheck don't help: length distribution (chars / words /
   sentences), top words and phrases (n-grams) with stopword removal,
   interactive `wordcloud2` word cloud, and keyword-in-context search.
7. **Recodes** — an editable grid of all rules with a validator (duplicate
   keys, rule chains, blanks, invalid enums/regex, stale rules).
8. **Recategorize** — derive a *new* column from term logic across several
   existing columns, instead of rewriting values in place. A row matches when
   **any** of the rule's columns contains **any** include term and **none**
   contains an exclude term; rules sharing an output column run in priority
   order, first match wins. Exports its own CSV + R script.
9. **Preview & export** — before/after diff with affected-cell counts;
   download the recode CSV and R script; import an existing CSV to merge.

## The recode CSV

`recodes_master.csv` is the canonical, re-importable rule set:

| column | meaning |
|---|---|
| `rule_id` | stable hash of (variable, match_type, old_value) |
| `variable` | column the rule targets |
| `apply_to_siblings` | `TRUE` → also apply across `sibling_pattern` |
| `sibling_pattern` | regex (e.g. `^cause[0-9]+$`); `NA` otherwise |
| `match_type` | how `old_value` is matched — see below |
| `old_value` | value (or regex pattern) to match |
| `new_value` | replacement (`<NA>` literally means "set to NA") |
| `action` | `recode` or `delete` |
| `notes`, `author`, `created_at`, `updated_at`, `source_dataset` | provenance |

NA round-trips as the literal `<NA>` so the file survives editing in Excel.

**Match types:**

- `trimmed_ci` (default) — compare after `str_squish(tolower(...))`. `" Asphyxiation "` matches `"asphyxiation"`.
- `exact_ci` — case-insensitive exact compare.
- `exact` — byte-for-byte exact compare.
- `regex` — `old_value` is an (unanchored) regular expression matched against the raw value with `grepl`; a match replaces the **whole cell** with `new_value` (no partial substitution or backreferences). Anchor with `^…$` to require a full match. An invalid pattern is flagged by Validate and matches nothing.

## The generated R

The exported `recode_<dataset>.R` is one `dplyr::case_when()` block per
sibling-pattern — **all match types share the block**, so every arm is tested
against the original column and the first matching arm wins:

```r
df <- df |>
  mutate(across(matches("^cause[0-9]+$"), function(.x) {
    case_when(
      str_squish(tolower(.x)) == "ascphyxiation" ~ "asphyxiation",
      str_squish(tolower(.x)) == "asphyziation"  ~ "asphyxiation",
      .default = .x
    )
  }))
```

Run it against a data frame named `df` to apply the recodes. The script and the
app's in-app preview are guaranteed to agree — a test evaluates the generated
script and compares it cell-for-cell against the app's own `apply_recodes()`.

## Project layout

```
recode-studio/
  app.R                    # entry point
  R/                       # helpers + modules
    string_helpers.R       # cluster / validate / apply / codegen (pure R)
    text_helpers.R         # long-text: tokens, n-grams, KWIC (pure R)
    recat_helpers.R        # recategorization logic + codegen (pure R)
    data_loader.R          # read CSV/Excel + column metadata
    ui_helpers.R
    mod_data_input.R       # upload / example loader
    mod_variable_picker.R
    mod_value_table.R
    mod_cluster_view.R
    mod_spellcheck_view.R
    mod_text_analysis.R
    mod_recode_editor.R
    mod_recategorize.R
    mod_preview_export.R
    mod_import_recodes.R
  dictionary/              # spellcheck tiers (seed / custom / user)
  inst/extdata/            # bundled example dataset
  tests/testthat/          # unit tests for the pure-R core
```

## Tests

```r
testthat::test_dir("tests/testthat")
```

## Dictionary system

Recode Studio uses [Hunspell](https://hunspell.github.io/) (`en_US`) plus
layered supplementary word lists. A token is accepted if **any** layer
recognises it.

### Tiers (always active)

| File | Scope | Git |
|---|---|---|
| `dictionary/seed_terms.txt` | Domain-neutral seed terms (ships empty) | committed |
| `dictionary/custom_terms.txt` | Project-shared additions | committed |
| `dictionary/user_terms.txt` | Personal additions | **gitignored** |

To add a word permanently for the whole team, append it to `custom_terms.txt`
and commit. To add a word just for yourself, use `user_terms.txt` (never
committed).

### Discipline dictionaries (optional)

`dictionary/disciplines/` holds domain-specific word lists. `medical.txt`,
`public_health.txt`, `public_policy.txt`, and `education.txt` ship bundled. Any
`.txt` file dropped in that folder appears as a selectable option on the
Spellcheck tab.

To add a new discipline via the UI: use the **Import discipline dictionary**
button on the Spellcheck tab — the file is copied into `dictionary/disciplines/`
and auto-selected. To share it with the team, `git add` and commit it.

To add one manually, create `dictionary/disciplines/<name>.txt` — one
lowercase term per line, `#` for comments — then restart the app.

## Contributing

1. Fork the repo and create a feature branch off `main`.
2. Run `renv::restore()` to get the pinned dependencies.
3. Make your changes. The pure-R core (`R/string_helpers.R`,
   `R/data_loader.R`) must stay Shiny-free so it remains unit-testable.
4. Run `testthat::test_dir("tests/testthat")` — all tests must pass.
5. Open a pull request against `main` with a short description of what changed
   and why.

Conventions: R + tidyverse, native pipe `|>`, 2-space indent. See
[`CLAUDE.md`](CLAUDE.md) for architecture details and gotchas.

## License

MIT. See [LICENSE](LICENSE).
