# Recode Studio

[![tests](https://github.com/amelia-m/recode-studio/actions/workflows/tests.yml/badge.svg)](https://github.com/amelia-m/recode-studio/actions/workflows/tests.yml)

A dataset-agnostic [Shiny](https://shiny.posit.co/) app for cleaning messy
string variables: typos, capitalization noise, duplicate tokens, and
near-duplicate variants.

It serves two kinds of users:

- **Spreadsheet-comfortable, not-fluent-in-R** reviewers get a point-and-click
  way to clean values with no code at all, and can download a cleaned copy of
  their data.
- **R users** get a fast, repeatable way to *generate and update recoding
  syntax*: let the app find variant clusters and spellcheck flags, then export
  a tidy `dplyr` script and re-importable rule CSVs instead of hand-writing and
  re-editing `case_when()` blocks every time the data changes.

Load any CSV or Excel file, pick a text column, and Recode Studio helps you:

- **Normalize** whole columns first: trim spaces, collapse repeated spaces, and set lower, UPPER or Title Case.
- **Browse** unique values alphabetically or grouped by string similarity.
- **Cluster** with 10 algorithms (Jaro-Winkler, Levenshtein, OSA, longest common substring, Cosine, Jaccard, linear $O(N)$ Key-Collision Fingerprint, N-gram Fingerprint, Soundex, Metaphone) with built-in pros/cons reference guides.
- **Match Taxonomies** against uploaded reference standard lists (e.g. ISO codes, state names, customer taxonomies).
- **Spot** likely typos via Hunspell spellcheck and rare-member-in-cluster flags.
- **Explore** long text: common words and phrases (with your own stopword lists), a word cloud, and which languages the entries are written in.
- **Build** "old value → new value" recode rules, with optional propagation to sibling columns (e.g. `drink1`, `drink2`, ...).
- **Recategorize** across multiple variables with conditional include/exclude rule priorities.
- **Preview** exactly which cells change, and which rules did nothing, before exporting.
- **Export** the rule CSVs plus a copyable R script, and optionally a zip with a cleaned copy of the data.
- **Re-import** previous rule CSVs (even ones edited in Excel) and merge them with new work, so you never start from scratch on a new batch of data.

The app **never changes your original data file.** It reads a copy. What you
download is separate: the rule files, an R script you can run yourself, and,
if you ask for it, a cleaned copy of the data as a new file.

## Before you get started

New to R or GitHub? This section walks you through the one-time setup. If you
already use R and Git, skip to [Quick start](#quick-start).

### 1. Install R (required)

Download and install **R** from [cran.r-project.org](https://cran.r-project.org/).
That's the engine the app runs on.

Optional but recommended: an editor that makes running R pleasant,
[**Positron**](https://positron.posit.co/) or
[**RStudio**](https://posit.co/download/rstudio-desktop/). Either lets you open
this project and click "Run" instead of typing at a console.

### 2. Get the code

You need a copy of this project on your computer. Two ways:

**Option A: Download a ZIP (easiest, no Git needed)**

1. Go to the project page:
   <https://github.com/amelia-m/recode-studio>
2. Click the green **`< > Code`** button, then **Download ZIP**.
3. Unzip it somewhere you'll remember (e.g. your Documents folder). You'll get
   a folder called `recode-studio`.

The downside: to get later updates you re-download and replace the folder.

**Option B: Clone with Git (better for staying up to date)**

[Git](https://git-scm.com/downloads) is a tool for copying and updating code.
Once it's installed, open a terminal (Command Prompt / PowerShell on Windows,
Terminal on Mac) and run:

```bash
git clone https://github.com/amelia-m/recode-studio.git
```

This creates a `recode-studio` folder. Later, to pull in updates, run
`git pull` from inside that folder. No re-downloading.

### 3. Open the project

Opening the project points your R session at the `recode-studio` folder, so the
commands in the next section "just work". Steps differ slightly by editor.

**RStudio:**

1. **File → Open Project…**
2. Browse into the `recode-studio` folder and pick **`recode-studio.Rproj`**.
3. The **Console** is the bottom-left pane. That's where you'll type commands.
4. Easiest way to launch the app: open `app.R` (click it in the **Files** pane,
   bottom-right) and click the **Run App** button at the top of the editor.

**Positron:**

1. **File → Open Folder…** and pick the `recode-studio` folder.
2. Positron starts an **R** session automatically; the **Console** tab is at the
   bottom. That's where you'll type commands. (If no R console appears, click
   the interpreter picker in the top-right and choose your R version, or open
   the Command Palette with **Ctrl/Cmd+Shift+P** → "Interpreter: Select".)
3. Easiest way to launch the app: open `app.R` and click the **Run App** /
   **▶** button at the top of the editor.

In either editor you can skip the button and just type the commands from
[Quick start](#quick-start) into the Console.

> **Tip:** "from the repo root" means your R session's working directory is the
> `recode-studio` folder. Opening the project / folder (above) handles that for
> you. Confirm with `getwd()`: it should end in `/recode-studio`.

## Quick start

This project uses [`renv`](https://rstudio.github.io/renv/) for reproducible
dependencies. On a fresh clone:

```r
renv::restore()   # installs the pinned package versions from renv.lock
shiny::runApp()   # run from the repo root
```

> **If `shiny::runApp()` says "there is no package called ..."** right after a
> successful restore, run `renv::repair()` and try again. Don't click the
> editor's "Install" button in the error pane - it installs outside `renv` and
> hides the real problem.

Without `renv`, install the direct dependencies manually:

```r
install.packages(c(
  "shiny", "bslib", "DT", "dplyr", "stringr", "readr", "tibble", "purrr",
  "tidyr", "ggplot2", "htmltools", "cld2", "stringdist", "igraph",
  "phonics", "hunspell", "readxl", "clipr", "jsonlite", "rlang",
  "wordcloud2", "digest", "zip"
))
shiny::runApp()
```

`cld2` powers the language breakdown on the Text analysis tab. If it will not
install on your machine, the rest of the app still works and that section
tells you it is unavailable.

Then on the **Data** tab, click **Load bundled example dataset** to try it
immediately, or upload your own CSV/Excel.

## How it works

The tabs run left to right in the order you would normally work. The column
you are working on is shown in the **Recoding:** bar above the tabs; you can
change it there from any tab, or by clicking a row on the Variable tab. The two
always agree.

1. **Data**: upload a CSV/Excel (or load the example). Every column is read as
   text. The app flags which columns look like free text (excluding dates,
   numbers, and small choice sets like Yes/No), and tags each as **short** (a
   few words / labels) or **long** (sentences / paragraphs).
2. **Variable**: a sortable, filterable table of columns (with a `kind` column:
   short / long). Click one to work on.
3. **Normalize**: tidy how *every* value in a column is written, before
   anything else happens. Tick what to do (trim spaces, collapse repeated
   spaces, lower / UPPER / Title Case), read the before / after list, then add
   the rule. You can apply the same rule to sibling columns (e.g. `drink1`,
   `drink2`). Things to know:
   - Normalizations always run **first**, then recodes, then
     recategorization. The Browse values, Clusters and Spellcheck tabs show
     the normalized values.
   - Normalizing **cannot be undone**: Title Case turns "McKenzie" into
     "Mckenzie". That is why the preview is shown before you add the rule.
   - One rule per column. Adding a column again replaces its rule.
   - This is **not** the same as the "normalize before clustering" options on
     the Clusters tab. Those only change how values are grouped for display
     and never change a value; a Normalize rule rewrites the column.
4. **Browse values**: frequency table of unique values; rows with duplicated
   adjacent tokens (e.g. `"oat milk oat milk"`) are highlighted. Long values are
   shortened to one line; hover to read the whole value. A value with no text
   at all is listed as an outlined, italic **(empty)**, and a rule made from it
   targets empty cells. Note that blank cells in a CSV or Excel file are read
   as **missing**, not as empty text, so they usually do not appear here.
5. **Clusters**: values grouped by similarity. Choose the metric (edit-distance:
   Jaro-Winkler, OSA, Levenshtein, longest-common-substring; token overlap:
   cosine, Jaccard q-gram; fingerprints: key collision, n-gram; phonetic:
   Soundex, Metaphone), the q-gram size, and which normalizations to apply
   before clustering (lowercase, strip punctuation, collapse whitespace, dedupe
   adjacent words, ignore word order). Each cluster proposes the most common
   spelling as canonical; pick a different member or type your own target,
   then recode the rest to it, optionally across sibling columns. A second
   sub-tab matches the column against a reference list. (Best for **short**
   columns.) Blank values are left out. On a column with more than 1,000
   different values, the algorithms that compare every value with every other
   (Jaro-Winkler, OSA, Levenshtein, longest common substring, cosine, Jaccard)
   use only the 1,000 most frequent and say so; key collision, n-gram
   fingerprint, Soundex and Metaphone stay fast on any number of values. At
   most 50 clusters are shown at once, largest first.
6. **Spellcheck**: Hunspell flags + clickable suggestions, a free-form
   correction box, selectable discipline dictionaries, and an "add to
   dictionary" button. (Best for **short** columns.) Picking a correction
   replaces **only the misspelled word**: `iced capuccino` becomes
   `iced cappuccino`, not `cappuccino`. Capitals follow the original word
   (`CAPUCCINO` becomes `CAPPUCCINO`). The rule matches that value **exactly
   as written**, so a differently capitalized copy elsewhere in the column is
   left alone. If the English dictionary cannot be loaded, the tab says
   spellcheck is unavailable.
7. **Text analysis**: for **long** (sentence/paragraph) columns where
   clustering and spellcheck don't help: length distribution (chars / words /
   sentences), top words and phrases (n-grams) with stopword removal,
   interactive `wordcloud2` word cloud, a language breakdown, and
   keyword-in-context search. This tab reads the values as they are in your
   file, before any normalization, and creates no rules.
   - **Stopwords** (common words such as "the" and "and") are left out of the
     word and phrase tables and the word cloud. Tick which lists to use: a
     standard English list, a project list shared with your team, and a
     personal list. Press **+ Stop** next to a word to add it to the project
     or personal list; it disappears from the views straight away. See
     [Stopword lists](#stopword-lists).
   - **Language** shows how many entries are in each language and lists the
     entries not detected as English. Entries shorter than 20 characters are
     counted as undetected rather than guessed.
8. **Recodes**: an editable grid of all rules with a **Validate** button.
   Rules apply in a single pass: each cell is claimed by the first rule that
   matches it. Editing a rule keeps it in its place in the list. If you
   change a rule's old value or match type so that it becomes identical to
   another rule, the edit is refused and the message names the other rule.
   Validate lists, without changing anything:
   - rules that clash, chain into each other, or have a blank new value;
   - a match type, action or regular expression that is not valid;
   - a **sibling pattern that is not a valid regular expression**. Such a rule
     is skipped, in the app and in the exported script, until you fix it. The
     grid refuses an invalid pattern when you type one, and clearing the
     pattern makes the rule apply to its own column only;
   - a **rule id that no longer matches its rule** (saved before an edit, or
     written by another tool). It still applies, but delete and re-make it to
     fix the id;
   - **older Spellcheck rules that may have dropped words** (from before the
     fix above), so you can review them;
   - rules whose old value is no longer in the data.

   A rule made by Spellcheck cannot be switched to `regex`.
9. **Recategorize**: derive a *new* column from term logic across several
   existing columns, instead of rewriting values in place. A row matches when
   **any** of the rule's columns contains **any** include term and **none**
   contains an exclude term; rules sharing an output column run in priority
   order, first match wins. The match counts are taken after normalizations
   and recodes. If you change the form after clicking Preview, the old result
   is greyed out and marked **Out of date** until you preview again. **Add**
   refuses a rule that would match nothing (no include terms, an unknown
   match type, or a term or sibling pattern that is not a valid regular
   expression), and a panel lists any problems with the saved rules. Exports
   its own CSV + R script.
10. **Preview & export**:
    - Three counts per rule: **matched** (cells the rule hit), **changed**
      (cells whose value actually differs afterwards) and **shadowed** (cells
      the rule hit that an earlier rule had already claimed).
    - A yellow box listing rules that did less than expected: a normalization
      or recode that changed nothing, a recode written under a different
      normalization than the column has now, or a recategorization rule that
      lost rows because a recode rewrote the words it looks for.
    - **Notes**, kept apart from the warnings in a collapsed block: a rule
      written under a different normalization whose match type already
      ignores the difference (for example a case-insensitive rule when only
      the capitalization changed). These cannot change the result; nothing
      needs fixing. Notes do not hide the "No problems found" line.
    - A before / after diff, and downloads for the recodes CSV, the
      normalizations CSV and the R script.
    - **Download cleaned data**: a zip with a cleaned copy of your data (see
      [Cleaned data download](#cleaned-data-download)).
    - Import an existing recodes or normalizations CSV to merge. Two recode
      rules count as the same when they have the same column, match type and
      old value, even if their stored ids differ. An incoming rule with an
      invalid sibling pattern is imported but marked **will not apply**.

## The recode CSV

`recodes_master.csv` is the canonical, re-importable rule set:

| column | meaning |
|---|---|
| `rule_id` | stable hash of (variable, match_type, old_value) |
| `variable` | column the rule targets |
| `apply_to_siblings` | `TRUE` → also apply across `sibling_pattern` |
| `sibling_pattern` | regex (e.g. `^drink[0-9]+$`); `NA` otherwise |
| `match_type` | how `old_value` is matched (see below) |
| `old_value` | value (or regex pattern) to match |
| `new_value` | replacement (`<NA>` literally means "set to NA") |
| `action` | `recode` or `delete` |
| `notes`, `author`, `created_at`, `updated_at`, `source_dataset` | provenance |
| `norm_spec` | the column normalization in force when the rule was made (empty = none) |

NA round-trips as the literal `<NA>` so the file survives editing in Excel.
Files saved before `norm_spec` existed still import; their rules are simply
unstamped. Import refuses a file with columns it doesn't recognise, rather than
quietly corrupting your rules.

**Match types:**

- `trimmed_ci` (default): compare after `str_squish(tolower(...))`. `" Cappuccino "` matches `"cappuccino"`.
- `exact_ci`: case-insensitive exact compare.
- `exact`: byte-for-byte exact compare.
- `regex`: `old_value` is an (unanchored) regular expression matched against the value with `grepl`; a match replaces the **whole cell** with `new_value` (no partial substitution or backreferences). Anchor with `^…$` to require a full match. An invalid or empty pattern is flagged by Validate and matches nothing; use `^$` to match empty cells. The app and the exported script use the same regular-expression engine (base R's), so a pattern behaves the same in both.

Rules are matched against the values **after** column normalizations. An
`exact` or `regex` rule written against the raw values may stop matching once
that column is normalized (an `exact` rule for `"latte"` misses a column that
is now `"Latte"`). `norm_spec` is how the app notices: Preview & export warns
about any rule whose column has a different normalization now.

## The normalizations CSV

`normalizations_master.csv` holds the Normalize tab's rules, one per column
(or sibling family):

| column | meaning |
|---|---|
| `norm_id` | stable hash of the target (variable + sibling pattern) |
| `variable` | column the rule targets |
| `apply_to_siblings`, `sibling_pattern` | as in the recode CSV |
| `ops` | what to do, `;`-separated: `trim`, `squish`, `lowercase`, `uppercase`, `titlecase` |
| `notes`, `author`, `created_at`, `updated_at`, `source_dataset` | provenance |

Blank cells mean "not set". Import it on Preview & export, next to the recode
CSV.

## The generated R

The exported `recode_<dataset>.R` runs the **normalization steps first**, one
`mutate()` per normalization rule, then the recodes. The recodes are one
`dplyr::case_when()` block per sibling-pattern. **All match types share the
block**, so every arm is tested against the column as the normalizations left
it, and the first matching arm wins:

```r
df <- df |>
  mutate(across(matches("^drink[0-9]+$", ignore.case = FALSE), function(.x) str_squish(.x)))

df <- df |>
  mutate(across(matches("^drink[0-9]+$", ignore.case = FALSE), function(.x) {
    case_when(
      str_squish(tolower(.x)) == "capuccino" ~ "cappuccino",
      str_squish(tolower(.x)) == "capccino"  ~ "cappuccino",
      .default = .x
    )
  }))
```

A regex rule becomes a `grepl("<pattern>", <col>)` arm. A rule whose sibling
pattern is not a valid regular expression gets a `# SKIPPED` comment instead
of code, because the app skips it too.

Run it against a data frame named `df` to apply the normalizations and
recodes. The script and the app's in-app preview are guaranteed to agree: a
test evaluates the generated script and compares it cell-for-cell against the
app's own results.

## Cleaned data download

On **Preview & export**, **Download cleaned data (zip)** gives you one zip
file. Your original file is not touched. Inside:

| file | what it is |
|---|---|
| `<name>_cleaned.csv` | the cleaned copy of your data: normalized, then recoded, then recategorized |
| `normalizations_master.csv` | the normalization rules that were applied |
| `recodes_master.csv` | the recode rules that were applied |
| `recat_<name>.csv` | the recategorization rules that were applied (present even if there are none) |
| `clean_<name>.R` | one R script that repeats the whole cleaning on your original file |
| `provenance.txt` | a plain-language record: which file was cleaned (name only, no folder), its size and fingerprint, when, rows and columns before and after, how many rules, and any warnings (notes listed separately) |
| `provenance.json` | the same record, for software |

Cleaning never adds or removes rows; if it somehow did, the export stops
instead of producing a file.

**About the cleaned CSV:** text is UTF-8, and a missing value is an **empty
cell** (no `NA` text), which spreadsheets, R and Python all read as missing.
The trade-off: in this file an empty text value and a missing value look the
same.

**To repeat the cleaning later** (for example, to show exactly how a file was
produced):

1. Put your original data file in the same folder as `clean_<name>.R`, or edit
   the `input_file` line at the top of the script.
2. Open the script in R, Positron or RStudio and run the whole thing (Source).
3. It writes `<name>_cleaned_reproduced.csv` next to the original file. That
   file matches `<name>_cleaned.csv` byte for byte (a test checks this).

The script warns you if the file it reads is not byte-for-byte the one the
rules were exported from.

## Project layout

```
recode-studio/
  app.R                    # entry point
  R/                       # helpers + modules
    string_helpers.R       # cluster / validate / apply / codegen, normalizations (pure R)
    text_helpers.R         # long-text: tokens, n-grams, KWIC, languages (pure R)
    stopword_helpers.R     # stopword lists for Text analysis (pure R)
    recat_helpers.R        # recategorization logic + codegen (pure R)
    rule_warnings.R        # "rules that did nothing" checks (pure R)
    export_bundle.R        # cleaned data zip + provenance + script (pure R)
    data_loader.R          # read CSV/Excel + column metadata
    rule_proxies.R         # in-session rule stores
    ui_helpers.R           # help text, empty states, table helpers
    mod_data_input.R       # upload / example loader
    mod_variable_picker.R  # Variable tab + "Recoding:" bar
    mod_normalize.R
    mod_value_table.R
    mod_cluster_view.R
    mod_spellcheck_view.R
    mod_text_analysis.R
    mod_recode_editor.R
    mod_recategorize.R
    mod_preview_export.R
    mod_import_recodes.R
  dictionary/              # spellcheck tiers (seed / custom / user)
  stopwords/               # Text analysis stopword tiers (base / custom / user)
  inst/extdata/            # bundled example dataset
  tests/testthat/          # unit and module tests
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
button on the Spellcheck tab. The file is copied into `dictionary/disciplines/`
and auto-selected. To share it with the team, `git add` and commit it.

To add one manually, create `dictionary/disciplines/<name>.txt` (one
lowercase term per line, `#` for comments), then restart the app.

## Stopword lists

The Text analysis tab leaves stopwords out of its word and phrase tables and
its word cloud. Like the dictionaries, they come in tiers, and a word is left
out if **any** ticked list has it:

| File | List | Git |
|---|---|---|
| `stopwords/base_stopwords.txt` | Standard English stopwords | committed |
| `stopwords/custom_stopwords.txt` | Project additions (**+ Stop**, target Project) | committed |
| `stopwords/user_stopwords.txt` | Personal additions (**+ Stop**, target User) | **gitignored** |

To share a project word with the team, commit `custom_stopwords.txt`. The app
never changes the base list. Stopwords only change what the Text analysis tab
counts; they never change your data, rules or exports. See
[stopwords/README.md](stopwords/README.md).

## Contributing

1. Fork the repo and create a feature branch off `main`.
2. Run `renv::restore()` to get the pinned dependencies.
3. Make your changes. The pure-R core (`R/string_helpers.R`,
   `R/text_helpers.R`, `R/stopword_helpers.R`, `R/recat_helpers.R`, `R/rule_warnings.R`,
   `R/export_bundle.R`, `R/data_loader.R`) must stay Shiny-free so it remains
   unit-testable.
4. Run `testthat::test_dir("tests/testthat")`. All tests must pass.
5. Open a pull request against `main` with a short description of what changed
   and why.

Conventions: R + tidyverse, native pipe `|>`, 2-space indent, no em dashes. See
[`CLAUDE.md`](CLAUDE.md) for architecture details and gotchas.

## License

MIT. See [LICENSE](LICENSE).
