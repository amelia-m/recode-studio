# Recode Studio

A dataset-agnostic [Shiny](https://shiny.posit.co/) app for cleaning messy
string variables — typos, capitalization noise, duplicate tokens, and
near-duplicate variants — without writing R by hand. Built for people who are
comfortable in spreadsheets but are not fluent R users.

Load any CSV or Excel file, pick a text column, and Recode Studio helps you:

- **Browse** unique values alphabetically or grouped by string similarity.
- **Spot** likely typos via Hunspell spellcheck and rare-member-in-cluster flags.
- **Build** "old value → new value" recode rules, with optional propagation to
  sibling columns (e.g. `cause1`, `cause2`, ...).
- **Preview** exactly which cells change before committing.
- **Export** a canonical recode CSV plus a copyable R script.
- **Re-import** a previous recode CSV (even one edited in Excel) and merge it
  with new work, so you never start from scratch on a new batch of data.

The app **never modifies your data file.** It produces a recode CSV and an R
script that you run yourself.

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
  "tidyr", "stringdist", "igraph", "hunspell", "readxl",
  "clipr", "jsonlite", "rlang"
))
shiny::runApp()
```

Then on the **Data** tab, click **Load bundled example dataset** to try it
immediately, or upload your own CSV/Excel.

## How it works

1. **Data** — upload a CSV/Excel (or load the example). Every column is read as
   text. The app flags which columns look like free text (excluding dates,
   numbers, and small choice sets like Yes/No).
2. **Variable** — a sortable, filterable table of columns. Click one to clean.
3. **Browse values** — frequency table of unique values; rows with duplicated
   adjacent tokens (e.g. `"asphyxia asphyxia"`) are highlighted.
4. **Clusters** — values grouped by similarity (Jaro-Winkler / OSA / Soundex).
   Each cluster suggests the most common spelling as canonical; one click
   recodes the rest to it. Optionally applies across sibling columns.
5. **Spellcheck** — Hunspell flags + clickable suggestions, a free-form
   correction box, and an "add to dictionary" button.
6. **Recodes** — an editable grid of all rules with a validator (duplicate
   keys, rule chains, blanks, stale rules).
7. **Preview & export** — before/after diff with affected-cell counts;
   download the recode CSV and R script; import an existing CSV to merge.

## The recode CSV

`recodes_master.csv` is the canonical, re-importable rule set:

| column | meaning |
|---|---|
| `rule_id` | stable hash of (variable, match_type, old_value) |
| `variable` | column the rule targets |
| `apply_to_siblings` | `TRUE` → also apply across `sibling_pattern` |
| `sibling_pattern` | regex (e.g. `^cause[0-9]+$`); `NA` otherwise |
| `match_type` | `exact` / `exact_ci` / `trimmed_ci` (default) / `regex` |
| `old_value` | value to match |
| `new_value` | replacement (`<NA>` literally means "set to NA") |
| `action` | `recode` or `delete` |
| `notes`, `author`, `created_at`, `updated_at`, `source_dataset` | provenance |

NA round-trips as the literal `<NA>` so the file survives editing in Excel.

## The generated R

The exported `recode_<dataset>.R` is a plain `dplyr::case_when()` block per
(variable, sibling-pattern), e.g.:

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

Run it against a data frame named `df` to apply the recodes.

## Project layout

```
recode-studio/
  app.R                    # entry point
  R/                       # helpers + modules
    string_helpers.R       # cluster / validate / apply / codegen (pure R)
    data_loader.R          # read CSV/Excel + column metadata
    ui_helpers.R
    mod_data_input.R       # upload / example loader
    mod_variable_picker.R
    mod_value_table.R
    mod_cluster_view.R
    mod_spellcheck_view.R
    mod_recode_editor.R
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

## License

MIT. See [LICENSE](LICENSE).
