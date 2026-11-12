# =============================================================================
# Recode Studio - standalone Shiny app
# =============================================================================
# A dataset-agnostic tool for cleaning string variables: load any CSV/Excel,
# pick a text column, normalize its case / whitespace, browse values
# alphabetically or as similarity clusters, spellcheck them, build recode
# rules, and export rule CSVs + an R script.
#
# Order of operations everywhere: normalizations, then recodes, then recat.
#
# Run with:
#   shiny::runApp()        # from the repo root
# =============================================================================

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(purrr)

# Source all helpers + modules.
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)


# --- UI ----------------------------------------------------------------------

ui <- bslib::page_navbar(
  id    = "main_nav",
  title = "Recode Studio",
  theme = bslib::bs_theme(version = 5, preset = "flatly"),
  navbar_options = bslib::navbar_options(bg = "#2c3e50", underline = TRUE),

  bslib::nav_panel(
    title = "Recode Studio",
    mod_variable_banner_ui("variable_banner"),
    bslib::navset_card_tab(
      id = "studio_tabs",
      # Normalize sits before the value tabs because that is the order rules
      # are APPLIED in (normalize, recode, recat): Browse values, Clusters and
      # Spellcheck show normalized values, so recodes are written against them.
      bslib::nav_panel("1. Data",             value = "data",       mod_data_input_ui("data_input")),
      bslib::nav_panel("2. Variable",         value = "variable",   mod_variable_picker_ui("variable_picker")),
      bslib::nav_panel("3. Normalize",        value = "normalize",  mod_normalize_ui("normalize")),
      bslib::nav_panel("4. Browse values",    value = "browse",     mod_value_table_ui("value_table")),
      bslib::nav_panel("5. Clusters",         value = "clusters",   mod_cluster_view_ui("cluster_view")),
      bslib::nav_panel("6. Spellcheck",       value = "spellcheck", mod_spellcheck_view_ui("spellcheck_view")),
      bslib::nav_panel("7. Text analysis",    value = "text",       mod_text_analysis_ui("text_analysis")),
      bslib::nav_panel("8. Recodes",          value = "recodes",    mod_recode_editor_ui("recode_editor")),
      bslib::nav_panel("9. Recategorize",     value = "recat",      mod_recategorize_ui("recategorize")),
      bslib::nav_panel("10. Preview & export", value = "export",
        mod_preview_export_ui("preview_export"),
        br(),
        mod_import_recodes_ui("import_recodes")
      )
    )
  ),
  bslib::nav_spacer(),
  bslib::nav_item(
    tags$a("GitHub", href = "https://github.com/amelia-m/recode-studio", target = "_blank",
           style = "color:#fff;")
  )
)


# --- Server ------------------------------------------------------------------

server <- function(input, output, session) {

  # App-wide state. mod_data_input writes data/meta/dataset_name/source_meta;
  # everything else reads. `source_meta` describes the loaded file (base name,
  # sheet, size, SHA-256, load time; source_file_meta()) for the export
  # bundle's provenance record.
  shared_state <- shiny::reactiveValues(
    data         = NULL,
    meta         = NULL,
    dataset_name = NULL,
    source_meta  = NULL
  )

  # In-session rule sets + proxies. Recategorization rules live in their OWN
  # store: they derive a new column instead of rewriting values, so they share
  # neither the schema nor the export path with the old->new recodes.
  # Normalizations are a third store: whole-column case / whitespace tidying,
  # applied before both of the others.
  rv <- shiny::reactiveValues(
    rules = empty_recodes_tibble(),
    recat = empty_recat_tibble(),
    norms = empty_normalizations_tibble()
  )
  norms_proxy <- make_norms_proxy(rv)
  # add() replaces a colliding rule IN PLACE (see R/rule_proxies.R): rules are
  # first-match-wins by row order, so moving an edited rule to the end could
  # change the output. add() also stamps each new rule's norm_spec with the
  # normalization in force for its columns, so a later change is detectable.
  rules_proxy <- make_rules_proxy(rv,
    norms_r     = norms_proxy$get,
    data_cols_r = function() names(shared_state$data))
  recat_proxy <- make_recat_proxy(rv)

  mod_data_input_server("data_input", shared_state)

  # The variable being recoded: one shared selection, changed from the table on
  # tab 2 or the "Recoding:" banner above the tabs (R/mod_variable_picker.R).
  # Every per-variable module reads selected_var_r.
  selected_var_r <- variable_selection_server(shared_state,
    picker_id = "variable_picker", banner_id = "variable_banner")

  mod_normalize_server("normalize", shared_state,
    selected_var_r = selected_var_r, norms_proxy = norms_proxy)

  # The loaded data with the session's normalizations applied: the frame the
  # recodes are written against and run on. Falls back to the raw data if the
  # normalizations cannot be applied.
  normalized_r <- shiny::reactive({
    d <- shared_state$data
    if (is.null(d)) return(NULL)
    nn <- norms_proxy$get()
    if (nrow(nn) == 0) return(d)
    tryCatch(apply_normalizations(d, nn)$df, error = function(e) d)
  })

  # Distinct values of the selected column AFTER normalization, so Browse
  # values, Clusters and Spellcheck show (and mint rules against) normalized
  # values.
  #
  # The EMPTY value "" is kept (including values a normalization squished down
  # to ""), so a rule can target empty cells from Browse values: "" to
  # "Unknown", or to missing with action delete. Clusters and Spellcheck skip
  # it themselves. Missing values (NA) stay out: a rule targets those with an
  # NA old_value (`<NA>` in the recodes CSV).
  # value_frequency_table() lives in R/mod_value_table.R so it can be tested.
  unique_values_r <- shiny::reactive({
    value_frequency_table(normalized_r(), selected_var_r())
  })

  mod_value_table_server("value_table", shared_state,
    selected_var_r = selected_var_r, unique_values_r = unique_values_r,
    rules_proxy = rules_proxy)

  mod_cluster_view_server("cluster_view", shared_state,
    selected_var_r = selected_var_r, unique_values_r = unique_values_r,
    rules_proxy = rules_proxy)

  mod_spellcheck_view_server("spellcheck_view", shared_state,
    selected_var_r = selected_var_r, unique_values_r = unique_values_r,
    rules_proxy = rules_proxy)

  # Text analysis deliberately reads the RAW values (shared_state$data), not
  # normalized_r(): it is descriptive (lengths, tokens, n-grams, keyword in
  # context) and should describe the file as delivered.
  mod_text_analysis_server("text_analysis", shared_state, selected_var_r)

  mod_recode_editor_server("recode_editor", shared_state, rules_proxy)

  # The exported recat script is meant to run AFTER the recode script (which
  # itself normalizes first), so the Recategorize tab previews against the
  # normalized-then-recoded frame, not the raw one. `df_before_recodes` is the
  # normalized frame, used to spot rows the recodes took away.
  recode_basis_r <- shiny::reactive({
    d <- shared_state$data
    if (is.null(d)) return(NULL)
    r  <- rules_proxy$get()
    dn <- normalized_r()
    n_norms <- nrow(norms_proxy$get())
    if (nrow(r) == 0) {
      return(list(df = dn, n_rules = 0L, n_norms = n_norms,
                  df_before_recodes = dn))
    }
    df <- tryCatch(apply_recodes(dn, r)$df, error = function(e) dn)
    list(df = df, n_rules = nrow(r), n_norms = n_norms,
         df_before_recodes = dn)
  })

  mod_recategorize_server("recategorize", shared_state, recat_proxy,
    seed_var_r = selected_var_r, recode_basis_r = recode_basis_r)

  mod_preview_export_server("preview_export", shared_state, rules_proxy,
    recat_proxy = recat_proxy, norms_proxy = norms_proxy)
  mod_import_recodes_server("import_recodes", shared_state, rules_proxy,
    norms_proxy = norms_proxy)
}

shinyApp(ui, server)
