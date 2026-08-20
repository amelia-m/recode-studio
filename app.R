# =============================================================================
# Recode Studio — standalone Shiny app
# =============================================================================
# A dataset-agnostic tool for cleaning string variables: load any CSV/Excel,
# pick a text column, browse values alphabetically or as similarity clusters,
# spellcheck them, build recode rules, and export a recode CSV + an R script.
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
    uiOutput("selected_var_banner"),
    bslib::navset_card_tab(
      id = "studio_tabs",
      bslib::nav_panel("1. Data",            value = "data",       mod_data_input_ui("data_input")),
      bslib::nav_panel("2. Variable",        value = "variable",   mod_variable_picker_ui("variable_picker")),
      bslib::nav_panel("3. Browse values",   value = "browse",     mod_value_table_ui("value_table")),
      bslib::nav_panel("4. Clusters",        value = "clusters",   mod_cluster_view_ui("cluster_view")),
      bslib::nav_panel("5. Spellcheck",      value = "spellcheck", mod_spellcheck_view_ui("spellcheck_view")),
      bslib::nav_panel("6. Text analysis",   value = "text",       mod_text_analysis_ui("text_analysis")),
      bslib::nav_panel("7. Recodes",         value = "recodes",    mod_recode_editor_ui("recode_editor")),
      bslib::nav_panel("8. Recategorize",    value = "recat",      mod_recategorize_ui("recategorize")),
      bslib::nav_panel("9. Preview & export", value = "export",
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

  # App-wide state. mod_data_input writes data/meta/dataset_name; everything
  # else reads.
  shared_state <- shiny::reactiveValues(
    data         = NULL,
    meta         = NULL,
    dataset_name = NULL
  )

  # In-session rule sets + proxies. Recategorization rules live in their OWN
  # store: they derive a new column instead of rewriting values, so they share
  # neither the schema nor the export path with the old->new recodes.
  rv <- shiny::reactiveValues(
    rules = empty_recodes_tibble(),
    recat = empty_recat_tibble()
  )
  rules_proxy <- list(
    get = shiny::reactive({ rv$rules }),
    add = function(new_rules) {
      cur <- rv$rules
      cur <- cur[!(cur$rule_id %in% new_rules$rule_id), ]
      rv$rules <- dplyr::bind_rows(cur, new_rules)
    },
    set = function(new_rules) { rv$rules <- new_rules }
  )
  recat_proxy <- list(
    get = shiny::reactive({ rv$recat }),
    add = function(new_rules) {
      cur <- rv$recat
      cur <- cur[!(cur$recat_id %in% new_rules$recat_id), ]
      rv$recat <- dplyr::bind_rows(cur, new_rules)
    },
    set = function(new_rules) { rv$recat <- new_rules }
  )

  mod_data_input_server("data_input", shared_state)

  selected_var_r <- mod_variable_picker_server("variable_picker", shared_state)

  output$selected_var_banner <- shiny::renderUI({
    v <- selected_var_r()
    grp <- if (!is.null(v)) column_group(v) else ""
    shiny::div(
      class = "alert alert-info",
      style = "padding: .35em .8em; margin: .6em 0;",
      shiny::strong("Recoding: "),
      if (is.null(v))
        shiny::tags$em("(no variable selected — load data on tab 1, pick a column on tab 2)")
      else
        shiny::tagList(
          shiny::tags$code(v),
          if (grp != "(none)") shiny::tags$small(sprintf(" — group: %s", grp))
        )
    )
  })

  unique_values_r <- shiny::reactive({
    v <- selected_var_r()
    d <- shared_state$data
    if (is.null(v) || is.null(d) || !(v %in% names(d))) return(NULL)
    x <- d[[v]]
    x <- x[!is.na(x) & nzchar(x)]
    if (length(x) == 0) return(NULL)
    tibble::tibble(value = x) |>
      dplyr::count(value, name = "n") |>
      dplyr::arrange(dplyr::desc(n))
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

  mod_text_analysis_server("text_analysis", shared_state, selected_var_r)

  mod_recode_editor_server("recode_editor", shared_state, rules_proxy)

  # The exported recat script is meant to run AFTER the recode script, so the
  # Recategorize tab previews against the recoded frame, not the raw one.
  recode_basis_r <- shiny::reactive({
    d <- shared_state$data
    r <- rules_proxy$get()
    if (is.null(d)) return(NULL)
    if (nrow(r) == 0) return(list(df = d, n_rules = 0L))
    df <- tryCatch(apply_recodes(d, r)$df, error = function(e) d)
    list(df = df, n_rules = nrow(r))
  })

  mod_recategorize_server("recategorize", shared_state, recat_proxy,
    seed_var_r = selected_var_r, recode_basis_r = recode_basis_r)

  mod_preview_export_server("preview_export", shared_state, rules_proxy)
  mod_import_recodes_server("import_recodes", shared_state, rules_proxy)
}

shinyApp(ui, server)
