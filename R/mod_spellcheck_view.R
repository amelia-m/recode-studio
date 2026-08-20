# =============================================================================
# Spellcheck view
# =============================================================================
# Hunspell pass over unique values. Each flagged value shows its top
# suggestions as CLICKABLE badges (one click -> create rule), a free-form
# "Other..." input for a custom correction, and an "Add to dictionary" button.
#
# Dictionary tiers (a word is OK if any tier accepts it, or the system
# en_US dictionary does):
#   dictionary/seed_terms.txt    bundled domain terms (committed)
#   dictionary/custom_terms.txt  project-shared additions (committed)
#   dictionary/user_terms.txt    personal additions (gitignored)
# =============================================================================

.dict_paths <- function() {
  dir <- "dictionary"
  list(
    seed         = file.path(dir, "seed_terms.txt"),
    project      = file.path(dir, "custom_terms.txt"),
    user         = file.path(dir, "user_terms.txt"),
    disciplines  = file.path(dir, "disciplines")
  )
}

.read_dict_terms <- function(path) {
  if (!file.exists(path)) return(character(0))
  x <- readLines(path, warn = FALSE, encoding = "UTF-8")
  x <- x[!startsWith(trimws(x), "#")]
  x <- tolower(trimws(x))
  unique(x[nzchar(x)])
}

#' Discipline dictionaries bundled in dictionary/disciplines/.
#'
#' @return named character vector: display-name -> file path. Empty if none.
list_discipline_dicts <- function() {
  dir <- .dict_paths()$disciplines
  if (!dir.exists(dir)) return(character(0))
  files <- list.files(dir, pattern = "\\.txt$", full.names = TRUE)
  if (length(files) == 0) return(character(0))
  names(files) <- tools::file_path_sans_ext(basename(files))
  files[order(names(files))]
}

#' Build a Hunspell dictionary from the three base tiers plus any selected
#' discipline dictionaries.
#'
#' @param discipline_paths Character vector of discipline file paths to include.
.build_hunspell <- function(discipline_paths = character(0)) {
  paths <- .dict_paths()
  extra_words <- unique(c(
    .read_dict_terms(paths$seed),
    .read_dict_terms(paths$project),
    .read_dict_terms(paths$user),
    unlist(lapply(discipline_paths, .read_dict_terms), use.names = FALSE)
  ))
  tryCatch(
    hunspell::dictionary(lang = "en_US", add_words = extra_words),
    error = function(e_en) {
      tryCatch(
        hunspell::dictionary(add_words = extra_words),
        error = function(e_default) {
          stop(
            sprintf(
              paste0(
                "Hunspell dictionary unavailable (en_US and default). ",
                "en_US error: %s; default error: %s"
              ),
              conditionMessage(e_en),
              conditionMessage(e_default)
            ),
            call. = FALSE
          )
        }
      )
    }
  )
}


mod_spellcheck_view_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = FALSE,
    fill        = FALSE,
    bslib::card_header("Spellcheck"),
    bslib::card_body(
      fillable = FALSE,
      shiny::tags$small(shiny::tags$em(
        "Click any blue suggestion to recode. Type into the 'Other' box for your own correction. '+ Dict' adds the flagged token to the chosen dictionary tier."
      )),
      shiny::fluidRow(
        shiny::column(6,
          shiny::selectizeInput(
            ns("disciplines"),
            "Discipline dictionaries (reduce false positives):",
            choices  = NULL,
            multiple = TRUE,
            options  = list(placeholder = "none selected")
          )
        ),
        shiny::column(6,
          shiny::fileInput(
            ns("import_discipline"),
            "Import a discipline dictionary (.txt, one word per line):",
            accept = c(".txt")
          )
        )
      ),
      shiny::div(
        style = "margin: 1em 0;",
        shiny::radioButtons(
          ns("dict_target"),
          "Add to dictionary writes to:",
          choices = c("Project (shared, committed)" = "project",
                      "User (personal, gitignored)" = "user"),
          selected = "project",
          inline = TRUE
        )
      ),
      shiny::hr(),
      DT::DTOutput(ns("tbl"))
    )
  )
}

#' Render the action cell HTML: clickable suggestion badges + custom input +
#' add-to-dict button. Built per row.
.render_action_cell <- function(row_idx, suggestions, ns) {
  parts <- character(0)

  if (!is.na(suggestions) && nzchar(suggestions)) {
    sugs <- strsplit(suggestions, "\\s*\\|\\s*")[[1]]
    sugs <- sugs[nzchar(sugs)]
    badges <- vapply(sugs, function(s) {
      s_js <- gsub("'", "\\\\'", s)
      sprintf(
        paste0(
          '<a href="#" class="badge bg-primary" ',
          'style="margin-right:.3em;text-decoration:none;color:#fff;cursor:pointer;" ',
          'onclick="Shiny.setInputValue(\'%s\', JSON.stringify({row:%d,val:\'%s\'}), {priority:\'event\'}); return false;">%s</a>'
        ),
        ns("pick_sug"), row_idx, s_js, htmltools::htmlEscape(s)
      )
    }, character(1))
    parts <- c(parts, badges)
  }

  custom <- sprintf(
    paste0(
      '<input type="text" id="%s_%d" class="form-control form-control-sm" ',
      'style="display:inline-block;width:11em;margin-right:.3em;" ',
      'placeholder="Other..." ',
      'onkeydown="if(event.key===\'Enter\'){var v=this.value;',
      'Shiny.setInputValue(\'%s\', JSON.stringify({row:%d,val:v}), {priority:\'event\'});event.preventDefault();}">'),
    ns("custom_in"), row_idx,
    ns("pick_custom"), row_idx
  )
  apply_btn <- sprintf(
    paste0(
      '<button class="btn btn-sm btn-outline-primary" style="margin-right:.3em;" ',
      'onclick="var v=document.getElementById(\'%s_%d\').value;',
      'Shiny.setInputValue(\'%s\', JSON.stringify({row:%d,val:v}), {priority:\'event\'});">Apply</button>'),
    ns("custom_in"), row_idx,
    ns("pick_custom"), row_idx
  )
  parts <- c(parts, custom, apply_btn)

  dict_btn <- sprintf(
    paste0(
      '<button class="btn btn-sm btn-outline-secondary" ',
      'onclick="Shiny.setInputValue(\'%s\', %d, {priority:\'event\'});">+ Dict</button>'),
    ns("add_dict"), row_idx
  )
  parts <- c(parts, dict_btn)

  paste(parts, collapse = " ")
}


#' @param selected_var_r reactive selected variable
#' @param unique_values_r reactive tibble(value, n)
#' @param rules_proxy list(add=, set=, get=)
mod_spellcheck_view_server <- function(id, shared_state,
                                       selected_var_r,
                                       unique_values_r,
                                       rules_proxy) {
  shiny::moduleServer(id, function(input, output, session) {

    dict_version <- shiny::reactiveVal(0L)
    spellcheck_error <- shiny::reactiveVal(NULL)

    # Populate / refresh the discipline selector. Bumping dict_version after
    # an import re-scans the folder so the new file appears immediately.
    shiny::observe({
      dict_version()
      choices <- list_discipline_dicts()  # name -> path
      shiny::updateSelectizeInput(
        session, "disciplines",
        choices  = choices,
        selected = shiny::isolate(input$disciplines)
      )
    })

    # Import a discipline dictionary: copy the uploaded .txt into
    # dictionary/disciplines/, then refresh + auto-select it.
    shiny::observeEvent(input$import_discipline, {
      f <- input$import_discipline
      if (is.null(f)) return()
      dir <- .dict_paths()$disciplines
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      base <- tools::file_path_sans_ext(basename(f$name))
      base <- gsub("[^A-Za-z0-9_-]+", "_", base)
      dest <- file.path(dir, paste0(base, ".txt"))
      ok <- tryCatch({
        file.copy(f$datapath, dest, overwrite = TRUE); TRUE
      }, error = function(e) FALSE)
      if (!ok) {
        shiny::showNotification("Import failed.", type = "error")
        return()
      }
      dict_version(dict_version() + 1L)
      shiny::updateSelectizeInput(
        session, "disciplines",
        choices  = list_discipline_dicts(),
        selected = unique(c(input$disciplines, dest))
      )
      n_terms <- length(.read_dict_terms(dest))
      shiny::showNotification(
        sprintf("Imported '%s' (%d terms).", base, n_terms), type = "message")
    })

    hun_r <- shiny::reactive({
      dict_version()
      tryCatch(
        {
          hun <- .build_hunspell(
            discipline_paths = input$disciplines %||% character(0)
          )
          spellcheck_error(NULL)
          hun
        },
        error = function(e) {
          msg <- paste("Spellcheck unavailable:", conditionMessage(e))
          if (!identical(spellcheck_error(), msg)) {
            shiny::showNotification(msg, type = "warning", duration = 10)
          }
          spellcheck_error(msg)
          NULL
        }
      )
    })

    flagged_r <- shiny::reactive({
      uv <- unique_values_r()
      hun <- hun_r()
      if (is.null(uv) || nrow(uv) == 0) return(NULL)
      if (is.null(hun)) {
        return(tibble::tibble(
          value = character(0),
          n = integer(0),
          flagged_token = character(0),
          suggestions = character(0)
        ))
      }
      tokens_per_value <- strsplit(tolower(uv$value), "[^a-z']+")
      flagged_tokens <- lapply(tokens_per_value, function(toks) {
        toks <- toks[nchar(toks) > 1]
        if (length(toks) == 0) return(character(0))
        ok <- hunspell::hunspell_check(toks, dict = hun)
        toks[!ok]
      })
      first_flag <- vapply(flagged_tokens, function(t)
                           if (length(t) > 0) t[1] else NA_character_,
                           character(1))
      suggestions <- vapply(first_flag, function(t) {
        if (is.na(t)) return(NA_character_)
        sug <- hunspell::hunspell_suggest(t, dict = hun)[[1]]
        if (length(sug) == 0) return(NA_character_)
        paste(head(sug, 5), collapse = " | ")
      }, character(1))

      tibble::tibble(
        value         = uv$value,
        n             = uv$n,
        flagged_token = first_flag,
        suggestions   = suggestions
      ) |>
        dplyr::filter(!is.na(flagged_token)) |>
        dplyr::arrange(dplyr::desc(n))
    })

    output$tbl <- DT::renderDT({
      df <- flagged_r()
      if (is.null(df) || nrow(df) == 0) {
        msg <- spellcheck_error() %||% "No spellcheck flags."
        return(DT::datatable(
          tibble::tibble(message = msg),
          rownames = FALSE, options = list(dom = "t")
        ))
      }
      ns <- session$ns
      df$action <- vapply(
        seq_len(nrow(df)),
        function(i) .render_action_cell(i, df$suggestions[i], ns),
        character(1)
      )
      # "action" MUST stay last: the DT below unescapes exactly one column.
      df <- df[, c("value", "n", "flagged_token", "action")]

      DT::datatable(
        df,
        rownames  = FALSE,
        # SECURITY: only the last column ("action") is generated markup.
        # "value" is a raw third-party dataset cell and "flagged_token" derives
        # from it, so both must stay HTML-escaped.
        escape    = -ncol(df),
        selection = "none",
        filter    = "top",
        options   = list(pageLength = 25, dom = "tip", autoWidth = FALSE,
                         columnDefs = list(
                           list(targets = 3, orderable = FALSE, width = "30em")
                         ))
      )
    })

    .apply_pick <- function(payload) {
      if (is.null(payload)) return()
      parsed <- tryCatch(jsonlite::fromJSON(payload), error = function(e) NULL)
      if (is.null(parsed) || is.null(parsed$row) || is.null(parsed$val)) return()
      i   <- as.integer(parsed$row)
      val <- as.character(parsed$val)
      if (!nzchar(val)) {
        shiny::showNotification("Type a value first.", type = "warning")
        return()
      }
      df <- flagged_r()
      if (is.null(df) || i < 1 || i > nrow(df)) return()
      v <- selected_var_r()
      if (is.null(v)) return()
      now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      new_rule <- tibble::tibble(
        rule_id           = recode_rule_id(v, "trimmed_ci", df$value[i]),
        variable          = v,
        apply_to_siblings = FALSE,
        sibling_pattern   = NA_character_,
        match_type        = "trimmed_ci",
        old_value         = df$value[i],
        new_value         = val,
        action            = "recode",
        notes             = sprintf("Spellcheck (token '%s')", df$flagged_token[i]),
        author            = Sys.info()[["user"]],
        created_at        = now,
        updated_at        = now,
        source_dataset    = shared_state$dataset_name %||% NA_character_
      )
      rules_proxy$add(new_rule)
      shiny::showNotification(
        sprintf("Rule added: %s -> %s", df$value[i], val), type = "message")
    }

    shiny::observeEvent(input$pick_sug,    ignoreInit = TRUE,
                        .apply_pick(input$pick_sug))
    shiny::observeEvent(input$pick_custom, ignoreInit = TRUE,
                        .apply_pick(input$pick_custom))

    shiny::observeEvent(input$add_dict, ignoreInit = TRUE, {
      df <- flagged_r()
      i  <- input$add_dict
      if (is.null(df) || is.na(i) || i < 1 || i > nrow(df)) return()
      tok <- df$flagged_token[i]
      if (is.na(tok)) return()
      paths <- .dict_paths()
      target <- if (input$dict_target == "user") paths$user else paths$project
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
      con <- file(target, open = "a", encoding = "UTF-8")
      on.exit(close(con), add = TRUE)
      writeLines(tolower(tok), con)
      dict_version(dict_version() + 1L)
      shiny::showNotification(
        sprintf("Added '%s' to %s.", tok, basename(target)), type = "message")
    })
  })
}
