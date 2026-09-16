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

#' Read one dictionary word list: one lowercase word per line, `#` comments.
#'
#' Also strips Hunspell `.dic` artifacts, because the Spellcheck tab imports
#' any .txt and a raw .dic pasted in would otherwise add dead entries that
#' hunspell can never match against a token:
#' - per-word affix flags after a slash (`espresso/SM` -> `espresso`)
#' - the leading word-count line (a purely numeric line)
#' - multi-word lines (the spellchecker checks single tokens only)
.read_dict_terms <- function(path) {
  if (!file.exists(path)) return(character(0))
  x <- readLines(path, warn = FALSE, encoding = "UTF-8")
  x <- x[!startsWith(trimws(x), "#")]
  x <- tolower(trimws(x))
  x <- sub("/.*$", "", x)
  x <- x[!grepl("^[0-9]+$", x)]
  x <- x[!grepl("\\s", x)]
  unique(x[nzchar(x)])
}

#' Write raw bytes to a file. Its own function so tests can stand in a writer
#' that fails, loudly or silently.
.write_bytes <- function(bytes, path) {
  writeBin(bytes, path)
  invisible(path)
}

#' Copy a file byte for byte, then prove the copy landed.
#'
#' Not `file.copy()`: on Windows it can return without copying and without an
#' error (paths with spaces are one known trigger), so its return value cannot
#' be trusted. This reads the source with `readBin()`, writes it with
#' `writer`, then reads the destination back and requires identical bytes.
#'
#' @param src    path of the file to copy.
#' @param dest   destination path (overwritten).
#' @param writer function(bytes, path) that writes `bytes` to `path`.
#' @return list(ok, message): `message` is a plain-language reason when
#'   `ok` is FALSE, NA otherwise.
.copy_file_verified <- function(src, dest, writer = .write_bytes) {
  fail <- function(msg) list(ok = FALSE, message = msg)
  size <- suppressWarnings(file.size(src))
  if (length(size) != 1L || is.na(size)) {
    return(fail("The uploaded file could not be read."))
  }
  bytes <- tryCatch(readBin(src, what = "raw", n = size),
                    error = function(e) NULL)
  if (is.null(bytes) || length(bytes) != size) {
    return(fail("The uploaded file could not be read."))
  }
  err <- tryCatch({
    writer(bytes, dest)
    NULL
  },
  error   = function(e) conditionMessage(e),
  warning = function(w) conditionMessage(w))
  if (!is.null(err)) {
    return(fail(sprintf("Could not write %s (%s).", basename(dest), err)))
  }
  dest_size <- suppressWarnings(file.size(dest))
  if (!file.exists(dest) || is.na(dest_size) || isTRUE(dir.exists(dest))) {
    return(fail(sprintf("%s was not created.", basename(dest))))
  }
  if (dest_size != size) {
    return(fail(sprintf("%s was written incompletely (%d of %d bytes).",
                        basename(dest), as.integer(dest_size),
                        as.integer(size))))
  }
  back <- tryCatch(readBin(dest, what = "raw", n = size + 1L),
                   error = function(e) NULL)
  if (!identical(back, bytes)) {
    return(fail(sprintf("%s does not match the uploaded file.",
                        basename(dest))))
  }
  list(ok = TRUE, message = NA_character_)
}

#' Drop missing, empty and whitespace-only values before spellchecking.
#'
#' `unique_values_r()` keeps the empty string so Browse values can target it;
#' there is nothing in a blank to spellcheck.
.spell_drop_blank <- function(uv) {
  if (is.null(uv) || nrow(uv) == 0) return(uv)
  v <- as.character(uv$value)
  uv[!is.na(v) & nzchar(trimws(v)), , drop = FALSE]
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
    card_header_help(
      "Spellcheck",
      shiny::p("Lists values containing words the dictionary does not ",
               "recognise, with suggested spellings. Click a suggestion, or ",
               "type your own, to create a rule that fixes that value."),
      shiny::p("If a flagged word is actually correct (a name, a product, a ",
               "field term), add it to a dictionary so it stops being ",
               "flagged, or pick a discipline dictionary that already has it.")
    ),
    bslib::card_body(
      fillable = FALSE,
      shiny::tags$small(shiny::tags$em(
        "Click any blue suggestion to recode. Type into the 'Other' box for your own correction. '+ Dict' adds the flagged token to the chosen dictionary tier."
      )),
      # Name the match type where the rule is created. Keep in step with the
      # Browse values tab's wording.
      help_line(shiny::tags$em(
        "Rules created here match with ", shiny::tags$code("trimmed_ci"),
        ": case and surrounding spaces are ignored. Change that per rule on ",
        "the Recodes tab.")),
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
      shiny::uiOutput(ns("tbl_empty")),
      DT::DTOutput(ns("tbl"))
    )
  )
}

# JSON-encode a value for use inside inline JavaScript. A JSON string or
# number is also a valid JavaScript literal, and jsonlite escapes quotes,
# backslashes and control characters.
.js_value <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
}

# Escape a whole attribute value. The browser decodes the entities before the
# JavaScript is parsed, so the script sees exactly what was built here, and a
# `"` in the data can no longer end the attribute.
.attr_value <- function(x) {
  as.character(htmltools::htmlEscape(x, attribute = TRUE))
}

# The JavaScript a suggestion badge runs: sends {"row":..,"val":..} as a JSON
# string, which .apply_pick() parses with jsonlite::fromJSON().
.pick_js <- function(input_id, row_idx, val) {
  payload <- .js_value(list(row = as.integer(row_idx), val = as.character(val)))
  sprintf(
    "Shiny.setInputValue(%s, JSON.stringify(%s), {priority: 'event'}); return false;",
    .js_value(input_id), payload)
}

#' Render the action cell HTML: clickable suggestion badges + custom input +
#' add-to-dict button. Built per row.
#'
#' The suggestions derive from dataset text, so every value that reaches
#' markup goes through one of two paths: visible text through
#' `htmltools::htmlEscape()`, and inline handlers built from JSON-encoded
#' values (`.js_value()`) and then attribute-escaped whole (`.attr_value()`).
#' Never paste data into a handler with sprintf("'%s'") quoting.
#'
#' @param row_idx     1-based row in the flagged table.
#' @param suggestions `" | "`-joined suggestions, or NA.
#' @param ns          the module's namespace function.
#' @return one HTML string. The table shows it in its LAST column, the only
#'   one with escaping turned off.
.render_action_cell <- function(row_idx, suggestions, ns) {
  row_idx <- as.integer(row_idx)
  parts <- character(0)

  if (length(suggestions) == 1L && !is.na(suggestions) && nzchar(suggestions)) {
    sugs <- strsplit(suggestions, "\\s*\\|\\s*")[[1]]
    sugs <- sugs[nzchar(sugs)]
    badges <- vapply(sugs, function(s) {
      sprintf(
        paste0(
          '<a href="#" class="badge bg-primary" ',
          'style="margin-right:.3em;text-decoration:none;color:#fff;cursor:pointer;" ',
          'onclick="%s">%s</a>'),
        .attr_value(.pick_js(ns("pick_sug"), row_idx, s)),
        htmltools::htmlEscape(s))
    }, character(1), USE.NAMES = FALSE)
    parts <- c(parts, badges)
  }

  box_id  <- sprintf("%s_%d", ns("custom_in"), row_idx)
  send_js <- sprintf(
    "Shiny.setInputValue(%s, JSON.stringify({\"row\": %s, \"val\": v}), {priority: 'event'});",
    .js_value(ns("pick_custom")), .js_value(row_idx))

  custom <- sprintf(
    paste0(
      '<input type="text" id="%s" class="form-control form-control-sm" ',
      'style="display:inline-block;width:11em;margin-right:.3em;" ',
      'placeholder="Other..." onkeydown="%s">'),
    .attr_value(box_id),
    .attr_value(paste0(
      "if (event.key === 'Enter') { var v = this.value; ", send_js,
      " event.preventDefault(); }")))
  apply_btn <- sprintf(
    paste0(
      '<button class="btn btn-sm btn-outline-primary" style="margin-right:.3em;" ',
      'onclick="%s">Apply</button>'),
    .attr_value(paste0(
      "var v = document.getElementById(", .js_value(box_id), ").value; ",
      send_js)))
  parts <- c(parts, custom, apply_btn)

  dict_btn <- sprintf(
    paste0(
      '<button class="btn btn-sm btn-outline-secondary" ',
      'onclick="%s">+ Dict</button>'),
    .attr_value(sprintf(
      "Shiny.setInputValue(%s, %s, {priority: 'event'});",
      .js_value(ns("add_dict")), .js_value(row_idx))))
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
    # dictionary/disciplines/, then refresh + auto-select it. Success is
    # reported only once the copy has been read back and matches the upload
    # (see .copy_file_verified()); otherwise nothing is selected.
    shiny::observeEvent(input$import_discipline, {
      f <- input$import_discipline
      if (is.null(f)) return()
      dir <- .dict_paths()$disciplines
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      base <- tools::file_path_sans_ext(basename(f$name))
      base <- gsub("[^A-Za-z0-9_-]+", "_", base)
      dest <- file.path(dir, paste0(base, ".txt"))
      copied <- .copy_file_verified(f$datapath, dest)
      if (!isTRUE(copied$ok)) {
        shiny::showNotification(
          sprintf("Import of '%s' failed: %s The dictionary was not added.",
                  f$name, copied$message),
          type = "error", duration = NULL)
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
      uv <- .spell_drop_blank(unique_values_r())
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

    # Empty state as a plain note, not a one-column datatable (which would
    # show a literal "message" header with a sort arrow).
    output$tbl_empty <- shiny::renderUI({
      df <- flagged_r()
      if (!is.null(df) && nrow(df) > 0) return(NULL)
      empty_state(spellcheck_error() %||% "No spellcheck flags.")
    })

    output$tbl <- DT::renderDT({
      df <- flagged_r()
      if (is.null(df) || nrow(df) == 0) return(NULL)
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
