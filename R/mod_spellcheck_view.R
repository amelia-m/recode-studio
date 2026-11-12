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
#'
#' TOTAL: never throws, whatever the file holds. A word list the user imported
#' is only validated at import (`.check_dict_upload()`), and a file can also
#' arrive in the folder by hand, so this has to survive a directory, a missing
#' path and bytes that are not text. It used to throw "invalid multibyte
#' string" on a line that is not valid UTF-8, which killed the whole
#' Spellcheck tab for the session (security F7).
.read_dict_terms <- function(path) {
  if (length(path) != 1L || is.na(path)) return(character(0))
  if (!file.exists(path) || isTRUE(dir.exists(path))) return(character(0))
  x <- tryCatch(suppressWarnings(readLines(path, warn = FALSE,
                                           encoding = "UTF-8")),
                error = function(e) NULL)
  if (is.null(x) || length(x) == 0L) return(character(0))
  # Lines that are not valid UTF-8 would make trimws(), tolower() and sub()
  # throw. Drop just those: the rest of the file still reads.
  x <- x[!is.na(x) & validUTF8(x)]
  if (length(x) == 0L) return(character(0))
  x <- x[!startsWith(trimws(x), "#")]
  x <- tolower(trimws(x))
  x <- sub("/.*$", "", x)
  x <- x[!grepl("^[0-9]+$", x)]
  x <- x[!grepl("\\s", x)]
  unique(x[nzchar(x)])
}

#' Is this upload a plain-text word list the dictionary reader can use?
#'
#' A file renamed `.txt` is not a word list. Copying one in used to succeed
#' and then break the Spellcheck tab for the whole session (security F7), so
#' the bytes are checked BEFORE anything is written and the user gets a reason
#' in plain language.
#'
#' @param src  path of the uploaded temp file.
#' @param name the name the user uploaded it under, for the message.
#' @return list(ok, message): `message` is the reason when `ok` is FALSE,
#'   NA otherwise.
.check_dict_upload <- function(src, name = basename(src)) {
  fail <- function(msg) list(ok = FALSE, message = msg)
  size <- suppressWarnings(file.size(src))
  if (length(size) != 1L || is.na(size)) {
    return(fail(sprintf("'%s' could not be read.", name)))
  }
  if (size == 0) {
    return(fail(sprintf("'%s' is empty, so it holds no words.", name)))
  }
  bytes <- tryCatch(readBin(src, what = "raw", n = size),
                    error = function(e) NULL)
  if (is.null(bytes)) {
    return(fail(sprintf("'%s' could not be read.", name)))
  }
  if (any(bytes == as.raw(0))) {
    return(fail(sprintf(
      paste0("'%s' is not a text file (it contains NUL bytes). A discipline ",
             "dictionary is a plain .txt file with one word per line."),
      name)))
  }
  text <- tryCatch(rawToChar(bytes), error = function(e) NULL)
  if (is.null(text) || !validUTF8(text)) {
    return(fail(sprintf(
      paste0("'%s' is not valid UTF-8 text. Save it as UTF-8, one word per ",
             "line, and import it again."),
      name)))
  }
  list(ok = TRUE, message = NA_character_)
}

#' A free path in `dir` for a dictionary called `base`: `base.txt`, else
#' `base_2.txt`, `base_3.txt` and so on.
#'
#' Never returns a path that exists. Uploading a file named `medical.txt` used
#' to overwrite the committed dictionary of that name and report success
#' (security F2); four bundled dictionaries were exposed.
#'
#' @return the path, or NULL when `max_tries` names are all taken.
.dict_free_path <- function(dir, base, max_tries = 500L) {
  dest <- file.path(dir, paste0(base, ".txt"))
  k <- 1L
  while (file.exists(dest)) {
    k <- k + 1L
    if (k > max_tries) return(NULL)
    dest <- file.path(dir, sprintf("%s_%d.txt", base, k))
  }
  dest
}

#' The file name part of an upload, reduced to characters that are safe in a
#' dictionary file name. Drops any directory part (a name can carry a path
#' separator), and falls back to "imported" when nothing usable is left (a
#' name that is empty, or has no letters or digits at all).
.dict_base_name <- function(name) {
  base <- tools::file_path_sans_ext(basename(as.character(name)))
  base <- gsub("[^A-Za-z0-9_-]+", "_", base)
  if (length(base) != 1L || is.na(base) || !grepl("[A-Za-z0-9]", base)) {
    base <- "imported"
  }
  base
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

# --- Tokens and word-level correction -----------------------------------------
#
# The flagger and the correction must agree on what a "word" is, so both use
# this one definition: lowercase the value, then a token is a maximal run of
# ASCII letters and apostrophes. Everything else (spaces, digits, hyphens,
# punctuation, non-ASCII letters) separates tokens.

SPELL_TOKEN_RUN <- "[a-z']+"

#' Tokens of each value, as the spellchecker sees them.
#'
#' @param values character vector.
#' @return list of character vectors, lowercase, one per value.
.spell_tokens <- function(values) {
  lv <- tolower(values)
  lapply(regmatches(lv, gregexpr(SPELL_TOKEN_RUN, lv)), as.character)
}

#' Character spans in `value` where `token` occurs as a whole token.
#'
#' Runs the tokenizer over `tolower(value)`. `tolower()` maps character by
#' character, so its positions are positions in `value` too. A span matches
#' when its lowercase text equals `tolower(token)` exactly, which makes the
#' match case-insensitive and whole-word by construction: `coffe` is not found
#' inside `coffees`, and `it` is not found inside `it's`.
#'
#' @return integer matrix with columns `start` and `end` (zero rows if none).
.spell_token_spans <- function(value, token) {
  none <- matrix(integer(0), ncol = 2, dimnames = list(NULL, c("start", "end")))
  if (length(value) != 1L || is.na(value) || length(token) != 1L ||
      is.na(token) || !nzchar(token)) {
    return(none)
  }
  lv <- tolower(value)
  m  <- gregexpr(SPELL_TOKEN_RUN, lv)[[1]]
  if (m[1] == -1L) return(none)
  start <- as.integer(m)
  end   <- start + attr(m, "match.length") - 1L
  keep  <- substring(lv, start, end) == tolower(token)
  cbind(start = start[keep], end = end[keep])
}

#' How many whole-token occurrences of `token` are in `value`.
spell_token_count <- function(value, token) {
  nrow(.spell_token_spans(value, token))
}

#' Case `replacement` to follow the word it replaces.
#'
#' ALL CAPS original (two or more letters, none lowercase): the replacement is
#' upper-cased. Title Case original (first letter upper, every other letter
#' lower): only the replacement's first letter is upper-cased, the rest is
#' left as given, so a suggestion such as `McKenzie` keeps its inner capital.
#' Anything else (lowercase, mixed case): the replacement is used as given.
.spell_mirror_case <- function(original, replacement) {
  letters_only <- gsub("[^A-Za-z]", "", original)
  if (!nzchar(letters_only)) return(replacement)
  first <- substr(letters_only, 1L, 1L)
  rest  <- substring(letters_only, 2L)
  if (nchar(letters_only) >= 2L && !grepl("[a-z]", letters_only)) {
    return(toupper(replacement))
  }
  if (grepl("[A-Z]", first) && !grepl("[A-Z]", rest)) {
    return(sub("^([^[:alpha:]]*)([[:alpha:]])", "\\1\\U\\2", replacement,
               perl = TRUE))
  }
  replacement
}

#' Replace one misspelled token inside a value, leaving the rest untouched.
#'
#' Every whole-token occurrence of `token` (see `.spell_token_spans()`) is
#' replaced; other words, punctuation and spacing are kept exactly. Each
#' occurrence is cased from its own original text (`.spell_mirror_case()`).
#' Leading or trailing apostrophes belong to the token as the tokenizer sees
#' it (`'coffe'`), but they are quotation marks to a reader, so they are kept
#' unless the replacement brings its own.
#'
#' @param value       one value (character scalar).
#' @param token       the flagged token, any case.
#' @param replacement the correction for that token.
#' @return the corrected value. `value` unchanged when the token is absent.
#'   NA when `value` is NA.
spell_replace_token <- function(value, token, replacement) {
  if (length(value) != 1L) stop("`value` must be a single string.")
  if (is.na(value)) return(NA_character_)
  spans <- .spell_token_spans(value, token)
  if (nrow(spans) == 0L || is.na(replacement)) return(value)
  out <- value
  # Right to left, so earlier spans keep their positions.
  for (k in rev(seq_len(nrow(spans)))) {
    s    <- spans[k, "start"]
    e    <- spans[k, "end"]
    orig <- substr(value, s, e)
    lead  <- sub("^('*).*$", "\\1", orig)
    trail <- if (grepl("[a-z]", tolower(orig))) sub("^.*?('*)$", "\\1", orig,
                                                   perl = TRUE) else ""
    rep_k <- .spell_mirror_case(orig, replacement)
    if (nzchar(lead)  && !startsWith(rep_k, "'")) rep_k <- paste0(lead, rep_k)
    if (nzchar(trail) && !endsWith(rep_k, "'"))   rep_k <- paste0(rep_k, trail)
    out <- paste0(substr(out, 1L, s - 1L), rep_k,
                  substring(out, e + 1L))
  }
  out
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

#' Load the en_US Hunspell dictionary with extra words. Its own function so
#' tests can stand in a loader that fails (the module looks it up by name).
.hunspell_loader <- function(add_words) {
  hunspell::dictionary(lang = "en_US", add_words = add_words)
}

#' Build a Hunspell dictionary from the three base tiers plus any selected
#' discipline dictionaries.
#'
#' en_US only, with no fallback. hunspell's "default" dictionary IS en_US, so
#' the old fallback (retry without a language) loaded the same file a second
#' time and could never succeed where the first attempt failed. When en_US
#' cannot load, this stops with a plain-language reason and the Spellcheck
#' tab says spellcheck is unavailable.
#'
#' @param discipline_paths Character vector of discipline file paths to include.
#' @param loader function(add_words) returning a hunspell dictionary.
.build_hunspell <- function(discipline_paths = character(0),
                            loader = .hunspell_loader) {
  paths <- .dict_paths()
  extra_words <- unique(c(
    .read_dict_terms(paths$seed),
    .read_dict_terms(paths$project),
    .read_dict_terms(paths$user),
    unlist(lapply(discipline_paths, .read_dict_terms), use.names = FALSE)
  ))
  tryCatch(
    loader(extra_words),
    error = function(e) {
      stop(sprintf(paste0(
        "the English (en_US) dictionary could not be loaded (%s). Reinstall ",
        "the hunspell package, which ships it, then restart the app."),
        conditionMessage(e)), call. = FALSE)
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
               "type your own, to create a rule for that value that replaces ",
               "only the flagged word and keeps the rest."),
      shiny::p("If a flagged word is actually correct (a name, a product, a ",
               "field term), add it to a dictionary so it stops being ",
               "flagged, or pick a discipline dictionary that already has it.")
    ),
    bslib::card_body(
      fillable = FALSE,
      shiny::tags$small(shiny::tags$em(
        "Click any blue suggestion to replace the flagged word in that value. Type into the 'Other' box for your own correction. '+ Dict' adds the flagged token to the chosen dictionary tier."
      )),
      # Name the match type where the rule is created. It differs from Browse
      # values on purpose: a Spellcheck rule is exact (see .apply_pick()).
      help_line(shiny::tags$em(
        "Rules created here match with ", shiny::tags$code("exact"),
        ": only the value exactly as it is written, so a differently cased ",
        "copy of it elsewhere in the column keeps its own casing. A ",
        "Spellcheck rule cannot be switched to ", shiny::tags$code("regex"),
        ".")),
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

# `.js_value()` and `.attr_value()` live in R/ui_helpers.R: the Recodes,
# Normalize and Recategorize action cells need them too, and two definitions
# would make precedence depend on app.R's source order.

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
    # Which dictionary selection the load last failed for (NULL: it never
    # has). Nothing a user does without changing the selection can make that
    # same load succeed, so it is not retried; CHANGING the selection does
    # retry, because the failure can come from a selected discipline file
    # rather than from en_US, and deselecting it has to bring the tab back.
    # A flag that latched for the session left Spellcheck dead until restart
    # (security F7).
    dict_failed_for <- shiny::reactiveVal(NULL)

    # The selection as a single comparable string.
    .selection_key <- function(sel) {
      paste(sort(as.character(sel %||% character(0))), collapse = "\n")
    }

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
    #
    # Two things have to hold before anything is written. The bytes must be a
    # plain-text word list (.check_dict_upload(): a binary file renamed .txt
    # used to be copied in and then break the tab for the session), and the
    # destination name must be free (.dict_free_path(): an upload named
    # medical.txt used to overwrite the committed dictionary of that name).
    # Success is reported only once the copy has been read back and matches
    # the upload (see .copy_file_verified()); otherwise nothing is selected.
    shiny::observeEvent(input$import_discipline, {
      f <- input$import_discipline
      if (is.null(f)) return()
      refuse <- function(msg) {
        shiny::showNotification(
          sprintf("Import of '%s' failed: %s The dictionary was not added.",
                  f$name, msg),
          type = "error", duration = NULL)
      }
      checked <- .check_dict_upload(f$datapath, f$name)
      if (!isTRUE(checked$ok)) {
        refuse(checked$message)
        return()
      }
      dir <- .dict_paths()$disciplines
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      base <- .dict_base_name(f$name)
      dest <- .dict_free_path(dir, base)
      if (is.null(dest)) {
        refuse(sprintf("too many dictionaries are already named '%s'.", base))
        return()
      }
      copied <- .copy_file_verified(f$datapath, dest)
      if (!isTRUE(copied$ok)) {
        refuse(copied$message)
        return()
      }
      dict_version(dict_version() + 1L)
      shiny::updateSelectizeInput(
        session, "disciplines",
        choices  = list_discipline_dicts(),
        selected = unique(c(input$disciplines, dest))
      )
      n_terms <- length(.read_dict_terms(dest))
      # Name the file that was written, not the name uploaded: they differ
      # whenever the uploaded name was already taken.
      shiny::showNotification(
        sprintf("Imported '%s' as %s (%d terms).", f$name, basename(dest),
                n_terms),
        type = "message")
    })

    hun_r <- shiny::reactive({
      dict_version()
      selection <- input$disciplines %||% character(0)
      key <- .selection_key(selection)
      if (identical(shiny::isolate(dict_failed_for()), key)) return(NULL)
      tryCatch(
        {
          hun <- .build_hunspell(discipline_paths = selection)
          spellcheck_error(NULL)
          dict_failed_for(NULL)
          hun
        },
        error = function(e) {
          msg <- paste0("Spellcheck is unavailable: ", conditionMessage(e))
          shiny::showNotification(msg, type = "warning", duration = 10)
          spellcheck_error(msg)
          dict_failed_for(key)
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
      tokens_per_value <- .spell_tokens(uv$value)
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
      i   <- suppressWarnings(as.integer(parsed$row))
      # The correction replaces ONE word, so surrounding spaces typed into the
      # Other box are not part of it.
      val <- trimws(as.character(parsed$val))
      if (length(i) != 1L || is.na(i) || length(val) != 1L) return()
      if (is.na(val) || !nzchar(val)) {
        shiny::showNotification("Type a value first.", type = "warning")
        return()
      }
      df <- flagged_r()
      if (is.null(df) || i < 1 || i > nrow(df)) return()
      v <- selected_var_r()
      if (is.null(v)) return()
      # The flag is one token inside the value, and so is the correction: the
      # rule still claims the whole cell, but its new value is the old one
      # with only that token replaced. Using the bare correction as the new
      # value would drop every other word.
      #
      # match_type is `exact`, on the value exactly as it appears: the new
      # value carries THIS value's casing and spacing, so a trimmed_ci rule
      # recased every differently cased copy of it to match (upstream
      # item 17; the Recodes editor refuses switching it to regex).
      old_val <- df$value[i]
      tok     <- df$flagged_token[i]
      n_occ   <- spell_token_count(old_val, tok)
      new_val <- spell_replace_token(old_val, tok, val)
      if (n_occ == 0L || identical(new_val, old_val)) {
        shiny::showNotification(
          sprintf("No rule added: '%s' would leave \"%s\" unchanged.",
                  val, old_val),
          type = "warning")
        return()
      }
      note <- sprintf("Spellcheck: token '%s' -> '%s'", tok, val)
      if (n_occ > 1L) {
        note <- sprintf("%s (all %d occurrences replaced)", note, n_occ)
      }
      now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      new_rule <- tibble::tibble(
        rule_id           = recode_rule_id(v, "exact", old_val),
        variable          = v,
        apply_to_siblings = FALSE,
        sibling_pattern   = NA_character_,
        match_type        = "exact",
        old_value         = old_val,
        new_value         = new_val,
        action            = "recode",
        notes             = note,
        author            = Sys.info()[["user"]],
        created_at        = now,
        updated_at        = now,
        source_dataset    = shared_state$dataset_name %||% NA_character_
      )
      rules_proxy$add(new_rule)
      shiny::showNotification(
        sprintf("Rule added: \"%s\" -> \"%s\"", old_val, new_val),
        type = "message")
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
