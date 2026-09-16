# Run with: testthat::test_dir("tests/testthat")
#
# Spellcheck tab (R/mod_spellcheck_view.R):
#
#   * .render_action_cell() builds raw HTML with inline handlers from
#     suggestions that derive from dataset text. A value containing a quote,
#     `<`, `>`, `&` or a backslash must not be able to end the attribute or the
#     JavaScript string, and the payload must reach the server unchanged.
#   * blank values (which unique_values_r() now keeps for Browse values) are
#     never flagged.
#   * a pick replaces only the flagged token inside the value
#     (spell_replace_token()), never the whole value.

source(file.path("..", "..", "R", "ui_helpers.R"))
source(file.path("..", "..", "R", "string_helpers.R"))
source(file.path("..", "..", "R", "mod_spellcheck_view.R"))

.ns <- shiny::NS("sp")

# Decode the entities htmltools::htmlEscape(attribute = TRUE) produces, the
# way a browser does before handing an attribute to the JavaScript engine.
# `&amp;` goes last so an escaped entity is not decoded twice.
.unescape_attr <- function(x) {
  x <- gsub("&lt;",  "<",  x, fixed = TRUE)
  x <- gsub("&gt;",  ">",  x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;", "'",  x, fixed = TRUE)
  x <- gsub("&#10;", "\n", x, fixed = TRUE)
  x <- gsub("&#13;", "\r", x, fixed = TRUE)
  x <- gsub("&#9;",  "\t", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

# Every opening tag in the cell, as list(name, attrs = named character).
# Fails the test if any tag's attribute list does not parse as a sequence of
# name="value" pairs with no stray text: that is what a quote breaking out of
# an attribute would leave behind.
.parse_tags <- function(html) {
  tags <- regmatches(html, gregexpr("<(a|input|button)\\b[^>]*>", html))[[1]]
  lapply(tags, function(tag) {
    name  <- sub("^<([a-z]+).*$", "\\1", tag)
    body  <- sub("^<[a-z]+", "", sub(">$", "", tag))
    pairs <- regmatches(body, gregexpr('\\s[a-z-]+="[^"]*"', body))[[1]]
    rest  <- body
    for (p in pairs) rest <- sub(p, "", rest, fixed = TRUE)
    expect_identical(trimws(rest), "", info = tag)
    attrs <- sub('^\\s[a-z-]+="([^"]*)"$', "\\1", pairs)
    names(attrs) <- sub('^\\s([a-z-]+)=.*$', "\\1", pairs)
    list(name = name, attrs = attrs)
  })
}

.nasty <- c(
  "it's",
  "say \"hi\"",
  "<script>alert(1)</script>",
  "fish & chips",
  "back\\slash",
  "\"); alert(1); (\"",
  "'); alert(1); ('",
  "&quot; already escaped",
  "tab\there",
  "café ☃")

.badge_prefix <- "Shiny.setInputValue(\"sp-pick_sug\", JSON.stringify("
.badge_suffix <- "), {priority: 'event'}); return false;"

test_that("suggestion badges stay well-formed and round-trip every character", {
  for (s in .nasty) {
    html <- .render_action_cell(3L, s, .ns)
    # No raw markup from the data anywhere in the cell.
    expect_false(grepl("<script", html, fixed = TRUE), info = s)

    tags <- .parse_tags(html)
    kinds <- vapply(tags, `[[`, character(1), "name")
    expect_equal(kinds, c("a", "input", "button", "button"), info = s)

    badge <- tags[[1]]$attrs
    expect_false(any(grepl("[<>\"]", badge)), info = s)
    js <- .unescape_attr(badge[["onclick"]])
    expect_true(startsWith(js, .badge_prefix), info = s)
    expect_true(endsWith(js, .badge_suffix), info = s)
    payload <- substr(js, nchar(.badge_prefix) + 1L,
                      nchar(js) - nchar(.badge_suffix))
    # The object literal is valid JSON, so JSON.stringify() sends it as is
    # and the server's jsonlite::fromJSON() gets the original string back.
    expect_true(jsonlite::validate(payload), info = s)
    parsed <- jsonlite::fromJSON(payload)
    expect_identical(parsed$row, 3L, info = s)
    expect_identical(parsed$val, s, info = s)

    # The visible badge text is the escaped suggestion, not markup.
    text <- sub("^<a\\b[^>]*>(.*?)</a>.*$", "\\1", html, perl = TRUE)
    expect_identical(text, as.character(htmltools::htmlEscape(s)), info = s)
  }
})

test_that("several suggestions each get their own well-formed badge", {
  html <- .render_action_cell(1L, "say \"hi\" | <b> | a&b", .ns)
  tags <- .parse_tags(html)
  badges <- Filter(function(t) t$name == "a", tags)
  expect_length(badges, 3)
  vals <- vapply(badges, function(t) {
    js <- .unescape_attr(t$attrs[["onclick"]])
    jsonlite::fromJSON(substr(js, nchar(.badge_prefix) + 1L,
                              nchar(js) - nchar(.badge_suffix)))$val
  }, character(1))
  expect_identical(vals, c("say \"hi\"", "<b>", "a&b"))
})

test_that("no suggestions still renders the Other box, Apply and + Dict", {
  for (sug in list(NA_character_, "")) {
    tags <- .parse_tags(.render_action_cell(7L, sug, .ns))
    expect_equal(vapply(tags, `[[`, character(1), "name"),
                 c("input", "button", "button"))
  }
})

test_that("the Other box, Apply and + Dict handlers are well-formed JavaScript payloads", {
  tags <- .parse_tags(.render_action_cell(12L, NA_character_, .ns))
  box <- tags[[1]]$attrs
  expect_identical(box[["id"]], "sp-custom_in_12")
  expect_identical(box[["placeholder"]], "Other...")

  send <- paste0("Shiny.setInputValue(\"sp-pick_custom\", ",
                 "JSON.stringify({\"row\": 12, \"val\": v}), ",
                 "{priority: 'event'});")
  expect_identical(
    .unescape_attr(box[["onkeydown"]]),
    paste0("if (event.key === 'Enter') { var v = this.value; ", send,
           " event.preventDefault(); }"))
  expect_identical(
    .unescape_attr(tags[[2]]$attrs[["onclick"]]),
    paste0("var v = document.getElementById(\"sp-custom_in_12\").value; ",
           send))
  expect_identical(
    .unescape_attr(tags[[3]]$attrs[["onclick"]]),
    "Shiny.setInputValue(\"sp-add_dict\", 12, {priority: 'event'});")
})

test_that("the table keeps the action column last and unescapes only it", {
  src <- paste(readLines(file.path("..", "..", "R", "mod_spellcheck_view.R"),
                         warn = FALSE), collapse = "\n")
  expect_match(src, 'df[, c("value", "n", "flagged_token", "action")]',
               fixed = TRUE)
  expect_match(src, "escape    = -ncol(df)", fixed = TRUE)
  expect_false(grepl("escape\\s*=\\s*FALSE", src))
})


# --- Server: blanks and the pick round trip ---------------------------------

.rules_store <- function() {
  e <- new.env(parent = emptyenv())
  e$rules <- empty_recodes_tibble()
  list(get  = function() e$rules,
       add  = function(r) e$rules <- dplyr::bind_rows(e$rules, r),
       set  = function(r) e$rules <- r,
       peek = function() e$rules)
}

.spell_args <- function(uv, proxy = .rules_store()) {
  list(shared_state    = shiny::reactiveValues(dataset_name = "orders.csv"),
       selected_var_r  = shiny::reactiveVal("drink1"),
       unique_values_r = shiny::reactive(uv),
       rules_proxy     = proxy)
}

.hunspell_ok <- function() {
  !is.null(tryCatch(.build_hunspell(), error = function(e) NULL))
}

test_that(".spell_drop_blank drops empty, whitespace-only and NA values", {
  uv <- tibble::tibble(value = c("latte", "", " \t ", NA), n = 4:1)
  expect_equal(.spell_drop_blank(uv)$value, "latte")
  expect_null(.spell_drop_blank(NULL))
})

test_that("empty and whitespace-only values are never flagged", {
  skip_if_not(.hunspell_ok(), "no Hunspell dictionary on this machine")
  uv <- tibble::tibble(value = c("", "   ", "cappucino", " \t", "latte"),
                       n     = c(9L, 8L, 3L, 2L, 1L))
  shiny::testServer(mod_spellcheck_view_server, args = .spell_args(uv), {
    df <- flagged_r()
    expect_true("cappucino" %in% df$value)
    expect_true(all(nzchar(trimws(df$value))))
  })
})

test_that("an all-blank column shows the empty state, not a table", {
  uv <- tibble::tibble(value = c("", "   "), n = c(2L, 1L))
  shiny::testServer(mod_spellcheck_view_server, args = .spell_args(uv), {
    expect_null(flagged_r())
    expect_match(paste(output$tbl_empty$html), "No spellcheck flags.",
                 fixed = TRUE)
  })
})

test_that("a badge payload with awkward characters creates the rule unchanged", {
  skip_if_not(.hunspell_ok(), "no Hunspell dictionary on this machine")
  uv <- tibble::tibble(value = "cappucino", n = 3L)
  proxy <- .rules_store()
  shiny::testServer(mod_spellcheck_view_server, args = .spell_args(uv, proxy), {
    # The pick handlers use ignoreInit, so the app's start-up flush must
    # happen before the click, as it does in a browser.
    session$flushReact()
    expect_equal(nrow(flagged_r()), 1L)
    target <- "Cappuccino \"grande\" <b> & back\\slash"
    js <- .unescape_attr(.parse_tags(
      .render_action_cell(1L, target, session$ns))[[1]]$attrs[["onclick"]])
    prefix <- sprintf("Shiny.setInputValue(\"%s\", JSON.stringify(",
                      session$ns("pick_sug"))
    payload <- substr(js, nchar(prefix) + 1L,
                      nchar(js) - nchar(.badge_suffix))
    session$setInputs(pick_sug = payload)
    rules <- proxy$peek()
    expect_equal(nrow(rules), 1L)
    expect_identical(rules$old_value, "cappucino")
    expect_identical(rules$new_value, target)
  })
})


# --- Word-level correction inside a whole-cell rule --------------------------
#
# Spellcheck flags ONE token inside a value. A pick must replace only that
# token: the rule is still whole-cell (trimmed_ci, old_value = the value), but
# its new_value keeps every other word. The old handler set new_value to the
# bare correction, so `iced coffe` became `coffee`.

test_that("spell_replace_token replaces a single-word value", {
  expect_identical(spell_replace_token("coffe", "coffe", "coffee"), "coffee")
})

test_that("spell_replace_token keeps the other words of a multi-word value", {
  expect_identical(spell_replace_token("iced coffe", "coffe", "coffee"),
                   "iced coffee")
  expect_identical(spell_replace_token("coffe latte", "coffe", "coffee"),
                   "coffee latte")
})

test_that("punctuation and spacing around the token are preserved exactly", {
  expect_identical(
    spell_replace_token("  iced   coffe,  no-sugar!! ", "coffe", "coffee"),
    "  iced   coffee,  no-sugar!! ")
  expect_identical(spell_replace_token("no-sugr (iced)", "sugr", "sugar"),
                   "no-sugar (iced)")
  # Digits separate tokens, exactly as the flagger splits them.
  expect_identical(spell_replace_token("coffe2go", "coffe", "coffee"),
                   "coffee2go")
})

test_that("every whole-token occurrence is replaced and counted", {
  expect_identical(
    spell_replace_token("coffe, then more coffe", "coffe", "coffee"),
    "coffee, then more coffee")
  expect_identical(spell_token_count("coffe, then more coffe", "coffe"), 2L)
  expect_identical(spell_token_count("iced coffe", "coffe"), 1L)
  expect_identical(spell_token_count("iced coffee", "coffe"), 0L)
})

test_that("a token inside a longer word is not replaced", {
  # `coffees` is one token to the flagger, so `coffe` is not in it.
  expect_identical(.spell_tokens("coffe coffees")[[1]], c("coffe", "coffees"))
  expect_identical(spell_replace_token("coffe coffees", "coffe", "coffee"),
                   "coffee coffees")
  expect_identical(spell_replace_token("coffees", "coffe", "coffee"),
                   "coffees")
  # The apostrophe is a token character, so `it` is not inside `it's`.
  expect_identical(spell_replace_token("it's it", "it", "at"), "it's at")
})

test_that("ALL CAPS and Title Case originals are mirrored, others kept as given", {
  expect_identical(spell_replace_token("ICED COFFE", "coffe", "coffee"),
                   "ICED COFFEE")
  expect_identical(spell_replace_token("Iced Coffe", "coffe", "coffee"),
                   "Iced Coffee")
  # Each occurrence is cased from its own text.
  expect_identical(spell_replace_token("Coffe or COFFE or coffe", "coffe",
                                       "coffee"),
                   "Coffee or COFFEE or coffee")
  # Title Case only raises the first letter; an inner capital survives.
  expect_identical(spell_replace_token("Dr Mckenze", "mckenze", "McKenzie"),
                   "Dr McKenzie")
  # Lowercase or mixed-case original: the correction exactly as given.
  expect_identical(spell_replace_token("iced coffe", "coffe", "Coffee"),
                   "iced Coffee")
  expect_identical(spell_replace_token("cOFFe", "coffe", "coffee"), "coffee")
  # The token may arrive in any case.
  expect_identical(spell_replace_token("iced coffe", "COFFE", "coffee"),
                   "iced coffee")
})

test_that("apostrophes inside and around a token are handled", {
  expect_identical(spell_replace_token("Barista's chioce", "chioce", "choice"),
                   "Barista's choice")
  expect_identical(spell_replace_token("dont know", "dont", "don't"),
                   "don't know")
  # Quote marks the tokenizer counts as part of the token are kept when the
  # suggestion (which hunspell gives unquoted) does not carry them.
  expect_identical(.spell_tokens("'coffe' please")[[1]], c("'coffe'", "please"))
  expect_identical(spell_replace_token("'Coffe' please", "'coffe'", "coffee"),
                   "'Coffee' please")
  expect_identical(spell_replace_token("'coffe'", "'coffe'", "'coffee'"),
                   "'coffee'")
})

test_that("a correction equal to the original leaves the value unchanged", {
  expect_identical(spell_replace_token("iced coffe", "coffe", "coffe"),
                   "iced coffe")
  # Title Case mirroring makes a lowercase retype of a Title Case word a no-op.
  expect_identical(spell_replace_token("Iced Coffe", "coffe", "coffe"),
                   "Iced Coffe")
  expect_identical(spell_replace_token("iced latte", "coffe", "coffee"),
                   "iced latte")
  expect_identical(spell_replace_token(NA_character_, "coffe", "coffee"),
                   NA_character_)
})

test_that("the flagger and the replacement share one tokenizer", {
  src <- paste(readLines(file.path("..", "..", "R", "mod_spellcheck_view.R"),
                         warn = FALSE), collapse = "\n")
  expect_match(src, "tokens_per_value <- .spell_tokens(uv$value)", fixed = TRUE)
})

.pick_payload <- function(row, val) {
  as.character(jsonlite::toJSON(list(row = row, val = val), auto_unbox = TRUE))
}

test_that("clicking a suggestion on a multi-word value keeps the other words", {
  skip_if_not(.hunspell_ok(), "no Hunspell dictionary on this machine")
  uv <- tibble::tibble(value = "Iced coffe, no sugar", n = 4L)
  proxy <- .rules_store()
  shiny::testServer(mod_spellcheck_view_server, args = .spell_args(uv, proxy), {
    session$flushReact()
    df <- flagged_r()
    expect_equal(nrow(df), 1L)
    expect_identical(df$flagged_token, "coffe")
    expect_match(df$suggestions, "coffee", fixed = TRUE)
    session$setInputs(pick_sug = .pick_payload(1L, "coffee"))
    rules <- proxy$peek()
    expect_equal(nrow(rules), 1L)
    expect_identical(rules$old_value, "Iced coffe, no sugar")
    expect_identical(rules$new_value, "Iced coffee, no sugar")
    expect_identical(rules$match_type, "trimmed_ci")
    expect_identical(rules$rule_id,
                     recode_rule_id("drink1", "trimmed_ci",
                                    "Iced coffe, no sugar"))
    expect_match(rules$notes, "'coffe' -> 'coffee'", fixed = TRUE)
  })
})

test_that("the Other... path replaces only the token, and notes repeats", {
  skip_if_not(.hunspell_ok(), "no Hunspell dictionary on this machine")
  uv <- tibble::tibble(value = "coffe and more COFFE", n = 2L)
  proxy <- .rules_store()
  shiny::testServer(mod_spellcheck_view_server, args = .spell_args(uv, proxy), {
    session$flushReact()
    expect_identical(flagged_r()$flagged_token, "coffe")
    # Surrounding spaces typed into the box are not part of the correction.
    session$setInputs(pick_custom = .pick_payload(1L, "  coffee "))
    rules <- proxy$peek()
    expect_equal(nrow(rules), 1L)
    expect_identical(rules$old_value, "coffe and more COFFE")
    expect_identical(rules$new_value, "coffee and more COFFEE")
    expect_match(rules$notes, "all 2 occurrences replaced", fixed = TRUE)
  })
})

test_that("a correction that changes nothing adds no rule", {
  skip_if_not(.hunspell_ok(), "no Hunspell dictionary on this machine")
  uv <- tibble::tibble(value = "iced coffe", n = 1L)
  proxy <- .rules_store()
  shiny::testServer(mod_spellcheck_view_server, args = .spell_args(uv, proxy), {
    session$flushReact()
    session$setInputs(pick_custom = .pick_payload(1L, "coffe"))
    expect_equal(nrow(proxy$peek()), 0L)
    session$setInputs(pick_custom = .pick_payload(1L, "   "))
    expect_equal(nrow(proxy$peek()), 0L)
    # A bad row index is ignored rather than throwing.
    session$setInputs(pick_sug = .pick_payload(9L, "coffee"))
    expect_equal(nrow(proxy$peek()), 0L)
  })
})
