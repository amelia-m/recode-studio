# Run with: testthat::test_dir("tests/testthat")
#
# Reactive wiring of the Text analysis tab (R/mod_text_analysis.R):
#
#   * the Language section: breakdown, the "not detected as English" filter,
#     and the install hint when cld2 is missing;
#   * the stopword tiers: the tier selection feeds Top words, Top phrases and
#     the word cloud, and "+ Stop" writes the word once to the chosen tier
#     and drops it from the views;
#   * the "+ Stop" button markup escapes the token.
#
# Stopword files are written to a temporary folder, never to stopwords/.
#
# Fixture words (a French sentence for the detector, a made-up compound token,
# an HTML attribute in the escaping probe):
# cspell:ignore etait froid matin tres oatmilk onerror

source(file.path("..", "..", "R", "ui_helpers.R"))
source(file.path("..", "..", "R", "text_helpers.R"))
source(file.path("..", "..", "R", "stopword_helpers.R"))
source(file.path("..", "..", "R", "mod_text_analysis.R"))


# --- Fixtures ----------------------------------------------------------------

.en_a <- "The oatmilk latte was smooth and the oatmilk foam was thick."
.en_b <- "Oatmilk again today, and the scone was warm and very fresh."
.fr   <- "Le cafe etait froid et le service etait tres lent ce matin."
.xss  <- "<img src=x onerror=alert(1)> this entry is long enough to check"

.reviews <- data.frame(
  order_id = as.character(1:7),
  review   = c(.en_a, .en_a, .en_b, .fr, .xss, "ok", NA),
  stringsAsFactors = FALSE)

.state <- function() {
  shiny::reactiveValues(data = .reviews, meta = NULL,
                        dataset_name = "reviews.csv")
}

.tier_dir <- function(env = parent.frame()) {
  d <- withr::local_tempdir(pattern = "stop words ", .local_envir = env)
  writeLines(c("# base", EN_STOPWORDS), file.path(d, "base_stopwords.txt"))
  writeLines("# project", file.path(d, "custom_stopwords.txt"))
  d
}

.args <- function(dir) {
  list(shared_state = .state(),
       selected_var_r = shiny::reactive("review"),
       stopword_dir = dir)
}

.set_defaults <- function(session) {
  session$setInputs(stop = TRUE, minchars = 3, topn = 25, ngram = 2,
                    sw_tiers = STOPWORD_TIERS, sw_target = "project",
                    only_non_en = TRUE, kw = "")
}

# Stand in an R session without cld2 (helpers live in the global env).
.local_no_cld2 <- function(env = parent.frame()) {
  real <- lang_detection_available
  assign("lang_detection_available", function() FALSE, envir = globalenv())
  withr::defer(assign("lang_detection_available", real, envir = globalenv()),
               envir = env)
}


# Undo htmltools::htmlEscape(attribute = TRUE), as a browser would.
.html_attr_decode <- function(x) {
  entities <- c("&quot;" = "\"", "&#39;" = "'", "&lt;" = "<", "&gt;" = ">",
            "&#10;" = "
", "&#13;" = "
", "&amp;" = "&")
  for (e in names(entities)) x <- gsub(e, entities[[e]], x, fixed = TRUE)
  x
}

# A DT output that renderDT() left empty (returned NULL) has no table header.
.dt_blank <- function(out) !grepl("<th>", out, fixed = TRUE)

# --- "+ Stop" markup ----------------------------------------------------------

test_that(".stop_btn JSON-encodes and attribute-escapes the token", {
  html <- .stop_btn("ta-add_stop", "it's\"<x>")
  onclick <- sub('.*onclick="([^"]*)".*', "\\1", html)
  # Nothing in the attribute can close it or open a tag.
  expect_false(grepl("[\"<>']", onclick))
  # Once the browser decodes the attribute, the handler passes the token as a
  # JSON string literal.
  decoded <- .html_attr_decode(onclick)
  expect_equal(
    decoded,
    paste0("Shiny.setInputValue(\"ta-add_stop\", ",
           jsonlite::toJSON("it's\"<x>", auto_unbox = TRUE),
           ", {priority: 'event'})"))
  expect_true(endsWith(html, ">+ Stop</button>"))
})


# --- Stopword tiers and "+ Stop" ----------------------------------------------

test_that("the tier selection feeds Top words, Top phrases and the cloud", {
  d <- .tier_dir()
  shiny::testServer(mod_text_analysis_server, args = .args(d), {
    .set_defaults(session)
    expect_true("oatmilk" %in% tokens_r()$token)
    expect_false("the" %in% tokens_r()$token)

    # No tiers selected: nothing is removed, even with removal ticked.
    session$setInputs(sw_tiers = character(0))
    expect_true("the" %in% tokens_r()$token)

    # Base only.
    session$setInputs(sw_tiers = "base")
    expect_false("the" %in% tokens_r()$token)
    expect_false(any(grepl("\\bthe\\b", ngrams_r()$ngram)))

    # The cloud renders from the same token table.
    expect_true(grepl("oatmilk", output$wordcloud, fixed = TRUE))
    expect_false(grepl("\"the\"", output$wordcloud, fixed = TRUE))
  })
})

test_that("+ Stop adds the word once and it leaves the views", {
  d <- .tier_dir()
  proj <- file.path(d, "custom_stopwords.txt")
  shiny::testServer(mod_text_analysis_server, args = .args(d), {
    .set_defaults(session)
    expect_equal(tokens_r()$token[1], "oatmilk")
    expect_false(is.null(output$tokens))
    expect_true(any(grepl("oatmilk", ngrams_r()$ngram)))
    expect_true(grepl("oatmilk", output$wordcloud, fixed = TRUE))

    session$setInputs(add_stop = "oatmilk")

    expect_false("oatmilk" %in% tokens_r()$token)
    expect_false(any(grepl("oatmilk", ngrams_r()$ngram)))
    expect_false(grepl("oatmilk", output$wordcloud, fixed = TRUE))
    expect_equal(sum(readLines(proj) == "oatmilk"), 1L)

    # A second press cannot write it twice (and it is no longer offered).
    session$setInputs(add_stop = "oatmilk")
    expect_equal(sum(readLines(proj) == "oatmilk"), 1L)
    expect_false(file.exists(file.path(d, "user_stopwords.txt")))
  })
})

test_that("+ Stop honours the User target and the active tiers", {
  d <- .tier_dir()
  user <- file.path(d, "user_stopwords.txt")
  shiny::testServer(mod_text_analysis_server, args = .args(d), {
    .set_defaults(session)
    # User tier written but not in use: the word stays visible.
    session$setInputs(sw_target = "user", sw_tiers = c("base", "project"))
    session$setInputs(add_stop = "scone")
    expect_equal(readLines(user), "scone")
    expect_true("scone" %in% tokens_r()$token)
    expect_false("scone" %in% .read_stopword_terms(
      file.path(d, "custom_stopwords.txt")))
    # Turning the user tier on hides it.
    session$setInputs(sw_tiers = STOPWORD_TIERS)
    expect_false("scone" %in% tokens_r()$token)
  })
})

test_that("+ Stop ignores words not in the table, and removal switched off", {
  d <- .tier_dir()
  proj <- file.path(d, "custom_stopwords.txt")
  shiny::testServer(mod_text_analysis_server, args = .args(d), {
    .set_defaults(session)
    session$setInputs(add_stop = "espresso")        # not a listed word
    session$setInputs(add_stop = "flat white")      # not a token at all
    session$setInputs(add_stop = 3)                 # wrong type
    expect_equal(readLines(proj), "# project")

    session$setInputs(stop = FALSE)
    expect_true("the" %in% tokens_r()$token)
    session$setInputs(add_stop = "oatmilk")
    expect_equal(readLines(proj), "# project")
    # No "+ Stop" column while removal is off: two header cells, not three.
    expect_equal(lengths(regmatches(output$tokens,
                                    gregexpr("<th>", output$tokens))), 2L)
  })
})

test_that("a failed stopword write leaves the views unchanged", {
  d <- .tier_dir()
  proj <- file.path(d, "custom_stopwords.txt")
  # Make the project tier unwritable by putting a folder where the file goes.
  unlink(proj)
  dir.create(proj)
  shiny::testServer(mod_text_analysis_server, args = .args(d), {
    .set_defaults(session)
    before <- stopword_version()
    session$setInputs(add_stop = "oatmilk")
    expect_equal(stopword_version(), before)
    expect_true("oatmilk" %in% tokens_r()$token)
  })
})


# --- Language -------------------------------------------------------------------

test_that("Language section: breakdown and the non-English filter", {
  skip_if_not_installed("cld2")
  d <- .tier_dir()
  shiny::testServer(mod_text_analysis_server, args = .args(d), {
    .set_defaults(session)
    s <- lang_summary_r()
    expect_named(s, c("language", "n", "share"))
    expect_type(s$share, "double")
    expect_equal(sum(s$n), 6L)                  # the NA row is not counted
    expect_gte(s$n[s$language == "en"], 3L)
    expect_equal(s$n[s$language == "fr"], 1L)
    expect_true("(undetected)" %in% s$language)  # "ok" is too short

    # Share is formatted by DT, not pre-formatted text.
    expect_true(grepl("formatPercentage", output$lang_summary, fixed = TRUE))

    lv <- lang_values_r()
    expect_false(any(lv$language == "en"))
    expect_true(.fr %in% lv$value)
    expect_true("ok" %in% lv$value)
    expect_null(output$lang_values_empty)

    session$setInputs(only_non_en = FALSE)
    lv <- lang_values_r()
    expect_true(.en_a %in% lv$value)
    expect_equal(lv$n[lv$value == .en_a], 2L)    # distinct values, counted
  })
})

test_that("Language section: every value English shows an empty state", {
  skip_if_not_installed("cld2")
  d <- .tier_dir()
  args <- .args(d)
  args$shared_state <- shiny::reactiveValues(
    data = data.frame(review = c(.en_a, .en_b), stringsAsFactors = FALSE),
    meta = NULL, dataset_name = "x.csv")
  shiny::testServer(mod_text_analysis_server, args = args, {
    .set_defaults(session)
    expect_equal(nrow(lang_values_r()), 0L)
    expect_true(grepl("Every value was detected as English",
                      output$lang_values_empty$html, fixed = TRUE))
    expect_true(.dt_blank(output$lang_values))
  })
})

test_that("Language values table keeps dataset text escaped", {
  lv <- lang_values_table(c(.xss, "b"), c(NA, "fr"))
  w <- .lang_values_dt(lv)
  # No escape argument: DT's default escapes every column.
  expect_identical(attr(w$x$options, "escapeIdx"), "true")
  esc <- DT:::escapeData(w$x$data, TRUE, names(w$x$data))
  expect_false(any(grepl("<img", esc$value, fixed = TRUE)))
})

test_that("Top words table escapes every column but the button column", {
  tk <- tibble::tibble(token = c("<b>bold</b>", "latte"), n = c(2L, 1L))
  w <- .top_words_table(tk, "ta-add_stop")
  expect_equal(names(w$x$data), c("token", "n", "stop"))
  idx <- attr(w$x$options, "escapeIdx")
  expect_identical(idx, "\"-3\"")
  esc <- DT:::escapeData(w$x$data, -3, names(w$x$data))
  expect_equal(esc$token[1], "&lt;b&gt;bold&lt;/b&gt;")
  expect_true(startsWith(esc$stop[1], "<button"))
  # The plain variant has no button column.
  expect_equal(names(.top_words_table(tk)$x$data), c("token", "n"))
})

test_that("Language section without cld2: install hint, no tables, no error", {
  .local_no_cld2()
  d <- .tier_dir()
  shiny::testServer(mod_text_analysis_server, args = .args(d), {
    .set_defaults(session)
    expect_null(langs_r())
    expect_null(lang_summary_r())
    expect_true(grepl("install.packages", output$lang_note$html, fixed = TRUE))
    expect_true(.dt_blank(output$lang_summary))
    expect_true(.dt_blank(output$lang_values))
    expect_null(output$lang_summary_empty)
    expect_null(output$lang_values_empty)
    # The rest of the tab still works.
    expect_true("oatmilk" %in% tokens_r()$token)
  })
})

test_that("lang_values_table filters and counts", {
  v <- c("a", "b", "b", "c")
  l <- c("en", NA, NA, "fr")
  out <- lang_values_table(v, l)
  expect_equal(out$value, c("b", "c"))
  expect_equal(out$language, c("(undetected)", "fr"))
  expect_equal(out$n, c(2L, 1L))
  expect_equal(nrow(lang_values_table("a", "en")), 0L)
  expect_equal(nrow(lang_values_table(v, l, only_non_en = FALSE)), 3L)
})
