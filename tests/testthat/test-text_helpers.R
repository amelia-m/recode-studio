# Run with: testthat::test_dir("tests/testthat")

source(file.path("..", "..", "R", "text_helpers.R"))

test_that("count_sentences counts terminators, >=1 for non-empty", {
  expect_equal(count_sentences(c("One. Two.", "no terminator", "", NA)),
               c(2L, 1L, 0L, 0L))
})

test_that("classify_text_length separates long from short", {
  short <- c("latte", "espresso", "flat white", "cold brew", "mocha")
  long  <- c("The latte was rich and creamy and the service was fast. Staff were friendly.",
             "My cappuccino arrived lukewarm and the line moved slowly. Otherwise it was fine.")
  expect_equal(classify_text_length(short), "short")
  expect_equal(classify_text_length(long),  "long")
  expect_equal(classify_text_length(character(0)), "short")
})

test_that("top_tokens ranks frequency and honors stopwords / min_chars", {
  vals <- c("the the cat", "the cat", "the bird")
  out <- top_tokens(vals, n = 10)
  expect_equal(out$token[1], "the")              # "the" strictly most frequent
  expect_equal(out$n[out$token == "the"], 4L)
  out2 <- top_tokens(vals, remove_stopwords = TRUE, min_chars = 3)
  expect_false("the" %in% out2$token)            # stopword removed
  expect_true(all(nchar(out2$token) >= 3))       # min_chars honored
  expect_equal(out2$n[out2$token == "cat"], 2L)
})

test_that("top_ngrams builds contiguous phrases", {
  vals <- c("cold brew coffee", "cold brew coffee")
  out <- top_ngrams(vals, ngram = 2, n = 5)
  expect_true("cold brew" %in% out$ngram)
  expect_equal(out$n[out$ngram == "cold brew"], 2L)
  expect_equal(top_ngrams("one word", ngram = 3)$ngram, character(0))  # too short
})

test_that("kwic returns context windows around a term", {
  vals <- c("the quick brown fox jumps over the lazy dog")
  out <- kwic(vals, "fox", window = 2)
  expect_equal(nrow(out), 1)
  expect_equal(out$keyword, "fox")
  expect_equal(out$before, "quick brown")
  expect_equal(out$after, "jumps over")
  expect_equal(nrow(kwic(vals, "")), 0)          # empty term -> no rows
})

test_that("text_length_stats returns per-value chars/words/sentences", {
  s <- text_length_stats(c("Hello world.", "One two three four."))
  expect_named(s, c("chars", "words", "sentences"))
  expect_equal(s$words, c(2L, 4L))
  expect_equal(s$sentences, c(1L, 1L))
})

# cspell:ignore etait froid matin tres oatmilk

# --- Custom stopword lists ------------------------------------------------------

test_that("top_tokens honours a custom stopword list", {
  x <- rep("coffee oatmilk coffee the", 3)
  default <- top_tokens(x, remove_stopwords = TRUE, min_chars = 1)
  expect_true("oatmilk" %in% default$token)
  expect_false("the" %in% default$token)
  custom <- top_tokens(x, remove_stopwords = TRUE, min_chars = 1,
                       stopwords = "oatmilk")
  expect_false("oatmilk" %in% custom$token)
  expect_true("coffee" %in% custom$token)
  # The custom list REPLACES the default one.
  expect_true("the" %in% custom$token)
  # Ignored when removal is off.
  off <- top_tokens(x, remove_stopwords = FALSE, stopwords = "oatmilk")
  expect_true("oatmilk" %in% off$token)
})

test_that("top_tokens and top_ngrams defaults are unchanged", {
  x <- c("the latte and the scone", "a latte with a scone")
  expect_identical(top_tokens(x, remove_stopwords = TRUE),
                   top_tokens(x, remove_stopwords = TRUE,
                              stopwords = EN_STOPWORDS))
  expect_identical(top_ngrams(x, remove_stopwords = TRUE),
                   top_ngrams(x, remove_stopwords = TRUE,
                              stopwords = EN_STOPWORDS))
})

test_that("top_ngrams honours a custom stopword list", {
  x <- rep("alpha oatmilk gamma", 3)
  custom <- top_ngrams(x, ngram = 2, remove_stopwords = TRUE, min_chars = 1,
                       stopwords = "oatmilk")
  expect_equal(custom$ngram, "alpha gamma")
  expect_equal(custom$n, 3L)
})

test_that("an empty stopword list removes nothing", {
  x <- "the latte"
  expect_setequal(top_tokens(x, remove_stopwords = TRUE,
                             stopwords = character(0))$token,
                  c("the", "latte"))
})

# --- Language detection ---------------------------------------------------------

# Stand in an R session without cld2. The helpers are source()d into the
# global environment, so the stand-in goes there and is restored afterwards.
.local_no_cld2 <- function(env = parent.frame()) {
  real <- lang_detection_available
  assign("lang_detection_available", function() FALSE, envir = globalenv())
  withr::defer(assign("lang_detection_available", real, envir = globalenv()),
               envir = env)
}

.en_long <- "The flat white was smooth and the staff were very friendly."
.fr_long <- "Le cafe etait froid et le service etait tres lent ce matin."

test_that("detect_languages returns one code per value, NA for short / NA input", {
  out <- detect_languages(c(.en_long, "hi", NA_character_, "   padded   "))
  expect_length(out, 4)
  expect_type(out, "character")
  expect_true(is.na(out[2]))   # under min_chars
  expect_true(is.na(out[3]))   # NA input
  expect_true(is.na(out[4]))   # under min_chars once trimmed
  expect_identical(detect_languages(character(0)), character(0))
})

test_that("detect_languages identifies languages when cld2 is available", {
  skip_if_not_installed("cld2")
  expect_equal(detect_languages(c(.en_long, .fr_long)), c("en", "fr"))
  # min_chars is honoured.
  expect_equal(detect_languages(.en_long, min_chars = 500), NA_character_)
})

test_that("detect_languages degrades to all-NA without cld2, never errors", {
  .local_no_cld2()
  expect_false(lang_detection_available())
  out <- detect_languages(c(.en_long, .fr_long, NA))
  expect_identical(out, rep(NA_character_, 3))
  s <- language_summary(c(.en_long, .fr_long))
  expect_equal(s$language, "(undetected)")
  expect_equal(s$n, 2L)
})

test_that("language_summary counts, shares sum to 1, stays numeric", {
  skip_if_not_installed("cld2")
  s <- language_summary(c(.en_long, .en_long, .fr_long, "hi"))
  expect_named(s, c("language", "n", "share"))
  expect_type(s$share, "double")
  expect_equal(sum(s$n), 4L)
  expect_equal(sum(s$share), 1)
  expect_equal(s$language[1], "en")         # most frequent first
  expect_equal(s$n[1], 2L)
  expect_true(all(c("fr", "(undetected)") %in% s$language))
})

test_that("language_summary / summarise_languages handle empty input", {
  s <- language_summary(character(0))
  expect_equal(nrow(s), 0)
  expect_named(s, c("language", "n", "share"))
  expect_identical(summarise_languages(c("en", NA, "en")),
                   tibble::tibble(language = c("en", "(undetected)"),
                                  n = c(2L, 1L), share = c(2, 1) / 3))
})
