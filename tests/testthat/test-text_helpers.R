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
