# Run with: testthat::test_dir("tests/testthat")

source(file.path("..", "..", "R", "text_helpers.R"))  # build_meta() uses these
source(file.path("..", "..", "R", "data_loader.R"))

# A small all-character fixture shaped like read_dataset() output.
.fixture <- function() {
  drinks <- c("cappuccino","capuccino","cappucino","capccino",
              "espresso","esspresso","expresso","macchiato","machiato",
              "americano","americanno","flat white","latte","mocha",
              "moccha","cortado","cold brew","hot chocolate","chai latte",
              "affogato")
  n <- length(drinks)
  tibble::tibble(
    id         = as.character(seq_len(n)),                 # numeric-like
    drink      = drinks,                                   # free text
    city       = paste0("City", sprintf("%02d", seq_len(n))),  # 20 unique text
    status     = rep(c("Yes","No"), length.out = n),       # 2-value choice set
    amount     = as.character(seq_len(n) * 1.5),           # numeric-like
    visit_date = sprintf("2026-01-%02d", seq_len(n))       # date-like
  )
}

test_that("build_meta classifies free-text vs non-candidates", {
  meta <- build_meta(.fixture())
  expect_named(meta, c("column","group","n_unique","na_share","median_len",
                       "median_words","is_free_text_candidate","is_long_text",
                       "text_kind","lang_share_en"))
  pick <- function(col) meta$is_free_text_candidate[meta$column == col]
  expect_true(pick("drink"))
  expect_true(pick("city"))
  expect_false(pick("id"))         # numeric-like
  expect_false(pick("amount"))     # numeric-like
  expect_false(pick("visit_date")) # date-like
  expect_false(pick("status"))     # only 2 unique values
})

test_that("build_meta counts uniques and NA share", {
  df <- tibble::tibble(x = c("a","a","b", NA, ""))
  meta <- build_meta(df)
  row <- meta[meta$column == "x", ]
  expect_equal(row$n_unique, 2L)           # "a","b" (NA + "" excluded)
  expect_equal(row$na_share, 0.4)          # NA + "" => 2/5
})

test_that("build_meta handles an empty data frame", {
  meta <- build_meta(tibble::tibble())
  expect_equal(nrow(meta), 0)
  expect_named(meta, c("column","group","n_unique","na_share","median_len",
                       "median_words","is_free_text_candidate","is_long_text",
                       "text_kind","lang_share_en"))
})

test_that("build_meta flags long-text vs short-text candidates", {
  long <- paste0("Order ", seq_len(24),
                 ": the service was excellent and the staff were friendly. ",
                 "Coffee was rich and the room felt calm and quiet.")
  short <- rep(c("latte","espresso","mocha","cortado","americano",
                 "cold brew","flat white","cappuccino","macchiato",
                 "ristretto","affogato","cubano"), 2)
  df <- tibble::tibble(review = long, drink = short)
  meta <- build_meta(df)
  expect_true(meta$is_long_text[meta$column == "review"])
  expect_equal(meta$text_kind[meta$column == "review"], "long")
  expect_false(meta$is_long_text[meta$column == "drink"])
  expect_equal(meta$text_kind[meta$column == "drink"], "short")
})

test_that(".is_id_name matches id / _id but not id-suffixed words", {
  expect_true(.is_id_name("id"))
  expect_true(.is_id_name("ID"))
  expect_true(.is_id_name("case_id"))
  expect_true(.is_id_name("sample_case_Id"))
  expect_false(.is_id_name("paid"))
  expect_false(.is_id_name("grid"))
  expect_false(.is_id_name("identifier"))
  expect_false(.is_id_name("id_number"))
})

test_that("build_meta excludes identifier-named columns from free text", {
  # High-cardinality alphanumeric ids clear every content heuristic (median
  # length 8, 0% NA, 60 uniques, neither date- nor numeric-like) -- only the
  # structural name guard rejects them.
  ids <- sprintf("AB%06d", seq_len(60))
  df <- tibble::tibble(
    sample_case_id = ids,
    id             = ids,
    paid           = ids   # control: "id"-suffixed word, still free text
  )
  meta <- build_meta(df)
  pick <- function(col) meta$is_free_text_candidate[meta$column == col]
  expect_false(pick("sample_case_id"))
  expect_false(pick("id"))
  expect_true(pick("paid"))
})

test_that("build_meta does NOT exclude 'label'-named columns (domain-neutral)", {
  # Deliberate divergence from the upstream copy: `label$` is a coded-value-twin
  # convention there, not a general one.
  df <- tibble::tibble(
    product_label = c("blue widget","red widget","green widget","teal widget",
                      "black widget","white widget","grey widget","pink widget",
                      "amber widget","olive widget","coral widget","navy widget")
  )
  expect_true(build_meta(df)$is_free_text_candidate)
})

test_that("bundled example dataset classification is unchanged", {
  # Regression guard for the free-text classifier (see CLAUDE.md Gotchas).
  path <- file.path("..", "..", "inst", "extdata", "example_messy.csv")
  skip_if_not(file.exists(path), "bundled example dataset not found")
  meta <- build_meta(read_dataset(path))
  pick <- function(col) meta$is_free_text_candidate[meta$column == col]
  for (col in c("drink1", "drink2", "city", "notes", "review")) {
    expect_true(pick(col), info = col)
  }
  for (col in c("order_id", "size", "price", "order_date")) {
    expect_false(pick(col), info = col)
  }
})

# cspell:ignore etait froid matin tres
# Distinct English sentences (a candidate needs more than 10 unique values).
.en_notes <- function(n) {
  paste0("Order ", seq_len(n),
         ": the flat white was smooth and the staff were very friendly.")
}

test_that("build_meta reports lang_share_en for free-text candidates only", {
  df <- tibble::tibble(
    note = c(.en_notes(12),
             "Le cafe etait froid et le service etait tres lent ce matin."),
    code = as.character(1:13)
  )
  m <- build_meta(df)
  expect_true("lang_share_en" %in% names(m))
  expect_type(m$lang_share_en, "double")
  # Non-candidate columns are never scanned.
  expect_true(is.na(m$lang_share_en[m$column == "code"]))
  # Reported only: the candidate flag is untouched.
  expect_true(m$is_free_text_candidate[m$column == "note"])
  expect_false(m$is_free_text_candidate[m$column == "code"])
})

test_that("build_meta lang_share_en is the English share of detectable values", {
  skip_if_not_installed("cld2")
  df <- tibble::tibble(
    note = c(.en_notes(12),
             "Le cafe etait froid et le service etait tres lent ce matin.",
             # Too short to detect: excluded from the share, not counted as
             # non-English.
             "ok", "fine", "good", "meh")
  )
  s <- build_meta(df)$lang_share_en
  expect_gte(s, 0)
  expect_lte(s, 1)
  expect_equal(s, 12 / 13)
})

test_that("build_meta lang_share_en is NA when nothing is detectable", {
  # All values under the detection minimum.
  df <- tibble::tibble(drink = c("latte", "espresso", "mocha", "cortado",
                                 "americano", "cold brew", "flat white",
                                 "cappuccino", "macchiato", "ristretto",
                                 "affogato"))
  m <- build_meta(df)
  expect_true(m$is_free_text_candidate)
  expect_true(is.na(m$lang_share_en))
})

test_that("build_meta lang_share_en is NA for every column without cld2", {
  # Stand in an R session without cld2. The helpers are source()d into the
  # global environment, so the stand-in goes there and is restored after.
  real <- lang_detection_available
  assign("lang_detection_available", function() FALSE, envir = globalenv())
  withr::defer(assign("lang_detection_available", real, envir = globalenv()))
  df <- tibble::tibble(note = .en_notes(12))
  m <- build_meta(df)
  expect_true(m$is_free_text_candidate)
  expect_true(is.na(m$lang_share_en))
})

test_that("bundled example: lang_share_en set only on the candidates", {
  path <- file.path("..", "..", "inst", "extdata", "example_messy.csv")
  skip_if_not(file.exists(path), "bundled example dataset not found")
  meta <- build_meta(read_dataset(path))
  non <- meta$column[!meta$is_free_text_candidate]
  expect_true(all(is.na(meta$lang_share_en[meta$column %in% non])))
  skip_if_not_installed("cld2")
  # The sentence-length review column is detectable and mostly English.
  expect_gt(meta$lang_share_en[meta$column == "review"], 0.5)
})

test_that("column_group extracts the prefix before the first underscore", {
  expect_equal(column_group("shop_drink1"), "shop")
  expect_equal(column_group("drink"), "(none)")
  expect_equal(column_group(c("a_b","c_d","plain")), c("a","c","(none)"))
})

test_that("suggest_sibling_pattern finds a trailing-digit family", {
  cols <- c("drink1","drink2","drink3","other")
  expect_equal(suggest_sibling_pattern("drink1", cols), "^drink[0-9]+$")
})

test_that("suggest_sibling_pattern returns NA when no family", {
  cols <- c("drink1","other","city")
  expect_true(is.na(suggest_sibling_pattern("drink1", cols)))   # only one drink#
  expect_true(is.na(suggest_sibling_pattern("city", cols)))     # no trailing digit
})

test_that("suggest_sibling_pattern escapes regex metacharacters in the stem", {
  # Regression for the TRE 'Invalid contents of {}' bug: a stem with a dot
  # must be escaped, and grep must not error.
  cols <- c("a.b1","a.b2","a.bx")
  pat <- suggest_sibling_pattern("a.b1", cols)
  expect_equal(pat, "^a\\.b[0-9]+$")
  expect_silent(grep(pat, cols))
  expect_setequal(grep(pat, cols, value = TRUE), c("a.b1","a.b2"))
})

test_that(".is_date_like / .is_numeric_like detect their shapes", {
  expect_true(.is_date_like(c("2026-01-04","2026-01-05","1/2/2026")))
  expect_false(.is_date_like(c("cappuccino","flat white")))
  expect_true(.is_numeric_like(c("1","2.5","-3","40")))
  expect_false(.is_numeric_like(c("1","two","3")))
})

test_that("read_dataset reads a CSV as all-character", {
  tmp <- tempfile(fileext = ".csv"); on.exit(unlink(tmp), add = TRUE)
  readr::write_csv(tibble::tibble(a = 1:3, b = c("x","y","z")), tmp)
  df <- read_dataset(tmp)
  expect_equal(nrow(df), 3)
  expect_true(all(vapply(df, is.character, logical(1))))
  expect_equal(df$a, c("1","2","3"))
})

test_that("dataset_sheets returns empty for a CSV", {
  expect_equal(dataset_sheets("foo.csv"), character(0))
})
