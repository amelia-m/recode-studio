# Run with: testthat::test_dir("tests/testthat")

source(file.path("..", "..", "R", "string_helpers.R"))

test_that("empty_recodes_tibble has expected schema", {
  x <- empty_recodes_tibble()
  expect_named(x, c(
    "rule_id", "variable", "apply_to_siblings", "sibling_pattern",
    "match_type", "old_value", "new_value", "action", "notes",
    "author", "created_at", "updated_at", "source_dataset"
  ))
  expect_equal(nrow(x), 0)
})

test_that("normalize_value applies match_type strategy", {
  expect_equal(normalize_value("  Foo  Bar  ", "trimmed_ci"), "foo bar")
  expect_equal(normalize_value("Foo Bar", "exact_ci"),        "foo bar")
  expect_equal(normalize_value("Foo Bar", "exact"),           "Foo Bar")
})

test_that("cluster_strings groups a variant family", {
  values <- c("asphyxiation", "ascphyxiation", "ashphxiation",
              "asphyziation", "gunshot wound")
  out <- cluster_strings(values, threshold = 0.85, algorithm = "jw")
  expect_equal(nrow(out), 5)
  variants <- c("asphyxiation", "ascphyxiation", "ashphxiation", "asphyziation")
  expect_length(unique(out$cluster_id[out$value %in% variants]), 1L)
  asph <- out$cluster_id[out$value == "asphyxiation"]
  gun  <- out$cluster_id[out$value == "gunshot wound"]
  expect_false(asph == gun)
})

test_that("cluster_strings handles empty input", {
  out <- cluster_strings(character(0))
  expect_equal(nrow(out), 0)
  expect_named(out, c("value", "n", "cluster_id", "is_rare"))
})

test_that("write_recodes -> read_recodes preserves NA encoding", {
  rules <- tibble::tibble(
    rule_id = c("a", "b"), variable = c("cause1", "city"),
    apply_to_siblings = c(TRUE, FALSE),
    sibling_pattern = c("^cause[0-9]+$", NA_character_),
    match_type = c("trimmed_ci", "trimmed_ci"),
    old_value = c("ascphyxiation", "linclon"),
    new_value = c("asphyxiation", NA_character_),
    action = c("recode", "delete"),
    notes = c("typo", NA_character_), author = c("t", "t"),
    created_at = c("2026-05-27T10:00:00", "2026-05-27T10:00:00"),
    updated_at = c("2026-05-27T10:00:00", "2026-05-27T10:00:00"),
    source_dataset = c("example_messy.csv", "example_messy.csv")
  )
  tmp <- tempfile(fileext = ".csv"); on.exit(unlink(tmp), add = TRUE)
  write_recodes(rules, tmp)
  rt <- read_recodes(tmp)
  expect_equal(nrow(rt), 2)
  expect_true(is.na(rt$new_value[2]))
  expect_equal(rt$apply_to_siblings, rules$apply_to_siblings)
})

test_that("recode_rule_id is stable", {
  a <- recode_rule_id("cause1", "trimmed_ci", "ascphyxiation")
  b <- recode_rule_id("cause1", "trimmed_ci", "ascphyxiation")
  c <- recode_rule_id("cause1", "trimmed_ci", "asphyxiation")
  expect_equal(a, b)
  expect_false(a == c)
  expect_equal(nchar(a), 12L)
})

test_that("validate_recodes flags duplicate key, rule chain, blank new_value", {
  rules <- tibble::tibble(
    rule_id = c("r1","r2","r3","r4","r5"),
    variable = rep("cause1", 5),
    apply_to_siblings = rep(FALSE, 5), sibling_pattern = rep(NA_character_, 5),
    match_type = rep("trimmed_ci", 5),
    old_value = c("a","a","b","x","c"),
    new_value = c("b","b","c","","a"),
    action = rep("recode", 5),
    notes = rep(NA_character_, 5), author = rep("t", 5),
    created_at = rep("", 5), updated_at = rep("", 5),
    source_dataset = rep("d", 5)
  )
  issues <- validate_recodes(rules)
  expect_true(nrow(issues$duplicate_keys) >= 1)
  expect_true(nrow(issues$blank_new_value) >= 1)
  expect_true(nrow(issues$rule_chains) >= 1)
})

test_that("apply_recodes expands to sibling columns when requested", {
  df <- tibble::tibble(
    cause1 = c("ascphyxiation", "x"),
    cause2 = c("ascphyxiation", NA_character_),
    other  = c("ascphyxiation", "z")
  )
  rules <- tibble::tibble(
    rule_id = "r1", variable = "cause1",
    apply_to_siblings = TRUE, sibling_pattern = "^cause[0-9]+$",
    match_type = "trimmed_ci", old_value = "ascphyxiation",
    new_value = "asphyxiation", action = "recode",
    notes = NA_character_, author = "t",
    created_at = "", updated_at = "", source_dataset = "d"
  )
  res <- apply_recodes(df, rules)
  expect_equal(res$df$cause1[1], "asphyxiation")
  expect_equal(res$df$cause2[1], "asphyxiation")
  expect_equal(res$df$other[1], "ascphyxiation")  # untouched
  expect_equal(res$summary$cells_changed, 2L)
})

test_that("apply_recodes delete sets NA", {
  df <- tibble::tibble(x = c("foo", "bar"))
  rules <- tibble::tibble(
    rule_id = "r1", variable = "x", apply_to_siblings = FALSE,
    sibling_pattern = NA_character_, match_type = "trimmed_ci",
    old_value = "foo", new_value = NA_character_, action = "delete",
    notes = NA_character_, author = "t",
    created_at = "", updated_at = "", source_dataset = "d"
  )
  res <- apply_recodes(df, rules)
  expect_true(is.na(res$df$x[1]))
  expect_equal(res$df$x[2], "bar")
})

test_that("generate_recode_R parses and contains case_when", {
  rules <- tibble::tibble(
    rule_id = c("r1","r2"), variable = c("cause1","cause1"),
    apply_to_siblings = c(FALSE, FALSE), sibling_pattern = c(NA_character_, NA_character_),
    match_type = c("trimmed_ci","trimmed_ci"),
    old_value = c("ascphyxiation","gone"),
    new_value = c("asphyxiation", NA_character_),
    action = c("recode","delete"),
    notes = c("typo","x"), author = c("t","t"),
    created_at = c("",""), updated_at = c("",""),
    source_dataset = c("d","d")
  )
  code <- generate_recode_R(rules, dataset_id = "example_messy")
  expect_silent(parse(text = code))
  expect_true(grepl("case_when", code))
  expect_true(grepl("NA_character_", code))
})

test_that("generate_recode_R handles values with quotes and backslashes", {
  rules <- tibble::tibble(
    rule_id = "r1", variable = "x",
    apply_to_siblings = FALSE, sibling_pattern = NA_character_,
    match_type = "trimmed_ci",
    old_value = 'it\'s "quoted"', new_value = "clean\\value",
    action = "recode", notes = "multi\nline note", author = "t",
    created_at = "", updated_at = "", source_dataset = "d"
  )
  code <- generate_recode_R(rules, "test_dataset")
  expect_silent(parse(text = code))
})

test_that("generate_recode_R sibling rule uses across()", {
  rules <- tibble::tibble(
    rule_id = "r1", variable = "cause1",
    apply_to_siblings = TRUE, sibling_pattern = "^cause[0-9]+$",
    match_type = "trimmed_ci", old_value = "ascphyxiation",
    new_value = "asphyxiation", action = "recode",
    notes = NA_character_, author = "t",
    created_at = "", updated_at = "", source_dataset = "d"
  )
  code <- generate_recode_R(rules, "example_messy")
  expect_silent(parse(text = code))
  expect_true(grepl("across", code))
  expect_true(grepl("matches", code))
})
