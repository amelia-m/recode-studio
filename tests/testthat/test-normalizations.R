# Run with: testthat::test_dir("tests/testthat")
#
# Column normalizations: the engine in R/string_helpers.R (ops, rule id,
# apply, preview, validation, CSV round trip, spec stamp, codegen), the norms
# proxy, and the Normalize tab (R/mod_normalize.R).
#
# The apply/codegen agreement test WITH normalizations lives next to the
# original one in test-string_helpers.R.

source(file.path("..", "..", "R", "ui_helpers.R"))
source(file.path("..", "..", "R", "string_helpers.R"))
source(file.path("..", "..", "R", "recat_helpers.R"))
source(file.path("..", "..", "R", "data_loader.R"))
source(file.path("..", "..", "R", "rule_proxies.R"))
source(file.path("..", "..", "R", "rule_warnings.R"))
# mod_preview_export.R calls app_version_from_description(); without this the
# file only passed when an earlier test file had sourced export_bundle.R.
source(file.path("..", "..", "R", "export_bundle.R"))
source(file.path("..", "..", "R", "mod_preview_export.R"))
source(file.path("..", "..", "R", "mod_normalize.R"))

# --- Fixtures ----------------------------------------------------------------

mk_norm <- function(variable, ops, siblings = FALSE, pattern = NA_character_) {
  pat <- if (siblings) pattern else NA_character_
  tibble::tibble(
    norm_id           = normalization_rule_id(variable, pat),
    variable          = variable,
    apply_to_siblings = siblings,
    sibling_pattern   = pat,
    ops               = normalize_ops_join(ops),
    notes             = NA_character_,
    author            = "test",
    created_at        = "2026-01-01T00:00:00",
    updated_at        = "2026-01-01T00:00:00",
    source_dataset    = "cafe"
  )
}

mk_recode <- function(variable, match_type, old_value, new_value,
                      siblings = FALSE, pattern = NA_character_) {
  tibble::tibble(
    rule_id           = recode_rule_id(variable, match_type, old_value),
    variable          = variable,
    apply_to_siblings = siblings,
    sibling_pattern   = pattern,
    match_type        = match_type,
    old_value         = old_value,
    new_value         = new_value,
    action            = "recode",
    notes             = NA_character_,
    author            = "test",
    created_at        = "2026-01-01T00:00:00",
    updated_at        = "2026-01-01T00:00:00",
    source_dataset    = "cafe"
  )
}

eval_script <- function(df, rules, norms) {
  code <- generate_recode_R(rules, "cafe", normalizations = norms)
  env  <- new.env(parent = globalenv())
  env$df <- df
  suppressWarnings(suppressMessages(eval(parse(text = code), envir = env)))
  env$df
}

# --- Schema, ops, id ---------------------------------------------------------

test_that("empty_normalizations_tibble has the expected schema", {
  x <- empty_normalizations_tibble()
  expect_named(x, c("norm_id", "variable", "apply_to_siblings",
                    "sibling_pattern", "ops", "notes", "author",
                    "created_at", "updated_at", "source_dataset"))
  expect_equal(nrow(x), 0)
  expect_type(x$apply_to_siblings, "logical")
  expect_identical(NORMALIZE_OPS,
                   c("trim", "squish", "lowercase", "uppercase", "titlecase"))
  expect_true(all(NORMALIZE_OPS %in% names(NORMALIZE_OP_LABELS)))
  expect_true(all(NORMALIZE_OPS %in% names(.NORMALIZE_FUNS)))
})

test_that("apply_normalize_ops does each op, and leaves NA and empty alone", {
  expect_equal(apply_normalize_ops("  latte  ", "trim"), "latte")
  expect_equal(apply_normalize_ops(" oat   milk ", "squish"), "oat milk")
  expect_equal(apply_normalize_ops("LATTE", "lowercase"), "latte")
  expect_equal(apply_normalize_ops("latte", "uppercase"), "LATTE")
  expect_equal(apply_normalize_ops("flat white", "titlecase"), "Flat White")
  expect_true(is.na(apply_normalize_ops(NA_character_, "titlecase")))
  expect_identical(apply_normalize_ops(c("", NA, " "), c("squish", "uppercase")),
                   c("", NA, ""))
  # No ops, or only unrecognised ones, is a no-op rather than an error.
  expect_equal(apply_normalize_ops("As Is", character(0)), "As Is")
  expect_equal(apply_normalize_ops("As Is", "sentencecase"), "As Is")
})

test_that("ops are stored and applied in one canonical order", {
  expect_equal(normalize_ops_join(c("titlecase", "squish")), "squish;titlecase")
  expect_equal(normalize_ops_join(c("squish", "titlecase")), "squish;titlecase")
  x <- c(" flat   white ", "MOCHA")
  expect_equal(apply_normalize_ops(x, c("titlecase", "squish")),
               apply_normalize_ops(x, c("squish", "titlecase")))
  expect_equal(normalize_ops_join(c("titlecase", "banana")), "titlecase")
  expect_true("banana" %in% normalize_ops_split("titlecase;banana"))
  expect_identical(normalize_ops_split(NA_character_), character(0))
})

test_that("a normalization id is keyed on the TARGET, not on the ops", {
  expect_identical(mk_norm("city", "titlecase")$norm_id,
                   mk_norm("city", "uppercase")$norm_id)
  expect_false(identical(mk_norm("city", "titlecase")$norm_id,
                         mk_norm("drink1", "titlecase")$norm_id))
  expect_false(identical(
    mk_norm("drink1", "uppercase")$norm_id,
    mk_norm("drink1", "uppercase", TRUE, "^drink[0-9]+$")$norm_id))
  expect_match(mk_norm("city", "trim")$norm_id, "^[0-9a-f]{12}$")
})

# --- Apply ---------------------------------------------------------------------

test_that("apply_normalizations rewrites a column and counts the cells", {
  df <- tibble::tibble(
    city  = c("uptown", "UPTOWN", "Uptown", NA, "  old   town  "),
    other = c("uptown", "UPTOWN", "Uptown", NA, "  old   town  "))
  res <- apply_normalizations(df, mk_norm("city", c("squish", "titlecase")))
  expect_equal(res$df$city, c("Uptown", "Uptown", "Uptown", NA, "Old Town"))
  expect_equal(res$df$other, df$other)
  expect_equal(res$summary$cells_changed, 3L)   # "Uptown" was already right
  expect_equal(res$summary$n_cols, 1L)
  expect_equal(res$summary$columns, "city")
  expect_equal(res$summary$ops, "squish;titlecase")
})

test_that("apply_normalizations expands to sibling columns", {
  df <- tibble::tibble(drink1 = c("latte", "mocha"),
                       drink2 = c("latte", NA),
                       Drink9 = c("latte", "latte"),
                       notes  = c("latte", "latte"))
  res <- apply_normalizations(
    df, mk_norm("drink1", "uppercase", TRUE, "^drink[0-9]+$"))
  expect_equal(res$df$drink1, c("LATTE", "MOCHA"))
  expect_equal(res$df$drink2, c("LATTE", NA))
  expect_equal(res$df$Drink9, c("latte", "latte"))   # case-sensitive pattern
  expect_equal(res$df$notes, c("latte", "latte"))
  expect_equal(res$summary$cells_changed, 3L)
  expect_equal(res$summary$n_cols, 2L)
  expect_equal(res$summary$columns, "drink1;drink2")
})

test_that("an empty rule set and a missing column are both no-ops", {
  df <- tibble::tibble(x = c("a", "b"))
  expect_identical(apply_normalizations(df, empty_normalizations_tibble())$df, df)
  expect_identical(apply_normalizations(df, NULL)$df, df)
  res <- apply_normalizations(df, mk_norm("nope", "uppercase"))
  expect_identical(res$df, df)
  expect_equal(res$summary$n_cols, 0L)
  expect_equal(res$summary$cells_changed, 0L)
})

test_that("a column with no normalization rule passes through untouched", {
  df <- tibble::tibble(city = c("  uptown ", "UPTOWN"),
                       order_code = c("A-001", "b-002"))
  out <- apply_normalizations(df, mk_norm("city", c("squish", "uppercase")))
  expect_equal(out$df$city, c("UPTOWN", "UPTOWN"))
  expect_identical(out$df$order_code, df$order_code)
})

# --- Preview -------------------------------------------------------------------

test_that("normalization_preview shows the lossy pairs before anything runs", {
  df <- tibble::tibble(city = c("McKenzie", "McKenzie", "harbor", "Harbor"))
  p  <- normalization_preview(df, mk_norm("city", "titlecase"))
  expect_named(p, c("column", "before", "after", "n_cells"))
  expect_equal(nrow(p), 2)
  expect_equal(p$n_cells[1], 2L)                       # most-affected first
  expect_true("Mckenzie" %in% p$after)
  expect_true("McKenzie" %in% p$before)
  expect_equal(nrow(normalization_preview(
    tibble::tibble(city = "Harbor"), mk_norm("city", "titlecase"))), 0)
  expect_equal(nrow(normalization_preview(NULL, mk_norm("city", "trim"))), 0)
})

test_that("recode_value_map counts distinct moved pairs, NA-aware", {
  before <- tibble::tibble(a = c("x", "x", NA, "y"), b = c("1", "1", "1", "1"))
  after  <- tibble::tibble(a = c("X", "X", "z", "y"), b = before$b)
  m <- recode_value_map(before, after)
  expect_equal(nrow(m), 2)
  expect_equal(m$n_cells[m$raw_value %in% "x"], 2L)
  expect_true(any(is.na(m$raw_value) & m$clean_value == "z"))
})

# --- Validation ----------------------------------------------------------------

test_that("validate_normalizations reports each issue class", {
  bad_ops <- mk_norm("c", "titlecase"); bad_ops$ops <- "titlecase;banana"
  rules <- dplyr::bind_rows(
    mk_norm("a", character(0)),
    mk_norm("b", c("lowercase", "uppercase")),
    bad_ops,
    mk_norm("d", "titlecase"),
    mk_norm("d", "uppercase"),
    mk_norm("drink1", "trim"),
    mk_norm("drink2", "trim", TRUE, "^drink[0-9]+$"))
  data <- tibble::tibble(a = "x", b = "x", d = "x", drink1 = "x", drink2 = "y")
  iss <- validate_normalizations(rules, data = data)
  expect_named(iss, c("no_ops", "invalid_ops", "conflicting_case",
                      "duplicate_targets", "missing_column",
                      "overlapping_targets"))
  expect_equal(iss$no_ops$variable, "a")
  expect_equal(iss$conflicting_case$variable, "b")
  expect_match(iss$invalid_ops$detail, "banana")
  expect_equal(sort(unique(iss$duplicate_targets$variable)), "d")
  expect_equal(iss$missing_column$variable, "c")
  expect_true(all(c("d", "drink1", "drink2") %in%
                    iss$overlapping_targets$variable))
  # Without data, the data-dependent classes stay empty.
  nodata <- validate_normalizations(rules)
  expect_equal(nrow(nodata$missing_column), 0)
  expect_equal(nrow(nodata$overlapping_targets), 0)
  # A clean set produces no issues at all.
  clean <- validate_normalizations(mk_norm("a", "titlecase"),
                                   data = tibble::tibble(a = "x"))
  expect_true(all(vapply(clean, nrow, integer(1)) == 0L))
})

# --- CSV round trip ----------------------------------------------------------

test_that("normalizations round-trip through the CSV", {
  path <- tempfile(fileext = ".csv"); on.exit(unlink(path), add = TRUE)
  rules <- dplyr::bind_rows(mk_norm("city", c("squish", "titlecase")),
                            mk_norm("drink1", "uppercase", TRUE,
                                    "^drink[0-9]+$"))
  write_normalizations(rules, path)
  expect_identical(strsplit(readLines(path, n = 1), ",", fixed = TRUE)[[1]],
                   names(empty_normalizations_tibble()))
  back <- read_normalizations(path)
  expect_identical(lapply(back, identity), lapply(rules, identity))
  expect_identical(read_normalizations(file.path(dirname(path), "nope.csv")),
                   empty_normalizations_tibble())
})

test_that("reading a partial or <NA>-encoded normalizations CSV conforms it", {
  path <- tempfile(fileext = ".csv"); on.exit(unlink(path), add = TRUE)
  writeLines(c("variable,ops,sibling_pattern,apply_to_siblings,norm_id",
               "city,titlecase,<NA>,FALSE,id1",
               "drink1,uppercase,,false,id2"), path)
  back <- read_normalizations(path)
  expect_named(back, names(empty_normalizations_tibble()))
  expect_true(all(is.na(back$sibling_pattern)))
  expect_identical(back$apply_to_siblings, c(FALSE, FALSE))
  raw <- read_normalizations(path, conform = FALSE)
  expect_named(raw, c("variable", "ops", "sibling_pattern",
                      "apply_to_siblings", "norm_id"))
})

# --- Spec stamp ----------------------------------------------------------------

test_that("normalization_spec_string is per-column and canonical", {
  norms <- dplyr::bind_rows(
    mk_norm("city", c("titlecase", "squish")),
    mk_norm("drink1", "uppercase", TRUE, "^drink[0-9]+$"))
  expect_equal(normalization_spec_string("city", norms), "city=squish;titlecase")
  expect_equal(normalization_spec_string(c("drink2", "drink1"), norms),
               "drink1=uppercase|drink2=uppercase")
  expect_equal(normalization_spec_string("notes", norms), "")
  expect_equal(normalization_spec_string("city",
                                         empty_normalizations_tibble()), "")
  expect_equal(normalization_spec_string(character(0), norms), "")
})

test_that("stamp_recode_norm_spec stamps each rule with its own columns", {
  norms <- mk_norm("city", "titlecase")
  rules <- dplyr::bind_rows(
    mk_recode("city",  "exact", "Uptown", "Uptown East"),
    mk_recode("notes", "exact", "x", "y"))
  out <- stamp_recode_norm_spec(rules, norms, c("city", "notes"))
  expect_equal(out$norm_spec, c("city=titlecase", ""))

  sib <- mk_recode("drink1", "exact", "LATTE", "Latte", TRUE, "^drink[0-9]+$")
  out2 <- stamp_recode_norm_spec(
    sib, mk_norm("drink1", "uppercase", TRUE, "^drink[0-9]+$"),
    c("drink1", "drink2", "city"))
  expect_equal(out2$norm_spec, "drink1=uppercase|drink2=uppercase")

  # Without data columns a sibling rule still sees the normalized variable.
  expect_equal(recode_norm_specs(sib, mk_norm("drink1", "uppercase")),
               "drink1=uppercase")

  empty <- stamp_recode_norm_spec(empty_recodes_tibble(), norms)
  expect_equal(nrow(empty), 0L)
  expect_true("norm_spec" %in% names(empty))
})

test_that("recode_rule_id does not depend on norm_spec", {
  a <- mk_recode("city", "exact", "Uptown", "U")
  b <- stamp_recode_norm_spec(a, mk_norm("city", "uppercase"), "city")
  expect_identical(recode_rule_id(b$variable, b$match_type, b$old_value),
                   a$rule_id)
})

test_that("the stamp survives the recodes CSV round trip", {
  path <- tempfile(fileext = ".csv"); on.exit(unlink(path), add = TRUE)
  rules <- stamp_recode_norm_spec(
    dplyr::bind_rows(mk_recode("city", "exact", "Uptown", "U"),
                     mk_recode("notes", "exact", "x", "y")),
    mk_norm("city", c("squish", "titlecase")), c("city", "notes"))
  write_recodes(rules, path)
  back <- read_recodes(path)
  expect_identical(back$norm_spec, c("city=squish;titlecase", ""))
})

test_that("an older 13-column recodes file reads unstamped and round-trips", {
  path <- tempfile(fileext = ".csv"); on.exit(unlink(path), add = TRUE)
  old <- mk_recode("city", "trimmed_ci", "uptwn", "Uptown")
  readr::write_csv(old, path, na = "<NA>")
  expect_no_warning(back <- read_recodes(path))
  expect_named(back, names(empty_recodes_tibble()))
  expect_true(is.na(back$norm_spec))
  expect_no_warning(raw <- read_recodes(path, conform = FALSE))
  expect_false("norm_spec" %in% names(raw))
  out <- tempfile(fileext = ".csv"); on.exit(unlink(out), add = TRUE)
  write_recodes(back, out)
  again <- read_recodes(out)
  expect_identical(lapply(again, identity), lapply(back, identity))
})

# --- Codegen ---------------------------------------------------------------------

test_that("generated normalization code parses and uses the right calls", {
  code <- generate_recode_R(empty_recodes_tibble(), "cafe",
    normalizations = dplyr::bind_rows(
      mk_norm("city", c("squish", "titlecase")),
      mk_norm("drink1", "uppercase", TRUE, "^drink[0-9]+$"),
      mk_norm("drink notes", "lowercase"),
      mk_norm("2nd drink", "trim")))
  expect_silent(parse(text = code))
  expect_true(grepl("mutate(city = str_to_title(str_squish(city)))", code,
                    fixed = TRUE))
  expect_true(grepl(
    'across(matches("^drink[0-9]+$", ignore.case = FALSE), function(.x) toupper(.x))',
    code, fixed = TRUE))
  expect_true(grepl("`drink notes` = tolower(`drink notes`)", code, fixed = TRUE))
  expect_true(grepl("`2nd drink` = str_trim(`2nd drink`)", code, fixed = TRUE))
  expect_true(grepl("No recode rules defined", code, fixed = TRUE))
})

test_that("a normalization on a metacharacter column reaches only that column", {
  df <- tibble::tibble(`q1.a` = c(" yes ", "no"), q1_a = c(" yes ", "no"),
                       `price ($)` = c(" free", "2"))
  norms <- dplyr::bind_rows(
    mk_norm("q1.a", c("trim", "uppercase")),
    mk_norm("price ($)", "trim"))
  applied <- apply_normalizations(df, norms)$df
  genned  <- eval_script(df, empty_recodes_tibble(), norms)
  expect_identical(applied$`q1.a`, c("YES", "NO"))
  expect_identical(applied$q1_a, df$q1_a)
  expect_identical(applied$`price ($)`, c("free", "2"))
  for (col in names(df)) {
    expect_identical(genned[[col]], applied[[col]], info = col)
  }
  # Normalization plus recode on the same metacharacter column.
  rules <- mk_recode("price ($)", "exact", "free", "0")
  genned <- eval_script(df, rules, norms)
  expect_identical(genned$`price ($)`, c("0", "2"))
  expect_identical(genned$`price ($)`,
                   apply_recodes(applied, rules)$df$`price ($)`)
  expect_identical(genned$q1_a, df$q1_a)
})

test_that("a normalization with no usable ops emits no code at all", {
  code <- generate_recode_R(empty_recodes_tibble(), "cafe",
                            normalizations = mk_norm("city", character(0)))
  expect_silent(parse(text = code))
  expect_false(grepl("city", code, fixed = TRUE))
  expect_identical(.normalize_blocks(NULL), character(0))
})

test_that(".r_col_token backticks only non-syntactic names", {
  expect_identical(.r_col_token("drink1"), "drink1")
  expect_identical(.r_col_token("drink.1"), "drink.1")
  expect_identical(.r_col_token("drink 1"), "`drink 1`")
  expect_identical(.r_col_token("if"), "`if`")
  expect_identical(.r_col_token("a`b"), "`a\\`b`")
})

test_that("a recode must be written against the NORMALIZED value", {
  df    <- tibble::tibble(x = c("  oat  milk ", "other"))
  norms <- mk_norm("x", "squish")

  delivered <- mk_recode("x", "exact", "  oat  milk ", "oat milk latte")
  out_old <- apply_recodes(apply_normalizations(df, norms)$df, delivered)$df
  expect_equal(out_old$x, c("oat milk", "other"))
  expect_identical(eval_script(df, delivered, norms)$x, out_old$x)

  normalized <- mk_recode("x", "exact", "oat milk", "oat milk latte")
  out_new <- apply_recodes(apply_normalizations(df, norms)$df, normalized)$df
  expect_equal(out_new$x, c("oat milk latte", "other"))
  expect_identical(eval_script(df, normalized, norms)$x, out_new$x)
})

test_that("a recode has the last word over a normalization, in that order", {
  df      <- tibble::tibble(city = c("uptown", "UPTOWN"))
  recodes <- mk_recode("city", "trimmed_ci", "uptown", "Uptown")
  norms   <- mk_norm("city", "uppercase")
  out <- apply_recodes(apply_normalizations(df, norms)$df, recodes)$df
  expect_equal(out$city, c("Uptown", "Uptown"))
  expect_identical(eval_script(df, recodes, norms)$city, out$city)
  other <- apply_normalizations(apply_recodes(df, recodes)$df, norms)$df
  expect_equal(other$city, c("UPTOWN", "UPTOWN"))
})

# --- Norms proxy + rules proxy stamping --------------------------------------

test_that("norms proxy add() is an in-place upsert, one rule per column", {
  rv <- shiny::reactiveValues(norms = empty_normalizations_tibble())
  p  <- make_norms_proxy(rv)
  p$add(mk_norm("city", "uppercase"))
  p$add(mk_norm("drink1", "lowercase"))
  p$add(mk_norm("notes", "squish"))
  p$add(mk_norm("city", c("squish", "titlecase")))   # same id
  out <- shiny::isolate(p$get())
  expect_equal(out$variable, c("city", "drink1", "notes"))
  expect_equal(out$ops[1], "squish;titlecase")

  # Same column, now a sibling family: different id, still replaces in place.
  p$add(mk_norm("drink1", "uppercase", TRUE, "^drink[0-9]+$"))
  out <- shiny::isolate(p$get())
  expect_equal(out$variable, c("city", "drink1", "notes"))
  expect_true(out$apply_to_siblings[2])

  # A sibling of that family adding the same family pattern replaces it too.
  p$add(mk_norm("drink2", "trim", TRUE, "^drink[0-9]+$"))
  out <- shiny::isolate(p$get())
  expect_equal(out$variable, c("city", "drink2", "notes"))

  p$add(empty_normalizations_tibble())
  expect_equal(nrow(shiny::isolate(p$get())), 3L)
  p$set(empty_normalizations_tibble())
  expect_equal(nrow(shiny::isolate(p$get())), 0L)
})

test_that("rules proxy add() stamps norm_spec from the current normalizations", {
  rv <- shiny::reactiveValues(rules = empty_recodes_tibble(),
                              norms = empty_normalizations_tibble())
  np <- make_norms_proxy(rv)
  cols <- c("city", "drink1", "drink2")
  rp <- make_rules_proxy(rv, norms_r = np$get, data_cols_r = function() cols)

  rp$add(mk_recode("city", "trimmed_ci", "uptwn", "Uptown"))
  expect_identical(shiny::isolate(rp$get())$norm_spec, "")

  np$add(mk_norm("drink1", "uppercase", TRUE, "^drink[0-9]+$"))
  rp$add(mk_recode("drink1", "exact", "LATTE", "Latte", TRUE, "^drink[0-9]+$"))
  out <- shiny::isolate(rp$get())
  expect_identical(out$norm_spec, c("", "drink1=uppercase|drink2=uppercase"))

  # The earlier rule keeps the stamp it was authored under.
  np$add(mk_norm("city", "titlecase"))
  expect_identical(shiny::isolate(rp$get())$norm_spec[1], "")
  # Re-adding it (re-authoring from a tab) restamps it, in place.
  rp$add(mk_recode("city", "trimmed_ci", "uptwn", "Uptown"))
  out <- shiny::isolate(rp$get())
  expect_identical(out$norm_spec, c("city=titlecase",
                                    "drink1=uppercase|drink2=uppercase"))
  expect_identical(out$variable, c("city", "drink1"))

  # set() never stamps.
  edited <- out
  edited$norm_spec[1] <- "kept"
  rp$set(edited)
  expect_identical(shiny::isolate(rp$get())$norm_spec[1], "kept")
})

test_that("rules proxy without a normalizations accessor does not stamp", {
  rv <- shiny::reactiveValues(rules = empty_recodes_tibble())
  rp <- make_rules_proxy(rv)
  rp$add(mk_recode("city", "exact", "a", "b"))
  expect_true(is.na(shiny::isolate(rp$get())$norm_spec))
})

# --- Normalize tab (testServer) ------------------------------------------------

.cafe <- tibble::tibble(
  drink1 = c("latte", "LATTE", " Latte ", "mocha", NA),
  drink2 = c("MOCHA", "latte", NA, "Latte", "latte"),
  city   = c("Uptown", "uptown", "Old  Town", "old town", "Uptown"))

.state <- function(data = .cafe) {
  shiny::reactiveValues(data = data, meta = NULL, dataset_name = "cafe.csv")
}

.norms_rv <- function() {
  rv <- shiny::reactiveValues(norms = empty_normalizations_tibble())
  make_norms_proxy(rv)
}

test_that("the preview shows distinct changes with counts before adding", {
  proxy <- .norms_rv()
  shiny::testServer(mod_normalize_server,
    args = list(shared_state = .state(),
                selected_var_r = shiny::reactive("drink1"),
                norms_proxy = proxy), {
      session$setInputs(ops = c("squish", "lowercase"), apply_siblings = FALSE)
      p <- preview_r()
      expect_equal(nrow(p), 2L)                 # "LATTE" and " Latte "
      expect_true(all(p$after == "latte"))
      expect_equal(sum(p$n_cells), 2L)
      txt <- paste(unlist(output$preview_note), collapse = " ")
      expect_match(txt, "2 distinct values would be rewritten, covering 2 cells")
      # Previewing adds nothing.
      expect_equal(nrow(shiny::isolate(proxy$get())), 0L)
    })
})

test_that("adding a rule updates the proxy; re-adding replaces it", {
  proxy <- .norms_rv()
  shiny::testServer(mod_normalize_server,
    args = list(shared_state = .state(),
                selected_var_r = shiny::reactive("drink1"),
                norms_proxy = proxy), {
      session$setInputs(ops = "uppercase", apply_siblings = FALSE)
      session$setInputs(add_rule = 1)
      out <- shiny::isolate(proxy$get())
      expect_equal(nrow(out), 1L)
      expect_equal(out$ops, "uppercase")
      expect_equal(out$source_dataset, "cafe.csv")

      session$setInputs(ops = c("titlecase", "squish"))
      session$setInputs(add_rule = 2)
      out <- shiny::isolate(proxy$get())
      expect_equal(nrow(out), 1L)
      expect_equal(out$ops, "squish;titlecase")

      # The rules table keeps the generated markup in the LAST column only.
      tbl <- rules_table_r()
      expect_identical(names(tbl), .NORMALIZE_TABLE_COLS)
      expect_identical(tail(names(tbl), 1), "remove")
      expect_equal(tbl$changes, "collapse spaces + Title Case")

      session$setInputs(delete_row = out$norm_id)
      expect_equal(nrow(shiny::isolate(proxy$get())), 0L)
    })
})

test_that("the sibling choice builds the family pattern in plain language", {
  proxy <- .norms_rv()
  shiny::testServer(mod_normalize_server,
    args = list(shared_state = .state(),
                selected_var_r = shiny::reactive("drink1"),
                norms_proxy = proxy), {
      session$setInputs(ops = "lowercase", apply_siblings = TRUE,
                        sibling_mode = "auto")
      expect_equal(sibling_pattern_r(), "^drink[0-9]+$")
      note <- paste(unlist(output$sibling_note), collapse = " ")
      expect_match(note, "Applies to 2 columns: drink1, drink2", fixed = TRUE)
      expect_true(all(c("drink1", "drink2") %in% preview_r()$column))
      session$setInputs(add_rule = 1)
      out <- shiny::isolate(proxy$get())
      expect_true(out$apply_to_siblings)
      expect_equal(out$sibling_pattern, "^drink[0-9]+$")

      # An invalid typed pattern degrades to a single-column rule.
      session$setInputs(sibling_mode = "custom", sibling_pattern = "drink[")
      expect_null(pattern_cols_r())
      expect_false(draft_rule_r()$apply_to_siblings)
      note <- paste(unlist(output$sibling_note), collapse = " ")
      expect_match(note, "not a valid regular expression")
    })
})

test_that("nothing is added without a column or without an op", {
  proxy <- .norms_rv()
  shiny::testServer(mod_normalize_server,
    args = list(shared_state = .state(),
                selected_var_r = shiny::reactive(NULL),
                norms_proxy = proxy), {
      session$setInputs(ops = "uppercase", apply_siblings = FALSE)
      session$setInputs(add_rule = 1)
      expect_equal(nrow(shiny::isolate(proxy$get())), 0L)
    })
  proxy2 <- .norms_rv()
  shiny::testServer(mod_normalize_server,
    args = list(shared_state = .state(),
                selected_var_r = shiny::reactive("city"),
                norms_proxy = proxy2), {
      session$setInputs(ops = character(0), apply_siblings = FALSE)
      session$setInputs(add_rule = 1)
      expect_equal(nrow(shiny::isolate(proxy2$get())), 0L)
    })
})

test_that("validation messages appear for problem rules", {
  proxy <- .norms_rv()
  bad <- mk_norm("gone", "uppercase")
  conflict <- mk_norm("city", "lowercase")
  conflict$ops <- "lowercase;uppercase"
  proxy$set(dplyr::bind_rows(bad, conflict))
  shiny::testServer(mod_normalize_server,
    args = list(shared_state = .state(),
                selected_var_r = shiny::reactive("city"),
                norms_proxy = proxy), {
      html <- output$validation$html
      expect_match(html, "Column not in the loaded data", fixed = TRUE)
      expect_match(html, "Conflicting case choices", fixed = TRUE)
      expect_match(html, "alert-warning", fixed = TRUE)

      proxy$set(mk_norm("city", "titlecase"))
      session$flushReact()
      expect_match(output$validation$html, "No problems found", fixed = TRUE)
    })
})

test_that("the UI builds and its help text avoids unescaped markup", {
  ui <- mod_normalize_ui("norm")
  html <- as.character(htmltools::renderTags(ui)$html)
  expect_match(html, "Normalize columns", fixed = TRUE)
  expect_match(html, "normalize before clustering", fixed = TRUE)
  expect_match(html, "exact", fixed = TRUE)
})

# --- Preview & export: normalizations summary and script -----------------------

test_that("normalization_summary_table joins counts by norm_id", {
  norms <- dplyr::bind_rows(mk_norm("drink1", "lowercase", TRUE,
                                    "^drink[0-9]+$"),
                            mk_norm("city", c("squish", "titlecase")))
  s <- apply_normalizations(.cafe, norms)$summary
  tbl <- normalization_summary_table(norms, s[2:1, ])
  expect_named(tbl, c("target", "changes", "columns", "cells_changed"))
  expect_equal(tbl$target, c("^drink[0-9]+$", "city"))
  expect_equal(tbl$columns, c("drink1, drink2", "city"))
  expect_equal(tbl$cells_changed, s$cells_changed)
  none <- normalization_summary_table(norms, NULL)
  expect_true(all(is.na(none$cells_changed)))
  expect_equal(nrow(normalization_summary_table(empty_normalizations_tibble())),
               0L)
})

test_that("Preview & export normalizes before recoding, and exports both", {
  norms <- mk_norm("drink1", c("squish", "lowercase"))
  rules <- mk_recode("drink1", "exact", "latte", "Latte")
  st <- .state()
  shiny::testServer(mod_preview_export_server,
    args = list(shared_state = st,
                rules_proxy = list(get = function() rules),
                norms_proxy = list(get = function() norms)), {
      d <- diff_r()
      # "latte", "LATTE" and " Latte " all become "latte" first, so the exact
      # rule matches three cells, not one.
      expect_equal(d$summary$cells_matched, 3L)
      expect_equal(d$norm_summary$cells_changed, 2L)
      expect_match(script_r(), "str_to_lower|tolower\\(str_squish\\(drink1\\)\\)")
      expect_identical(
        rule_warnings_r(),
        collect_rule_warnings(.cafe, rules, empty_recat_tibble(), norms))
    })
})
