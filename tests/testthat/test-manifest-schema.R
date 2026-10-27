# Structural invariants of the packaged manifest (see R/manifest.R's header).

manifest <- read_manifest_file(
  system.file(MANIFEST_REL, package = "datazoom.saude")
)

test_that("the manifest has exactly the expected columns", {
  expect_identical(names(manifest), MANIFEST_CORE_COLS)
  expect_true(validate_manifest(manifest))
})

test_that("no row is missing its survey or dataset", {
  expect_false(any(is.na(manifest$survey)))
  expect_false(any(is.na(manifest$dataset)))
})

test_that("no (survey, dataset, geo_level, year) key is duplicated", {
  keys <- manifest[, MANIFEST_KEY_COLS]
  expect_false(any(duplicated(keys)))
})

test_that("every dataset is either unkeyed or keyed, never both", {
  groups <- split(manifest, list(manifest$survey, manifest$dataset), drop = TRUE)
  for (g in groups) {
    has_geo <- !is.na(g$geo_level)
    has_year <- !is.na(g$year)
    label <- paste0(g$survey[1], "/", g$dataset[1])

    # a row is never keyed by both geo_level and year
    expect_false(any(has_geo & has_year), label = label)

    if (nrow(g) == 1 && !has_geo && !has_year) next

    # keyed: every row carries a key, all on the same axis
    expect_true(all(has_geo) || all(has_year), label = label)
  }
})

test_that("every available_time parses to years", {
  for (x in manifest$available_time) {
    yrs <- parse_years(x)
    expect_true(length(yrs) > 0 && !anyNA(yrs), label = x)
  }
})

test_that("dataset_field() stops on a keyed dataset called without a key", {
  keyed <- tibble::tibble(
    survey = "x", dataset = "keyed",
    geo_level = c("state", "municipality"), year = NA_character_,
    url = c("a", "b"), docs_url = NA_character_,
    available_time = "2000-2001", available_geo = "State, Municipality",
    resolver = NA_character_
  )

  withr::defer(clear_manifest_cache())
  clear_manifest_cache()
  assign("tbl", keyed, envir = .dz_manifest_cache)

  expect_error(dataset_field("x", "keyed", "url"), "keyed by geo_level")
  expect_identical(dataset_field("x", "keyed", "url", geo_level = "municipality"), "b")
  expect_identical(dataset_meta("x", "keyed", "available_time"), "2000-2001")
})
