# Golden test: datasets_link() must return exactly what the old hardcoded
# tribble returned (captured before the migration into
# fixtures/datasets_link_pre_migration.rds), with only these deltas:
#   - the `link` column is now named `url`
#   - `sidra_code` (always NA) is dropped

old <- readRDS(test_path("fixtures", "datasets_link_pre_migration.rds"))

expected <- old %>%
  dplyr::select(-sidra_code) %>%
  dplyr::rename(url = link)

test_that("datasets_link() reproduces the pre-migration table", {
  clear_manifest_cache()
  new <- datasets_link()

  expect_identical(names(new), names(expected))
  expect_identical(as.data.frame(new), as.data.frame(expected))
})

test_that("datasets_link() filters by source and dataset", {
  sih <- datasets_link(source = "datasus_sih")
  expect_identical(sih$dataset, expected$dataset[expected$survey == "datasus_sih"])

  one <- datasets_link(source = "datasus", dataset = "datasus_sinasc")
  expect_equal(nrow(one), 1)
})

test_that("datasets_link(url = TRUE) and dataset_url() return the same url", {
  for (i in seq_len(nrow(expected))) {
    s <- expected$survey[i]
    d <- expected$dataset[i]
    expect_identical(datasets_link(source = s, dataset = d, url = TRUE), expected$url[i])
    expect_identical(dataset_url(s, d), expected$url[i])
  }
})
