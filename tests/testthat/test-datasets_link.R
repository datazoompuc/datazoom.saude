# Golden test: datasets_link() must return exactly what the old hardcoded
# tribble returned (captured before the migration into
# fixtures/datasets_link_pre_migration.rds), with only these deltas:
#   - the `link` column is now named `url`
#   - `sidra_code` (always NA) is dropped
#   - documented post-migration deltas for onboarded datasets

old <- readRDS(test_path("fixtures", "datasets_link_pre_migration.rds"))

expected <- old %>%
  dplyr::select(-sidra_code) %>%
  dplyr::rename(url = link)

# Documented post-migration deltas:
# - datasus_sinasc: available_time updated from generic "1979-2026" to "1996-2024"
#   based on actual FTP availability.
sinasc_idx <- which(expected$survey == "datasus" & expected$dataset == "datasus_sinasc")
if (length(sinasc_idx) == 1) {
  expected$available_time[sinasc_idx] <- "1996-2024"
}

# - datasus_sipni: SI-PNI Web URL added post-migration
po_idx <- which(expected$survey == "datasus" & expected$dataset == "datasus_po")
sipni_row <- tibble::tibble(
  survey = "datasus",
  dataset = "datasus_sipni",
  available_time = "1994-2022",
  available_geo = "State",
  url = "https://sipni.datasus.gov.br/si-pni-web/faces/relatorio/consolidado/dosesAplicadasMensal.jsf"
)
expected <- dplyr::bind_rows(
  expected[1:po_idx, ],
  sipni_row,
  expected[(po_idx + 1):nrow(expected), ]
)

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

  pni <- datasets_link(source = "datasus", dataset = "datasus_sipni")
  expect_equal(nrow(pni), 1)
})

test_that("datasets_link(url = TRUE) and dataset_url() return the same url", {
  for (i in seq_len(nrow(expected))) {
    s <- expected$survey[i]
    d <- expected$dataset[i]
    expect_identical(datasets_link(source = s, dataset = d, url = TRUE), expected$url[i])
    expect_identical(dataset_url(s, d), expected$url[i])
  }
})
