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

# Documented post-migration deltas based on empirical FTP coverage:

# 1. SINASC & Painel de Oncologia
expected$available_time[expected$dataset == "datasus_sinasc"] <- "1996-2024"
expected$available_time[expected$dataset == "datasus_po"] <- "2013-2026"

# 2. SIM (DOFET, DOEXT, DOINF, DOMAT)
sim_2025 <- c("datasus_sim_dofet", "datasus_sim_doext", "datasus_sim_doinf", "datasus_sim_domat")
expected$available_time[expected$dataset %in% sim_2025] <- "1996-2025"

# 3. CNES
cnes_2007 <- c("datasus_cnes_hb", "datasus_cnes_ep", "datasus_cnes_rc", "datasus_cnes_in", "datasus_cnes_ef", "datasus_cnes_gm")
expected$available_time[expected$dataset %in% cnes_2007] <- "2007-2026"
expected$available_time[expected$dataset == "datasus_cnes_ee"] <- "2007-2019, 2021"

# 4. SIHSUS
expected$available_time[expected$dataset %in% c("datasus_sih_rd", "datasus_sih_rj", "datasus_sih_sp")] <- "2008-2026"
expected$available_time[expected$dataset == "datasus_sih_er"] <- "2011-2026"

# 5. SIASUS
expected$available_time[expected$dataset == "datasus_siasus_ab"] <- "2008-2017, 2025"
expected$available_time[expected$dataset == "datasus_siasus_pa"] <- "2008-2026"
expected$available_time[expected$dataset == "datasus_siasus_ps"] <- "2012-2026"
expected$available_time[expected$dataset %in% c("datasus_siasus_abo", "datasus_siasus_acf")] <- "2014-2026"
expected$available_time[expected$dataset == "datasus_siasus_sad"] <- "2012-2018"

# 6. SI-PNI Web URL added post-migration
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

  po <- datasets_link(source = "datasus", dataset = "datasus_po")
  expect_equal(nrow(po), 1)

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
