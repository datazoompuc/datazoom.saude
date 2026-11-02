test_that("collect_fragility_notes() extracts FRAGILE tags from treatment code", {
  root <- rprojroot::find_package_root_file()
  notes_script <- file.path(root, "actions", "scripts", "fragility_notes.R")
  skip_if_not(file.exists(notes_script), "fragility_notes.R not present")

  source(notes_script)

  # births.R
  births_path <- file.path(root, "R", "births.R")
  skip_if_not(file.exists(births_path), "R/births.R not present")
  notes_births <- collect_fragility_notes(births_path, "R/births.R")
  expect_true(length(notes_births) >= 4)
  expect_true(any(grepl("FTP filenames", notes_births)))
  expect_true(any(grepl("state abbreviation", notes_births)))
  expect_true(any(grepl("SINASC date fields", notes_births)))
  expect_true(any(grepl("birth time", notes_births)))

  # oncology_case.R
  po_path <- file.path(root, "R", "oncology_case.R")
  notes_po <- collect_fragility_notes(po_path, "R/oncology_case.R")
  expect_true(length(notes_po) >= 2)
  expect_true(any(grepl("POBR<YYYY>", notes_po)))
  expect_true(any(grepl("dt_diag", notes_po)))

  # mortality.R
  sim_path <- file.path(root, "R", "mortality.R")
  notes_sim <- collect_fragility_notes(sim_path, "R/mortality.R")
  expect_true(length(notes_sim) >= 4)
  expect_true(any(grepl("DO<UF><YYYY>", notes_sim)))
  expect_true(any(grepl("2-digit YY", notes_sim)))
  expect_true(any(grepl("dtobito", notes_sim)))

  # web_scraping_pni.R
  pni_path <- file.path(root, "R", "web_scraping_pni.R")
  notes_pni <- collect_fragility_notes(pni_path, "R/web_scraping_pni.R")
  expect_true(length(notes_pni) >= 1)
  expect_true(any(grepl("JSF/PrimeFaces", notes_pni)))
})
