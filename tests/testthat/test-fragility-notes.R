test_that("collect_fragility_notes() extracts FRAGILE tags from treatment code", {
  root <- tryCatch(rprojroot::find_package_root_file(), error = function(e) "")
  notes_script <- file.path(root, "actions", "scripts", "fragility_notes.R")
  skip_if_not(file.exists(notes_script), "fragility_notes.R not present")

  source(notes_script)

  # births.R
  births_path <- file.path(root, "R", "births.R")
  skip_if_not(file.exists(births_path), "R/births.R not present")
  notes_births <- collect_fragility_notes(births_path, "R/births.R")
  expect_true(length(notes_births) >= 4)

  # oncology_case.R
  po_path <- file.path(root, "R", "oncology_case.R")
  notes_po <- collect_fragility_notes(po_path, "R/oncology_case.R")
  expect_true(length(notes_po) >= 2)

  # mortality.R
  sim_path <- file.path(root, "R", "mortality.R")
  notes_sim <- collect_fragility_notes(sim_path, "R/mortality.R")
  expect_true(length(notes_sim) >= 4)

  # web_scraping_pni.R
  pni_path <- file.path(root, "R", "web_scraping_pni.R")
  notes_pni <- collect_fragility_notes(pni_path, "R/web_scraping_pni.R")
  expect_true(length(notes_pni) >= 1)

  # hospital_beds.R
  cnes_path <- file.path(root, "R", "hospital_beds.R")
  notes_cnes <- collect_fragility_notes(cnes_path, "R/hospital_beds.R")
  expect_true(length(notes_cnes) >= 3)

  # hospital_admissions.R
  sih_path <- file.path(root, "R", "hospital_admissions.R")
  notes_sih <- collect_fragility_notes(sih_path, "R/hospital_admissions.R")
  expect_true(length(notes_sih) >= 2)

  # outpatient_procedures.R
  sia_path <- file.path(root, "R", "outpatient_procedures.R")
  notes_sia <- collect_fragility_notes(sia_path, "R/outpatient_procedures.R")
  expect_true(length(notes_sia) >= 1)
})
