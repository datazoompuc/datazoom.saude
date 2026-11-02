test_that("collect_fragility_notes() extracts FRAGILE tags from R/births.R", {
  root <- rprojroot::find_package_root_file()
  notes_script <- file.path(root, "actions", "scripts", "fragility_notes.R")
  skip_if_not(file.exists(notes_script), "fragility_notes.R not present")

  source(notes_script)

  births_path <- file.path(root, "R", "births.R")
  skip_if_not(file.exists(births_path), "R/births.R not present")

  notes <- collect_fragility_notes(births_path, "R/births.R")
  expect_true(length(notes) >= 4)
  expect_true(any(grepl("FTP filenames", notes)))
  expect_true(any(grepl("state abbreviation", notes)))
  expect_true(any(grepl("SINASC date fields", notes)))
  expect_true(any(grepl("birth time", notes)))
})

