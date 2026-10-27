# The remote manifest is fetched first; any failure must fall back silently to
# the packaged snapshot.

test_that("an unreachable remote manifest falls back to the snapshot", {
  withr::local_options(
    datazoom.saude.use_remote_manifest = TRUE,
    datazoom.saude.manifest_url = "https://127.0.0.1:9/does-not-exist.csv"
  )
  withr::defer(clear_manifest_cache())
  clear_manifest_cache()

  tbl <- link_table()

  expect_identical(manifest_info()$source, "snapshot")
  expect_true(validate_manifest(tbl))
})

test_that("a manifest that fails validation is rejected", {
  expect_error(validate_manifest(data.frame(survey = "x")))
})
