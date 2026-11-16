# Offline unit tests for FTP 3-attempt retry

test_that("perform_download() retries up to 3 times before succeeding", {
  tmp <- tempfile(fileext = ".dbc")
  on.exit(unlink(tmp), add = TRUE)

  attempts <- 0L
  testthat::local_mocked_bindings(
    download_file_once = function(url, destfile, source) {
      attempts <<- attempts + 1L
      if (attempts < 3L) {
        stop("FTP connection reset by peer")
      }
      writeBin(as.raw(1:10), destfile)
      0L
    },
    .package = "datazoom.saude"
  )

  expect_message(
    ok <- perform_download("ftp://ftp.datasus.gov.br/test.dbc", tmp, source = "datasus", retry_wait = 0),
    "Retrying"
  )
  expect_true(ok)
  expect_equal(attempts, 3L)
  expect_true(file.exists(tmp))
})

test_that("perform_download() stops after 3 failed attempts and cleans up temp file", {
  tmp <- tempfile(fileext = ".dbc")
  on.exit(unlink(tmp), add = TRUE)

  attempts <- 0L
  testthat::local_mocked_bindings(
    download_file_once = function(url, destfile, source) {
      attempts <<- attempts + 1L
      writeBin(raw(0), destfile)
      1L
    },
    .package = "datazoom.saude"
  )

  expect_error(
    suppressMessages(
      perform_download("ftp://ftp.datasus.gov.br/fail.dbc", tmp, source = "datasus", retry_wait = 0)
    ),
    "Failed to download .* after 3 attempts"
  )
  expect_equal(attempts, 3L)
  expect_false(file.exists(tmp))
})
