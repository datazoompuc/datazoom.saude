# Offline unit tests for session-scoped download disk cache

test_that("external_download() caches files on disk within the session", {
  clear_download_cache()
  on.exit(clear_download_cache(), add = TRUE)

  downloads <- 0L
  testthat::local_mocked_bindings(
    datasets_link = function(...) "ftp://ftp.datasus.gov.br/Dados/$file_name$",
    download_file_once = function(url, destfile, source) {
      downloads <<- downloads + 1L
      writeBin(as.raw(1:8), destfile)
      0L
    },
    read.dbc = function(file) data.frame(x = 1:3),
    .package = "datazoom.saude"
  )

  res1 <- external_download(dataset = "sinasc", source = "datasus", file_name = "DNAC2020.dbc")
  res2 <- external_download(dataset = "sinasc", source = "datasus", file_name = "DNAC2020.dbc")

  expect_equal(downloads, 1L)
  expect_identical(res1, res2)

  # Clearing cache forces a fresh download
  clear_download_cache()
  res3 <- external_download(dataset = "sinasc", source = "datasus", file_name = "DNAC2020.dbc")
  expect_equal(downloads, 2L)
  expect_identical(res1, res3)
})

test_that("options(datazoom.saude.cache = FALSE) bypasses the session disk cache", {
  clear_download_cache()
  on.exit(clear_download_cache(), add = TRUE)

  withr::local_options(list(datazoom.saude.cache = FALSE))

  downloads <- 0L
  testthat::local_mocked_bindings(
    datasets_link = function(...) "ftp://ftp.datasus.gov.br/Dados/$file_name$",
    download_file_once = function(url, destfile, source) {
      downloads <<- downloads + 1L
      writeBin(as.raw(1:8), destfile)
      0L
    },
    read.dbc = function(file) data.frame(x = 1:2),
    .package = "datazoom.saude"
  )

  external_download(dataset = "sinasc", source = "datasus", file_name = "DNAC2021.dbc")
  external_download(dataset = "sinasc", source = "datasus", file_name = "DNAC2021.dbc")

  expect_equal(downloads, 2L)
})

test_that("external_download() does not cache files when read.dbc() fails", {
  clear_download_cache()
  on.exit(clear_download_cache(), add = TRUE)

  downloads <- 0L
  reads <- 0L
  testthat::local_mocked_bindings(
    datasets_link = function(...) "ftp://ftp.datasus.gov.br/Dados/$file_name$",
    download_file_once = function(url, destfile, source) {
      downloads <<- downloads + 1L
      writeBin(as.raw(1:8), destfile)
      0L
    },
    read.dbc = function(file) {
      reads <<- reads + 1L
      if (reads == 1L) stop("corrupt dbc")
      data.frame(x = 42L)
    },
    .package = "datazoom.saude"
  )

  expect_error(
    external_download(dataset = "sinasc", source = "datasus", file_name = "DNAC2022.dbc"),
    "corrupt dbc"
  )
  res <- external_download(dataset = "sinasc", source = "datasus", file_name = "DNAC2022.dbc")
  expect_equal(downloads, 2L)
  expect_equal(res$x, 42L)
})
