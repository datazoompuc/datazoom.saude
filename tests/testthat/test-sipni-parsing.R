# Unit tests for SI-PNI tabular output parsing (offline, no network access)

test_that("parse_pni_table() correctly transforms raw tabular data to long format", {
  # Mock CSV layout returned by SI-PNI portal:
  # Column 1: "Município" ("120001 - Acrelandia")
  # Columns 2..13: 12 months for 1 dose ("DU")
  mock_cols <- c("Municipio", paste0("col_", 1:12))
  mock_df <- tibble::tibble(
    Municipio = c("120001 - Acrelandia", "120005 - Assis Brasil"),
    col_1 = c("10", "5"),
    col_2 = c("12", "6"),
    col_3 = c("8", "7"),
    col_4 = c("15", "9"),
    col_5 = c("20", "11"),
    col_6 = c("14", "8"),
    col_7 = c("18", "12"),
    col_8 = c("22", "14"),
    col_9 = c("19", "10"),
    col_10 = c("25", "15"),
    col_11 = c("30", "18"),
    col_12 = c("28", "16")
  )

  res <- parse_pni_table(
    dat = mock_df,
    state = "AC",
    year = 2020,
    strategy = "Rotina",
    product = "BCG - BCG",
    dose_labels = c("DU")
  )

  expect_s3_class(res, "tbl_df")
  expect_named(res, c("state", "year", "munic_code", "munic_name", "strategy", "product", "month", "dose", "quantity"))
  expect_equal(nrow(res), 2 * 12 * 1) # 2 municipalities * 12 months * 1 dose = 24 rows
  expect_equal(unique(res$state), "AC")
  expect_equal(unique(res$year), 2020L)
  expect_equal(unique(res$dose), "DU")
  expect_equal(unique(res$munic_code), c(120001, 120005))
  expect_equal(unique(res$munic_name), c("acrelandia", "assis brasil"))
  expect_equal(res$quantity[1], 10)
  expect_equal(res$quantity[2], 12)
})

test_that("parse_pni_table() handles multiple dose columns per month", {
  # 2 doses ("D1", "D2") across 12 months = 24 data columns
  col_names <- c("Municipio", paste0("m_", rep(1:12, each = 2), "_d_", rep(1:2, 12)))
  data_vals <- as.list(as.character(1:24))
  names(data_vals) <- col_names[-1]
  mock_df <- tibble::as_tibble(c(list(Municipio = "120010 - Brasileia"), data_vals))

  res <- parse_pni_table(
    dat = mock_df,
    state = "AC",
    year = 2019,
    strategy = "Rotina",
    product = "Poliomielite",
    dose_labels = c("D1", "D2")
  )

  expect_equal(nrow(res), 24)
  expect_equal(unique(res$dose), c("D1", "D2"))
  expect_equal(res$quantity[1], 1)
  expect_equal(res$quantity[2], 2)
})

test_that("parse_pni_table() handles empty dataset gracefully", {
  res <- parse_pni_table(
    dat = NULL,
    state = "AC",
    year = 2020,
    strategy = "Rotina",
    product = "BCG",
    dose_labels = character(0)
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)
})
