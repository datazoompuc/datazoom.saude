# Offline unit tests for SI-PNI 2024+ (Qlik dashboard) mapping, parsing, and routing

test_that("pni_2024_map_month() translates Portuguese abbreviations to full English names", {
  expect_equal(
    pni_2024_map_month(c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")),
    tolower(month.name)
  )
})

test_that("pni_2024_map_dose() standardizes Qlik dose labels consistently with legacy SI-PNI", {
  raw_doses <- c(
    "1 - 1\u00aa Dose",
    "2 - 2\u00aa Dose",
    "3 - 3\u00aa Dose",
    "6 - 1\u00ba Refor\u00e7o",
    "38 - Refor\u00e7o",
    "9 - \u00danica",
    "10 - Revacina\u00e7\u00e3o",
    "36 - Dose Inicial",
    "37 - Dose Adicional",
    "57 - Dose Zero",
    "44 - 1\u00aa Dose Fracionada"
  )
  expect_equal(
    pni_2024_map_dose(raw_doses),
    c("D1", "D2", "D3", "REF", "REF", "DU", "REV", "Inicial", "Adicional", "D0", "Fracionada")
  )
})

test_that("pni_2024_map_product() maps legacy names and passes through native 2024+ names", {
  expect_equal(pni_2024_map_product("BCG - BCG"), "15 - vacina BCG")
  expect_equal(pni_2024_map_product("Febre amarela - FA"), "14 - vacina febre amarela")
  expect_equal(pni_2024_map_product("DTP/HB/Hib - Penta"), "42 - vacina penta (DTP/HB/Hib)")
  expect_equal(
    pni_2024_map_product("87 - vacina covid-19-RNAm, Pfizer (Comirnaty)"),
    "87 - vacina covid-19-RNAm, Pfizer (Comirnaty)"
  )
})

test_that("pni_2024_map_strategy() maps renamed monitoring strategy and preserves others", {
  expect_equal(
    pni_2024_map_strategy("Monitoramento R\u00e1pido de Cobertura Vacinal"),
    "Monitoramento das Estrat\u00e9gias de Vacina\u00e7\u00e3o"
  )
  expect_equal(pni_2024_map_strategy("Rotina"), "Rotina")
})

test_that("pni_2024_parse_rows() returns harmonized 9-column tibble", {
  mock_raw <- tibble::tibble(
    munic_code = c("120001", "120005"),
    munic_raw = c("120001 - Acrel\u00e2ndia", "120005 - Assis Brasil"),
    month_pt = c("jan", "fev"),
    dose_raw = c("9 - \u00danica", "1 - 1\u00aa Dose"),
    quantity = c(7, 3)
  )

  res <- pni_2024_parse_rows(mock_raw, state = "ac", year = 2024, strategy = "Rotina", product = "BCG - BCG")

  expect_s3_class(res, "tbl_df")
  expect_named(res, c("state", "year", "munic_code", "munic_name", "strategy", "product", "month", "dose", "quantity"))
  expect_equal(res$state, c("AC", "AC"))
  expect_equal(res$year, c(2024L, 2024L))
  expect_equal(res$munic_code, c(120001, 120005))
  expect_equal(res$munic_name, c("acrel\u00e2ndia", "assis brasil"))
  expect_equal(res$month, c("january", "february"))
  expect_equal(res$dose, c("DU", "D1"))
  expect_equal(res$quantity, c(7, 3))

  empty <- pni_2024_parse_rows(NULL, state = "AC", year = 2024, strategy = "Rotina", product = "BCG - BCG")
  expect_equal(nrow(empty), 0L)
  expect_named(empty, c("state", "year", "munic_code", "munic_name", "strategy", "product", "month", "dose", "quantity"))
})

test_that("load_vaccines() routes year >= 2024 without data to load_pni_2024() and applies dictionary", {
  mock_parsed <- tibble::tibble(
    state = "AC",
    year = 2024L,
    munic_code = 120001,
    munic_name = "acrel\u00e2ndia",
    strategy = "Rotina",
    product = "BCG - BCG",
    month = "january",
    dose = "DU",
    quantity = 7
  )

  testthat::local_mocked_bindings(
    load_pni_2024 = function(year, state, strategy, product) mock_parsed,
    .package = "datazoom.saude"
  )

  res_eng <- load_vaccines(year = 2024, state = "AC", strategy = "Rotina", product = "BCG - BCG", language = "eng")
  expect_named(res_eng, c("state", "year", "munic_code", "munic_name", "strategy", "product", "month", "dose", "quantity"))

  res_pt <- load_vaccines(year = 2024, state = "AC", strategy = "Rotina", product = "BCG - BCG", language = "pt")
  expect_named(res_pt, c("estado", "ano", "cod_munic", "nome_munic", "estrategia", "produto", "mes", "dose", "quantidade"))
})
