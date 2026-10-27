test_that("parse_years() handles ranges, lists and NA", {
  expect_identical(parse_years("2005-2007"), 2005:2007)
  expect_identical(parse_years("2003, 2007-2008, 2014"), c(2003L, 2007L, 2008L, 2014L))
  expect_identical(parse_years("2023"), 2023L)
  expect_identical(parse_years(NA), integer(0))
  expect_identical(parse_years(NULL), integer(0))
})
