test_that("input error", {
  expect_error(tillage_intensity())
  expect_error(tillage_intensity("w"))
})

test_that("test EXAMPLE data", {
  expect_equal(tillage_intensity(EXAMPLE_data)[7,],dplyr::tibble(year = 2019,
                                                                 STIR = 42))
  expect_equal(tillage_intensity(EXAMPLE_data, extended.output = TRUE)[37,20],
               dplyr::tibble(STIR = 64))
})
