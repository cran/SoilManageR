test_that("input error", {
  expect_error(productivity_indicator())
  expect_error(productivity_indicator("w"))
})

test_that("test EXAMPLE data", {
  expect_equal(productivity_indicator(EXAMPLE_data)[5,],dplyr::tibble(year = 2017,
                                                                 relative_yield = 1.38537436))
  expect_equal(productivity_indicator(EXAMPLE_data, extended.output = TRUE)[6,],
               dplyr::tibble(crop = "ley, temporary",
                             year = 2018,
                             date = as.Date("2018-10-10"),
                             crop_product = 11.9,
                             relative_yield = 0.91538462) %>% dplyr::rowwise())
})
