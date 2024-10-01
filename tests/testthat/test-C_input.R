test_that("input error", {
  expect_error(C_input())
  expect_error(C_input("w"))
})

test_that("test EXAMPLE data", {
  expect_equal(C_input(EXAMPLE_data)[7,],dplyr::tibble(year = 2019,
                                                       crop = "wheat, winter",
                                                       C_input = 1112,
                                                       C_input_org_amendment = 0,
                                                       C_input_crop = 1112,
                                                       C_input_cover_crop= 0))
  expect_equal(C_input(EXAMPLE_data, extended.output = TRUE)[37,20],
               dplyr::tibble(C_input_CC = 2045))
})
