test_that("input error", {
  expect_error(N_input())
  expect_error(N_input("w"))
})

test_that("test EXAMPLE data", {
  expect_equal(N_input(EXAMPLE_data)[6,],dplyr::tibble(year = 2018,
                                                       N_input_org = 126,
                                                       N_input_min = 0,
                                                       N_input = 126,
                                                       LSU = 0))
  expect_equal(N_input(EXAMPLE_data, extended.output = TRUE)[2,20] %>% as.integer (),
               67)
})
