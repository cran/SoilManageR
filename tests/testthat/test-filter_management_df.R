test_that("Check number of rows", {
  expect_equal(nrow(filter_management_df(EXAMPLE_data,"UFA")), 112)
})
