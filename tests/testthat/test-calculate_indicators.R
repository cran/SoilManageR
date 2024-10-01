test_that("input error", {
  expect_error(suppressMessages(calculate_indicators("garbage data")))
})

test_that("test EXAMPLE data", {
  expect_equal(
    suppressMessages(
      calculate_indicators(EXAMPLE_data)[5,c(2,3,7,8,13,14)]),
                dplyr::tibble(crop = "ley, temporary",
                             C_input = 3701,
                             STIR = 0,
                             soil_cover_days = 365,
                             N_input = 34,
                             LSU = 0))

})
