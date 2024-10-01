test_that("input error", {
  expect_error(plant_cover())
  expect_error(plant_cover("wheat, winter", varDays = -20),
               "Cannot estimate for negative days since sowing")
})

test_that("use cases", {
  expect_equal(plant_cover("cover crop", 40), 29)
  expect_equal(plant_cover("wheat, winter", 200), 80)
})
