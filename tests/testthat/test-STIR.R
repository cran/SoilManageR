test_that("input error", {
  expect_error(STIR())
  expect_error(STIR("invalid_device"))
  })

test_that("use default values", {
  expect_equal(STIR("plough"),64)
  expect_equal(STIR("plough", depth = 10),32)
  expect_equal(STIR("rotary_harrow", depth = 10),17)
})


test_that("custom values", {
  expect_equal(STIR(speed = 15, type_modifier = 0.8, depth = 10, area_disturbed = 1), 48)
  expect_equal(STIR(speed = 10, speed_unit = "mph", type_modifier = 0.8, depth = 6, depth_unit = "inch", area_disturbed = 1), 78)
})
