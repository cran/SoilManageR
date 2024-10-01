test_that("input error", {
  expect_error(relative_yield())
  expect_error(relative_yield("green plant"))
  expect_error(relative_yield("wheat, winter"))
  })

test_that("test cases", {
  expect_equal(relative_yield("wheat, winter", 6),1.1764706)
})

