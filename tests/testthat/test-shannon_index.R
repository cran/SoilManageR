test_that("input error", {
  expect_error(shannon_index())
  expect_error(shannon_index("invalid"))
})

tibble_example <- dplyr::tibble(Plant = c("A","B","C","D","E"), count = c(10,5,8,20,10))

test_that("test cases", {
  expect_equal(shannon_index(tibble_example),1.50521514)
})

rm(tibble_example)

