test_that("input error", {
  expect_error(suppressMessages(check_management_df()))
  expect_error(suppressMessages(check_management_df("w")))
})


test_that("test EXAMPLE data", {
  expect_equal(suppressMessages(check_management_df(EXAMPLE_data)),NULL)
})

suppressMessages(
  test_that("test EXAMPLE data", {
    expect_message(check_management_df(EXAMPLE_data))
  })
)
