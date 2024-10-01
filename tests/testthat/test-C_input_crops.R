test_that("input error", {
  expect_error(C_input_crops())
  expect_error(C_input_crops("w"))
})

test_that("use default values", {
  expect_equal(C_input_crops("wheat, winter"),dplyr::tibble(C_input_product = 0,
                                                            C_input_straw = 474,
                                                            C_input_root = 361,
                                                            C_input_exudate = 238,
                                                            C_input_total = 1073))
  expect_equal(C_input_crops("ley, temporary"),dplyr::tibble(C_input_product = 0,
                                                             C_input_straw = 0,
                                                             C_input_root = 1500,
                                                             C_input_exudate = 750,
                                                             C_input_total = 2250))
  })

test_that("custom values", {
  expect_equal(C_input_crops("wheat, winter", crop_product = 6),
               dplyr::tibble(C_input_product = 0,
                             C_input_straw = 528,
                             C_input_root = 361,
                             C_input_exudate = 238,
                             C_input_total = 1127))
  expect_equal(C_input_crops("barley, spring", straw.removal = TRUE),
               dplyr::tibble(C_input_product = 0,
                             C_input_straw = 0,
                             C_input_root = 362,
                             C_input_exudate = 237,
                             C_input_total = 599))
})

