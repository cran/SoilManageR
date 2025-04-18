test_that("test default values", {
  expect_equal(C_input_cover_crops(),C_input_cover_crops(days = 180,
                                                         min_C_abvg = 0,
                                                         min_days = 0,
                                                         max_C_abvg = 1916,
                                                         max_days = 240,
                                                         Cc_biomass = 450))
})

test_that("test custom values", {
  expect_equal(C_input_cover_crops(days=200),dplyr::tibble(C_input_product = 1596,
                                                           C_input_straw = 0,
                                                           C_input_root = 435,
                                                           C_input_exudate = 134,
                                                           C_input_total = 2165))
  expect_equal(C_input_cover_crops(abvg_biomass = 2.5),dplyr::tibble(C_input_product = 1125,
                                                                     C_input_straw = 0,
                                                                     C_input_root = 306,
                                                                     C_input_exudate = 94,
                                                                     C_input_total = 1525))
  expect_equal(C_input_cover_crops(abvg_biomass = 2.5, Cc_biomass=460),
               dplyr::tibble(C_input_product = 1150,
                             C_input_straw = 0,
                             C_input_root = 306,
                             C_input_exudate = 94,
                             C_input_total = 1550))
})
