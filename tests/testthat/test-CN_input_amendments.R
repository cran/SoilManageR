test_that("input error", {
  expect_error(CN_input_amendments(),"DMC of organic amendment is missing. Supply DMC = or amd_type =")
  expect_error(CN_input_amendments(20, "poop"),"Type of organic amendment not included in the table with default values. See CN_input_amendments_LUT for available amendments.")
})

test_that("use default values", {
  expect_equal(CN_input_amendments(20, "Slurry_cattle"),dplyr::tibble(C_input_org = 377.1,
                                                                      N_input_org = 39.6))
  expect_equal(CN_input_amendments(20, "Slurry_cattle", return.comment = TRUE),dplyr::tibble(C_input_org = 377.1,
                                                                                             N_input_org = 39.6,
                                                                                             comment = "Default value from the Swiss fertilizer recomendation (GRUD, 2017; Chapter 4, Table 6), Assumption that 57,97% of the organic matter are carbon DMC from default values,  C_content from default values,  N_content from default values, "
                                                                                             ))
})


# test_that("custom values", {
#   expect_equal(C_input_crops("wheat, winter", crop_product = 6),
#                dplyr::tibble(C_input_product = 0,
#                              C_input_straw = 528,
#                              C_input_root = 361,
#                              C_input_exudate = 238,
#                              C_input_total = 1127))
#   expect_equal(C_input_crops("barley, spring", straw.removal = TRUE),
#                dplyr::tibble(C_input_product = 0,
#                              C_input_straw = 0,
#                              C_input_root = 362,
#                              C_input_exudate = 237,
#                              C_input_total = 599))
# })
