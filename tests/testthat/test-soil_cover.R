test_that("input error", {
  expect_error(soil_cover())
  expect_error(soil_cover("w"))
})

test_that("test EXAMPLE data", {
  expect_equal(soil_cover(EXAMPLE_data)[2,],dplyr::tibble(year = 2014,
                                                          soil_cover_days = 259,
                                                          plant_cover_days = 236,
                                                          residue_cover_days = 23
                                                      ))

  expected_outcome <- dplyr::tibble(year = 2018,
                                    date = as.Date("2018-06-23"),
                                    soil_cover = 100,
                                    plant_cover = 100,
                                    residue_cover = 0,
                                    soil_cover_days = 1,
                                    plant_cover_days = 1,
                                    residue_cover_days = 0
                                    ) %>% dplyr::group_by(year,date)


  expect_equal(soil_cover(EXAMPLE_data, extended.output = TRUE)[2000,],expected_outcome)
})
