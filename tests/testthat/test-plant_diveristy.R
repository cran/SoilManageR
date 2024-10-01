test_that("input error", {
  expect_error(plant_diversity())
  expect_error(plant_diversity("w"))
  expect_error(plant_diversity("w",2000,2012))
})

test_that("test EXAMPLE data", {
  expect_equal(plant_diversity(EXAMPLE_data,2015,2018),dplyr::tibble(PDI_Tiemann = 42.25,
                                                                     Species_rotation = 13,
                                                                     Shannon = 2.56494936),
               ignore_attr = TRUE)
})
