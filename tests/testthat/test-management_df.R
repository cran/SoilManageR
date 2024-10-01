test_that("input error", {
  expect_error(management_df(irrelevant))
})


expected_result <- dplyr::tibble(crop = NA,
              year = NA,
              date = as.Date(NA),
              category = NA,
              operation = NA,
              device = NA,
              value = NA,
              unit = NA,
              machine = NA,
              product = NA,
              combination = NA,
              comments = NA,
              DMC = NA,
              C_content = NA,
              N_content = NA,
              crop_product = NA,
              crop_residue = NA,
              Cc_product = NA,
              Cc_residue = NA) %>%
  dplyr::filter(!is.na(crop))

class(expected_result) <- c("management_df",class(expected_result))

test_that("test class and columns", {
  expect_equal(management_df(),expected_result)
})
