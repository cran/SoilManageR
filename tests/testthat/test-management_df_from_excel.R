
#define path to dataset
internal_path <- "/extdata/EXAMPLE_data.xlsx"
path_to_xlsx_template <- system.file(internal_path, package = "SoilManageR")


test_that("input error", {
  expect_error(management_df_from_excel())
  expect_error(management_df_from_excel("wrong_path"))
  expect_error(management_df_from_excel(path_to_xlsx_template, var_sheet = "non_exisitng_sheet"))
})

test_that("test EXAMPLE data", {
  expect_equal(management_df_from_excel(path_to_xlsx_template),EXAMPLE_data)
})

#remove path to dataset
rm(internal_path,path_to_xlsx_template)
