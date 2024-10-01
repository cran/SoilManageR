## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----import data, eval=FALSE--------------------------------------------------
#  library(SoilManageR)
#  
#  #define path to excel template in the SoilManageR package
#  internal_path <- "/extdata/SoilManageR_mgmt_data_template_V2.5.xlsx"
#  
#  #create local path
#  path_to_xlsx_template <- system.file(internal_path, package = "SoilManageR")
#  
#  #load management_df
#  mgmt_data <- management_df_from_excel(path_to_xlsx_template)
#  

## ----check data, eval=FALSE---------------------------------------------------
#  #create management_df from example data (delivered with the package)
#  mgmt_data <- EXAMPLE_data
#  
#  #check the consistency of the example data
#  check_management_df(mgmt_data)

## ----calculate all indicators, eval=FALSE-------------------------------------
#  calculate_indicators(mgmt_data)

## ----calculate some indicators, eval=FALSE------------------------------------
#  C_input(mgmt_data)
#  tillage_intensity(mgmt_data)
#  soil_cover(mgmt_data)
#  plant_diversity(mgmt_data,2013,2023)

