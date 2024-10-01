#' Import management_df from excel file
#'
#' This function imports management data from an excel template and
#'  transforms it into a [management_df].
#'  Additionaly, it checks if all columns
#'  that are expected for a managment_df are available.
#'  The excel template can be found in the SoilManageR Package under 
#'  inst/extdata/SoilManageR_mgmt_data_template. Optionaly, the parameter 
#'  *year* can be overwritten by the year extracted from *date*.
#'  
#' @param path_to_xlsx path to the excel file with the management data
#' @param var_sheet name of the sheet with the management data template in 
#'  the excel sheet, default is "Management_template"
#' @param overwrite_year logical: if TRUE (default), the *year* will be set to
#'  the year extracted from the *date*
#'
#' @return a management_df
#' 
#' @examples
#' #create path
#' path_to_xlsx_template <- system.file(
#' "/extdata/SoilManageR_mgmt_data_template_V2.5.xlsx", package = "SoilManageR")
#' 
#' #load management_df
#' management_df_from_excel(path_to_xlsx_template)
#' @seealso 
#' * [management_df()] for creating an management_df
#' * [check_management_df()] to check the integrity of a management_df
#' @md
#' 
#' @export

management_df_from_excel <- function(path_to_xlsx, 
                                     var_sheet = "Management_template", 
                                     overwrite_year = TRUE){
  
  # Read xlsx
  output_tibble <- readxl::read_excel(path_to_xlsx, sheet = var_sheet)
  
  # Assign date type
  output_tibble$date <- as.Date(output_tibble$date)
  
  # Remove rows that contain only NAs
  output_tibble <- output_tibble[rowSums(is.na(output_tibble)) != ncol(output_tibble), ]
  
  # Check if all expected columns are there
  expected_col_names <- c("crop", "year", "date", "category", "operation",
                          "device", "value", "unit", "machine", "product",
                          "combination", "comments", "DMC", "C_content",
                          "N_content", "crop_product", "crop_residue",
                          "Cc_product", "Cc_residue")
  
  # Check for unexpected columns
  unexpected_cols <- setdiff(colnames(output_tibble), expected_col_names)
  if(length(unexpected_cols) > 0) {
    message(paste("Unexpected column(s) in the management_df: ",
                  paste(unexpected_cols, collapse = ", "),
                  sep = ""))
  }
  
  # Check for missing expected columns
  missing_cols <- setdiff(expected_col_names, colnames(output_tibble))
  if(length(missing_cols) > 0) {
    message(paste("Expected column(s) are missing in the management_df: ",
                  paste(missing_cols, collapse = ", "),
                  sep = ""))
  }
  
  # Overwrite the year with the year from the date field
  if(overwrite_year){
  output_tibble$year <- as.double(format(output_tibble$date, "%Y"))
  }
  
  # replace outdated names
  output_tibble$crop[output_tibble$crop == "maize, corn"] <- "maize, grain" # replace "maize, corn" by "maize, grain"
  output_tibble$device[output_tibble$device == "grasland_reseeder"] <- "grassland_reseeder" # replace typo in earlier version of the manuscript
  
  # Assign the object to the class management_df
  class(output_tibble) <- c("management_df", class(output_tibble))
  
  return(output_tibble)
  
}

