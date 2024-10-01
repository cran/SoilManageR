#' Filter management_df for pattern in comments
#' 
#' Excludes operations (lines) from management_df based on a character pattern
#'  in the comments column.
#'
#' @param var_MGMT_data management_df
#' @param filter_pattern string based on which the lines should be excluded 
#'
#' @return a filtered management_df
#' @export
#'
#' @examples
#' # filter EXAMPLE_data and exclude all lines that contain "UFA 330" in comments
#' filter_management_df(EXAMPLE_data,"UFA 330")
#' 
filter_management_df <- function(var_MGMT_data, filter_pattern) {
  
  # Check if filter_pattern is a character string
    if(!(is.character(filter_pattern))) {
      stop("filter_pattern must be a string")
    }
    
  # exclude based on pattern in comments
  var_MGMT_data <- var_MGMT_data %>%
    dplyr::filter(!grepl(filter_pattern,comments))
  
  class(var_MGMT_data) <- c("management_df",class(var_MGMT_data))
    
     
  return(var_MGMT_data)
  
}
