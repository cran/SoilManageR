
#' arrange management_df by date, category
#'
#' the funciton arranges the management_df by date and by category of operations.
#'  The order of the operations is harvest, fertilizer_application,crop_protection,
#'  tillage, sowing, irrigation, other
#'
#' @param var_MGMT_data management_df to be arranged
#' @param include.combination logical, if the combinations should considered
#'  Default is TRUE
#'
#' @return a rearranged management_df
#' @export
#'
#' @examples
#' #rearrange EXAMPLE data
#' arrange_management_df(EXAMPLE_data)
#' 
arrange_management_df <- function(var_MGMT_data, include.combination = TRUE) {

  if(include.combination) {
    
    var_MGMT_data <- var_MGMT_data %>%
    dplyr::arrange(date, 
                   match(category, c('harvest',
                                     'fertilizer_application',
                                     'crop_protection',
                                     'tillage',
                                     'sowing',
                                     'irrigation',
                                     'other')),
                   combination)
  } else {
    
    var_MGMT_data <- var_MGMT_data %>%
      dplyr::arrange(date, 
                     match(category, c('harvest',
                                       'fertilizer_application',
                                       'crop_protection',
                                       'tillage',
                                       'sowing',
                                       'irrigation',
                                       'other')))
  }
  
  
  # Assign class to the rearranged data only if it's not already of that class
  if (!inherits(var_MGMT_data, "management_df")) {
    class(var_MGMT_data) <- c("management_df", class(var_MGMT_data))
  }
  
  return(var_MGMT_data)
  
}
