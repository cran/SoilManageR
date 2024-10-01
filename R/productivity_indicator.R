#' Calculate average productivity
#' 
#' Estimates estimates the relative yield of a cropping sequence per year. 
#'  The function takes a `management_df` as input and 
#'  returns a `relative_yield` value per year in the `management_df.` 
#'  Alternatively, it can return a tibble with additional information on each crop. 
#'  The [productivity_indicator()] calculates the relative yields 
#'  with the [relative_yield()] function.
#' 
#' @md
#' @seealso 
#'  * [relative_yield()] for caluclating relative yields
#'  * [calculate_indicators()] for calculating all soil management indicators
#' 
#' @param var_MGMT_data a management_df that contains the management information
#' @param extended.output an optional logical value. 
#'  * If FALSE, relative yields are aggregated by year. 
#'  * If TRUE, a tibble with relative yield of each crop is returned. 
#'  * Default value is FALSE
#' 
#' 
#' @return 
#' * By default, a tibble with relative yields by year is returned. 
#' * If extended.output = TRUE, a tibble with additional information is returned. 
#' 
#' 
#' @examples
#' #example that returns annual relative yield values
#' productivity_indicator(EXAMPLE_data)
#' 
#' #example that returns a tibble with additional information
#' productivity_indicator(EXAMPLE_data, extended.output = TRUE)
#' 
#' @export
#' 

productivity_indicator <- function(var_MGMT_data, extended.output = FALSE) {
  
  # Check if the data is of the right class   -------------
  if (!("management_df" %in% class(var_MGMT_data))) {stop("Input if not of the class management_df")}
  
  var_MGMT_data_productivity <- var_MGMT_data
  
  # select only harvest events
  var_MGMT_data_productivity <- var_MGMT_data_productivity %>%
    dplyr::filter(category == "harvest") %>%
    dplyr::filter(operation == "harvest_main_crop")
  
  # Summarize multiple harvests of temporay leys 
  harvest_leys <- var_MGMT_data_productivity %>% dplyr::filter(crop == "ley, temporary") %>%
    dplyr::group_by(year,crop) %>%
    dplyr::summarise(crop_product = sum(crop_product))
  
  # Recombine the harvests of the temporary leys
  var_MGMT_data_productivity <- dplyr::full_join(var_MGMT_data_productivity,harvest_leys, by = dplyr::join_by(crop,year)) %>%
    dplyr::mutate(crop_product.x = dplyr::case_when(is.na(crop_product.y) ~ crop_product.x,
                                      TRUE ~ crop_product.y)) %>%
    dplyr::select(-crop_product.y) %>%
    dplyr::rename(crop_product = crop_product.x)
  
  # Remove cuts of autum sown temporary leys and only keep the first cut of the leys
  var_MGMT_data_productivity <- var_MGMT_data_productivity %>% 
    dplyr::ungroup() %>%
    dplyr::filter(( crop == "ley, temporary" & year != dplyr::lag(year)) |
                    crop != "ley, temporary" |
                    is.na(dplyr::lag(year)))
  
  # calculcate relative yield
  
  var_MGMT_data_productivity <- var_MGMT_data_productivity %>%
    dplyr::rowwise() %>%
    dplyr::mutate(relative_yield = relative_yield(crop,crop_product)) %>%
    dplyr::select(crop,year,date,crop_product,relative_yield)
  
  # produce and return output tibble
  if(extended.output == FALSE) {
    
    var_MGMT_data_productivity_year <- var_MGMT_data_productivity %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(relative_yield = mean(relative_yield))
    
    output_tibble <- var_MGMT_data_productivity_year
    
  } else {
    output_tibble <- var_MGMT_data_productivity
  }

  
  
  return(output_tibble)
    
}

