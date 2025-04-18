#' Estimate nitrogen input
#' 
#' This function estimates the nitrogen (N) input by mineral and organic 
#'  fertilization into the soil system per year. 
#'
#' The function takes a `management_df` as input and returns N input values 
#'  per year in the `management_df`. 
#'  
#' Alternatively, it can return a extensive tibble with all management operations 
#'  and their N input values.
#'  
#' The functions calculates the N input by 
#'  organic fertilization with the [calculate_N_input_tibble()] and the 
#'  [CN_input_amendments()] function. 
#'  Furthermore, it calculates the livestock intensity (`LSU/ha`) by deviding 
#'  the animal derived N by `105kgN/LSU`.
#'  
#' Be aware that the function currently neglects N that is fixated by plants (e.g. legumes)
#'  
#' @seealso 
#' * [calculate_indicators()] to calculate all management indicators 
#'    for a `management_df`
#' * [CN_input_amendments()] for the calculation of N inputs from organic amendments
#' * [calculate_N_input_tibble()] a helper function that calculates the N input tibble
#' 
#' @md
#'      
#' @param var_MGMT_data a `management_df` that contains the management information
#' @param extended.output an optional logical value: 
#' * If `FALSE`, N input values are aggregated by year 
#' * If `TRUE`, a tibble with all management operations is returned 
#' * Default value is `FALSE`
#' 
#' 
#' @return  by default, a tibble with N input values (organic N, mineral N and total N) by year is returned. If extended.output = TRUE, a tibble with daily resolution is returned. 
#' 
#' @seealso 
#' * [calculate_indicators()] to calculate all management indicators 
#'    for a `management_df`
#' * [CN_input_amendments()] for the calculation of N inputs from organic amendments
#' * [calculate_N_input_tibble()] a helper function that calculates the N input tibble
#' 
#' @md
#' 
#' 
#' @examples
#' #example that returns annual N input values
#' N_input(EXAMPLE_data)
#' 
#' #example that returns a tibble with all management operations and their N input
#' N_input(EXAMPLE_data, extended.output = TRUE) 
#' @export
#' 


N_input <- function(var_MGMT_data, extended.output = FALSE) {
  
  # Check if the data is of the right class   -------------
  if (!("management_df" %in% class(var_MGMT_data))) {stop("Input if not of the class management_df")}
  
  # extract all years --------------
  tibble_years <- var_MGMT_data %>%
    dplyr::ungroup() %>%
    dplyr::select(year) %>%
    unique()
  
  # calculate N input tibble
  var_MGMT_data_N <- calculate_N_input_tibble(var_MGMT_data)
  
  # Aggregate by year -------------
  output_tibble <- var_MGMT_data_N %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(N_input_org = sum(N_input_org, na.rm = TRUE),
                     N_input_min = sum(N_input_min, na.rm = TRUE),
                     N_input = sum(N_input, na.rm = TRUE),
                     LSU = sum(LSU, na.rm = TRUE)) %>%
    dplyr::relocate(year,N_input_org,N_input_min,N_input) %>%
    dplyr::right_join(tibble_years, by = dplyr::join_by(year)) %>%
    dplyr::arrange(year) %>%
    tidyr::replace_na(list(N_input_org = 0, N_input_min =  0 , N_input = 0 , LSU = 0 ))
  
    # create and return output tibble  
  if (extended.output == TRUE) {
    output_tibble <- dplyr::left_join(var_MGMT_data,var_MGMT_data_N, 
                                      by = dplyr::join_by(crop, year, date,
                                                          category, operation,
                                                          device, value, unit,
                                                          machine, product, combination,
                                                          comments))
  } 
  
  return(output_tibble)
  
}
