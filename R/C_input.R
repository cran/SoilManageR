#' Estimate carbon input
#' 
#' [C_input()] estimates the carbon (C) input into the soil system per year of a
#'  `management_df`.
#' 
#' @details
#' The function takes a `management_df` as input and returns 
#'  a C input values per year in the `management_df`. 
#' 
#' Alternatively, it can return a tibble with all managememnt operations and 
#'  their respective C input values.
#' 
#' The functions calculates the C input with the [C_input_crops()], 
#'  [C_input_cover_crops()] and [CN_input_amendments()] functions.
#'  
#' @seealso
#' * [calculate_indicators()] to calculate all management indicators 
#'      for a `management_df`
#' * [calculate_C_input_tibble()] a helper function that calculates the C input tibble
#' * [C_input_crops()] to calculate C input for crops
#' * [C_input_cover_crops()] to calculate C input for cover crops
#' * [CN_input_amendments()] to calculate C (and N) inputs for organic amendments
#' 
#' @md 
#'     
#' @param var_MGMT_data a `management_df` that contains the management information
#' @param extended.output an optional logical value. 
#' * If FALSE, C input values are aggregated by year. 
#' * If TRUE, a tibble with daily resolution is returned. 
#' * Default value is FALSE
#' 
#' 
#' @return 
#' * By default, a tibble with C input values (total and by category) 
#'  by year is returned. 
#' * If `extended.output = TRUE`, a tibble with all management operations and their
#'  respecitve C inputs is returned. 
#' 
#' 
#' @examples
#' #example that returns annual C input values
#' C_input(EXAMPLE_data)
#' 
#' #example that returns a tibble with all operations that have a C input
#' C_input(EXAMPLE_data, extended.output = TRUE)
#' 
#' @export


C_input <- function(var_MGMT_data, extended.output = FALSE) {

  # Check if the data is of the right class   -------------
  if (!("management_df" %in% class(var_MGMT_data))) {stop("Input if not of the class management_df")}
  
  
  # Calculate C input tibble --------------
  var_MGMT_data <- calculate_C_input_tibble(var_MGMT_data)
  
  
  # create output tibble  ----------------
  if (extended.output == TRUE) {
    output_tibble <- var_MGMT_data
    } else {
    output_tibble <- var_MGMT_data %>%
      dplyr::ungroup() %>%
      dplyr::group_by(year) %>%
      dplyr::summarize(C_input_org_amendment = sum(C_input_org, na.rm = TRUE),
                C_input_crop = sum(C_input_crop, na.rm = TRUE),
                C_input_cover_crop = sum(C_input_CC, na.rm = TRUE),
                crop = dplyr::first(crop)) %>%
      dplyr::mutate(C_input = C_input_org_amendment+C_input_crop+C_input_cover_crop) %>%  #add more parameters later
      dplyr::relocate(year,crop,C_input,C_input_org_amendment,C_input_crop,C_input_cover_crop)  
  }
  
  return(output_tibble)

}
