#' Calculate N input tibble
#' 
#' A function that calculates the N input by mineral and organic fertilization
#' as well as the livestock intensity of the animal derived N inputs.
#' 
#' The function is mainly a helper function for the [N_input()] function.
#'
#' @param var_MGMT_data a `management_df` that contains the management information
#' 
#' @md 
#'
#' @return tibble with N and LSU values
#' @export
#' 
#' @seealso 
#' * [calculate_indicators()] to calculate all management indicators 
#'    for a `management_df`
#' * [CN_input_amendments()] for the calculation of N inputs from organic amendments
#' 
#' * [N_input()] a function that calculates the N input by mineral and organic fertilization and summarizes it by year
#'
#' @examples 
#' #Calculate N input tibble
#' calculate_N_input_tibble(EXAMPLE_data)
calculate_N_input_tibble <- function(var_MGMT_data) {
  
  # Check if the data is of the right class   -------------
  if (!("management_df" %in% class(var_MGMT_data))) {stop("Input if not of the class management_df")}
  
  
  # Calculate N inputs by organic amendments  -------------
  var_MGMT_data_amendments <- var_MGMT_data %>%
    dplyr::rowwise() %>%
    dplyr::filter (operation == "organic_fertilization")
  
  if (nrow(var_MGMT_data_amendments) > 0) {
    var_MGMT_data_amendments <- var_MGMT_data_amendments %>%
      dplyr::mutate(N_input_org = as.integer(CN_input_amendments(value, amd_type=product, C_content = C_content,
                                                                 N_content = N_content, DMC = DMC)[2]))
  } else {
    var_MGMT_data_amendments <- var_MGMT_data_amendments %>%
      dplyr::mutate(N_input_org = NA)
  }
  
  var_MGMT_data <- dplyr::left_join(var_MGMT_data,var_MGMT_data_amendments, by = dplyr::join_by(crop, year, date, category, operation, device, value, unit, machine, product, combination,
                                                                                                comments, DMC, C_content, N_content, crop_product, crop_residue, Cc_product, Cc_residue)) %>%
    dplyr::select(crop,year,date,category,operation,device,value,unit,machine,product,combination,comments,N_input_org)
  
  rm(var_MGMT_data_amendments)
  
  
  # Select events with N inputs and calculate inputs  -------------
  
  var_MGMT_data_N <- var_MGMT_data %>%
    dplyr::filter(N_input_org >= 0 | unit == "kg N/ha")
  
  var_MGMT_data_N <- var_MGMT_data_N %>% 
    dplyr::rowwise() %>%
    dplyr::mutate(N_input_min = dplyr::case_when(
      operation == "organic_fertilization" ~ 0,
      operation == "mineral_fertilization" ~ value)) %>%
    tidyr::replace_na(list(N_input_org = 0)) %>%
    dplyr::mutate(N_input = N_input_org + N_input_min) %>%
    dplyr::group_by(year)
  
  # Calculate Livestock intensity -------------
  
  animal_origin <- CN_input_amendments_LUT %>% 
    dplyr::filter(animal_origin == TRUE) %>% 
    dplyr::select(Amendment)
  
  animal_origin <- animal_origin$Amendment
  
  var_MGMT_data_N <- var_MGMT_data_N %>%
    dplyr::rowwise() %>%
    dplyr::mutate(LSU = dplyr::case_when(product %in% animal_origin ~ N_input_org / 105,
                                         TRUE ~ 0)) %>%
    dplyr::group_by(year)
  
  
  return(var_MGMT_data_N)
  
}
