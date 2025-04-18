#' Calculate STIR tibble
#' 
#' A function that calculates the STIR value of all operations in a `management_df`.
#' The output is returned as a tibble.
#' 
#' The function is mainly a helper function for the [tillage_intensity()] function.
#'
#' @param var_MGMT_data a `management_df` that contains the management information
#'  
#' @seealso
#' * [STIR()] for the calculation of a `STIR` value for operations
#' * [STIR_values_LUT()] for the reference data used for tillage operations
#' * [calculate_indicators()] to calculate all management indicators 
#'    for a `management_df`
#' * [tillage_intensity()] aggregates the STIR values
#'
#' @md 
#' 
#' 
#' @return a tibble with all management operations and STIR values 
#' 
#' @examples
#' 
#' #example that returns a tibble with all operations that have a STIR value
#' calculate_STIR_tibble(EXAMPLE_data)
#' 
#' @export
#' 


calculate_STIR_tibble <- function(var_MGMT_data) {

  # select relevant management events ----------------
  var_MGMT_data_STIR <- var_MGMT_data %>%
    dplyr::filter(device %in% STIR_values_LUT$Operation) %>%
    dplyr::select(-DMC,-C_content,-N_content,-crop_product,-crop_residue,-Cc_product,-Cc_residue) %>%
    dplyr::mutate(STIR = NA)
  
  # exclude multiple sowing operations with the same device at the same date (e.g. mixtures of cover crops) ------
  var_MGMT_data_STIR <- var_MGMT_data_STIR %>%
    dplyr::ungroup() %>%
    dplyr::filter((date != dplyr::lag(date) & device != dplyr::lag(device)) | #exclude events with same date and device
                    category != "sowing" | # only apply it to sowing events
                    is.na(dplyr::lag(category))) # always include the first line of the dataset
  
  # exclude multiple liquid_injections at the same date ------
  var_MGMT_data_STIR <- var_MGMT_data_STIR %>%
    dplyr::ungroup() %>%
    dplyr::filter((date != dplyr::lag(date) & device != dplyr::lag(device)) | #exclude events with same date and device
                    device != "liquid_injection" | # only apply it to liquid_injection devicdes
                    is.na(dplyr::lag(category))) # always include the first line of the dataset
  
  # calculate STIR ----------------
  var_MGMT_data_STIR <- var_MGMT_data_STIR %>%
    dplyr::rowwise() %>%
    dplyr::mutate(STIR = dplyr::case_when(
      unit == "cm" ~ STIR(device, depth = value),
      TRUE ~ STIR(device)))
  
  return(var_MGMT_data_STIR)

}
