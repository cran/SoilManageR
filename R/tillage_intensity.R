#' Estimate tillage intensity 
#' 
#' Calculates the soil tillage intensity per year.
#'  The function takes a `management_df` as input and returns a `STIR` value
#'  per year in the `management_df.` Alternatively, it can return a
#'  extensive tibble with each operation and their respective `STIR` values.
#'  
#' @seealso
#' * [STIR()] for the calculation of a `STIR` value for operations
#' * [STIR_values_LUT()] for the reference data used for tillage operations
#' * [calculate_indicators()] to calculate all management indicators 
#'    for a `management_df`
#'   
#'     
#' @param var_MGMT_data a `management_df` that contains the management information
#' @param extended.output an optional logical value. Default value is `FALSE`
#'  * If `FALSE`, `STIR` values are aggregated by year. 
#'  * If `TRUE`, a tibble with all management operations is returned.
#' @md 
#' 
#' 
#' @return by default, a tibble with STIR values by year is returned. If extended.output = TRUE, a tibble with daily resolution is returned. 
#' 
#' 
#' @examples
#' #example that returns annual STIR values
#' tillage_intensity(EXAMPLE_data)
#' 
#' #example that returns a tibble with all operations that have a STIR value
#' tillage_intensity(EXAMPLE_data, extended.output = TRUE)
#' 
#' @export
#' 

tillage_intensity <- function(var_MGMT_data,extended.output = FALSE) {
  
  
  # Check if the data is of the right class   -------------
  if (!("management_df" %in% class(var_MGMT_data))) {stop("Input if not of the class management_df")}
  
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
  
  # aggregate per year ----------------
  var_MGMT_data_STIR <- dplyr::left_join(var_MGMT_data,var_MGMT_data_STIR,
                                         by = dplyr::join_by(crop, date, year,category,operation,device,value,unit,machine,product,combination,comments)) %>% 
    dplyr::group_by(year)
  
  tillage_intensity_year <- var_MGMT_data_STIR %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(STIR = sum(STIR, na.rm = TRUE))
  
  
  # create output tibble ----------------
  if (extended.output) {
    output.tibble <- var_MGMT_data_STIR
  } else {
    output.tibble <- tillage_intensity_year
  }
  
  return(output.tibble)
  }
