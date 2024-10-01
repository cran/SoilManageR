#' Calculate all soil management indicators
#'
#' Checks the `management_df` for consitency with the [check_management_df()] function. 
#'  Then it calculates the [C_input()], [tillage_intensity()], [soil_cover()],[plant_diversity()]
#'  [N_input()] and [productivity_indicator()].
#' 
#' @seealso 
#' * [check_management_df()]
#' * [C_input()]
#' * [tillage_intensity()]
#' * [soil_cover()]
#' * [N_input()]
#' * [plant_diversity()]
#' * [productivity_indicator()]
#' 
#' @md
#' @param var_MGMT_data a `management_df` with a management history
#'
#' @return data frame with indices per year
#' 
#' @examples
#' 
#' \donttest{
#'   #example input
#'   calculate_indicators(EXAMPLE_data)
#' }
#' 
#' @export

calculate_indicators <- function(var_MGMT_data) {

  ####################
  ### Check management_df
  ####################
  check_management_df(var_MGMT_data)    

  ####################
  ### Calculate C inputs
  ####################
  C_input <- C_input(var_MGMT_data, extended.output = FALSE)
  
  ####################
  ###Calculate Soil tillage intensity rating (STIR value)
  ####################
  tillage_intensity <- tillage_intensity(var_MGMT_data, extended.output = FALSE)
  
  ####################
  ###Calculate soil cover days
  ####################
  soil_cover <- soil_cover(var_MGMT_data, extended.output = FALSE)
  
  ####################
  ###Calculate N fertilization and livestock intensity
  ####################
  N_input <- N_input(var_MGMT_data, extended.output = FALSE)
  
  ####################
  ###Calculate productivity indicator
  ####################
  productivity_indicator <- productivity_indicator(var_MGMT_data, extended.output = FALSE)
  
  
  ####################
  ###Generate output
  ####################
  # generate summary tibble  ----------
  output_tibble <- dplyr::left_join(C_input,tillage_intensity, by = dplyr::join_by(year))
  output_tibble <- dplyr::left_join(output_tibble,soil_cover, by = dplyr::join_by(year))
  output_tibble <- dplyr::left_join(output_tibble,N_input, by = dplyr::join_by(year))
  output_tibble <- dplyr::left_join(output_tibble,productivity_indicator, by = dplyr::join_by(year))
  
  # return summary tibble  ----------
  return(output_tibble)
  
}
