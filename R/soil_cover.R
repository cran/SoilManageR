#' Estimate soil cover by plants and residues
#' 
#' @description
#' Derives the days where soil cover by living plants or resiudes is `>=30%`. 
#' 
#' @details
#' The function takes a `management_df` as input and returns soil cover 
#'  days per year in the `management_df.` 
#'  Alternatively, it can return a `soil_cover_tibble` with daily resolution 
#'  of the estimated soil cover.
#' 
#' The function calculates plant soil cover with the [plant_cover()] function
#'  and the soil coverage by residues.
#' 
#' The residue mass is dependent on the residue supply by crops,
#'  its decay and its incorporation by tillage operations 
#'  \insertCite{buchi2016}{SoilManageR}.
#'  
#' Residue supply is estimated with the yield dependent 
#'  residue C `C_input_straw` provided by the function [C_input_crops()] and 
#'  a C content of \eqn{450 \ [mgC/gDM]}. 
#'  If residues are removed, the removed residue mass is subtracted.
#' 
#' Residue decay is calculated with the formula of 
#'  \insertCite{steiner1999;textual}{SoilManageR}:
#'  \deqn{M_t = M_{t-1} * (1 - k_{decay})}
#'  Where \eqn{M_{t-1}} is the residue mass of the prior day \eqn{[g/m^2]} and 
#'  \eqn{k_{decay}} is the daily decay rate that was assumed to be 
#'  `0.028` \eqn{g/g}, the average decomposition rate of winter wheat straw 
#'  \insertCite{steiner1999}{SoilManageR}. 
#'  
#' Residue incorporation by tillage was estimated with the operation-specific 
#'  burial coefficient extracted from the RUSLE2 database 
#'  \insertCite{RUSLE2}{SoilManageR} that are provided in the 
#'  look-up-table `STIR_values_LUT`.
#'  
#' Residue mass is translated into percentage of soil cover 
#'  by the formula of \insertCite{steiner2000;textual}{SoilManageR}:
#'  \deqn{cover_{residues} = (1-e^{-k(M)})* 100 \%}
#'  Where \eqn{M} is the residue mass \eqn{[g/m^2]} and \eqn{k} is a cover 
#'  coefficient \eqn{[m^2/g]}. \eqn{k} was assumed to be `0.0175` 
#'  \insertCite{steiner2000}{SoilManageR}.
#' 
#' 
#' @md
#' @references
#'  \insertAllCited{}
#' 
#' @seealso
#' * [calculate_indicators()] to calculate all management indicators 
#'      for a `management_df`
#' * [calculate_soil_cover_tibble()] a helper function that calculates the soil cover tibble
#' * [plant_cover()] for more detail on the plant cover function
#' * [plot.soil_cover_tibble()] for plotting the `soil_cover_tibble`
#' * `STIR_values_LUT` for tillage operation specific burial coefficients
#'    
#' @param var_MGMT_data a `management_df` that contains the management information
#' @param extended.output an optional logical value. 
#'  * If FALSE, soil cover days are aggregated by year. 
#'  * If TRUE, a soil_cover_tibble with daily resolution is returned. 
#'  * Default value is FALSE
#' 
#' @return 
#'  * By default, a tibble with soil cover days by year is returned. 
#'  * If extended.output = TRUE, an object of the class `soil_cover_tibble` 
#'    with daily resolution is returned. 
#' 
#' 
#' @examples
#' \donttest{
#'   #example that returns annual soil cover days by plants and residues
#'   soil_cover(EXAMPLE_data)
#' 
#'   #example that returns a soil_cover_tibble
#'   soil_cover(EXAMPLE_data, extended.output = TRUE)
#' }
#' 
#' @export
#' 


soil_cover <- function(var_MGMT_data, extended.output = FALSE) {
  
  # Check if the data is of the right class   -------------
  if (!("management_df" %in% class(var_MGMT_data))) {stop("Input if not of the class management_df")}
  

  # Calculate soil cover tibble --------------
  
  var_MGMT_data_cover <- calculate_soil_cover_tibble(var_MGMT_data)
  
  # Aggregate per year --------------
  
  soil_cover_year <- var_MGMT_data_cover %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(soil_cover_days = sum(soil_cover_days, na.rm = TRUE),
              plant_cover_days = sum(plant_cover_days, na.rm = TRUE),
              residue_cover_days = sum(residue_cover_days, na.rm = TRUE))
  
  
  # create and return output tibble ----------------
  
  if (extended.output) {
    output.tibble <- var_MGMT_data_cover
  } else {
    output.tibble <- soil_cover_year
  }
  
  return(output.tibble)
}

