#' Estimate C inputs by cover crops 
#' 
#' This function estimates the Carbon (C) input into the soil system by cover crops 
#' based on the duration of the cover crop stand. 
#'
#' [C_input_cover_crops()] internally calls [C_input_crops()] to calculate the 
#'  different C fractions. The C in the above ground biomass (\eqn{C_{Product}}) 
#'  is a function of the time a cover crop is established. A minimum and 
#'  a maximum cover crop biomass are assumed at 180 and 240 days respectively, 
#'  and linearly interpolated for the period in between.
#'  \deqn{C_{Product} = \begin{cases} 
#'  1253\ kgC/ha \ , \ duration < 180 \ days \\
#'  1253 \ kgC/ha \ + \ (duration - 180  \ days) \ * \frac{663 \ kgC/ha}{60 \ days} \     , \ 180 \ days \leq duration \leq 240 \ days \\
#'  1916 \ kgC/ha \ , \ duration > 240 \ days
#'  \end{cases}}
#'  
#'  Assumptions on the C inputs at day 180 and 240 are based on values 
#'  extracted from \insertCite{seitz2022;textual}{SoilManageR}.
#'
#' The remaining parameters to calculate the C input by cover crops are 
#'  `HI = 1`, `SER = 3.67`, and `REF = 0.31`, all derived from \insertCite{seitz2022;textual}{SoilManageR}.
#'
#' Note, that with these assumptions the C input of short term cover crops 
#'  (e.g. few weeks) is overestimated.
#'  
#' The function [C_input_cover_crops()] estimates the C input by applying the
#'  assumptions mentioned above. Alternatively, the user can 
#'  supply an above ground biomass and a CC of the biomass,
#'  or other parameters to estimate the C input by cover crops.
#'  
#' @seealso 
#' * [C_input()] to calculate C inputs for a `management_df`
#' * [C_input_crops()] to calculate C input for crops
#' * [CN_input_amendments()] to calculate C (and N) inputs for organic amendments
#' * [C_input_crops_LUT()] for the look-up-table for crop reference values
#'  
#' @references
#'  \insertAllCited{}
#' @md
#' @param abvg_biomass (optional): Dry weight of aboveground biomass of the cover crop (`tDM/ha`)
#' @param days (optional): Number of days that the cover crop was established. If no value is provided, mind_days is assumed (`days`)
#' @param min_C_abvg (optional): Minimal above ground C that the cover crop produces, given it is established for the min_days number of days. Default value is 1.253 (`kgC/ha`)
#' @param min_days (optional): Number of days where the interpolation of the biomass starts. The default value is 180 (`days`)
#' @param max_C_abvg (optional): Maximum biomass that the cover crop can produce. Default value is 1.916 (`kgC/ha`)
#' @param max_days (optional): Number of days when the maximum biomass of the cover crop is reached. The default value is 240 (`days`)
#' @param Cc_biomass (optional): Assumed C content of the cover crop biomass. Default value is 450 (`kgC/tDM`)
#' 
#' @return a tibble with the following parameters: 
#' * C_input_product: Estimated soil carbon input from product (i.e., the cover corp aboveground biomass) (`kgC/ha`), 
#' * C_input_straw: Estimated soil carbon input by straw or other residues (typically 0 for cover crops) (`kgC/ha`), 
#' * C_input_root: Estimated soil carbon input by roots (`kgC/ha`), 
#' * C_input_exudate: Estimated soil carbon input by roots (`kgC/ha`),
#' * C_input_total: Total estimated Soil carbon input, sum of C_input_straw, C_input_root, C_input_exudate (`kgC/ha`),
#' 
#' @examples
#' #example when only the duration is known
#' C_input_cover_crops(days = 205) 
#' 
#' #example if the cover crop biomass is known
#' C_input_cover_crops(abvg_biomass = 2.5, Cc_biomass = 450) 
#' 
#' #example with custom assumptions on the above ground biomass developnent over time
#' C_input_cover_crops(days = 60, min_C_abvg = 600 , min_days = 50, max_C_abvg = 1916, max_days = 240)
#' 
#' @export
#'  
C_input_cover_crops <- function(abvg_biomass = NA,
                          days = 180,
                          min_C_abvg = 1253,
                          min_days = 180,
                          max_C_abvg = 1916,
                          max_days = 240,
                          Cc_biomass = 450) {
  
  # Calculate above ground biomass (if necessary)
  
  if (is.na(abvg_biomass)) {
    
    interpol_x <- c(min_days,max_days)
    interpol_y <- c(min_C_abvg,max_C_abvg)
    
    abvg_biomass <- dplyr::case_when(days < min_days ~ min_C_abvg/Cc_biomass,
                              days > max_days ~ max_C_abvg/Cc_biomass,
                              TRUE ~ approx(interpol_x,interpol_y,xout=days)$y/Cc_biomass  #Linear interpolation of the values above
                              )
  }
  
  # Calculate C input by Cover crops  ------------
  output_tibble <- C_input_crops("cover crop", crop_product = abvg_biomass, Cc_product = Cc_biomass)
    

  # Define output  ---------------
  
  return (output_tibble)
  
}

