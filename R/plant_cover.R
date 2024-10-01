#' Estimate soil cover percentage by plants
#' 
#' This function estimates the percentage of soil cover 
#'  based on the number of days since sowing. 
#'  The parameters used are derived from \insertCite{mosimann2006;textual}{SoilManageR}.
#'  
#' @details
#' The function assumes that plant cover unfolds in four phases with different soil cover rates:
#' * 0 to 10 % of soil cover
#' * 10 to 50 % of soil cover
#' * 50 to 75 % of soil cover
#' * 75 to 100 % of soil cover
#' 
#' @md
#' @references
#'  \insertAllCited{}
#'  
#' @seealso 
#' * [soil_cover()] to calculate soil coverage by plants and residue for a management_df
#' * [plant_cover_LUT()] for the data used by the [plant_cover()] function
#'    
#' @param varCrop Crop type, must match with crop name in `plant_cover_LUT`
#' @param varDays Number of days since sowing of the crop
#' 
#' @return percentage of soil cover by plants, value of 0 to 100 %.
#' 
#' @examples
#'plant_cover("wheat, winter", 140)
#'
#' 
#' @export
#' 

plant_cover <- function(varCrop, varDays = 0) {
  
  # check if inputs are valid
  if (!(varCrop %in% plant_cover_LUT$Crop)) {
    stop("Crop not in plant_cover_LUT, cannot estimate soil cover")
  }
  
  if (varDays < 0) {
    stop("Cannot estimate for negative days since sowing")
  }
  
  
  # look up values form plant_cover_LUT
  LOT_values <- plant_cover_LUT %>%
    dplyr::filter(Crop == varCrop)
  
  #calculate soil cover
  cover <- varDays*LOT_values$Slope_0_10
  
  if (cover > 10) {
    days_10 <- 10/LOT_values$Slope_0_10
    cover <- 10 + (varDays - days_10)*LOT_values$Slope_10_50
  }
  
  if (cover > 50) {
    days_50 <- days_10 + 40/LOT_values$Slope_10_50
    cover <- 50 + (varDays - days_50)*LOT_values$Slope_50_75
  }
  
  if (cover > 75) {
    days_75 <- days_50 + 25/LOT_values$Slope_50_75
    cover <- 75 + (varDays - days_75)*LOT_values$Slope_75_100
  }
  
  if (cover > 100) {
    cover <- 100
  }
  
  # define output value
  output_value <- round(cover,0)
  
  return(output_value)
}
