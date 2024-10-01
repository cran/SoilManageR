#' Calculate relative yield
#' 
#' This function calculates the relative yield of an observed 
#'  dry matter yield compared to the reference dry matter yield 
#'  in the Swiss fertilizing guidelines \insertCite{GRUD2017_ch8}{SoilManageR}.
#' 
#' @md
#' @references
#'  \insertAllCited{}
#' 
#' @seealso 
#' * [productivity_indicator()] to calculate relative yields for a `management_df`
#' * [C_input_crops_LUT()] for reference yield used in the [relative_yield()] function
#' 
#' @param var_crop string with name of crop type, 
#'  must match with the Crops in the C_input_crops_LUT
#' @param var_yield observed dry matter yield (tDM/ha)
#' 
#' @return the numeric value for the relative yield
#' 
#' @examples
#' relative_yield("wheat, winter", 4.8)
#' @export
#' 

relative_yield <- function(var_crop,var_yield) {
  
  reference_yield <- SoilManageR::C_input_crops_LUT %>%
    dplyr::filter(Crop == var_crop) %>%
    dplyr::select(crop_product) %>%
    as.numeric()
  
  relative_yield <- var_yield/reference_yield
  
  return(relative_yield)
}
