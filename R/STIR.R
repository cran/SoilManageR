#' Calculate STIR value 
#' 
#' The function calculates the soil tillage intensity rating (STIR) value 
#'  of tillage and sowing operations.
#'  By default, the [STIR()] function of `SoilManageR` 
#'  operates on SI units (cm and km/h) and not in imperial units (inch, mph) 
#'  as the original STIR equation \insertCite{RUSLE2}{SoilManageR}. 
#'  However, the user can specifiy that the input is in imperial units.
#'  The function can process custom input, if no such input is provided 
#'  it assumes default values from the `STIR_value_LUT`. 
#' 
#' @details
#' The concept of the STIR value was developed within the 
#'  RUSLE2 framework by the \insertCite{RUSLE2;textual}{SoilManageR}.
#'  The STIR equation is defined as 
#'  
#'  \deqn{STIR = (0.5 * speed) * (3.25 * type_{modifier}) * depth * area_{disturbed}}
#'   
#'  where \eqn{speed} and \eqn{depth} are provided in mph and inches(!). 
#'  For the purpose of this function we assume that `1 inch = 2.54 cm` and `1 mph = 1.609 km/h`. 
#'  The tillage \eqn{type_{modifier}} is defined to be:
#'  * `1.00` for inversion operation
#'  * `0.80` for mixing and some inversion operations
#'  * `0.70` for mixing operations
#'  * `0.40` for lifting and fracturing operations
#'  * `0.15` for compression operations 
#' 
#' In the `STIR_value_LUT` there are more than 400 operations, 
#'  incl. the original operations from the RUSLE2 software (as of 2023-02-24) 
#'  and operations defined by the SoilX project.
#' 
#' For further details on the STIR please consider the RUSLE2 website (https://fargo.nserl.purdue.edu/rusle2_dataweb/RUSLE2_Index.htm) or the description of the SoilManageR package
#'   
#' @md
#' @references
#'  \insertAllCited{}
#' 
#' @seealso 
#' * [tillage_intensity()] to calculate STIR values for management_df
#' * [STIR_values_LUT()] for the reference data used by the [STIR()] function
#' 
#' @param device tillage or sowing implement or operation, must match predefined list, 
#'  or all necessary inputs (speed, type modidifier, depth, area_disturbed) must be provided
#' @param speed speed of the operation (km/h or mph)
#' @param speed_unit unit of the speed input, must be either NA, "km/h" or "mph". 
#'  Default value is "km/h"
#' @param type_modifier tillage operation type modifier, must be a value between 0 and 1
#' @param depth depth of the soil that is affected by the operation (cm or inch)
#' @param depth_unit unit of the depth input, must be either NA, "cm" or "inch". 
#'  Default value is "cm"
#' @param area_disturbed share of the surfave that is disturbed by the operation (0 to 1)
#' @param original.STIR.value logical value, if TRUE, the original STIR value of
#'  the operation in the `STIR_value_LUT` (instead of the calculated STIR value) is returned. 
#'  Default value is FALSE
#' 
#' @return STIR value of the operation
#' 
#' @examples
#' #example without additional information
#' STIR("plough")
#' 
#' #example with additional information
#' STIR("rotary_harrow", depth = 15)
#' 
#' #custom example
#' STIR(speed = 10, type_modifier = 0.8, depth = 15, area_disturbed = 0.45)
#' 
#' #example that returns orginial STIR value
#' STIR("plough", original.STIR.value = TRUE)
#' 
#' #example that uses imperial units
#' STIR("plough", depth = 5, depth_unit = "inch", speed = 8, speed_unit = "mph")
#' @export

STIR <- function(device = NA, speed = NA, speed_unit = "km/h", type_modifier = NA,
                 depth = NA, depth_unit = "cm", area_disturbed = NA, original.STIR.value = FALSE) {

  # set all NA parameters to NULL or default value ---------------
  if (is.na(speed)) {speed <- NULL}
  if (is.na(speed_unit)) {speed_unit <- "km/h"}
  if (is.na(type_modifier)) {type_modifier <- NULL}
  if (is.na(depth)) {depth <- NULL}
  if (is.na(depth_unit)) {depth_unit <- "cm"}
  if (is.na(area_disturbed)) {area_disturbed <- NULL}
  
  # check if all necessary parameters are provided  ---------------
  if(is.na(device)) {
      if(is.null(speed) |
           is.null(type_modifier) |
           is.null(depth) |
           is.null(area_disturbed)) {
    stop("device is not supplied, provide a valid device or provide necessary optional parameters (speed, type_modifier, depth and area_disturbed)")
    }
  }
  
  
  if (!(device %in% STIR_values_LUT$Operation)) {
    if (is.null(speed) |
        is.null(type_modifier) |
        is.null(depth) |
        is.null(area_disturbed)) {
      stop("The selected device is not in the list of available devices. Please consider the list of available devices (STIR_values_LUT) or provide necessary optional parameters (speed, type_modifier, depth and area_disturbed)")
      } 
    }

  if (!(speed_unit %in% c("mph","km/h"))) {
    stop("Invalid input: speed_unit must be NA, mph or km/h")
  }
  
  if (!(depth_unit %in% c("inch","cm"))) {
    stop("Invalid input: speed_unit must be NA, inch or cm")
  }
  
  # load default values from LOT ---------------
  
  if (is.null(speed)) {
    speed <- STIR_values_LUT %>%
      dplyr::filter(Operation == device) %>%
      dplyr::select(Speed) %>%
      as.numeric()
  }
  
  if (is.null(type_modifier)) {
    type_modifier <- STIR_values_LUT %>%
      dplyr::filter(Operation == device) %>%
      dplyr::select(TILLAGE_TYPE_Modifier) %>%
      as.numeric()
  }
  
  if (is.null(depth)) {
    depth <- STIR_values_LUT %>%
      dplyr::filter(Operation == device) %>%
      dplyr::select(Depth) %>%
      as.numeric()
  }
  
  if (is.null(area_disturbed)) {
    area_disturbed <- STIR_values_LUT %>%
      dplyr::filter(Operation == device) %>%
      dplyr::select(Surf_Disturbance) %>%
      as.numeric()
    area_disturbed <- area_disturbed / 100 # percent to no unit
  }
    
  
  
  # change units from imperial to SI (if necessary)  ---------------
  
  if (speed_unit == "mph") {
    speed <- speed * 1.609      # 1.609 km/h = 1 mph,
  }
  
  if (depth_unit == "inch") {
    depth <- depth * 2.54      # 2.54 cm = 1 inch
  }
  
  
  # calculate STIR value  ---------------
  
  STIR <- (speed / 1.609 * 0.5) * (type_modifier * 3.25) * (depth / 2.54) * area_disturbed # 1.609 km/h = 1 mph, 2.54 cm = 1 inch
  STIR <- round(STIR)
  
  # overwrite STIR value with original STIR value (if wanted) ---------------
  
  if(original.STIR.value == TRUE) {
    STIR <- STIR_values_LUT %>%
      dplyr::filter(Operation == device) %>%
      dplyr::select(STIR_original) %>%
      as.numeric()
  }
  
  # return STIR value  ---------------
  return (STIR)
}
