#' Estimate C inputs by crops
#' 
#' Calculates the estimated carbon (C) input into the soil system by harvested main crops.
#' 
#' @details
#' The annual C input by crops were estimated based on crop type and crop yield
#'  with the allometric functions of \insertCite{bolinder2007;textual}{SoilManageR}:
#' 
#' \deqn{ 
#' C_{Product} = Product * CC_{Product}
#' }
#' \deqn{ 
#' C_{Straw} = {Product}* \frac{1 - HI}{HI} * CC_{Straw}
#' }
#' \deqn{  
#' C_{Root} = \frac{Product}{SRR*HI} * CC_{Root}
#' }
#' \deqn{  
#' C_{Exudates} = C_{Root} * REF
#' }
#' 
#' Where \eqn{C} is the C per fraction (in kgC/ha) and \eqn{CC} is the C
#'  content of given fraction (kgC/tDM). \eqn{Prodcuct} is the dry matter
#'  yield of a crop in tDM/ha, \eqn{HI} is the harvest index
#'  (ratio of product total of product and straw), \eqn{SRR} is the
#'  ratio of the shoot biomass (product and straw) to the root biomass,
#'  and \eqn{REF} is the root exudation factor 
#'  (i.e., the ratio of the C exudated by the roots
#'  to the C in the root biomass). All fractions are multiplied with 
#'  a crop and fraction specific \eqn{S}-factor that determines the share
#'  of the fraction that is returned to the soil.
#'         
#' If not mentioned otherwise parameters were taken from the
#'  publications of \insertCite{bolinder2007;textual}{SoilManageR},
#'  \insertCite{keel2017;textual}{SoilManageR} or  
#'  \insertCite{wuest2020;textual}{SoilManageR}. 
#'  Parameters for potatoes and sugar beets were derived from 
#'  \insertCite{bolinder2015;textual}{SoilManageR}
#'  For temporary leys we assumed yield-independent annual 
#'  \eqn{C_{Root}} of 1.5 MgC/ha and a \eqn{REF} of 0.5 
#'  \insertCite{taghizadeh-toosi2020}{SoilManageR}. 
#'  Furthermore, like \insertCite{wuest2020;textual}{SoilManageR}, the 
#'  belowground C input (\eqn{C_{Root} + C_{Exudates}}) of corn maize,
#'  silage maize and cereals were fixed to 0.46 MgC/ha, 1.1 MgC/ha and
#'  0.6 MgC/ha respectively, based on the values from 
#'  \insertCite{hirte2018;textual}{SoilManageR}.
#'  Additionally, we applied the yield dependent harvest index 
#'  \eqn{(HI = Intercept + Product * Slope)} proposed by 
#'  \insertCite{fan2017;textual}{SoilManageR} for cereals, faba beans, 
#'  peas, corn, rapeseed, and soybeans. 
#'  
#' Reference yields were derived from the Swiss fertilizer recommendations 
#'  \insertCite{GRUD2017_ch8;textual}{SoilManageR}. 
#'
#' All default values can be found in the look-up-table `C_input_crops_LUT`.
#' 
#' @seealso
#' * [C_input()] to calculate C inputs for a `management_df`
#' * [C_input_cover_crops()] to calculate C input for cover crops
#' * [CN_input_amendments()] to calculate C (and N) inputs for organic amendments
#' * [C_input_crops_LUT()] for the look-up-table for crop reference values
#' 
#' @md   
#' @references
#'  \insertAllCited{}
#' 
#' @param crop Crop type. Must match predefined list (see `C_input_crops_LUT`)
#' @param crop_product (optional) Dry weight of the exported product, i.e. yield. Default value is taken from table (tDM/ha)
#' @param crop_residue (optional) Dry weight of the residues of the main crop (e.g., straw, sugar beet leaves) (tDM/ha)
#' @param harvest_index (optional) Ratio of the product to the total above ground biomass
#' @param variable_harvest_index (optional) Logical value that is TRUE if the variable harvest index assumptions of Fan et al. (2017) are to be applied (TRUE / FALSE)
#' @param HI_intercept (optional) Intercept of the variable harvest index. Values provided by Fan et al. (2017)
#' @param HI_slope (optional) Slope of the variable harvest index. Values provided by Fan et al. (2017) (ha/tDM)
#' @param shoot_root_ratio (optional) Ratio of the total above ground biomass to the root biomass
#' @param root_exudation_factor (optional) Ratio of the root exudated C to the C in the root biomass
#' @param Cc_root (optional) C concentration in the roots. Default value is 450 (gC/kgDM) 
#' @param Cc_product (optional) C concentration in the exported product. Default value is 450 (gC/kgDM)
#' @param Cc_residue (optional) C concentration in the residues of the main crop. Default value is 450 (gC/kgDM)
#' @param fixed_belowground_input (optional) Logical value that is TRUE if fixed below ground Carbon inputs are to be assumed (e.g. for temporary leys) (TRUE / FALSE)
#' @param fixed_C_input_root (optional) amount of root C that is assumed if fixed_belowground_input is TRUE (kgC/ha)
#' @param straw.removal (optional) Logical value that is TRUE if straw is removed at harvest. 
#' * When NA then SR value from table is take
#' * When TRUE SS is 1
#' * When FALSE SS is 0. 
#' * Default value is NA
#' @param return.comment (optional) logical value if comment are returned or not. Default = FALSE (TRUE/FALSE)
#' 
#' @return a tibble with the following parameters: 
#' * C_input_product: Estimated soil carbon input from product (e.g. damaged patatos) (kgC/ha), 
#' * C_input_straw: Estimated soil carbon input by straw or other residues (kgC/ha), 
#' * C_input_root: Estimated soil carbon input by roots (kgC/ha), 
#' * C_input_exudate: Estimated soil carbon input by roots (kgC/ha),
#' * C_input_total: Total estimated Soil carbon input, sum of C_input_straw, C_input_root, C_input_exudate (kgC/ha),
#' * comment (optional): comment on the derived values
#' 
#' @examples
#' #example without yield information, default yield is assumed
#' C_input_crops("wheat, winter")
#' 
#' #example with yield information and straw retention
#' C_input_crops("barley, spring", crop_product = 4.5, straw.removal = FALSE)
#' 
#' #example with more information
#' C_input_crops("barley, spring", crop_product = 4.5, harvest_index = 0.4,
#'               shoot_root_ratio = 2.4, root_exudation_factor = 0.5)
#' 
#' #example with variable harvest index
#' C_input_crops("barley, spring", crop_product = 4.5, variable_harvest_index = TRUE,
#'               HI_intercept = 0.35, HI_slope = 0.015, shoot_root_ratio = 2.4,
#'               root_exudation_factor = 0.5)
#' 
#' #example with fixed below ground input
#' C_input_crops("maize, silage", crop_product = 18.5, 
#'               fixed_belowground_input = TRUE, fixed_C_input_root = 1500,
#'               root_exudation_factor = 0.3) 
#' @export
#' 
C_input_crops <- function(crop,
                          crop_product = NA,
                          crop_residue = NA,
                          harvest_index = NA,
                          variable_harvest_index = NA,
                          HI_intercept = NA,
                          HI_slope = NA,
                          shoot_root_ratio = NA,
                          root_exudation_factor = NA,
                          Cc_product = 450,
                          Cc_residue = 450,
                          Cc_root = 450,
                          straw.removal = NA,
                          fixed_belowground_input = NA,
                          fixed_C_input_root = NA,
                          return.comment = FALSE) {
 
  # load default value table   ---------------
  #Crop_data_path <- "./inst/extdata/C_input_crops_LUT.xlsx"
  #Default_Crop <- readxl::read_excel(Crop_data_path)
  Default_Crop <- C_input_crops_LUT
   
  # set all optional NA parameters to NULL or default value ---------------
  if (is.na(crop_product)) {crop_product <- NULL}
  if (is.na(crop_residue)) {crop_residue <- NULL}
  if (is.na(harvest_index)) {harvest_index <- NULL}
  if (is.na(variable_harvest_index)) {variable_harvest_index <- NULL}
  if (is.na(HI_intercept)) {HI_intercept <- NULL}
  if (is.na(HI_slope)) {HI_slope <- NULL}
  if (is.na(shoot_root_ratio)) {shoot_root_ratio <- NULL}
  if (is.na(root_exudation_factor)) {root_exudation_factor <- NULL}
  if (is.na(Cc_product)) {Cc_product <- 450}
  if (is.na(Cc_residue)) {Cc_residue <- 450}
  if (is.na(Cc_root)) {Cc_root <- 450}
  if (is.na(straw.removal)) {straw.removal <- NULL}
  if (is.na(fixed_belowground_input)) {fixed_belowground_input <- NULL}
  if (is.na(fixed_C_input_root)) {fixed_C_input_root <- NULL}
  

  # check if all necessary parameters are provided  ---------------
  if (is.na(crop)) {stop("Crop type is not supplied, provide a valid input")}
  
  if (!(crop %in% Default_Crop$Crop)) {
    if (is.null(crop_product) |
        is.null(harvest_index) |
        is.null(shoot_root_ratio) |
        is.null(root_exudation_factor)) {
      stop("The selected crop type is not in the list of available crops. Please extend the list of available crops or provide necessary optional parameters (crop_product, crop_residue, harves_index, shoot_root_ratio, root_exudation_factor)")
    }
  }
  
  # check if all optional parameters are of the expected format  ---------------
  if (!is.numeric(crop_product) & !is.null(crop_product)) {stop("crop_product is not numeric")}
  if (!is.numeric(crop_residue) & !is.null(crop_residue)) {stop("crop_residue is not numeric")}
  if (!is.numeric(harvest_index) & !is.null(harvest_index)) {stop("harvest_index is not numeric")}
  if (!is.logical(variable_harvest_index) & !is.null(variable_harvest_index)) {stop("variable_harvest_index is not numeric")}
  if (!is.numeric(HI_intercept) & !is.null(HI_intercept)) {stop("HI_intercept is not numeric")}
  if (!is.numeric(HI_slope) & !is.null(HI_slope)) {stop("HI_slope is not numeric")}
  if (!is.numeric(shoot_root_ratio) & !is.null(shoot_root_ratio)) {stop("shoot_root_ratio is not numeric")}
  if (!is.numeric(root_exudation_factor) & !is.null(root_exudation_factor)) {stop("root_exudation_factor is not numeric")}
  if (!is.numeric(Cc_product) & !is.null(Cc_product)) {stop("Cc_product is not numeric")}
  if (!is.numeric(Cc_residue) & !is.null(Cc_residue)) {stop("Cc_residue is not numeric")}
  if (!is.numeric(Cc_root) & !is.null(Cc_root)) {stop("Cc_root is not numeric")}
  if (!is.logical(straw.removal) & !is.null(straw.removal)) {stop("straw.removal is not logical value")}
  if (!is.logical(fixed_belowground_input) & !is.null(fixed_belowground_input)) {stop("fixed_belowground_input is not logical value")}
  if (!is.numeric(fixed_C_input_root) & !is.null(fixed_C_input_root)) {stop("fixed_C_input_root is not numeric")}
 
  
  # initialize comment  ---------------
  comment <- ""
 
  # load parameters from default value table, if not provided by user input  ---------------
  if (is.null(crop_product)) {
        crop_product <- Default_Crop %>%
          dplyr::filter(Crop == crop) %>%
          dplyr::select(crop_product) %>%
          as.numeric() # replace by input from table later
        comment <- paste(comment,"crop_product taken from table, ")
        }
  
  if (is.null(variable_harvest_index)) {
    variable_harvest_index <- Default_Crop %>%
      dplyr::filter(Crop == crop) %>%
      dplyr::select(variable_harvest_index) %>%
      as.logical()
    comment <- paste(comment,"variable_harvest_index taken from table,")
  } 
  
  
  if (is.null(harvest_index)) {
    if(!is.null(crop_product) & !is.null(crop_residue)){
        harvest_index <- crop_product / (crop_product+crop_residue)
        comment <- paste(comment,"harvest_index calculated from crop_product and crop_residue,")
      } else {
        harvest_index <- Default_Crop %>%
          dplyr::filter(Crop == crop) %>%
          dplyr::select(harvest_index) %>%
          as.numeric()
        comment <- paste(comment,"harvest_index taken from table,")
      }
  }
  
  # Overwrite Harvest index, if the variable harvest assumption of Fan et al. (2017) is to be applied
  
  if (variable_harvest_index == TRUE) {
    
    if(is.null(HI_intercept)){
      HI_intercept <- Default_Crop %>%
        dplyr::filter(Crop == crop) %>%
        dplyr::select(HI_intercept) %>%
        as.numeric()
      comment <- paste(comment,"HI_intercept taken from table,")
    }
    
    if(is.null(HI_slope)){
      HI_slope <- Default_Crop %>%
        dplyr::filter(Crop == crop) %>%
        dplyr::select(HI_slope) %>%
        as.numeric()
      comment <- paste(comment,"HI_slope taken from table,")
    }
    
    harvest_index <- HI_intercept + (HI_slope * crop_product)
    comment <- paste(comment,"variable harvest_index calculated,")
  }

  
  
  if (is.null(shoot_root_ratio)) {
        shoot_root_ratio <- Default_Crop %>%
          dplyr::filter(Crop == crop) %>%
          dplyr::select(shoot_root_ratio) %>%
          as.numeric()
        comment <- paste(comment,"shoot_root_ratio taken from table,")
        } 
  
  if (is.null(root_exudation_factor)) {
    root_exudation_factor <- Default_Crop %>%
      dplyr::filter(Crop == crop) %>%
      dplyr::select(root_exudation_factor) %>%
      as.numeric()
        comment <- paste(comment,"root_exudation_factor taken from table,")
  }
  
  if (is.null(fixed_belowground_input)) {
    fixed_belowground_input  <- Default_Crop %>%
      dplyr::filter(Crop == crop) %>%
      dplyr::select(fixed_belowground_input) %>%
      as.logical()
  }
  
  if (fixed_belowground_input == TRUE) {
    if (is.null(fixed_C_input_root)) {
      fixed_C_input_root <- Default_Crop %>%
        dplyr::filter(Crop == crop) %>%
        dplyr::select(C_input_root) %>%
        as.numeric()
    comment <- paste(comment,"fixed C_input_root taken from table,")
    }
  }
  
  
  # Calculate Carbon in product, straw, root and exudate [kgC/ha] ---------------
  C_input_product <- as.integer(crop_product * Cc_product)
  C_input_straw <- as.integer(crop_product * (1 - harvest_index)/harvest_index * Cc_residue)
  
  if (fixed_belowground_input == FALSE) {
    C_input_root <- as.integer(crop_product / (shoot_root_ratio * harvest_index) * Cc_root)
    C_input_exudate <- as.integer(C_input_root * root_exudation_factor)
  } else { 
    #C_input_root and C_input_exudates for fixed below ground C inputs
    C_input_root <- fixed_C_input_root
    C_input_exudate <- as.integer(C_input_root * root_exudation_factor)
    } 
  
  # Calculate Carbon transfered to the soil --------------
  
  S_product <- Default_Crop %>%
    dplyr::filter(Crop == crop) %>%
    dplyr::select(SP) %>%
    as.numeric()
  
  S_straw <- Default_Crop %>%
    dplyr::filter(Crop == crop) %>%
    dplyr::select(SS) %>%
    as.numeric()
 
  S_root <- Default_Crop %>%
    dplyr::filter(Crop == crop) %>%
    dplyr::select(SR) %>%
    as.numeric()
  
  S_exudate <- Default_Crop %>%
    dplyr::filter(Crop == crop) %>%
    dplyr::select(SE) %>%
    as.numeric()
  
  # Leave C input from straw for cereals where straw was be removed  --------------
  
  if (!is.null(straw.removal)) {
  if (straw.removal == FALSE) {
    S_straw <- 1}
  
  if (straw.removal == TRUE) {
    S_straw <- 0}
    }
  
  C_input_product <- C_input_product * S_product
  C_input_straw <- C_input_straw * S_straw
  C_input_root <- C_input_root * S_root
  C_input_exudate <- C_input_exudate * S_exudate
 
  
  # Define output_tibble  ---------------
  output_tibble <- dplyr::tibble(
    C_input_product = round(C_input_product,0),
    C_input_straw = round(C_input_straw,0),
    C_input_root = round(C_input_root,0),
    C_input_exudate = round(C_input_exudate,0),
    C_input_total = C_input_product + C_input_straw + C_input_root + C_input_exudate)
  
  if(return.comment == TRUE) {
    output_tibble <- cbind(output_tibble,comment)
  }
  
  # Return output_tibble  ---------------
  return (output_tibble)
  
}

