#' Estimate C and N inputs of organic amendments
#' 
#' Estimates the carbon (C) and total nitrogen (N) input into 
#' the soil system by organic amendments.
#' 
#' @details
#' The C and N inputs by organic amendments is calculated based on the 
#'  dry matter content (DMC) and the C and N content of the dry matter 
#'  of the amendment. If the contents are not specified, 
#'  default values from the Swiss fertilizer recommendations 
#'  \insertCite{GRUD2017_ch8}{SoilManageR} are assumed. 
#'  The default values are available the look-up-table `CN_input_amendments_LUT`
#'  For all slurries and liquid amendments (DMC < 150 g/kg), a dilution of 50% is 
#'  assumed if default DMC values from the `CN_input_amendments_LUT` are used.
#'  
#' @md   
#' @references
#'  \insertAllCited{}
#' 
#' @seealso 
#' * [C_input()] to calculate C inputs for a management_df
#' * [N_input()] to calculate N inputs for a management_df
#' * [C_input_crops()] to calculate C inputs by crops
#' * [C_input_cover_crops()] to calculate C input for cover crops
#' * [CN_input_amendments_LUT()] for the look-up-table for organic amendments reference values
#'    
#' @param amount Amount of organic amendment in fresh weight (t/ha)
#' @param amd_type Type of organic manure, there are default values available for
#'  "Slurry_dairy_cow", "Manure_dairy_cow", "Slurry_cattle", "Manure_cattle", "Slurry_pig",
#'  "Manure_pig", "Compost", "Biorga_Quick_12N" and others (see `CN_input_amendments_LUT`)
#' @param DMC dry matter content of the organic amendment (gDM/kgFM)
#' @param C_content C content of the dry matter (gC/kgDM)
#' @param N_content N content of the dry matter (gN/kgDM)
#' @param return.comment (optional): logical value if comments are returned or not. Default = FALSE
#' @param concentration_liquids concentration factor for liquid amendments. Default value is .5. 
#'
#' @return a tibble with the following parameters:
#' * C_input_org: C input by organic ammendment (kgC/ha)
#' * N_input_org: N input by organic ammendment (kgN/ha)
#' * comment (optional): Source of information on properties of organic amendment
#' 
#' @examples
#' #example where amount, dry matter content, C and N content are known.
#' CN_input_amendments(40, DMC = 300, C_content = 300, N_content = 30)
#' 
#' #example where only amount and type of amendment are known 
#' CN_input_amendments(20, "Manure_pig")
#' 
#' #example of a diluted slurry
#' CN_input_amendments(20, amd_type = "Slurry_dairy_cow", DMC = 50)
#' 
#' 
#' @export
#' 
CN_input_amendments <- function(amount,amd_type = NA, DMC = NA, C_content = NA, N_content = NA, return.comment = FALSE, concentration_liquids = 0.5 ) {
  
  #set NA inputs to represent NULL --------------
  if (is.na(amd_type)) {amd_type <- NULL}
  if (is.na(DMC)) {DMC <- NULL}
  if (is.na(C_content)) {C_content <- NULL}
  if (is.na(N_content)) {N_content <- NULL}
  
  # load default values from supporting files --------------
  #Org_Am_data_path <- "./inst/extdata/CN_input_amendments_LUT.xlsx"
  #Default_Org_Am <- readxl::read_excel(Org_Am_data_path)
  Default_Org_Am <- CN_input_amendments_LUT
  
  
  #extract comment from default value table --------------
  if (!is.null(amd_type) & (is.null(DMC)|is.null(C_content)|is.null(N_content))) {
    comment <- Default_Org_Am %>% 
      dplyr::filter(Default_Org_Am$Amendment == amd_type) %>%
      dplyr::select(Comment) %>% as.character()
    
  } else {
    comment <- "All properties of orgamic amendments from function input"
  }
  
  #change comment
  if(is.null(DMC)) {comment <- paste(comment,"DMC from default values, ")}
  if(is.null(C_content)) {comment <- paste(comment,"C_content from default values, ")}
  if(is.null(N_content)) {comment <- paste(comment,"N_content from default values, ")}
  
  #Check if type is in default amendment --------------
  if (is.null(amd_type)) {
    if (is.null(DMC)) {
      stop("DMC of organic amendment is missing. Supply DMC = or amd_type = ")
    }
    if (is.null(C_content)) {
      stop("C_content of organic amendment is missing. Supply C_content = or amd_type = ")
    }
    if (is.null(N_content)) {
      stop("N_content of organic amendment is missing. Supply N_content = or amd_type = ")
    }
  }
   
  if (!is.null(amd_type)) {
    if(is.null(DMC) | is.null(C_content) | is.null(N_content)) {
      if(!(amd_type %in% Default_Org_Am$Amendment)){
        stop("Type of organic amendment not included in the table with default values. See CN_input_amendments_LUT for available amendments.")
      }
    }
  }
  
  #define DMC from default value table --------------
  if (is.null(DMC)) {
    DMC <- Default_Org_Am %>% 
      dplyr::filter(Default_Org_Am$Amendment == amd_type) %>%
      dplyr::select(DMC) %>% as.numeric()
    
    # Check if ammendment is liquid, and dilute with concentration factor
    if(DMC < 150){
      DMC <- DMC * concentration_liquids
    }
    
  }
  
  #define C_content from default value table --------------
  if (is.null(C_content)) {
    C_content <- Default_Org_Am %>% 
      dplyr::filter(Default_Org_Am$Amendment == amd_type) %>%
      dplyr::select(C_content) %>% as.numeric()
    
  }
  
  #define N_content from default value table --------------
  if (is.null(N_content)) {
    N_content <- Default_Org_Am %>% 
      dplyr::filter(Default_Org_Am$Amendment == amd_type) %>%
      dplyr::select(N_content) %>% as.numeric()
    
  }
  

  #calculate Inputs --------------
  C_input_org <- round(amount * DMC/1000 * C_content/1000 * 1000,1)
  N_input_org <- round(amount * DMC/1000 * N_content/1000 * 1000,1)
  
  
  #define output_tibble
  output_tibble <- tibble::tibble(C_input_org, N_input_org)
  if (return.comment == TRUE) {
    output_tibble <- tibble::tibble(output_tibble,comment = comment)
  }
  
  #return output tibble --------------
  return(output_tibble)
  
}
