## This file documents the data sets that are exported with the SoilManageR package

#' Look-up-table with default values to calculate carbon (C) inputs by crops
#'
#' The data set is a look-up-table that is used to calculate the C inputs by
#'  crops with the Bolinder formula, that is implemented in the function 
#'  `SoilManageR::C_input_crops()`. The data set is produced from the excel table 
#'  `C_input_crops_LUT.xlsx` file under `/inst/extdata/`.
#'
#' @format A tibble with 28 rows and 19 columns:
#' \describe{
#'   \item{Crop}{Name of the crop}
#'   \item{RP}{Ratio of the C in the product to the total carbon that is allocated by the plant (in a year)}
#'   \item{RS}{Ratio of the C in the above ground residues (e.g. straw) to the total carbon that is allocated by the plant (in a year)}
#'   \item{RR}{Ratio of the C in the plant roots to the total carbon that is allocated by the plant (in a year)}
#'   \item{RE}{Ratio of the C in the root exudates to the total carbon that is allocated by the plant (in a year)}
#'   \item{SP}{Proportion of the C in the Product that is transfered to the soil}
#'   \item{SS}{Proportion of the C in the above ground residues (e.g. straw) that is transfered to the soil}
#'   \item{SR}{Proportion of the C in the roots that is transfered to the soil}
#'   \item{SE}{Proportion of the C in root exudates that is transfered to the soil}
#'   \item{crop_product}{Reference yield, derived from the Swiss fertilizer recommendations (GRUD, 2017, Chapters 8 and 9) [tDM/ha]}
#'   \item{harvest_index}{Ratio of the product to the total of the product and the above ground residues. Calculated by RP/(RP+RS) (assuming all biomass has 45\% C)}
#'   \item{varible_harvest_index}{Logical value, if the variable harvest index assumption of Fan et al. (2017) are to be aplied or not.}
#'   \item{HI_intercept}{Intercept of the variable harvest index assumption of Fan et al. (2017) are to be aplied.}
#'   \item{HI_slope}{Slope of the variable harvest index assumption of Fan et al. (2017) are to be aplied. [ha/tDM]}
#'   \item{shoot_root_ratio}{Ratio of the product and the above ground residues to the root biomass. Calculated by (RP+RS)/RR (assuming all biomass has 45\% C)}
#'   \item{root_exudation_factor}{Ratio of the root exudates to the root biomass. Calculated by RE/RR (assuming all biomass has 45\% C)}
#'   \item{fixed_belowground_input}{Logical value if the fixed below ground C allocation assumption of Taghizadeh-Toosi et al. (2020) is to be applied or not.}
#'   \item{C_input_root}{Fixed value of root carbon input that is to be assumed. [kgC/ha]}
#'   \item{Source}{Source where the information was derived.}
#' }
#' @references 
#' Compilation of values from the SoilX project. Please check the 
#'  `C_input_crops_LUT.xlsx` file under `/inst/extdata/` for more information.
#'  
#' \insertRef{bolinder2007}{SoilManageR}
#' 
#' \insertRef{bolinder2015}{SoilManageR}
#' 
#' \insertRef{fan2017}{SoilManageR}
#' 
#' \insertRef{hirte2018}{SoilManageR}
#' 
#' \insertRef{keel2017}{SoilManageR}
#' 
#' \insertRef{seitz2022}{SoilManageR}
#' 
#' \insertRef{GRUD2017_ch8}{SoilManageR}
#' 
#' \insertRef{taghizadeh-toosi2020}{SoilManageR}
#'
#' \insertRef{wuest2020}{SoilManageR}


#' 
"C_input_crops_LUT"


#' Look-up-table with default values to calculate C and N inputs by organic amendments
#'
#' The dataset is a look-up-table that is used to calculate the carbon (C) and
#'  nitrogen (N) inputs by organic amendments with the function 
#'  `SoilManageR::CN_input_amendments()`. The data set is produced from the excel
#'  table `CN_input_amendments_LUT.xlsx` file under `/inst/extdata/`.
#'
#' @format A tibble with 27 rows and 7 columns:
#' \describe{
#'   \item{Amendment}{Name of the amendment}
#'   \item{DMC}{Dry matter content of the amendment [gDM/kgFM]}
#'   \item{OM}{Organic matter content of the amendment, relative to its fresh weight [gDM/kgFM]}
#'   \item{C_content}{Carbon content of the amendment, relative to its dry matter [gC/kgDM]}
#'   \item{N_tot}{Total N conent of the amendment, relative to it's dry fresh weight [gN/kgFM]}
#'   \item{N_content}{Nitrogen content of the amendment, relative to its dry matter [gN/kgDM]}
#'   \item{Comment}{Comment, e.g. source of the information. These lines are shown as part of the function output}
#' }
#' @references
#' Compilation of Values from the SoilX project.
#'  Please check the `CN_input_amendments_LUT.xlsx` file under `/inst/extdata/` 
#'  for more information
#'  
#' \insertRef{GRUD2017_ch8}{SoilManageR}
#' 
#' \insertRef{GRUD2017_ch4}{SoilManageR}
#' 
#'  
"CN_input_amendments_LUT"

#' Look-up-table with default values to estimate soil cover by plants
#'
#' The dataset is a look-up-table that is used to estimate the soil cover
#'  percentage by plant with the function `SoilManageR::plant_cover()`. 
#'  The data set is produced from the excel table `plant_cover_LUT.xlsx`
#'  file under `/inst/extdata/`.
#'
#' @format A tibble with 28 rows and 7 columns:
#' \describe{
#'   \item{Crop}{Name of the crop}
#'   \item{Slope_0_10}{Increase of crop cover per day between 0 and 10\% soil cover	[\%/day]}
#'   \item{Slope_10_50}{Increase of crop cover per day between 10 and 50\% soil cover	[\%/day]}
#'   \item{Slope_50_75}{Increase of crop cover per day between 50 and 75\% soil cover	[\%/day]}
#'   \item{Slope_75_100}{Increase of crop cover per day between 75 and 100\% soil cover	[\%/day]}
#'   \item{days_30}{Number of days it takes to reach 30\% soil cover	[day]}
#'   \item{Comments}{Source where the information was derived}
#' }
#' @references 
#' \insertRef{mosimann2006}{SoilManageR}
"plant_cover_LUT"

#' Look-up-table with default values for tillage operations
#'
#' The dataset is a look-up-table that is used to derive STIR values with the 
#'  function `SoilManageR::STIR()` and residue incorperation by
#'  tillage operations with the function `SoilManageR::soil_cover()`.
#'  The data set is produced from the excel table `STIR_value_LUT.xlsx` file 
#'  under `/inst/extdata/` and was mostly derived from the official 
#'  RUSLE2 database \insertCite{RUSLE2}{SoilManageR}.
#'  
#' @format A tibble with 50 rows and 15 columns:
#' \describe{
#'   \item{Operation}{Name of the operation}
#'   \item{Speed}{Average speed of the operation	[km/h]}
#'   \item{Speed_MIN}{Min speed of the operation	[km/h]}
#'   \item{Speed_Max}{Max speed of the operation	[km/h]}
#'   \item{Surf_Disturbance}{Share of the disturbed soil surface	[\%]}
#'   \item{Depth}{Average depth of the operation	[cm]}
#'   \item{Depth_MIN}{Min depth of the operation	[cm]}
#'   \item{Depth_MAX}{Max depth of the operation	[cm]}
#'   \item{TILLAGE_TYPE}{Type of tillage operation}
#'   \item{TILLAGE_TYPE_Modifier}{Numerical value of the tillage type modifier	[0-1]}
#'   \item{STIR}{Soil tillage intensity rating value, based on default values}
#'   \item{Diesel_use}{iesel use of the operation per area	[l/ha]}
#'   \item{Burial_Coefficient}{Burial of plant residues on the soil surface	[0-1]}
#'   \item{Source}{Source of the values in the table}
#'   \item{Description}{Description of the operation}
#' }
#' 
#' @references 
#' \insertAllCited{}
"STIR_values_LUT"

#' Example of a management_df
#'
#' The dataset is derived from a Swiss long term agricultural field experiment.
#'  It is intended for demonstration purposes only
#'  
#' @format A tibble with 130 rows and 13 columns:
#' \describe{
#'   \item{crop}{Name of the main crop. Cover crop related operations are linked to the next main crop in the rotation [String from list]}
#'   \item{date}{Date of the management operation [Date]}
#'   \item{year}{Year of the management operation [Integer]}
#'   \item{category}{Categorization of the managment operation [1 level] [String from list]}
#'   \item{operation}{Categorization of the managment operation [2 level] [String from list]}
#'   \item{device}{Categorization of the managment operation [3 level] [String from list]}
#'   \item{value}{Numerical value linked to managment operation (e.g., depth of tillage operation, mass of organic amendment) [Integer]}
#'   \item{unit}{Unit of the numerical value (e.g. cm, t/ha) [String from list]}
#'   \item{machine}{Further information on the machine used (e.g., type, manufacturer, tool) [String [UTF-8]]}
#'   \item{product}{Further information on the applied product (e.g., name, manufacturer, C content) [String [UTF-8]]}
#'   \item{combination}{Indicate if a operation was done in combination with others.
#'     Use consequtive integer numbers if combinded operations occur.
#'     Leave empty if not combined}
#'   \item{comments}{Comments related to the management operation [String [UTF-8]]}
#'   \item{DMC}{Dry matter content of organic amendments [gDM/kgFM]}
#'   \item{C_content}{Carbon content of the amendments, relative to its dry matter [gC/kgDM]}
#'   \item{N_content}{Nitrogen content of organic amendments, relative to its dry matter [gN/kgDM]}
#'   \item{crop_product}{Crop product yield [tDM/ha]}
#'   \item{crop_residue}{Crop residue mass [tDM/ha]}
#'   \item{Cc_product}{Carbon content of the crop product [gC/kgDM$]}
#'   \item{Cc_residue}{Carbon content of the crop residue [gC/kgDM]}
#' }
#' 
"EXAMPLE_data"
