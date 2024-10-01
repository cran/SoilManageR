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
#' * [plant_cover()] for more detail on the plant cover function
#' * [plot.soil_cover_tibble()] for plotting the `soil_cover_tibble`
#' * `STIR_values_LUT` for tillage operation specific burial coefficients
#'    
#' @param var_MGMT_data a `management_df` that contains the management information
#' @param extended.output an optional logical value. 
#'  * If FALSE, soil cover days are aggregated by year. 
#'  * If TRUE, a soil_cover_tibble with daily resolution is returned. 
#'  * Defaul value is FALSE
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
  

  # create a table with all days --------------
  start_date <- as.Date(paste(as.character(lubridate::year(min(var_MGMT_data$date))),"-01-01",sep=""))
  end_data <- as.Date(paste(as.character(lubridate::year(max(var_MGMT_data$date))),"-12-31",sep=""))
  all_days <- dplyr::tibble(date = seq(start_date,end_data,by="1 day"))
  var_MGMT_data_cover <- dplyr::full_join(var_MGMT_data,all_days, by = dplyr::join_by(date)) %>%
    arrange_management_df() %>% #rearrange by date, category and s
    dplyr::mutate(year = lubridate::year(date))
  
  var_MGMT_data_cover <- var_MGMT_data_cover %>%
    dplyr::mutate(crop_duration = 0,
           cover_crop_duration = 0,
           crop_cover = 0,
           cover_crop_cover = 0,
           plant_cover = 0,
           residue_mass = 0,
           residue_cover = 0,
           soil_cover = 0)
  
  # loop to calculate soil cover by plants per day ----------------
  for (l in 1:nrow(var_MGMT_data_cover)) {
    
    if (l == 1) {next} #jump first line
    
    if (is.na(var_MGMT_data_cover$crop[l])) {
      var_MGMT_data_cover$crop[l] <- var_MGMT_data_cover$crop[l-1] # copy crop from the line above, if empty
    }
    
    # calculate crop duration --------------
    
      # No crop - duration is 0 when it was 0 in the line above
      if (isTRUE(var_MGMT_data_cover$crop_duration[l-1] == 0)) {
        var_MGMT_data_cover$crop_duration[l] <- 0
      }
    
      
      # Calcualtions if the line above hat crop duration >= 1
      if (isTRUE(var_MGMT_data_cover$crop_duration[l-1] >= 1)) {
        
        if(var_MGMT_data_cover$date[l-1] != var_MGMT_data_cover$date[l]) {
          var_MGMT_data_cover$crop_duration[l] <- var_MGMT_data_cover$crop_duration[l-1] + 1
        } else {var_MGMT_data_cover$crop_duration[l] <- var_MGMT_data_cover$crop_duration[l-1]} #only increase the duration if it is a new day
      }
    
      # termination of crop by harvest (except temporary ley)
      if (isTRUE(var_MGMT_data_cover$operation[l] == "harvest_main_crop") & isTRUE(var_MGMT_data_cover$crop[l] != "ley, temporary"))  {
        var_MGMT_data_cover$crop_duration[l] <- 0
      }
      
      # termination of crop by tillage (except for potato bedder and roller)
      if (isTRUE(var_MGMT_data_cover$category[l] == "tillage") &
          isTRUE(var_MGMT_data_cover$device[l] != "bedder") &
          isTRUE(var_MGMT_data_cover$device[l] != "roller"))  {
        var_MGMT_data_cover$crop_duration[l] <- 0
      }
      
      # termination of temporary ley by herbizide application
      if (isTRUE(isTRUE(var_MGMT_data_cover$operation[l] == "weed_herbicide")) & isTRUE(var_MGMT_data_cover$crop[l-1] == "ley, temporary"))  {
        var_MGMT_data_cover$crop_duration[l] <- 0
      }
      
      # duration is 1 if the line above was a sowing event
      if (isTRUE(var_MGMT_data_cover$operation[l-1] == "sowing_main_crop")) {
        var_MGMT_data_cover$crop_duration[l] <- 1
      }
      
    
    # calculate cover crop duration -------------
      # No cover crop
        if (isTRUE(var_MGMT_data_cover$cover_crop_duration[l-1] == 0)) {
          var_MGMT_data_cover$cover_crop_duration[l] <- 0
        }
      
      # increase duration if there is a cover crop and a new day
        if (isTRUE(var_MGMT_data_cover$cover_crop_duration[l-1] >= 1)) {
          if(var_MGMT_data_cover$date[l-1] != var_MGMT_data_cover$date[l]) {
            var_MGMT_data_cover$cover_crop_duration[l] <- var_MGMT_data_cover$cover_crop_duration[l-1] + 1
          } else {var_MGMT_data_cover$cover_crop_duration[l] <- var_MGMT_data_cover$cover_crop_duration[l-1]} #only increase the duration if it is a new day
        }
      
      #termination of cover crops by tillage (except roller) or all herbizide application
        if ((isTRUE(var_MGMT_data_cover$category[l] == "tillage") | 
            isTRUE(var_MGMT_data_cover$operation[l] == "weed_herbicide")) &
            isTRUE(var_MGMT_data_cover$device[l] != "roller"))  {
          var_MGMT_data_cover$cover_crop_duration[l] <- 0
        }
      
      # set duration is 1 if the line above was a sowing event
        if (isTRUE(var_MGMT_data_cover$operation[l-1] == "sowing_cover_crop")) {
          var_MGMT_data_cover$cover_crop_duration[l] <- 1
        }
        
    # calculate plant cover --------------
      if (isTRUE(var_MGMT_data_cover$crop_duration[l] >= 1)) {
        var_MGMT_data_cover$crop_cover[l] <- plant_cover(var_MGMT_data_cover$crop[l],var_MGMT_data_cover$crop_duration[l])
        
      }
      
      if (isTRUE(var_MGMT_data_cover$cover_crop_duration[l] >= 1)) {
        var_MGMT_data_cover$cover_crop_cover[l] <- plant_cover("cover crop",var_MGMT_data_cover$cover_crop_duration[l])
        
      }
      
      var_MGMT_data_cover$plant_cover[l] <- min(var_MGMT_data_cover$cover_crop_cover[l] + var_MGMT_data_cover$crop_cover[l], 100)
    
  }
  
  # loop to calculate soil cover by residues per day ----------------
  for (l in 1:nrow(var_MGMT_data_cover)) {
    # jump first line
    if (l == 1) {next}
    
    # Calcualte residue mass
    # Add residues by main crop
    if (isTRUE(var_MGMT_data_cover$operation[l] == "harvest_main_crop")) {
      additional_residue_C <- C_input_crops(var_MGMT_data_cover$crop[l],
                                          crop_product = var_MGMT_data_cover$crop_product[l],
                                          straw.removal = FALSE)$C_input_straw
      CC_residues <- 450 # Assumption that all biomass has a C content of 450 mgC/gDM
      additional_residue <- additional_residue_C * 1000/CC_residues
      rm(additional_residue_C,CC_residues)
    } else {additional_residue <- 0}
    
    # export residues by straw removal
    if (isTRUE(var_MGMT_data_cover$operation[l] == "straw_removal")) {
      total_straw_C <- C_input_crops(var_MGMT_data_cover$crop[l],
                                            crop_product = var_MGMT_data_cover$crop_product[l],
                                            straw.removal = FALSE)$C_input_straw
      left_after_removal_C <- C_input_crops(var_MGMT_data_cover$crop[l],
                                            crop_product = var_MGMT_data_cover$crop_product[l],
                                            straw.removal = NA)$C_input_straw
      removed_C <- total_straw_C - left_after_removal_C
      CC_residues <- 450 # Assumption that all biomass has a C content of 450 mgC/gDM
      removed_residue <- removed_C * 1000/CC_residues
      additional_residue <- additional_residue - removed_residue
      rm(total_straw_C,left_after_removal_C,removed_C,CC_residues,removed_residue)
    }
    
    # Add residues by cover crop at termination
    if (isTRUE(var_MGMT_data_cover$cover_crop_duration[l] == 0 &
               var_MGMT_data_cover$cover_crop_duration[l-1] > 0)) {
      Cover_crop_residue_C <- C_input_cover_crops(days = var_MGMT_data_cover$cover_crop_duration[l-1])
      Cover_crop_residue_C <- Cover_crop_residue_C$C_input_product + Cover_crop_residue_C$C_input_straw 
      CC_cover_crops <- 450 # Assumption that all biomass has a C content of 450 mgC/gDM
      Cover_crop_residue <- Cover_crop_residue_C * 1000/CC_cover_crops
      additional_residue <- additional_residue + Cover_crop_residue
      rm(Cover_crop_residue_C,CC_cover_crops,Cover_crop_residue)
    }
    
    # Add residues by termination of temporary ley
    if (isTRUE(var_MGMT_data_cover$crop_duration[l] == 0 &
               var_MGMT_data_cover$crop_duration[l-1] > 0 &
               var_MGMT_data_cover$crop[l-1] == "ley, temporary")) {
      ley_residue <- 1500 # assumption, leads to a initial soil cover of 93%
      additional_residue <- additional_residue + ley_residue
      rm(ley_residue)
    }
    
    # Calculate residue mass, before tillage operation and decay
    var_MGMT_data_cover$residue_mass[l] = var_MGMT_data_cover$residue_mass[l-1] + round(additional_residue)
    rm(additional_residue)
    
    # remove residue by tillage operations
    if (isTRUE(var_MGMT_data_cover$device[l] %in% STIR_values_LUT$Operation)) {
      Burial_Coefficient <- STIR_values_LUT %>% 
        dplyr::filter(Operation == var_MGMT_data_cover$device[l]) %>%
        dplyr::select(Burial_Coefficient) %>%
        as.numeric()
      
      var_MGMT_data_cover$residue_mass[l] = 
        var_MGMT_data_cover$residue_mass[l] -
        var_MGMT_data_cover$residue_mass[l] * Burial_Coefficient
      
      rm(Burial_Coefficient)
    }
    
    # calculate residue mass after decay
    if(var_MGMT_data_cover$date[l-1] != var_MGMT_data_cover$date[l]) {
      decay_rate <- 0.028 #Value for winter wheat from Steiner et al. 1999 (see Büchi et al. 2016)
      var_MGMT_data_cover$residue_mass[l] = var_MGMT_data_cover$residue_mass[l] * (1 - decay_rate)
      rm(decay_rate)
    }
    
    # set negative residue mass to 0 and round to 0 digits
    if (isTRUE(var_MGMT_data_cover$residue_mass[l] < 0)) {
      var_MGMT_data_cover$residue_mass[l] <- 0
    }
    
    var_MGMT_data_cover$residue_mass[l] <- round(var_MGMT_data_cover$residue_mass[l])
    
    # Calculate soil cover based on residue mass
    # based on the formula by Steiner et al. (2000) (see Büchi et al. 2016)
    k_t <- 0.0175 
    M <- var_MGMT_data_cover$residue_mass[l] * 0.1 #Convert kg/ha to g/m2
    residue_cover <- round((1-exp(-k_t*M))*100)
    
    var_MGMT_data_cover$residue_cover[l] <- residue_cover
    
    
    
  }
  
  
  # summarize soil cover variables  ----------------
  var_MGMT_data_cover <- var_MGMT_data_cover %>%
    dplyr::rowwise() %>%
    dplyr::mutate(soil_cover = min(plant_cover + residue_cover, 100))
  
  #calculate only one value per day (minium of multipe values)
  var_MGMT_data_cover <- var_MGMT_data_cover %>%
    dplyr::group_by(year,date) %>%
    dplyr::summarize(soil_cover = min(soil_cover, na.rm=TRUE),
              plant_cover = min(plant_cover,na.rm=TRUE),
              residue_cover = min(residue_cover, na.rm=TRUE),
              .groups = "keep") %>%
    dplyr::mutate(soil_cover_days = dplyr::case_when(soil_cover >= 30 ~ 1,
                                         TRUE ~ 0),
           plant_cover_days = dplyr::case_when(plant_cover >= 30 ~ 1,
                                              TRUE ~ 0),
           residue_cover_days = dplyr::case_when(residue_cover >= 30 ~ 1,
                                              TRUE ~ 0))
  
  soil_cover_year <- var_MGMT_data_cover %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(soil_cover_days = sum(soil_cover_days, na.rm = TRUE),
              plant_cover_days = sum(plant_cover_days, na.rm = TRUE),
              residue_cover_days = sum(residue_cover_days, na.rm = TRUE))
  
  
  # create output tibble ----------------
  
  if (extended.output) {
    output.tibble <- var_MGMT_data_cover
    class(output.tibble) <-  c("soil_cover_tibble",class(output.tibble))
  } else {
      output.tibble <- soil_cover_year
  }
  
  return(output.tibble)
}

