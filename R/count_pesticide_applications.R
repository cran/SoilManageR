
#' Count number of pesticide applications
#' 
#' Counts the number of pesticide applications in the management dataframe. 
#' Besides, seperate counts of herbicide, fungicide and 
#' insecticide applications are provided.
#' 
#'
#' @param var_management_df management dataframe
#'
#' @returns list with two tibbles: yearly and cropwise counts of pesticide applications
#' @export
#'
#' @examples
#' # count pesticide applications in the EXAMPLE_data
#' count_pesticide_applications(EXAMPLE_data)
#' 
count_pesticide_applications <- function(var_management_df){
  
  # identify main crops -------
  var_management_df <- identify_main_crops(var_management_df)
  
  crops <- var_management_df %>%
    dplyr::select(crop_no,crop,year,date) %>%
    dplyr::filter(!is.na(crop_no)) %>%
    dplyr::group_by(crop_no) %>%
    dplyr::distinct() %>%
    dplyr::summarise(crop_no = dplyr::last(crop_no),
                     crop = dplyr::last(crop),
                     year = dplyr::last(year),
                     harvest = dplyr::last(date))
  
  # filter years -------
  years <- var_management_df %>%
    dplyr::select(year) %>%
    dplyr::distinct()
  
 
  
  
  
  # filter relvant events -------
    # filter for crop protention events and harvests
    CP_MGMT <- var_management_df %>% 
      dplyr::filter(category == "crop_protection")
    
    
    # filter for fungicide
    fungicide_MGMT <- CP_MGMT %>% 
      dplyr::filter(operation == "fungicide")
    
    # filter for insecticide
    insecticide_MGMT <- CP_MGMT %>% 
      dplyr::filter(operation == "insecticide")
    
    # filter for herbicide
    herbicide_MGMT <- CP_MGMT %>% 
      dplyr::filter(operation %in% 
      c("weed_herbicide",
        "total_herbicide"))
    
    # filter for weeding (other than herbicide)
    weeding_MGMT <- CP_MGMT %>% 
      dplyr::filter(operation %in% 
                      c("weed_mechanical",
                        "weed_other"))
    
    # filter for growth regulators
    growth_regulator_MGMT <- CP_MGMT %>% 
      dplyr::filter(operation == "growth_regulator")
    
    # filter for biocontrol
    biocontrol_MGMT <- CP_MGMT %>% 
      dplyr::filter(operation == "biocontrol")
    
    # filter for pest control
    pest_control_MGMT <- CP_MGMT %>% 
      dplyr::filter(operation == "pest_control")
    
  # summarise by year ----------------
    
    # summerise fungicide
    fungicide_year <- dplyr::summarise(fungicide_MGMT, .by = year, 
                     fungicide = dplyr::n())
    
    # summerise insecticide
    insecticide_year <- dplyr::summarise(insecticide_MGMT, .by = year, 
                                       insecticide = dplyr::n())
    
    # summerise herbicide
    herbicide_year <- dplyr::summarise(herbicide_MGMT, .by = year, 
                                         herbicide = dplyr::n())
    
    # join data
    pesticide_year <- dplyr::left_join(years,fungicide_year, by = dplyr::join_by(year)) %>%
      dplyr::left_join(insecticide_year, by = dplyr::join_by(year)) %>%
      dplyr::left_join(herbicide_year, by = dplyr::join_by(year)) %>%
      tidyr::replace_na(list(fungicide = 0,
                             insecticide = 0,
                             herbicide = 0)) %>%
      dplyr::mutate(pesticide = fungicide +insecticide+herbicide)
    
    # summarise by crop ----------------
    
    # summerise fungicide
    fungicide_crop <- dplyr::summarise(fungicide_MGMT, .by = crop_no, 
                                       fungicide = dplyr::n())
    
    # summerise insecticide
    insecticide_crop <- dplyr::summarise(insecticide_MGMT, .by = crop_no, 
                                         insecticide = dplyr::n())
    
    # summerise herbicide
    herbicide_crop <- dplyr::summarise(herbicide_MGMT, .by = crop_no, 
                                       herbicide = dplyr::n())
    
    # join data
    pesticide_crop <- dplyr::left_join(crops,fungicide_crop, by = dplyr::join_by(crop_no)) %>%
      dplyr::left_join(insecticide_crop, by = dplyr::join_by(crop_no)) %>%
      dplyr::left_join(herbicide_crop, by = dplyr::join_by(crop_no)) %>%
      tidyr::replace_na(list(fungicide = 0,
                             insecticide = 0,
                             herbicide = 0)) %>%
      dplyr::mutate(pesticide = fungicide +insecticide+herbicide)
    
    #return
    return(list(yearly = pesticide_year,cropwise = pesticide_crop))
}
