#' Extract indicators by a specific date
#' 
#' Calculate for a specific date (e.g. day of sampling) and discount the 
#' indicator values of events in the past. The default half life time of all 
#' discounts is set to 1 year (365 days).
#'
#' @param var_management_df input management data frame
#' @param var_date date of the extraction in the format "YYYY-MM-DD"
#' @param half_life_time_STIR half life time for the discounting of the STIR value
#' @param half_life_time_C_input half life time for the discounting of the C inputs
#' @param half_life_time_N_input half life time for the discounting of the N inputs
#' @param half_life_time_soil_cover half life time for the discounting of soil cover days
#'
#' @returns tibble with the discounted indicator values per date
#' @export
#'
#' @examples
#' # calculate indicators by a specific date
#' \donttest{
#'   indicators_by_date(EXAMPLE_data, "2019-10-03")
#'  }
#' # this would return a tibble, but it may take a while
#' 
indicators_by_date <- function(var_management_df, var_date, 
                               half_life_time_STIR = 365,
                               half_life_time_C_input = 365,
                               half_life_time_N_input = 365,
                               half_life_time_soil_cover = 365){
  
# check if date is within management data frame ---------------
  min_date <- min(var_management_df$date)
  max_date <- max(var_management_df$date)
  
  if(var_date < min_date | var_date > max_date){stop("Extraction date is outside the management time line")}

# STIR ------------
  # calculate STIR values
  STIR_table <- tillage_intensity(var_MGMT_data = var_management_df, extended.output = T)
  
  # calculate days since event and select relevant variables
  STIR_table <- STIR_table %>%
    dplyr::filter(!is.na(STIR)) %>%
    dplyr::filter(date <= as.Date(var_date)) %>%
    dplyr::mutate(days_since = as.integer(as.Date(var_date) - date), .before = crop) %>%
    dplyr::select(days_since,crop,date,year,category,operation,device,value,unit,machine,STIR)

  # discount STIR values
  STIR_table <- STIR_table %>% 
    dplyr::mutate(STIR_temp = round(STIR * 2^(-days_since/half_life_time_STIR)))
  
  # sum up values
  STIR <- sum(STIR_table$STIR_temp)

# C input -----------------
  
  # calculate C input values
  C_input_table <- C_input(var_MGMT_data = var_management_df, extended.output = T)
  
  # calculate days since event and select relevant variables
  C_input_table <- C_input_table %>%
    dplyr::filter(!is.na(C_input_org) |
                    !is.na(C_input_crop) |
                    !is.na(C_input_CC)) %>%
    dplyr::filter(date <= as.Date(var_date)) %>%
    dplyr::mutate(days_since = as.integer(as.Date(var_date) - date), .before = crop) %>%
    dplyr::select(days_since,crop,date,year,category,operation,device,C_input_org,C_input_crop,C_input_CC)
  
  # discount C_input values
  C_input_table <- C_input_table %>% 
    dplyr::mutate(C_input_org_temp = round(C_input_org * 2^(-days_since/half_life_time_C_input))) %>% 
    dplyr::mutate(C_input_crop_temp = round(C_input_crop * 2^(-days_since/half_life_time_C_input))) %>% 
    dplyr::mutate(C_input_CC_org_temp = round(C_input_CC * 2^(-days_since/half_life_time_C_input)))
  
  # sum up values
  C_input_org <- sum(C_input_table$C_input_org_temp,na.rm = T)
  C_input_crop <- sum(C_input_table$C_input_crop_temp, na.rm = T)
  C_input_CC <- sum(C_input_table$C_input_CC_org_temp, na.rm = T)
  C_input <- C_input_org + C_input_crop + C_input_CC
  

# N inputs ---------------
  
  # calculate C input values
  N_input_table <- N_input(var_MGMT_data = var_management_df, extended.output = T)
  
  # calculate days since event and select relevant variables
  N_input_table <- N_input_table %>%
    dplyr::filter(!is.na(N_input_org) |
                    !is.na(N_input_min)) %>%
    dplyr::filter(date <= as.Date(var_date)) %>%
    dplyr::mutate(days_since = as.integer(as.Date(var_date) - date), .before = crop) %>%
    dplyr::select(days_since,crop,date,year,category,operation,device,N_input_org,N_input_min)
  
  # discount C_input values
  N_input_table <- N_input_table %>% 
    dplyr::mutate(N_input_org_temp = round(N_input_org * 2^(-days_since/half_life_time_N_input))) %>% 
    dplyr::mutate(N_input_min_temp = round(N_input_min * 2^(-days_since/half_life_time_N_input)))
  
  # sum up values
  N_input_org <- sum(N_input_table$N_input_org_temp,na.rm = T)
  N_input_min <- sum(N_input_table$N_input_min_temp, na.rm = T)
  N_input <- N_input_org + N_input_min
  
# soil cover ----------------
  # calculate soil cover tables
  soil_cover_table <- calculate_soil_cover_tibble(var_MGMT_data = var_management_df)
  
  # calculate days since event and select relevant variables
  soil_cover_table <- soil_cover_table %>%
    dplyr::filter(date <= as.Date(var_date)) %>%
    dplyr::mutate(days_since = as.integer(as.Date(var_date) - date), .before = year)
  
  # discount soil cover values
  soil_cover_table <- soil_cover_table %>% 
    dplyr::mutate(soil_cover_days_temp = round(soil_cover_days * 2^(-days_since/half_life_time_soil_cover), digits = 2)) %>%
    dplyr::mutate(plant_cover_days_temp = round(plant_cover_days * 2^(-days_since/half_life_time_soil_cover), digits = 2)) %>%
    dplyr::mutate(residue_cover_days_temp = round(residue_cover_days * 2^(-days_since/half_life_time_soil_cover), digits = 2))
  
  # sum up values
  soil_cover_days <- round(sum(soil_cover_table$soil_cover_days_temp))
  plant_cover_days <- round(sum(soil_cover_table$plant_cover_days_temp))
  residue_cover_days <- round(sum(soil_cover_table$residue_cover_days_temp))
  
# combine into output table ----------------
return(
  dplyr::tibble(STIR,
         C_input,C_input_org,C_input_crop,C_input_CC,
         N_input,N_input_org,N_input_min,
         soil_cover_days,plant_cover_days,residue_cover_days)
  )
  
 }
