#' Calculate soil management indicators by crop
#' 
#' Alternative to `calculate_indicators()` that C inputs, N inputs, STIR and
#' soil cover for each crop separately.
#' The function identifies the last harvest of each crop per year and aggregates
#' the management intensities for the periods in between. Harvests of temporary 
#' leys that are sown after a main crop and harvest in the same year are not considered.
#'
#' @param var_management_df a `management_df()`
#'
#' @return a tibble with the indicator values per crop
#' @export 
#'
#' @examples
#' # calculate examples
#' \donttest{
#'   calculate_indicators_by_crop(EXAMPLE_data)
#' }
#' # this would return a tibble, but it can take a while
#' 
calculate_indicators_by_crop <- function(var_management_df){
  
  # Calculate Indicator tibbles ----------
  var_management_df_C <- calculate_C_input_tibble(var_management_df)
  var_management_df_N <- calculate_N_input_tibble(var_management_df)
  var_management_df_STIR <- calculate_STIR_tibble(var_management_df)
  var_management_df_soil_cover <- calculate_soil_cover_tibble(var_management_df)
  
  # join C, N and STIR tibbles -------------
  suppressMessages(
    var_management_df <- var_management_df %>% dplyr::ungroup() %>%
      dplyr::left_join(var_management_df_C %>% dplyr::select(-N_input_org)) %>%
      dplyr::left_join(var_management_df_N) %>% 
      dplyr::left_join(var_management_df_STIR)
  )
  
  # identify last harvest events -------------
  var_management_df <- identify_main_crops(var_management_df)
  
  last_harvest_events <- var_management_df %>% dplyr::ungroup() %>%
    dplyr::select(crop_no,crop,year,date) %>%
    dplyr::filter(!is.na(crop_no)) %>%
    dplyr::group_by(crop_no) %>%
    dplyr::distinct() %>%
    dplyr::summarise(crop_no = dplyr::last(crop_no),
                     crop = dplyr::last(crop),
                     year = dplyr::last(year),
                     harvest = dplyr::last(date))
  
  suppressMessages(
    var_management_df_soil_cover <- dplyr::left_join(var_management_df_soil_cover, 
                                                     last_harvest_events %>%
                                                       dplyr::select(harvest,crop,crop_no),
                                                     dplyr::join_by(date == harvest)) %>%
      dplyr::relocate(crop_no,crop, .after = "date") %>% 
      dplyr::ungroup() %>% 
      tidyr::fill(crop_no,.direction = "up") %>%
      tidyr::fill(crop,.direction = "up")
  )
  
  
  
  # append class to the management data -------------
  class(var_management_df) <- append("management_df",class(var_management_df))
  
  
  
  
  # summarize -------------
  summary <- var_management_df %>% dplyr::ungroup() %>%
    dplyr::group_by(crop_no) %>%
    dplyr::summarize(year = dplyr::last(year),
              crop = dplyr::last(crop),
              harvest = dplyr::last(date),
              crop_product = dplyr::last(crop_product),
              relative_yield = relative_yield(crop,crop_product),
              C_input = sum(C_input_org,C_input_crop,C_input_CC, na.rm = T),
              C_input_org = sum(C_input_org, na.rm = T),
              C_input_crop = sum(C_input_crop, na.rm = T),
              C_input_CC = sum(C_input_CC, na.rm = T),
              N_input_org = sum(N_input_org, na.rm = T),
              N_input_min = sum(N_input_min, na.rm = T),
              N_input = sum(N_input, na.rm = T),
              LSU = sum(LSU, na.rm = T),
              STIR = sum(STIR, na.rm = T)) # go on here, add more details and do it for the other indicators
  
  summary_soil_cover <- var_management_df_soil_cover %>% dplyr::ungroup() %>%
    dplyr::group_by(crop_no) %>%
    dplyr::summarize(year = dplyr::last(year),
              crop = dplyr::last(crop),
              harvest = dplyr::last(date),
              soil_cover_days = sum(soil_cover_days, na.rm = T),
              plant_cover_days = sum(plant_cover_days, na.rm = T),
              residue_cover_days = sum(residue_cover_days, na.rm = T),
              days = dplyr::n()) %>%
    dplyr::mutate(soil_cover = soil_cover_days/days * 100,
           plant_cover = plant_cover_days/days * 100,
           residue_cover = residue_cover_days/days * 100)
  
  summary <- dplyr::left_join(summary, summary_soil_cover,
                       by = dplyr::join_by(crop_no, year, crop, harvest))
  
  return(summary)
}
