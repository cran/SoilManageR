#' Identify main crops
#' 
#' Identifies all main crops that are harvested in the management dataframe.
#'
#' @param var_management_df management dataframe
#'
#' @returns a management dataframe with a column "crop_no" that identifies the main crops
#' @export
#'
#' @examples 
#' # identify main crop in the EXAMPLE data
#' identify_main_crops(EXAMPLE_data)
#' 
#' 
identify_main_crops <- function(var_management_df){

# identify last harvest events -------------
last_harvest_events <- var_management_df %>% dplyr::ungroup() %>%
  # only consider harvest events
  dplyr::filter(category == "harvest") %>%
  
  # choose the last harvest per crop per year
  dplyr::group_by(crop,year) %>%
  dplyr::summarise(dplyr::across(dplyr::everything(), dplyr::last), .groups = "keep") %>%
  dplyr::arrange(date) %>%
  dplyr::ungroup() %>%
  
  # remove if the last harvest per crop was a temporary ley after the harvest of an other crop in the same year
  dplyr::filter(crop != "ley, temporary" |
                  is.na(dplyr::lag(year)) | 
                  dplyr::lag(year) != year) %>%
  
  # add marker to the data
  dplyr::mutate(crop_no = dplyr::row_number())

# mark last harvest events in the management dataframes and fill periods in between -------------
suppressMessages(
  var_management_df <- dplyr::left_join(var_management_df, last_harvest_events) %>%
    dplyr::relocate(crop_no, .after = "crop") %>% 
    dplyr::ungroup() %>% 
    tidyr::fill(crop_no,.direction = "up") %>% 
    tidyr::fill(crop,.direction = "up")
)
# return the updated management dataframe
return(var_management_df)}
