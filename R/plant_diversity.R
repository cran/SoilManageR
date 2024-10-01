#' Calculate plant diversity indicators
#'
#' @description
#' Derives three indicators for plant diversity of a crop rotation based 
#'  on management information (mainly sowing events).
#'  
#' @details
#' For the function to work properly the species (or variety) must be mentioned
#'  in the "product" column, and all sown species of mixtures must be 
#'  represented in a single row of the `management_df` each.
#' 
#' The function calculates the plant diversity index (\eqn{PDI}), the total
#'  number of different species, and the Shannon index  for all sown species.
#' 
#' The \eqn{PDI} is calculated in the following way inspired by 
#'  \insertCite{Tiemann_2015;textual}{SoilManageR}: 
#'  \deqn{PDI = \overline{S_{year}} * S_{rotation}}
#'  where \eqn{\overline{S_{year}}} is the average number of sown species per 
#'  year and \eqn{S_{rotation}} is the total number of different species 
#'  sown in the full crop rotation or cropping sequence.
#'
#' The Shannon Index is calculated with the [shannon_index()] function.
#'  
#' @md
#' @references
#'  \insertAllCited{}
#' 
#' @seealso
#' * [calculate_indicators()] to calculate all management indicators 
#'      for a `management_df`
#' * [shannon_index()] for more detail on the Shannon index
#'  
#' @param var_MGMT_data a 'management_df' with a management history
#' @param start_year start year of the cropping sequence of interest
#' @param end_year end year of the cropping sequence of interest
#' 
#' @return a tibble with three indicators for plant diversity
#' 
#' @examples
#' plant_diversity(EXAMPLE_data,2013,2020)
#' 
#' @export
#' 
#' 

plant_diversity <- function(var_MGMT_data,start_year,end_year) {

  start_year <- as.integer(start_year)
  end_year <- as.integer(end_year)
  
  years <- tibble::tibble(year = start_year:end_year)
  
  # Check if the data is of the right class   -------------
  if (!("management_df" %in% class(var_MGMT_data))) {stop("Input if not of the class management_df")}
  if (!(is.integer(start_year))) {stop("start_year is no integer")}
  if (!(is.integer(end_year))) {stop("start_year is no integer")}
  
  # PDI with the Tiemann approach
    # Filter data  -------------  
    var_MGMT_data_PDI <- var_MGMT_data %>%
      dplyr::filter(year >= start_year,
                    year <= end_year)
    
    var_MGMT_data_PDI <- var_MGMT_data_PDI %>%
      dplyr::group_by(year) %>%
      dplyr::filter(category == "sowing") %>%
      dplyr::filter(device != "roller")
      
    
    # calculate average number of distinct products seeded per per year ----------------
    PDI_avg_year <- var_MGMT_data_PDI %>% 
      dplyr::select(year,product) %>% 
      dplyr::distinct() %>% 
      dplyr::summarise(count = dplyr::n()) %>%
      dplyr::full_join(years, by = dplyr::join_by(year)) %>%
      tidyr::replace_na(list(count = 0)) %>%
      dplyr::summarise(average = mean(count,na.rm = TRUE)) %>%
      as.numeric()
    
    # calculate total main crops (products = varieties) per rotation ----------------
    PDI_total_main_crop <- var_MGMT_data_PDI %>%
      dplyr::filter(operation == "sowing_main_crop") %>%
      dplyr::ungroup() %>%
      dplyr::select(crop,product) %>%
      dplyr::distinct() %>%
      dplyr::summarise(count = dplyr::n()) %>%
      as.numeric() 
    
    # calculate total cover crops products per rotation ----------------
    PDI_total_cover_crop <- var_MGMT_data_PDI %>%
      dplyr::filter(operation == "sowing_cover_crop") %>%
      dplyr::ungroup() %>%
      dplyr::select(product) %>%
      dplyr::distinct() %>%
      dplyr::summarise(count = dplyr::n()) %>%
      as.numeric() 
    
    # calculate PDI_Tiemann ----------------
    PDI_Tiemann <- PDI_avg_year * (PDI_total_cover_crop + PDI_total_main_crop)
  
  # Count species per rotation -----------
    Species_rotation <- PDI_total_cover_crop + PDI_total_main_crop
    rm(PDI_total_cover_crop,PDI_total_main_crop,PDI_avg_year,var_MGMT_data_PDI)
    
  # Calculate Shannon index for sowing events -----------
    # Filter data  -------------  
    var_MGMT_data_Shannon <- var_MGMT_data %>%
      dplyr::filter(year >= start_year,
                    year <= end_year)
    
    var_MGMT_data_Shannon <- var_MGMT_data_Shannon %>%
      dplyr::group_by(year) %>%
      dplyr::filter(category == "sowing") %>%
      dplyr::filter(device != "roller") %>%
      dplyr::filter(operation == "sowing_cover_crop" |
                      operation == "sowing_main_crop")
    
    # Create tibble for input  -------------
    var_MGMT_data_Shannon <- var_MGMT_data_Shannon %>%
      dplyr::ungroup() %>%
      dplyr::select(product) %>%
      dplyr::group_by(product) %>%
      dplyr::summarise(count = dplyr::n())
    
    # Calculate shannon index  -------------
    Shannon <- shannon_index(var_MGMT_data_Shannon)
    rm(var_MGMT_data_Shannon)
    
    
  # return results ----------------
  return(tibble::tibble(PDI_Tiemann,Species_rotation,Shannon))
}
