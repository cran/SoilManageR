#' Check management_df for consitency
#' 
#' The function checkes objects of the class [management_df()] for internal consistency.
#'  It formally checks the class and the column names.
#'  Additionally, the function checks if dates are consistently increasing
#'  and if all organic amendments, tillage and sowing devices and crops
#'  are in the relevant look-up-tables. It is checked if the are operations
#'  where the device is not mentioned.
#'  Furthermore, the amount of organic amendments (<100t/ha) and
#'  N fertilizer (<100kgN/ha) application rates per event are checked.
#'  The depth of tillage operations are compared with the min and max depth
#'  from the `STIR_value_LUT`. 
#'  Finally, the order of tillage, sowing and harvest operations are checked
#'  for plausibility (see details for more information). 
#'
#' @details
#' The order of tillage, sowing and harvest operations are checked with the following assumptions:
#' * after "stubble_cultivation" the following operations are allowed: "stubble_cultivation", "primary_tillage", "seedbed_preparation", "sowing_main_crop", "sowing_cover_crop"
#' * after "primary_tillage" the following operations are allowed: "primary_tillage", "seedbed_preparation", "sowing_main_crop", "sowing_cover_crop"
#' * after "seedbed_preparation" the following operations are allowed: "stubble_cultivation", "seedbed_preparation", "sowing_main_crop", "sowing_cover_crop"
#' * after "sowing_main_crop" the following operations are allowed: "sowing_main_crop", "harvest_main_crop", "sowing_cover_crop"
#' * after "sowing_cover_crop" the following operations are allowed: "stubble_cultivation", "primary_tillage", "seedbed_preparation", "sowing_main_crop", "sowing_cover_crop", "harvest_main_crop"
#' * after "harvest_main_crop" the following operations are allowed: "harvest_main_crop", "straw_removal", "hay_removal", "stubble_cultivation", "primary_tillage", "seedbed_preparation", "sowing_main_crop", "sowing_cover_crop"
#' * after "straw_removal" the following operations are allowed: "harvest_main_crop", "straw_removal", "stubble_cultivation", "primary_tillage", "seedbed_preparation", "sowing_main_crop", "sowing_cover_crop"
#' * after "hay_removal" the following operations are allowed: "harvest_main_crop", "hay_removal", "stubble_cultivation", "primary_tillage", "seedbed_preparation", "sowing_main_crop", "sowing_cover_crop"
#' 
#' Additionally, there are exceptions for potato crops: "bedder" can be used after a "potato_planter" and "mulching" can be applied before a "potato_harvester"
#' @md
#'     
#' @param var_MGMT_data an object of the [management_df()] class
#' 
#' @return a test_report list (only returned if some tests failed)
#' @seealso 
#' * [management_df()] for creating an management_df
#' * [management_df_from_excel()] for importing a management_df from an excel template
#' 
#' @examples
#' #example input
#' check_management_df(EXAMPLE_data)
#' @export

check_management_df <- function(var_MGMT_data) {
  
  
  
  # Print initial message ---------------
  message("** check of managment_df initiated **")
  
  ########################
  #### Formal Checks #####
  ########################
  
  # Check if the data is of the right class   -------------
  
  if (!("management_df" %in% class(var_MGMT_data))) {stop("Input if not of the class management_df")}
  
 
  
 # Check if the expected columns are in the management_df
  
  expected_col_names <- c("crop","year","date","category","operation",
                          "device","value","unit","machine","product",
                          "combination","comments","DMC","C_content",
                          "N_content","crop_product","crop_residue",
                          "Cc_product","Cc_residue")
  
   if(FALSE %in% (colnames(var_MGMT_data) %in% expected_col_names)) {
    stop(paste("Unexcpected column(s) in the management_df: ",
               paste(setdiff(colnames(var_MGMT_data),expected_col_names),collapse = ", "),
               sep=""))
  }
  
  if(FALSE %in% (expected_col_names %in% colnames(var_MGMT_data))) {
    stop(paste("Expected column(s) are missing in the management_df: ",
               paste(setdiff(expected_col_names,colnames(var_MGMT_data)),collapse = ", "),
               sep=""))
  }
  
  rm(expected_col_names)
  
  
  # Check if the management_df is empty ----------------
  
  #remove rows that contain only NAs
  var_MGMT_data <- var_MGMT_data %>%
    dplyr::ungroup() %>%
    dplyr::filter(rowSums(is.na(var_MGMT_data)) != ncol(var_MGMT_data))
  
  if(nrow(var_MGMT_data) == 0) {stop("management_df is empty")}
  
  ########################
  #### Content Checks ####
  ########################
  
  # Check if the date is increasing and the year is matching the date ----------------
  
  if (TRUE %in% (diff(var_MGMT_data$date) < 0)) {
    warning("Entries are not chronological, dates sometimes go backwards.")}
  
  if (FALSE %in% (lubridate::year(var_MGMT_data$date)==var_MGMT_data$year)) {
    warning("Year and Date are not allways matching")}
  
  # Check if there are missing devices -------------------
  var_MGMT_check <- var_MGMT_data %>%
    dplyr::filter(category %in% c("tillage",
    "sowing",
    "fertilizer_application",
    "crop_protection",
    "harvest",
    "irrigation")) %>%
    dplyr::select(device) %>%
    dplyr::filter(is.na(device))
  
  if(nrow(var_MGMT_check) != 0) {warning("At least one device is not specified. Check if this is an error. STIR value calculation and other functions may be affected by missing devices.")}
  
  rm(var_MGMT_check)

  
  
  # Check if all entries exist in the relevant LOT -------------------
  
  # Check if organic amendments are in the CN_input_amendments_LUT.rda
    organic_amendments <- var_MGMT_data %>%
      dplyr::ungroup() %>%
      dplyr::filter(operation == "organic_fertilization") %>%
      dplyr::select(product) %>%
      unique()
    
    if(FALSE %in% (organic_amendments$product %in% CN_input_amendments_LUT$Amendment)) {
      warning(paste("Not all organic amendments (products) are in the CN_input_amendments_LUT.rda! The missing products are: ",
      paste(setdiff(organic_amendments$product,CN_input_amendments_LUT$Amendment),collapse = ", "),
      sep=""))
      }
  
    rm(organic_amendments)
  
  # Check if tillage operations are in the STIR_values_LUT.rda
    
    tillage_operations <- var_MGMT_data %>%
      dplyr::ungroup() %>%
      dplyr::filter(category == "tillage" | category == "sowing") %>%
      dplyr::select(device) %>%
      dplyr::filter(device != "mulching") %>% #mulching has no STIR value and is not in the STIR_values_LUT.rda
      unique()
    
    if(FALSE %in% (tillage_operations$device %in% STIR_values_LUT$Operation)) {
      warning(paste("Not all tillage and sowing devices are in the STIR_values_LUT.rda! The missing devices are: ",
                    paste(setdiff(tillage_operations$device,STIR_values_LUT$Operation),collapse = ", "),
                    sep=""))}
    
    rm(tillage_operations)
  
  # Check if all crops are in the C_input_crops_LUT.rda and the plant_cover_LUT.rda
    
    crops <- var_MGMT_data %>%
      dplyr::ungroup() %>%
      dplyr::select(crop) %>%
      unique()
    
    if (FALSE %in% (crops$crop %in% C_input_crops_LUT$Crop)) {
      warning(paste("Not all crops are in the C_input_crops_LUT.rda! The missing crops are: ",
                    paste(setdiff(crops$crop,C_input_crops_LUT$Crop),collapse = ", "),
                    sep=""))}
    
    if (FALSE %in% (crops$crop %in% plant_cover_LUT$Crop)) {
      warning(paste("Not all crops are in plant_cover_LUT.rda! The missing crops are: ",
                    paste(setdiff(crops$crop,plant_cover_LUT$Crop),collapse = ", "),
                    sep=""))}
    rm(crops)
  
  # Check order of operations -------------------
    
    #define expected next operations
    
    
    after_stubble_cultivation <- c("stubble_cultivation","primary_tillage","seedbed_preparation","sowing_main_crop","sowing_cover_crop")
    after_primary_tillage <- c("primary_tillage","seedbed_preparation","sowing_main_crop","sowing_cover_crop")
    after_seedbed_preparation <- c("stubble_cultivation","seedbed_preparation","sowing_main_crop","sowing_cover_crop")
    after_sowing_main_crop <- c("sowing_main_crop","harvest_main_crop","sowing_cover_crop")
    after_sowing_cover_crop <- c("stubble_cultivation","primary_tillage","seedbed_preparation","sowing_main_crop","sowing_cover_crop","harvest_main_crop")
    after_harvest_main_crop <- c("harvest_main_crop","straw_removal","hay_removal","stubble_cultivation","primary_tillage","seedbed_preparation","sowing_main_crop","sowing_cover_crop")
    after_straw_removal <- c("harvest_main_crop","straw_removal","stubble_cultivation","primary_tillage","seedbed_preparation","sowing_main_crop","sowing_cover_crop")
    after_hay_removal <- c("harvest_main_crop","hay_removal","stubble_cultivation","primary_tillage","seedbed_preparation","sowing_main_crop","sowing_cover_crop")
    
    order_of_operations <- var_MGMT_data %>%
      dplyr::ungroup() %>%
      dplyr::filter(category == "tillage" | category == "sowing" | category == "harvest") %>%
      dplyr::select(crop,year,date,category,operation,device)%>%
      unique() %>%
      dplyr::mutate(next.operation = dplyr::lead(operation)) %>%
      dplyr::mutate(check_next_operation = dplyr::case_when(
        is.na(next.operation) ~ TRUE, #exception for the last line in the tibble
        crop == "potato" & device == "potato_planter" & dplyr::lead(device) == "bedder" ~ TRUE, #exception for potato bedder after planting potatoes
        crop == "potato" & device == "mulching" & dplyr::lead(device) == "potato_harvester" ~ TRUE, #exception for potato mulching before harvest 
        operation == "stubble_cultivation" ~ dplyr::case_when(next.operation %in% after_stubble_cultivation ~ TRUE, TRUE ~ FALSE),
        operation == "primary_tillage" ~ dplyr::case_when(next.operation %in% after_primary_tillage ~ TRUE, TRUE ~ FALSE),
        operation == "seedbed_preparation" ~ dplyr::case_when(next.operation %in% after_seedbed_preparation ~ TRUE, TRUE ~ FALSE),
        operation == "sowing_main_crop" ~ dplyr::case_when(next.operation %in% after_sowing_main_crop ~ TRUE, TRUE ~ FALSE),
        operation == "sowing_cover_crop" ~ dplyr::case_when(next.operation %in% after_sowing_cover_crop ~ TRUE, TRUE ~ FALSE),
        operation == "harvest_main_crop" ~ dplyr::case_when(next.operation %in% after_harvest_main_crop ~ TRUE, TRUE ~ FALSE),
        operation == "straw_removal" ~ dplyr::case_when(next.operation %in% after_straw_removal ~ TRUE, TRUE ~ FALSE),
        operation == "hay_removal" ~ dplyr::case_when(next.operation %in% after_hay_removal ~ TRUE, TRUE ~ FALSE),
        TRUE ~ FALSE
      )) %>%
      dplyr::filter(check_next_operation == FALSE | dplyr::lag(check_next_operation) == FALSE | dplyr::lead(check_next_operation) == FALSE)
    
    if (nrow(order_of_operations) != 0) {
      warning("Some tillage, sowing or harvest operation have unexpected order. See test report for details")
    }
    
    if (nrow(order_of_operations >= 1)) {
    test_report_order <- order_of_operations
    }
    
    rm(order_of_operations,after_stubble_cultivation,after_primary_tillage,
       after_seedbed_preparation,after_sowing_main_crop,after_sowing_cover_crop,
       after_harvest_main_crop,after_straw_removal)
    
  # Check the amount of fertilizer inputs-------------------
    fertilizer_amount <- var_MGMT_data %>%
      dplyr::ungroup() %>%
      dplyr::filter(category == "fertilizer_application")
    
    fertilizer_amount_organic <- fertilizer_amount %>%
      dplyr::filter(operation == "organic_fertilization") %>%
      dplyr::filter(unit == "t/ha") %>%
      dplyr::filter(value > 100)
    
    fertilizer_amount_mineral <- fertilizer_amount %>%
      dplyr::filter(operation == "mineral_fertilization") %>%
      dplyr::filter(unit == "kg N/ha") %>%
      dplyr::filter(value > 100)
    
    if (nrow(fertilizer_amount_organic != 0)) {
      warning("At least one organic fertilization events have high application rates (>100 t/ha). Is this correct?")
    }
    
    if (nrow(fertilizer_amount_mineral != 0)) {
      warning("At least mineral N fertilization events have high application rates (>100 kg N/ha). Is this correct?")
    }
    
    rm(fertilizer_amount,fertilizer_amount_organic,fertilizer_amount_mineral)
      
      
  # Check the depth of tillage operations -------------------
    if (("cm" %in% var_MGMT_data$unit) & ("tillage" %in% var_MGMT_data$category)) {  
      tillage_operations_depth <- var_MGMT_data %>%
        dplyr::ungroup() %>%
        dplyr::filter(unit == "cm" & category == "tillage") %>%
        dplyr::select(crop,date,device,value,unit,machine) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(Depth_min = STIR_values_LUT %>% 
                        dplyr::filter(Operation == device) %>% 
                        dplyr::select(Depth_MIN) %>% as.numeric() - 2,
                      Depth_max = STIR_values_LUT %>% 
                        dplyr::filter(Operation == device) %>% 
                        dplyr::select(Depth_MAX) %>% as.numeric() + 2) %>%
        dplyr::mutate(Max_depht_check = dplyr::case_when(value <= Depth_max ~ TRUE,
                                                        TRUE ~ FALSE),
                      Min_depht_check = dplyr::case_when(value >= Depth_min ~ TRUE,
                                                         TRUE ~ FALSE))
      
      if (FALSE %in% tillage_operations_depth$Min_depht_check | FALSE %in% tillage_operations_depth$Max_depht_check) {
        warning("Some tillage operation have unexpected shallow or deep operation depth. See test report for detail.")
        test_report_tillage <- tillage_operations_depth %>%
                dplyr::filter(Min_depht_check == FALSE | Max_depht_check == FALSE) %>%
                dplyr::select(date,device,value,unit,Depth_min,Depth_max)}
  
      rm(tillage_operations_depth)
    }
    
  # Print final message ------------
    message("** check of managment_df completed **")
    
  # Create output object
    
    if(exists("test_report_order") & exists("test_report_tillage")) {
      test_report <- list("unexpected order of operations" = test_report_order,
                          "unexpected depth of tillage operations" = test_report_tillage)
    }
    
    if(exists("test_report_order") & !(exists("test_report_tillage"))) {
      test_report <- list("unexpected order of operations" = test_report_order)
    }
    
    if(!(exists("test_report_order")) & exists("test_report_tillage")) {
      test_report <- list("unexpected depth of tillage operations" = test_report_tillage)}
    
  # return test_report (if it exists)
    
  if (exists("test_report")){
    return(test_report)}
    
}
