#' Calculate C input tibble
#' 
#' A function that calculates the C input by crops, cover crops and organic amendments.
#' The output is returned as a tibble.
#' 
#' The function is mainly a helper function for the [C_input()] function.
#'
#' @param var_MGMT_data a `management_df` that contains the management information
#' 
#' @md 
#'
#' @return tibble with C input values
#' @export
#' 
#' @seealso 
#' * [calculate_indicators()] to calculate all management indicators 
#'    for a `management_df`
#' * [C_input_crops()] to calculate C input for crops
#' * [C_input_cover_crops()] to calculate C input for cover crops
#' * [CN_input_amendments()] to calculate C (and N) inputs for organic amendments
#' * [C_input()] a function that calculates the N input by mineral and organic fertilization and summarizes it by year
#'
#' @examples 
#' #Calculate C input tibble
#' calculate_C_input_tibble(EXAMPLE_data)
calculate_C_input_tibble <- function(var_MGMT_data) {
  
  # Check if the data is of the right class   -------------
  if (!("management_df" %in% class(var_MGMT_data))) {stop("Input if not of the class management_df")}
  
  # Calculate C and N inputs by organic amendments with function, choose default values if necessary  -------------
  var_MGMT_data_amendments <- var_MGMT_data %>%
    dplyr::rowwise() %>%
    dplyr::filter (operation == "organic_fertilization")
  
  if (nrow(var_MGMT_data_amendments) > 0) {
    var_MGMT_data_amendments <- var_MGMT_data_amendments %>%
      dplyr::mutate(C_input_org = as.integer(CN_input_amendments(value, amd_type=product, C_content = C_content,
                                                                 N_content = N_content, DMC = DMC)[1])) %>%
      dplyr::mutate(N_input_org = as.integer(CN_input_amendments(value, amd_type=product, C_content = C_content,
                                                                 N_content = N_content, DMC = DMC)[2]))
  } else {
    var_MGMT_data_amendments <- var_MGMT_data_amendments %>%
      dplyr::mutate(C_input_org = NA, N_input_org = NA)
  }
  
  var_MGMT_data <- dplyr::left_join(var_MGMT_data,var_MGMT_data_amendments, by = dplyr::join_by(crop, year, date, category, operation, device, value, unit, machine, product, combination,
                                                                                                comments, DMC, C_content, N_content, crop_product, crop_residue, Cc_product, Cc_residue)) %>%
    dplyr::select(-DMC,-C_content, -N_content)
  
  rm(var_MGMT_data_amendments)
  
  
  # Calculate carbon input of crop with the Bolinder Formula ----------
  
  var_MGMT_data_C_input <- var_MGMT_data %>%
    dplyr::mutate(C_input_product = NA,
                  C_input_straw = NA,
                  C_input_root = NA,
                  C_input_exudate = NA,
                  SS_factor = NA) %>%
    dplyr::filter(category == "harvest") %>%
    dplyr::filter(operation != "harvest_cover_crop")
  
  # Remove cuts of autum sown temporary leys 
  var_MGMT_data_C_input <- var_MGMT_data_C_input %>%
    dplyr::arrange(date,operation) %>%
    dplyr::mutate(Remove = FALSE)
  
  var_MGMT_data_C_input <- var_MGMT_data_C_input %>% 
    dplyr::ungroup() %>%
    dplyr::filter(( crop == "ley, temporary" & year != dplyr::lag(year)) |
                    crop != "ley, temporary" |
                    is.na(dplyr::lag(year)))
  
  # Calculate C input by crops 
  for (j in 1:nrow(var_MGMT_data_C_input)) {
    
    C_input_data <- C_input_crops(var_MGMT_data_C_input$crop[j],
                                  crop_product = var_MGMT_data_C_input$crop_product[j],
                                  crop_residue = var_MGMT_data_C_input$crop_residue[j],
                                  straw.removal = FALSE)
    
    var_MGMT_data_C_input$C_input_product[j] <- as.integer(C_input_data$C_input_product)
    var_MGMT_data_C_input$C_input_straw[j] <- as.integer(C_input_data$C_input_straw)
    var_MGMT_data_C_input$C_input_root[j] <- as.integer(C_input_data$C_input_root)
    var_MGMT_data_C_input$C_input_exudate[j] <- as.integer(C_input_data$C_input_exudate)
    
    # extract removal factor for straw
    var_MGMT_data_C_input$SS_factor[j] <- SoilManageR::C_input_crops_LUT %>%
      dplyr::filter(Crop == var_MGMT_data_C_input$crop[j]) %>%
      dplyr::select(SS) %>%
      as.numeric()
    
    # set straw removal factor to 0 if it is 1 in the table (1 is effectively no removal)
    if (var_MGMT_data_C_input$SS_factor[j] == 1) {var_MGMT_data_C_input$SS_factor[j] <- 0}
    
    rm (C_input_data)
  }
  
  # Calculate C removal by straw removal (C straw is 0 after removal)
  
  
  var_MGMT_data_C_input <- var_MGMT_data_C_input %>%
    dplyr::mutate(C_input_product = dplyr::case_when(operation == "straw_removal" ~ 0,
                                                     TRUE ~ C_input_product),
                  C_input_straw = dplyr::case_when(operation == "straw_removal" ~ round(-(1-SS_factor)*C_input_straw,0),
                                                   TRUE ~ C_input_straw),
                  C_input_root = dplyr::case_when(operation == "straw_removal" ~ 0,
                                                  TRUE ~ C_input_root),
                  C_input_exudate = dplyr::case_when(operation == "straw_removal" ~ 0,
                                                     TRUE ~ C_input_exudate))
  
  var_MGMT_data_C_input <- var_MGMT_data_C_input %>%
    dplyr::mutate(C_input_crop = C_input_product + C_input_straw + C_input_root + C_input_exudate) %>%
    dplyr::select(crop,year,date,category,operation,C_input_crop)
  
  var_MGMT_data <- dplyr::left_join(var_MGMT_data, var_MGMT_data_C_input, by = dplyr::join_by(crop, year, date, category, operation))
  
  rm(var_MGMT_data_C_input)
  
  
  # Select Management Operations with cover crops relevance  ------------
  # Make sure these are the same as in the calculate soil cover tibble function!
  
  var_MGMT_data_cover_crops <- var_MGMT_data %>%
    dplyr::filter((  operation == "sowing_cover_crop" |
                       operation == "sowing_main_crop" |
                       category == "tillage" |
                       operation == "total_herbicide" |
                       device == "frost_kill_cover_crop" |
                       (operation == "weed_herbicide" & 
                        product == "Glyphosate") |
                       operation == "harvest_cover_crop") &  
                    device != "roller") %>% 
    dplyr::select(crop,year,date,category,operation) %>%
    dplyr::mutate(duration_CC = NA,
                  biomass_CC = NA) %>%
    dplyr::arrange(date) %>%
    dplyr::distinct()
  
  # Identify termination and seeding events, delete all other events  ------------
  
  var_MGMT_data_cover_crops <- var_MGMT_data_cover_crops %>%
    dplyr::ungroup() %>%
    dplyr::filter(operation == "sowing_cover_crop"|
                    dplyr::lag(operation) == "sowing_cover_crop")
  
  
  # Calculate cover crop duration and C input by cover crop  ------------
  # Assumption: If the cover crop was still established when the timeline ends, 
  # the termination is assumed to be at the end of the last year
  
  if (nrow(var_MGMT_data_cover_crops) > 0) {
    #Calculate duration of cover crop
    
    for (j in 1:nrow(var_MGMT_data_cover_crops)) {
      
      if (j == 1) {next}
      
      if (var_MGMT_data_cover_crops$operation[j] != "sowing_cover_crop") {
        if (var_MGMT_data_cover_crops$operation[j-1] == "sowing_cover_crop") {
          var_MGMT_data_cover_crops$duration_CC[j] <-  (var_MGMT_data_cover_crops$date[j] - var_MGMT_data_cover_crops$date[j-1])
        }
      }
      
      # extra condition, if the last line is a cover crop seeding event  
      if (j == nrow(var_MGMT_data_cover_crops) & 
          var_MGMT_data_cover_crops$operation[j] == "sowing_cover_crop") {
        
        var_MGMT_data_cover_crops <- var_MGMT_data_cover_crops %>%
          dplyr::ungroup() %>%
          dplyr::add_row(crop = var_MGMT_data_cover_crops$crop[j],
                         year = var_MGMT_data_cover_crops$year[j],
                         date = as.Date(paste(as.character(year(max(var_MGMT_data$date))),"-12-31",sep = "")),
                         category = NA,
                         operation = "generic_kill_cover_crop",
                         duration_CC = as.numeric(as.Date(paste(as.character(year(max(var_MGMT_data$date))),"-12-31",sep = "")) - 
                                                    var_MGMT_data_cover_crops$date[j]),
                         biomass_CC = NA)
      } 
    }
    # Calculate Cover crop C input
    
    var_MGMT_data_cover_crops <- var_MGMT_data_cover_crops %>%
      dplyr::rowwise() %>%
      dplyr::mutate(C_input_CC = C_input_cover_crops(abvg_biomass = NA, days = duration_CC)$C_input_total) %>%
      
      #extra calculation for events where the cover crop was removed by a harvest event
      dplyr::mutate(C_input_CC = dplyr::case_when(operation != "harvest_cover_crop" ~ C_input_CC,
                                           TRUE ~ 
                                             C_input_cover_crops(abvg_biomass = NA,
                                                                 days = duration_CC)[3:4] %>% 
                                             sum)) %>%
      
      dplyr::select(crop,year,date,category,operation,duration_CC,C_input_CC)
  } else {
    # if no cover crop were established at all
    var_MGMT_data_cover_crops <- var_MGMT_data_cover_crops %>% dplyr::mutate(C_input_CC = NA)
  }
  
  var_MGMT_data_cover_crops <- var_MGMT_data_cover_crops %>% 
    dplyr::filter(operation != "sowing_cover_crop")
  
  # Integrate C inputs by cover crops into the management file --------------
  # extra detour to avoid multiple reporting of mixtures that were seeded with multiple operations at the same date
  var_MGMT_data_cover_crops <- dplyr::left_join(var_MGMT_data_cover_crops, var_MGMT_data,
                                                by = dplyr::join_by(crop, date, year,category,operation),
                                                multiple = "first") %>% 
    dplyr::select(crop,year,date,category,operation,product,C_input_CC) %>%
    dplyr::group_by(year)
  
  
  var_MGMT_data <- dplyr::full_join(var_MGMT_data,var_MGMT_data_cover_crops,
                                    by = dplyr::join_by(crop, date, year,category,operation, product)) %>% 
    dplyr::group_by(year)
  
  # Return output tibble --------------
  return(var_MGMT_data)
  
}

