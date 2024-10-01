#' Estimate carbon input
#' 
#' [C_input()] estimates the carbon (C) input into the soil system per year of a
#'  `management_df`.
#' 
#' @details
#' The function takes a `management_df` as input and returns 
#'  a C input values per year in the `management_df`. 
#' 
#' Alternatively, it can return a tibble with all managememnt operations and 
#'  their respective C input values.
#' 
#' The functions calculates the C input with the [C_input_crops()], 
#'  [C_input_cover_crops()] and [CN_input_amendments()] functions.
#'  
#' @seealso
#' * [calculate_indicators()] to calculate all management indicators 
#'      for a `management_df`
#' * [C_input_crops()] to calculate C input for crops
#' * [C_input_cover_crops()] to calculate C input for cover crops
#' * [CN_input_amendments()] to calculate C (and N) inputs for organic amendments
#' 
#' @md 
#'     
#' @param var_MGMT_data a `management_df` that contains the management information
#' @param extended.output an optional logical value. 
#' * If FALSE, C input values are aggregated by year. 
#' * If TRUE, a tibble with daily resolution is returned. 
#' * Default value is FALSE
#' 
#' 
#' @return 
#' * By default, a tibble with C input values (total and by category) 
#'  by year is returned. 
#' * If `extended.output = TRUE`, a tibble with all management operations and their
#'  respecitve C inputs is returned. 
#' 
#' 
#' @examples
#' #example that returns annual C input values
#' C_input(EXAMPLE_data)
#' 
#' #example that returns a tibble with all operations that have a C input
#' C_input(EXAMPLE_data, extended.output = TRUE)
#' 
#' @export


C_input <- function(var_MGMT_data, extended.output = FALSE) {

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
    dplyr::filter (category == "harvest")
  
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
    
    # set straw removal factor to 0 if it is 1 in the table (1 is efectively no removal)
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
  
  var_MGMT_data_cover_crops <- var_MGMT_data %>%
    dplyr::filter((  operation == "sowing_cover_crop" |
                     category == "tillage" |
                     operation == "weed_herbicide") &  device != "roller") %>% 
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

# Purge obsolete variables
rm (var_MGMT_data_cover_crops)
  
  

  
  # create output tibble  
  if (extended.output == TRUE) {
    output_tibble <- var_MGMT_data
    } else {
    output_tibble <- var_MGMT_data %>%
      dplyr::ungroup() %>%
      dplyr::group_by(year) %>%
      dplyr::summarize(C_input_org_amendment = sum(C_input_org, na.rm = TRUE),
                C_input_crop = sum(C_input_crop, na.rm = TRUE),
                C_input_cover_crop = sum(C_input_CC, na.rm = TRUE),
                crop = dplyr::first(crop)) %>%
      dplyr::mutate(C_input = C_input_org_amendment+C_input_crop+C_input_cover_crop) %>%  #add more parameters later
      dplyr::relocate(year,crop,C_input,C_input_org_amendment,C_input_crop,C_input_cover_crop)  
  }
  
  return(output_tibble)

}
