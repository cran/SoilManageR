#' Constructor for management_df
#'
#' This function is a constructor for empty objects of the [management_df()] class,
#'  the core of the SoilManageR package.
#'  
#' @param crop Name of the main crop. Must match a pre-existing list
#' @param year Year of the management operation ("YYYY")
#' @param date Date of the management operation ("YYYY-MM-DD")
#' @param category Categorization of the managment operation (1 level). Must match a pre-existing list.
#' @param operation Categorization of the managment operation (2 level). Must match a pre-existing list.
#' @param device Categorization of the managment operation (3 level). Must match a pre-existing list.
#' @param value Numerical value linked to managment operation (e.g., depth of tillage operation, mass of organic amendment)
#' @param unit Unit of the numerical value (e.g. cm, t/ha)
#' @param machine Further information on the machine used (e.g., type, manufacturer, tool)
#' @param product Further information on the applied product (e.g., name, manufacturer). Must match pre-existing list for organic amendments.
#' @param combination Indicate if a operation was done in combination with others. Use consequtive integer numbers if combinded operations occur. Leave empty if not combined.
#' @param comments Comments related to the management operation
#' @param DMC Dry matter content of organic amendments (gDM/kgFM)
#' @param C_content Carbon content of the amendments, relative to its dry matter (gC/kgDM)
#' @param N_content Nitrogen content of organic amendments, relative to its dry matter (gN/kgDM)
#' @param crop_product Crop product yield (tDM/ha)
#' @param crop_residue Crop residue mass (tDM/ha)
#' @param Cc_product Carbon content of the crop product (gC/kgDM)
#' @param Cc_residue Carbon content of the crop residue (gC/kgDM)
#' 
#' @return a [management_df()]
#' 
#' @seealso 
#' * [management_df_from_excel()] for importing a `management_df` from an excel template
#' * [check_management_df()] to check the integrity of a `management_df`
#' * [EXAMPLE_data()] for an example of a `management_df`
#' 
#' @md
#' 
#' 
#' @examples
#' #creation of an empty management_df
#' management_df()
#' @export

management_df <- function(crop = NA,
                          year = NA,
                          date = NA,
                          category = NA,
                          operation = NA,
                          device = NA,
                          value = NA,
                          unit = NA,
                          machine = NA,
                          product = NA,
                          combination = NA,
                          comments = NA,
                          DMC = NA,
                          C_content = NA,
                          N_content = NA,
                          crop_product = NA,
                          crop_residue = NA,
                          Cc_product = NA,
                          Cc_residue = NA) {
  
  #check if all expected columns are there
  output_tibble <- dplyr::tibble(crop = crop,
                          year = year,
                          date = as.Date(date),
                          category = category,
                          operation = operation,
                          device = device,
                          value = value,
                          unit = unit,
                          machine = machine,
                          product = product,
                          combination = combination,
                          comments = comments,
                          DMC = DMC,
                          C_content = C_content,
                          N_content = N_content,
                          crop_product = crop_product,
                          crop_residue = crop_residue,
                          Cc_product = Cc_product,
                          Cc_residue = Cc_residue)
  
  #remove rows that contain only NAs
  output_tibble <- output_tibble %>%
    dplyr::filter(rowSums(is.na(output_tibble)) != ncol(output_tibble))
  
  #assign the object to the class management_df
  class(output_tibble) <- c("management_df",class(output_tibble))
  
  return(output_tibble)
}
