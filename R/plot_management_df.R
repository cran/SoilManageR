#' Plot a management dataframe
#' 
#' Visual representation of a management_df.
#' 
#' The colors in the background represent the soil cover by plants (green), 
#'  and crop residues (brown). "T" indicates tillage event with a STIR value >= 0.
#' "P" indicates a crop protection event.
#' Dotted vertical lines represent sowing events, the sown crop is noted in 
#'  the lowest quarter at the right side of the line.
#' The values in the middle indicate C inputs into the soil system, either 
#'  by crops, cover crops or organic amendments. Negative values are withdrawls
#'  by residue removal.
#' Note that large management_df can be heavy to compute and the plots may be
#'  messy.
#'
#' @param management_df a management data frame
#' @param title the title of the plot
#'
#' @return a plot
#' @export
#'
#' @examples
#' 
#' \donttest{
#'   # creates a visual representation of the EXAMPLE data
#'   plot_management_df(EXAMPLE_data)
#'  }

plot_management_df <- function(management_df, title="Management Timeline") {
  
  # Calculate separate indicators
  C_input_data <- C_input(management_df, extended.output = TRUE) 
  STIR_data <- tillage_intensity(management_df, extended.output = TRUE)
  soil_cover_data <- soil_cover(management_df, extended.output = TRUE)
  
  # Define plot (use soil cover plot)
  plot <- plot(soil_cover_data)
  
  # Redefine title
  plot <- plot + ggplot2::ggtitle(title)
  
  # Add relevant events
  # Horizontal lines for sowing event
  # Define events
  sowing_events <- subset(management_df, category == "sowing")
  sowing_CC <- subset(sowing_events, operation == "sowing_cover_crop")
  sowing_MC <- subset(sowing_events, operation == "sowing_main_crop")
  
  # Add line for all events
  plot <- plot + ggplot2::geom_vline(xintercept = as.numeric(sowing_events$date), linetype = "dotted")
  
  # Add label for main crop
  plot <- plot + ggplot2::annotate(geom = "text",
                                   label = sowing_MC$crop,
                                   x = sowing_MC$date,
                                   y = 10,
                                   angle = 90, 
                                   vjust = 1.5, hjust = 0,
                                   size = 3)
  
  # Add label for cover crop
  plot <- plot + ggplot2::annotate(geom = "text",
                                   label = "cover crop",
                                   x = sowing_CC$date,
                                   y = 10,
                                   angle = 90, 
                                   vjust = 1.5, hjust = 0,
                                   size = 3)
  
  # Tillage events with STIR values
  # Define events
  tillage_events <- subset(STIR_data, !is.na(STIR_data$STIR) & STIR_data$STIR > 0)
  
  # Add T label
  plot <- plot + ggplot2::annotate(geom = "text",
                                   label = "T",
                                   x = tillage_events$date,
                                   y = 95,
                                   vjust = 0.5, hjust = 0.5,
                                   size = 3)
  
  # Crop protection events
  # Define events
  crop_protection_events <- subset(management_df, category == "crop_protection")
  
  # Add P label
  plot <- plot + ggplot2::annotate(geom = "text",
                                   label = "P",
                                   x = crop_protection_events$date,
                                   y = 93,
                                   vjust = 0.5, hjust = 0.5,
                                   size = 3)
  
  # C inputs
  # Define events
  C_events <- subset(C_input_data, !(is.na(C_input_data$C_input_org)) | 
                       !(is.na(C_input_data$C_input_crop)) | 
                       !(is.na(C_input_data$C_input_CC)))
  C_events <- C_events[, c("date", "year", "category", "operation", "device", 
                           "C_input_org", "C_input_crop", "C_input_CC")]
  
  C_org_events <- subset(C_events, !is.na(C_events$C_input_org))
  C_crop_events <- subset(C_events, !is.na(C_events$C_input_crop))
  C_CC_events <- subset(C_events, !is.na(C_events$C_input_CC))
  
  # Add C labels
  plot <- plot + ggplot2::annotate(geom = "text",
                                   label = paste("organic:", C_org_events$C_input_org, "kgC/ha"),
                                   x = C_org_events$date,
                                   y = 40,
                                   angle = 90,
                                   vjust = -1, hjust = 0,
                                   size = 3)
  
  plot <- plot + ggplot2::annotate(geom = "text",
                                   label = paste("crop:", C_crop_events$C_input_crop, "kgC/ha"),
                                   x = C_crop_events$date,
                                   y = 40,
                                   angle = 90,
                                   vjust = -1, hjust = 0,
                                   size = 3)
  
  plot <- plot + ggplot2::annotate(geom = "text",
                                   label = paste("cover crop:", C_CC_events$C_input_CC, "kgC/ha"),
                                   x = C_CC_events$date,
                                   y = 40,
                                   angle = 90,
                                   vjust = -1, hjust = 0,
                                   size = 3)
  
  return(plot)
}
