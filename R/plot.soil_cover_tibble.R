#' Plotting soil_cover_tibbles
#' 
#' This function plots objects of the class soil_cover_tibble in a custom format
#' 
#' @param x an soil_cover_tibble object
#' @param ... arguments to be passed to methods
#' 
#' @return none, it plots the soil_cover_tibble
#' 
#' @examples
#' \donttest{
#'   data <- soil_cover(EXAMPLE_data, extended.output = TRUE)
#'   plot(data)
#' }
#'
#' 
#' @rdname plot.soil_cover_tibble
#' @export

plot.soil_cover_tibble <- function(x,...) {
  ggplot2::ggplot(data = x, ggplot2::aes(x = date)) + 
    ggplot2::scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
    ggplot2::geom_area(fill = "brown", ggplot2::aes(y = soil_cover)) +
    ggplot2::geom_area(fill = "chartreuse4", ggplot2::aes(y = plant_cover)) +
    ggplot2::geom_point(data = x %>% dplyr::filter(soil_cover_days != 0), fill = "black", ggplot2::aes(y = soil_cover_days*30), size = .2) +
    ggthemes::theme_few() +
    ggplot2::labs(y  = "soil cover [%]", x = "date") +
    ggplot2::ggtitle("Soil cover")
}


