% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_soil_cover_tibble.R
\name{calculate_soil_cover_tibble}
\alias{calculate_soil_cover_tibble}
\title{Caculate soil cover tibble}
\usage{
calculate_soil_cover_tibble(var_MGMT_data)
}
\arguments{
\item{var_MGMT_data}{a \code{management_df} that contains the management information}
}
\value{
object of the class \code{soil_cover_tibble} with daily resolution is returned.
}
\description{
The function takes a \code{management_df} as input and returns a \code{soil_cover_tibble} with daily resolution
of the estimated soil cover.
}
\details{
The function is mainly a helper function for the \code{\link[=soil_cover]{soil_cover()}} function.
}
\examples{
\donttest{
  #example that returns a soil_cover_tibble
  calculate_soil_cover_tibble(EXAMPLE_data)
}

}
\seealso{
\itemize{
\item \code{\link[=calculate_indicators]{calculate_indicators()}} to calculate all management indicators
for a \code{management_df}
\item \code{\link[=soil_cover]{soil_cover()}} aggregates the soil cover information
\item \code{\link[=plant_cover]{plant_cover()}} for more detail on the plant cover function
\item \code{\link[=plot.soil_cover_tibble]{plot.soil_cover_tibble()}} for plotting the \code{soil_cover_tibble}
\item \code{STIR_values_LUT} for tillage operation specific burial coefficients
}
}
