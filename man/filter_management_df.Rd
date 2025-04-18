% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_management_df.R
\name{filter_management_df}
\alias{filter_management_df}
\title{Filter management_df for pattern in comments}
\usage{
filter_management_df(var_MGMT_data, filter_pattern)
}
\arguments{
\item{var_MGMT_data}{management_df}

\item{filter_pattern}{string based on which the lines should be excluded}
}
\value{
a filtered management_df
}
\description{
Excludes operations (lines) from management_df based on a character pattern
 in the comments column.
}
\examples{
# filter EXAMPLE_data and exclude all lines that contain "UFA 330" in comments
filter_management_df(EXAMPLE_data,"UFA 330")

}
