![](inst/logos/hex-SoilManageR.png){width="150"}

# SoilManageR

## Introduction

This R package is a compilation of functions to calculate numerical agricultural soil management indicators from on a management timeline of an arable field. The functions can also be used independently of the management timeline to calculate some indicators or parts thereof. Througout the package, many assumptions were made (e.g. on time to crop establishment). These assumptions are, whenever possible, based on the literature that is cited within the function descriptions. The assumptions are considered to be representative for temperate agroecosystems, i.e. in the Swiss midlands.

![Fig. 1: Contents of the SoilManager package](vignettes/SoilManager_graphic.png){width="700"}

SoilManageR organizes management data in management data frames `management_df()`, from this the indicators can then be calculated. For the calculations there are main functions and helper functions that are called by the main functions (Fig. 1). Both type of functions rely on reference data for their calculations. The package is distributed with some supporting documents.

## Installation

You can install the current release version of SoilManageR from [CRAN](https://cran.r-project.org/) with:

``` r
install.packages("SoilManageR")
```


or you can install the development version of SoilManageR from [GitLab](https://about.gitlab.com/) with:

``` r
install.packages("devtools")
devtools::install_gitlab("SoilManageR/SoilManageR")
```

## Suggested Workflow

### Load and check management data frame

We suggest to start the workflow with importing a `management_df()` from a XLSX template.

```         
library(SoilManageR)

#define path to excel template in the SoilManageR package
internal_path <- "/extdata/SoilManageR_mgmt_data_template_V2.5.xlsx"

#create local path
path_to_xlsx_template <- system.file(internal_path, package = "SoilManageR")

#load management_df
mgmt_data <- management_df_from_excel(path_to_xlsx_template)
```

Then the `management_df()` can be checked for internal consistency with the `check_management_df()` function.

```         
#create management_df from example data (delivered with the package)
mgmt_data <- EXAMPLE_data

#check the consistency of the example data
check_management_df(mgmt_data)
```

### Calculate indicators

When the `management_df()` shows no issues, then all indicators can be calcualted with `calculate_indicators()`

```         
calculate_indicators(mgmt_data)
```

Alternatively, the indicators could be calculated individually

```         
C_input(mgmt_data)
tillage_intensity(mgmt_data)
soil_cover(mgmt_data)
plant_diversity(mgmt_data,2013,2023)
```

### Access helper functions

All helper functions can be called directly, please check the description of the functions for more information on them.

## About the Package

### Acknowledgments

This R package was developed by following the book [R Packages (2nd Edition)](https://r-pkgs.org/) by [Hadley Wickham](https://hadley.nz/) and [Jennifer Bryan](https://jennybryan.org/). Our work is based on the accomplishments of many others. In particular, we would like to thank Sonja Keel (Agroscope, Switzerland), Martin Bolinder (SLU, Sweden), Giulio Feruzzi (USDA-NRCS, USA), Lucie Büchi (University of Greenwich, United Kingdom) for their advice and support.

### Contact

For further information on SoilManageR please contact [olivier.heller\@agroscope.admin.ch](mailto:olivier.heller@agroscope.admin.ch) or [raphael.wittwer\@agroscope.admin.ch](mailto:raphael.wittwer@agroscope.admin.ch).

### Funding information

The development of this R package was supported by EJP SOIL.

[![](vignettes/EJP_SOIL.png){width="300"}](https://ejpsoil.eu/)

![](vignettes/EJP_H2020_EU.jpg){width="200"}
