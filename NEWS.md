# SoilManageR 1.1.0

* Integrated undersowing into soil_cover(), cover crops can now overlap with crops. However, crops can not overlap with each other (no intercropping)
* Updated the management data template (V2.6), incl. new operations (harvest_cover_crop, total_herbicide, frost_kill_cover_crop)
* Updated the way the C input by cover crops is calculated (now goes to 0 for short term cover crops)
* New indicator to count pesticide applications (count_pesticide_applications())
* New function to calculate management indicators by crop (calculate_indicators_by_crop())
* New function to calculate indicators by a specific date (indicators_by_date())
* Added specific color for cover crops into plot.soil_cover_tibble()
* PDI (plant diversity indicator) was renamed into CDI (crop diversity indicator)
* Updated Vignette and ReadMe files


# SoilManageR 1.0.1

* Initial release version of the SoilManageR

