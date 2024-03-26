#' export_data
#'
#' @description A fct function to export the rds data of the package in good
#' format for the app
#'
#' @param path string of th folder path
#' @param catch dataframe
#' @param operation dataframe
#' @param impact_polygon sf polygon
#'
#' @return No return value
#'
#'

export_data <- function(path, catch, operation, impact_polygon){
  ### export the files ####
  # export polygon ###
  # Set the output path and filename (adjust as needed)
  output_filename <- "impact_zone"

  # Write the polygon to shapefile
  st_write(impact_polygon, dsn = paste(path, "shapefile"), layer = output_filename, driver = "ESRI Shapefile", append=FALSE)

  # export csv ###
  write.csv(catch, paste(path,"catch.csv", sep =""), row.names=FALSE)
  write.csv(operation, paste(path,"operation.csv", sep =""), row.names=FALSE)
  #####

}
