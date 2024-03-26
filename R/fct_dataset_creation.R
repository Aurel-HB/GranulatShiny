#' dataset_creation
#'
#' @description A fct function
#'   #creation of a fictional dataset of granulate concession following
#' there are 3 species found
#' a list of 3 data frame, 3 shape file (shp, shx, dbf) :
#'  data_catch give information of the sample
#'  data_operation give information of the concession
#'  save_input give information about the ref and impact stations
#'  shape files give the delimitation of the concession
#'  don't care between continuous or discrete distribution because on boat
#'  number is an estimation and sometimes there is relative number
#'
#'
#'  The number of species per (station,season,year) is determined randomly
#'  by probability distribution :
#' species 1 no effect####
#' rnbinom(720, mu=100, size = 1.1)
#' ####
#' species 2 processing effect####
#'  less variance and it's possible to have several 0
#'  rnbinom(1, mu=20, size = 3)
#' to model the effect when process we keep only value < 15
#' rnbinom(1, mu=20, size = 3) < 15
#' ####
#' species 3  season effect and interaction processing*season####
#' less variance and it's possible to have several 0
#' rnbinom(1, mu=100, size = 1)
#' the gamma have high variance, no 0 and high value
#' it is easiest to control an increase than with the lnorm
#' if "summer" = rgamma(1, shape = 2, scale = 300)
#' if "winter" = rgamma(1, shape = 2, scale = 50)
#' #same for spring and autumn = rgamma(1, shape = 2, scale = 150)
#' ####
#' species 4 no effect but big variation pop dynamics
#' rnbinom(1, mu=100, size = 0.1)
#'
#' Some parameters needed in the standard format of the app have been fixed
#' the distance of one station sample is fixed at 1.2e-5
#' the impact area size is fixed to polygon of 1 latitude per 1 longitude
#' the date use the format %d%m%Y %H%M and the survey are (01/01, 01/04, 01/07,
#'  01/10) of each year
#'
#' There are 20 stations with 10 randomly choose in the impact area and
#' 10 randomly choose outside the impact zone but inside the sample zone
#'
#' @param start_year numeric corresponding to the year of initial statement
#' @param end_year numeric corresponding to the end of the values series
#' @param nb_year numeric corresponding to total number of year including survey
#' @param start_lat numeric : latitude of the corner of the impact area
#' @param start_long numeric : longitude of the corner of the impact area
#'
#' @return A list composed of 2 dataframe and 1 polygon.
#'
#'

dataset_creation <- function(start_year = 2000, end_year = 2030, nb_year = 9, start_lat = 44, start_long = -2){

  #dataframe skeleton ####
  catch <- data.frame("Campagne" = rep(NA, 1),"Annee" = rep(NA, 1),
                      "Trait" = rep(NA, 1),"Nom_Scientifique" = rep(NA, 1),
                      "Code_Campagne" = rep(NA, 1),"Nombre" = rep(NA, 1),
                      "Poids" = rep(NA, 1),"Pmoy" = rep(NA, 1),
                      "longueurmoy" = rep(NA, 1),"DateDeb" = rep(NA, 1),
                      "LatDeb" = rep(NA, 1),"LongDeb" = rep(NA, 1),
                      "DateFin" = rep(NA, 1),"LatFin" = rep(NA, 1),
                      "LongFin" = rep(NA, 1))

  operation <- data.frame("Annee" = rep(NA, 1),"Serie" = rep(NA, 1),
                          "Serie_Partielle" = rep(NA, 1),"Code_Station" = rep(NA, 1),
                          "Id_Operation" = rep(NA, 1),"DateDeb" = rep(NA, 1),
                          "LatDeb" = rep(NA, 1),"LongDeb" = rep(NA, 1),
                          "DateFin" = rep(NA, 1),"LatFin" = rep(NA, 1),
                          "LongFin" = rep(NA, 1),"Distance" = rep(NA, 1))


  #####

  #stations ####
  num <- c(1:10)
  station_impact <- c()
  station_ref <- c()
  for (i in num){
    station_impact <- c(station_impact, paste("H",i,sep=""))
    station_ref <- c(station_ref, paste("B",i,sep=""))
  }
  stations <- rep(c(station_impact, station_ref), 36)
  #####

  #year ####
  year <- c(rep(start_year-2,80), rep(start_year-1,80),
            rep(start_year,80), rep(start_year+5,80),
            rep(start_year+10,80), rep(start_year+15,80),
            rep(start_year+20,80), rep(start_year+25,80), rep(start_year+30,80))
  #####

  #season ####
  season <- rep(c(rep("winter",20),rep("spring",20), rep("summer",20), rep("autumn",20)), nb_year)
  ####

  #campagne ####
  campagne <- c()
  for (i in 1:(nb_year*4)){
    campagne <- c(campagne, rep(i,20))
  }
  #

  # match season, year, catch ####
  #c1 = catch_species1
  c1 <- data.frame(year, season, stations, campagne,
                   "nombre" = rnbinom(720, mu=100, size = 1.1))
  c2 <- data.frame(year, season, stations, campagne, "nombre" = rep(0,720))
  c3 <- data.frame(year, season, stations, campagne, "nombre" = rep(0,720))
  c4 <- data.frame(year, season, stations, campagne,
                   "nombre" = rnbinom(720, mu=100, size = 0.1))

  for (indice in 1:720){
    ## test for s2##
    if (grepl("H", c2$stations[indice]) & c2$year[indice] >= start_year){
      value <- rnbinom(1, mu=20, size = 3)
      while (value > 15) {
        value <- rnbinom(1, mu=20, size = 3)
      }
      c2$nombre[indice] <- value
    } else {
      c2$nombre[indice] <- rnbinom(1, mu=20, size = 3)
    }

    ## test for s3##
    if (grepl("H", c3$stations[indice]) & c3$year[indice] >= start_year){
      c3$nombre[indice] <- rnbinom(1, mu=100, size = 1)
    } else {
      if (c3$season[indice] == "summer"){
        c3$nombre[indice] <- round(rgamma(1, shape = 2, scale = 300), digits = 2)
      } else   if (c3$season[indice] == "winter"){
        c3$nombre[indice] <- round(rgamma(1, shape = 2, scale = 50), digits = 2)
      } else { #same for spring and autumn
        c3$nombre[indice] <- round(rgamma(1, shape = 2, scale = 150), digits = 2)
      }
    }
  }

  #####

  # name, weight and date ####

  DateDeb <- c()
  DateFin <- c()
  for (indice in 1:720){
    if (c3$season[indice] == "summer"){
      DateDeb <- c(DateDeb, paste("01/07/", c3$year[indice], " 00:00", sep = ""))
      DateFin <- c(DateFin, paste("01/07/", c3$year[indice], " 00:00", sep = ""))
    } else   if (c3$season[indice] == "autumn"){
      DateDeb <- c(DateDeb, paste("01/10/", c3$year[indice], " 00:00", sep = ""))
      DateFin <- c(DateFin, paste("01/10/", c3$year[indice], " 00:00", sep = ""))
    } else   if (c3$season[indice] == "winter"){
      DateDeb <- c(DateDeb, paste("01/01/", c3$year[indice], " 00:00", sep = ""))
      DateFin <- c(DateFin, paste("01/01/", c3$year[indice], " 00:00", sep = ""))
    } else {
      DateDeb <- c(DateDeb, paste("01/04/", c3$year[indice], " 00:00", sep = ""))
      DateFin <- c(DateFin, paste("01/04/", c3$year[indice], " 00:00", sep = ""))
    }
  }

  c1 <- data.frame(c1, "Nom_Scientifique"= c("Cephalaspis.tenuicornis"),
                   "Poids" = c1$nombre*1, DateDeb, DateFin)
  c2 <- data.frame(c2, "Nom_Scientifique"= c("Dimichtys.terreli"),
                   "Poids" = c2$nombre*2, DateDeb, DateFin)
  c3 <- data.frame(c3, "Nom_Scientifique"= c("Leedsischthys.problematicus"),
                   "Poids" = c3$nombre*10, DateDeb, DateFin)
  c4 <- data.frame(c4, "Nom_Scientifique"= c("Latimeria.chalumnae"),
                   "Poids" = c3$nombre*0.5, DateDeb, DateFin)



  #####

  # creation of the geographic area ####
  # the example is in the bay of biscay
  # Define the coordinates of the polygon vertices
  area_coords <- matrix(c(
    c(-2, -1),
    c(-2, 2),
    c(2, 2),
    c(2, -1),
    c(-2, -1)
  ), ncol = 2, byrow = TRUE)

  # Create a simple polygon
  area_polygon <- st_sf(geometry = st_sfc(st_polygon(list(area_coords))))
  # now we change the start point and apply in all the polygon
  area_polygon[[1]][[1]][[1]][,1] <- area_polygon[[1]][[1]][[1]][,1]/20 + start_long
  area_polygon[[1]][[1]][[1]][,2] <- area_polygon[[1]][[1]][[1]][,2]/20 + start_lat

  # Define the coordinates of the polygon vertices
  concession_coords <- matrix(c(
    c(0, 0),
    c(0, 1),
    c(1, 1),
    c(1, 0),
    c(0, 0)
  ), ncol = 2, byrow = TRUE)
  # Create a simple polygon
  impact_polygon <- st_sf(geometry = st_sfc(st_polygon(list(concession_coords))))
  # now we change the start point and apply in all the polygon
  impact_polygon[[1]][[1]][[1]][,1] <- impact_polygon[[1]][[1]][[1]][,1]/20 + start_long
  impact_polygon[[1]][[1]][[1]][,2] <- impact_polygon[[1]][[1]][[1]][,2]/20 + start_lat


  # station position
  map_station <- data.frame("trait" = c(station_impact, station_ref),
                            "LatDeb" = c(NA), "LatFin" = c(NA),
                            "LongDeb" = c(NA), "LongFin" = c(NA))

  min_lat_impact <- min(impact_polygon[[1]][[1]][[1]][,2])
  min_long_impact <- min(impact_polygon[[1]][[1]][[1]][,1])
  max_lat_impact <- max(impact_polygon[[1]][[1]][[1]][,2])
  max_long_impact <- max(impact_polygon[[1]][[1]][[1]][,1])

  # creation of the geographic position of the station randomly in the zone
  for (g in 1:10){
    # impact zone ####
    map_station$LongDeb[g] <- runif(n=1, min= min_long_impact +0.01,
                                    max= max_long_impact -0.01)
    map_station$LatDeb[g] <- runif(n=1, min= min_lat_impact +0.01,
                                   max= max_lat_impact -0.01)

    map_station$LongFin[g] <- map_station$LongDeb[g] + sample(c(-0.02, 0.02), 1)
    map_station$LatFin[g] <- map_station$LatDeb[g] + sample(c(-0.01, 0.01, 0,0), 1)

    # area zone ####
    map_station$LongDeb[g+10] <- runif(n=1, min= min(area_polygon[[1]][[1]][[1]][,1]) +0.02,
                                       max= max(area_polygon[[1]][[1]][[1]][,1]) -0.02)
    map_station$LatDeb[g+10] <- runif(n=1, min= min(area_polygon[[1]][[1]][[1]][,2]+0.02),
                                      max= max(area_polygon[[1]][[1]][[1]][,2])-0.02)
    #check if the point is in the impact zone ####
    while (with(map_station,
                min_long_impact-0.02<= map_station$LongDeb[g+10] &
                max_long_impact+0.02 >= map_station$LongDeb[g+10]) &&
           with(map_station,
                min_lat_impact-0.02<= map_station$LatDeb[g+10] &
                max_lat_impact+0.02 >= map_station$LatDeb[g+10])) {
      map_station$LongDeb[g+10] <- runif(n=1, min= min(area_polygon[[1]][[1]][[1]][,1]+0.02),
                                         max= max(area_polygon[[1]][[1]][[1]][,1])-0.02)
      map_station$LatDeb[g+10] <- runif(n=1, min= min(area_polygon[[1]][[1]][[1]][,2]+0.02),
                                        max= max(area_polygon[[1]][[1]][[1]][,2])-0.02)
    }
    map_station$LongFin[g+10] <- map_station$LongDeb[g+10] + sample(c(-0.02, 0.02), 1)
    map_station$LatFin[g+10] <- map_station$LatDeb[g+10] + sample(c(-0.01, 0.01, 0,0), 1)
  }


  #completion of the catch and operation dataframe #####
  c1 <- merge(map_station, c1, by.x = "trait", by.y = "stations")
  c2 <- merge(map_station, c2, by.x = "trait", by.y = "stations")
  c3 <- merge(map_station, c3, by.x = "trait", by.y = "stations")
  c4 <- merge(map_station, c4, by.x = "trait", by.y = "stations")

  tot <- rbind(c1,c2,c3,c4)

  ####
  catch <- data.frame("Serie" = rep("SHINY", length(tot[,1])),"Annee" = tot$year,
                      "Code_Station" = tot$trait,"Nom_Scientifique" = tot$Nom_Scientifique,
                      "Serie_Partielle" = tot$campagne,"Nombre" = tot$nombre,
                      "Poids" = tot$Poids,"Pmoy" = rep(NA, length(tot[,1])),
                      "longueurmoy" = rep(NA, length(tot[,1])),"DateDeb" = tot$DateDeb,
                      "LatDeb" = tot$LatDeb,"LongDeb" = tot$LongDeb,
                      "DateFin" = tot$DateFin,"LatFin" = tot$LatFin,
                      "LongFin" = tot$LongFin)

  ####
  Code_station <- data.frame("station" =c(station_impact,station_ref),
                             "code" =c(1:20))

  optot <- merge(c1, Code_station, by.x = "trait", by.y = "station")

  serie <- data.frame("season"=c("winter", "spring", "summer","autumn"),
                      "serie" = c(1,2,3,4))
  optot <- merge(optot, serie)

  operation <- data.frame("Annee" = optot$year,"Serie" = c("Campagne SHINY"),
                          "Serie_Partielle" = optot$serie,
                          "Code_Station" = optot$trait,
                          "Id_Operation" = optot$code,
                          "DateDeb" = optot$DateDeb,
                          "LatDeb" = optot$LatDeb,"LongDeb" = optot$LongDeb,
                          "DateFin" = optot$DateFin,"LatFin" = optot$LatFin,
                          "LongFin" = optot$LongFin,"Distance" = c(1200))


return(list(catch, operation, impact_polygon))

}
