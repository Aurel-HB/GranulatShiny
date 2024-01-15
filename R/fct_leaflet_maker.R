#' leaflet_maker
#'
#' @description Creer la carte leaflet à partir du jeu de donnée mise en forme par la fonction preleaflet
#' create a leaflet map from a dataset created by the preleaflet function
#'
#' @param df dataframe
#' @param shape polygon
#'
#' @return a leaflet map
#'
#'

leaflet_maker <- function(df, shape) {

  campagne <- c(1)
  for (i in 2:nrow(df)) {
    if (df$Id_Operation[i] > df$Id_Operation[i - 1]) {
      campagne[i] <- campagne[i - 1]
    } else {
      campagne[i] <- campagne[i - 1] + 1
    }
  }

  if (is.null(shape)) {
    carte <- leaflet() %>% addTiles()
  } else {
    carte <-
      leaflet() %>% addTiles() %>% addPolygons(
        data = shape,
        opacity = 1,
        dashArray = "5,10",
        label = "Zone de la concession",
        labelOptions = labelOptions(textsize = "15px"),
        weight = 3,
        fillOpacity = 0.1,
        color = "black"
      )
  }
  for (y in 1:max(campagne)) {
    data <- df[which(campagne == y), ]
    for (i in 1:nrow(data)) {
      if (!is.na(data$impact_date_1[i])) {
        carte <-
          carte %>% addPolylines(
            lng = c(data$LongDeb[i], data$LongFin[i]) ,
            lat = c(data$LatDeb[i], data$LatFin[i]),
            label =  paste(
              data$Code_Station[i],
              ": Zone",
              data$zones[i],
              "Station impactée du",
              as.Date(data$impact_date_1[i]),
              "au",
              data$impact_date_2[i]
            ),
            labelOptions = labelOptions(textsize = "15px"),
            color = brewer.pal(n = 9, name = "Reds")[10 - y],
            opacity = 0.8,
            dashArray = c("5,10", "10,10", "5,5", "15,10", "5,15")[as.numeric(data$zones[i])]
          )
      } else {
        carte <-
          carte %>% addPolylines(
            lng = c(data$LongDeb[i], data$LongFin[i]) ,
            lat = c(data$LatDeb[i], data$LatFin[i]),
            label = paste(data$Code_Station[i], ": Station de référence"),
            labelOptions = labelOptions(textsize = "15px"),
            color = brewer.pal(n = 9, name = "Blues")[10 - y],
            opacity = 0.8
          )
      }
    }
  }
  carte <- carte %>% setView(lng = df["LongDeb"][1,1],
                             lat = df["LatDeb"][1,1], zoom = 11)
  carte
}
