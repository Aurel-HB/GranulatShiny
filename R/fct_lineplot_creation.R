#' lineplot_creation
#'
#' @description A fct function that create lineplot using the dataframe
#' created by the function diversity_table
#' It represents the indicators from the diversity_table data in lineplot
#' with the standard error associated
#' The plot is just one survey
#' The indicators are (Abun, Biom, Richness, Shannon, Simpson)
#'
#'  @param data dataframe
#'  @param indicator character
#'  @param campagne character
#'
#' @return The return value is a lineplot of the indicator chosen
#'
#' @export

lineplot_creation <- function(data, indicator, campagne){
  #First prepare the data
  data <- data %>% dplyr::filter(ID_campagne == campagne)
  name_indic <- names(data)
  position <- grep(indicator, name_indic)
  data <- data[,position]
  value <- c()
  sd <- c()
  for(i in 1:length(names(data))){
    if(grepl("mean", names(data)[i])){
      value <- c(value, data[1,i])
    } else {
      sd <- c(sd, data[1,i])
    }
  }
  lineplot_data <- data.frame(factor(c("int", "ext", "tot"),
                                     c("int", "ext", "tot")),value, sd)
  names(lineplot_data) <- c("loc", "value", "sd")

  #add the limits errorbar
  lineplot_data$sd_min <- lineplot_data$value - lineplot_data$sd
  lineplot_data$sd_max <- lineplot_data$value + lineplot_data$sd

  #delete the negative value
  for (i in 1:nrow(lineplot_data)){
    if(lineplot_data$sd_min[i] < 0){
      lineplot_data$sd_min[i] <- 0
    }
  }

  # Default line plot
  p<- ggplot(lineplot_data, aes(x=loc, y=value, color=loc)) +
    geom_point()+
    geom_errorbar(aes(ymin=sd_min, ymax=sd_max), width=.2,
                  position=position_dodge(0.05))+
    # Finished line plot
    labs(title=paste("Average", indicator, "per location", sep =" "), x="location", y = indicator)+
    theme_classic()
  return(p)
}
