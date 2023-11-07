#' Title
#'
#' @param longterm_predictions
#' @param end_year
#'
#' @return
#' @export
#'
#' @examples
long_term_future <- function(longterm_predictions,end_year){

  weo_data_set <- oRaklE::weo_data

  country<- countrycode::countrycode(unique(longterm_predictions$country),origin = "iso2c",destination = "country.name")
  country_subset <-  weo_data_set[weo_data_set$Country==country, ]
  colnames(country_subset)[10:58] <- 1980:2028


  new_data_length <- end_year-max(longterm_predictions$year)
  new_rows <- as.data.frame(matrix(nrow=new_data_length,ncol = ncol(longterm_predictions) ))
  colnames(new_rows) <- colnames(longterm_predictions)
  longterm_predictions = rbind(longterm_predictions, new_rows)

  first_year <- min(longterm_predictions$year,na.rm = T)
  start_year <- max(longterm_predictions$year,na.rm = T)+1
  col_year_first <- which(colnames(country_subset)==as.character(first_year))
  col_year_start <- which(colnames(country_subset)==as.character(start_year))
  col_year_end   <- which(colnames(country_subset)==as.character(end_year))
  longterm_predictions$GDP_growth_weo <- 0
  longterm_predictions$GDP_deflator_weo <- 0
  longterm_predictions$population_growth_weo <-0

  ngdp_r <- as.numeric(country_subset[1,col_year_first:col_year_end])
  ngdp_d <- as.numeric(country_subset[2,col_year_first:col_year_end])
  pl     <- as.numeric(country_subset[3,col_year_first:col_year_end])


  for (i in 2:nrow(longterm_predictions)){
    longterm_predictions$GDP_growth_weo[i] <- (ngdp_r[i]/ngdp_r[(i-1)]-1)*100
    longterm_predictions$GDP_deflator_weo[i] <- (ngdp_d[i]/ngdp_d[(i-1)]-1)*100
    longterm_predictions$population_growth_weo[i] <- (pl[i]/pl[(i-1)]-1)*100
  }

  new_row_start <- (which(longterm_predictions$year==max(longterm_predictions$year,na.rm = T))+1)
  longterm_predictions$year[new_row_start:nrow(longterm_predictions)] <- (max(longterm_predictions$year,na.rm = T)+1):end_year
  longterm_predictions$country <- unique(longterm_predictions$country)[1]
  longterm_predictions$test_set_steps <- unique(longterm_predictions$test_set_steps)[1]

  longterm_predictions$GDP_deflator[new_row_start:nrow(longterm_predictions)] <- longterm_predictions$GDP_deflator_weo[new_row_start:nrow(longterm_predictions)]
  longterm_predictions$GDP_growth[new_row_start:nrow(longterm_predictions)] <- longterm_predictions$GDP_growth_weo[new_row_start:nrow(longterm_predictions)]

  for (i in new_row_start:nrow(longterm_predictions)){
    longterm_predictions$GDP[i] <- longterm_predictions$GDP[(i-1)]*(1+longterm_predictions$GDP_growth_weo[i]/100)
    longterm_predictions$population[i] <- longterm_predictions$population[(i-1)]*(1+longterm_predictions$population_growth_weo[i]/100)
  }

  for (i in new_row_start:nrow(longterm_predictions)){
    longterm_predictions$industrial_value_added[i] <-  mean(longterm_predictions$industrial_value_added[(i-3):(i-1)]/longterm_predictions$GDP[(i-3):(i-1)]) * longterm_predictions$GDP[i]
    longterm_predictions$manufacturing_value_added[i] <-       mean(longterm_predictions$manufacturing_value_added[(i-3):(i-1)]/longterm_predictions$GDP[(i-3):(i-1)]) * longterm_predictions$GDP[i]
    longterm_predictions$service_value_added[i] <-  mean(longterm_predictions$service_value_added[(i-3):(i-1)]/longterm_predictions$GDP[(i-3):(i-1)]) * longterm_predictions$GDP[i]
    longterm_predictions$household_consumption_expenditure[i] <-  mean(longterm_predictions$household_consumption_expenditure[(i-3):(i-1)]/longterm_predictions$GDP[(i-3):(i-1)]) * longterm_predictions$GDP[i]
    longterm_predictions$GNI[i] <-  mean(longterm_predictions$GNI[(i-3):(i-1)]/longterm_predictions$GDP[(i-3):(i-1)]) * longterm_predictions$GDP[i]
  }

  rural_model <- lm(rural_population~ year, data = longterm_predictions[1:(new_row_start-1),])
  longterm_predictions$rural_population[new_row_start:nrow(longterm_predictions)]<-predict(rural_model,newdata = longterm_predictions[new_row_start:nrow(longterm_predictions),] )

  for (i in 1:3){
    model_path= paste0("./", unique(longterm_predictions$country),"/models/longterm/best_lm_model",i,".Rdata")
    load(model_path)
    prediction_column <- which(colnames(longterm_predictions)==paste0("longterm_model_predictions",i))
    longterm_predictions[new_row_start:nrow(longterm_predictions),prediction_column] <- predict(best_lm_model,newdata = longterm_predictions)[new_row_start:nrow(longterm_predictions)]
  }

  write.csv(longterm_predictions,"./data/longterm_future_predictions.csv",row.names = F)

  intercept <- longterm_predictions$year[(new_row_start-1)]-unique(longterm_predictions$test_set_steps)
  lt_plot <- ggplot(longterm_predictions)+geom_line(aes(year,avg_hourly_demand,color="actual"),lwd=1)+
    geom_line(aes(year,longterm_model_predictions1,color="Model1"))+
    geom_line(aes(year,longterm_model_predictions2,color="Model2"))+
    geom_line(aes(year,longterm_model_predictions3,color="Model3"))+
    xlab("\nYear")+ylab("Avg Hourly Demand\n [MW]\n")+
    geom_vline(xintercept=intercept,linetype=2)+
    ggthemes::theme_foundation(base_size=14, base_family="sans")+
    xlab("\nYear")+ylab("Avg Hourly Demand\n [MW]\n")+
    ggtitle(paste("Long Term Model Results -",unique(longterm_predictions$country)))+
    theme(plot.title = element_text(face = "bold",
                                    size = rel(1.2), hjust = 0.5),
          plot.subtitle = element_text(size = rel(1), hjust = 0.5),
          text = element_text(),
          panel.background = element_rect(colour = NA),
          plot.background = element_rect(colour = NA),
          panel.border = element_rect(colour = NA),
          axis.title = element_text(face = "bold",size = rel(1)),
          axis.title.y = element_text(angle=90,vjust =2),
          axis.title.x = element_text(vjust = -0.2),
          axis.text = element_text(),
          axis.line.x = element_line(colour="black"),
          axis.line.y = element_line(colour="black"),
          axis.ticks = element_line(),
          panel.grid.major = element_line(colour="#f0f0f0"),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = NA),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.size= unit(0.2, "cm"),
          plot.margin=unit(c(10,5,5,5),"mm"),
          strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
          strip.text = element_text(face="bold"))+
    theme(legend.title = element_blank())

  suppressWarnings(print(lt_plot))



  lt_plot2 <- ggplot(longterm_predictions)+geom_line(aes(year,avg_hourly_demand,color="actual"),lwd=1)+
    geom_line(aes(year,longterm_model_predictions1,color="Model1"))+
    geom_line(aes(year,longterm_model_predictions2,color="Model2"))+
    geom_line(aes(year,longterm_model_predictions3,color="Model3"))+
    xlab("\nYear")+ylab("Avg Hourly Demand\n [MW]\n")+
    geom_vline(xintercept=intercept,linetype=2)+
    ggthemes::theme_foundation(base_size=14, base_family="sans")+
    xlab("\nYear")+ylab("Avg Hourly Demand\n [MW]\n")+
    ggtitle(paste("Long Term Model Results -",unique(longterm_predictions$country)))+
    theme(plot.title = element_text(face = "bold",
                                    size = rel(1.2), hjust = 0.5),
          plot.subtitle = element_text(size = rel(1), hjust = 0.5),
          text = element_text(),
          panel.background = element_rect(colour = NA),
          plot.background = element_rect(colour = NA),
          panel.border = element_rect(colour = NA),
          axis.title = element_text(face = "bold",size = rel(1)),
          axis.title.y = element_text(angle=90,vjust =2),
          axis.title.x = element_text(vjust = -0.2),
          axis.text = element_text(),
          axis.line.x = element_line(colour="black"),
          axis.line.y = element_line(colour="black"),
          axis.ticks = element_line(),
          panel.grid.major = element_line(colour="#f0f0f0"),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = NA),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.size= unit(0.2, "cm"),
          plot.margin=unit(c(10,5,5,5),"mm"),
          strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
          strip.text = element_text(face="bold"))+
    theme(legend.title = element_blank())+
    theme(axis.title=element_text(size=23))+
    theme(legend.text=element_text(size=23))+
    theme(axis.text=element_text(size=20))+
    theme(plot.title = element_text(size=26))

  suppressWarnings(
    ggsave(file=paste0("./",unique(longterm_predictions$country),"/plots/Long_term_results_future.png"), plot=lt_plot2, width=12, height=8)
  )

  longterm_predictions_future <- longterm_predictions

  return(longterm_predictions_future)

}
