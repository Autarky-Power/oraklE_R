#' Decomposing the load data into long-, mid- and short-term component
#'
#' This function decomposes the load data into three components: a yearly long-term trend, a daily mid-term seasonality, and an hourly short-term seasonality. If the data is available only at a daily resolution, the calculation of hourly seasonality is skipped. The results of the decomposition are returned as a list of dataframes. The series are plotted additionally.
#'
#' @param load_data A dataframe object with "load", "date", "unit", and "country" columns
#' \item{load} consisting of the load values,
#' \item{date} consisting of the datetime values,
#' \item{unit} indicating the measured unit (e.g. MW),
#' \item{country} indicating the country's ISO2C code
#'
#' @return A list of three dataframes with
#' \item{longterm} dataframe of long-term trend with country, year and yearly average hourly demand.
#' \item{midterm} datafram of mid-term component with country, date, year, month, day, week day, average hourly demand and seasonal average hourly demand.
#' \item{shortterm} dataframe of short-term component with country, date, year, month, day, week day, hour, hourly demand and hourly demand trend and trend and season corrected.
#' @export
#' @import ggplot2
#'
#' @examples decomposed_load_example <- decompose_load_data(no_missing_data_example)

decompose_load_data <- function(load_data){


  resolution <- as.numeric(difftime(load_data$date[2], load_data$date[1],units="hours"))

  if (resolution <= 1){
    timepoint <- seq(as.POSIXct(paste0(as.character(min(unique(load_data$year))),'-01-01 00:00'),tz="UTC"),
                     as.POSIXct(paste0(as.character(max(unique(load_data$year))),'-12-31 23:00'),tz="UTC"),by="hour")
  } else{
    timepoint <- seq(as.POSIXct(paste0(as.character(min(unique(load_data$year))),'-01-01'),tz="UTC"),
                     as.POSIXct(paste0(as.character(max(unique(load_data$year))),'-12-31'),tz="UTC"),by="day")
  }


  if (is.character(load_data$time_interval)){
    intervals <- as.numeric(sub(" mins", "", load_data$time_interval))
    load_data$time_interval <- as.difftime(intervals, units = "mins")
  }


  ordered_data <- as.data.frame(timepoint)
  colnames(ordered_data)<- "date"
  ordered_data$year <- lubridate::year(ordered_data$date)
  ordered_data$month <- lubridate::month(ordered_data$date)
  ordered_data$day <- lubridate::day(ordered_data$date)
  ordered_data$wday <- lubridate::wday(ordered_data$date,label = T,locale = "English")
  ordered_data$load <-0
  years= unique(load_data$year)
  suppressWarnings(
    if (resolution <=1){
      ordered_data$hour <- lubridate::hour(ordered_data$date)
      for (year in years){

      if(load_data$time_interval[load_data$year==year][1] == 15){
        ordered_data$load[ordered_data$year==year] <- colMeans(matrix(load_data$load[load_data$year==year], nrow=4))
      }
      if(load_data$time_interval[load_data$year==year][1] == 30){
        ordered_data$load[ordered_data$year==year] <- colMeans(matrix(load_data$load[load_data$year==year], nrow=2))
      }
      if(load_data$time_interval[load_data$year==year][1] == 60){
        ordered_data$load[ordered_data$year==year] <- load_data$load[load_data$year==year]
      } }
      }else{ordered_data$load <- load_data$load}
  )
  if ("unit" %in% colnames(load_data)){
    ordered_data$unit <- unique(load_data$unit)}

  ordered_data$country <- unique(load_data$country)

  all_data <- ordered_data
  all_data <- all_data[! (all_data$month==2 & all_data$day==29),]

  longterm <- data.frame(matrix(nrow=length(unique(all_data$year)),ncol=3))
  colnames(longterm)<- c("country","year","avg_hourly_demand")
  longterm$year <- unique(all_data$year)
  country=unique(all_data$country)
  longterm$country<- country
  for (i in (min(longterm$year):max(longterm$year))){
    longterm$avg_hourly_demand[longterm$year==i]<- mean(all_data$load[all_data$year==i],na.rm = T)
  }
  if (resolution <= 1){
    midterm <- data.frame(matrix(nrow=(length(unique(all_data$year))*365),ncol=7))
    colnames(midterm)<- c("country","date","year","month","day","wday","avg_hourly_demand")

    for (i in 1:length(unique(all_data$year))){
      midterm$year[((i-1)*365+1):(i*365)] <- unique(all_data$year)[i]
    }

    for (i in 1:nrow(midterm)){
      midterm$date[i] <- all_data$date[((i-1)*24+1)]
      midterm$month[i] <- all_data$month[((i-1)*24+1)]
      midterm$day[i] <- all_data$day[((i-1)*24+1)]
      midterm$wday[i] <- all_data$wday[((i-1)*24+1)]
      midterm$avg_hourly_demand[i] <- mean(all_data$load[((i-1)*24+1):(i*24)],na.rm = T)
    }
    midterm$date <- as.POSIXct(midterm$date, format="%Y-%m-%d",origin = "1970-01-01")
    midterm$date <-as.Date(midterm$date, format="%Y-%m-%d","UTC")
    midterm$country<- country}else{
      midterm <- data.frame(matrix(nrow=nrow(ordered_data),ncol=7))
      midterm[,1:7] <- ordered_data[,c(8,1:6)]
      colnames(midterm)<- c("country","date","year","month","day","wday","avg_hourly_demand")
    }

  midterm$seasonal_avg_hourly_demand <-0
  for (i in min(all_data$year):max(all_data$year)){
    midterm$seasonal_avg_hourly_demand[midterm$year==i] <- midterm$avg_hourly_demand[midterm$year==i]-
      longterm$avg_hourly_demand[longterm$year==i]
  }


  if (resolution <= 1){
    shortterm <- data.frame(matrix(nrow=(nrow(all_data)),ncol=1))
    colnames(shortterm) <- "country"

    shortterm$country <- country
    shortterm$date <- all_data$date
    shortterm$year <- all_data$year
    shortterm$month <- all_data$month
    shortterm$day <- all_data$day
    shortterm$wday <- all_data$wday
    shortterm$hour <- all_data$hour
    shortterm$hourly_demand <- all_data$load
    shortterm$hourly_demand_trend_corrected <- 0

    #
    shortterm$yearly <- 0
    shortterm$daily <- 0
    for (i in min(all_data$year):max(all_data$year)){
      shortterm$yearly[shortterm$year==i] <- longterm$avg_hourly_demand[longterm$year==i]
      shortterm$hourly_demand_trend_corrected[shortterm$year==i] <- shortterm$hourly_demand[shortterm$year==i]-
        longterm$avg_hourly_demand[longterm$year==i]
    }

    shortterm$hourly_demand_trend_and_season_corrected <- 0


    for (i in 1:(nrow(midterm))){
      shortterm$daily[((i-1)*24+1):(i*24)]<- midterm$seasonal_avg_hourly_demand[i]
      shortterm$hourly_demand_trend_and_season_corrected[((i-1)*24+1):(i*24)] <-
        shortterm$hourly_demand_trend_corrected[((i-1)*24+1):(i*24)]- midterm$seasonal_avg_hourly_demand[i]
    }
    shortterm_seasonality_plot <-  ggplot(shortterm)+geom_line(aes(1:nrow(shortterm),hourly_demand_trend_and_season_corrected, color="Average hourly demand"),linewidth=1.1)+
      theme(legend.title = element_blank()) +ggtitle('Short-term seasonality \n')+
      theme(plot.title = element_text(hjust = 0.5))+ylab("MW")+xlab("Hour")
  }

  trend_plot<- ggplot(longterm)+geom_line(aes(year,avg_hourly_demand, color="Average hourly demand"),linewidth=1.1)+
    theme(legend.title = element_blank()) +ggtitle('Long term trend \n')+
    theme(plot.title = element_text(hjust = 0.5))+ylab("MW")

  midterm_seasonality_plot <-  ggplot(midterm)+geom_line(aes(1:nrow(midterm),seasonal_avg_hourly_demand, color="Average hourly demand"),linewidth=1.1)+
    theme(legend.title = element_blank()) +ggtitle('Mid-term seasonality \n')+
    theme(plot.title = element_text(hjust = 0.5))+ylab("MW")+xlab("Day")


  if (resolution <= 1){
    all_plots <- patchwork::wrap_plots(trend_plot,midterm_seasonality_plot,shortterm_seasonality_plot,ncol=1)
    print(all_plots)

    return(list("longterm"=longterm, "midterm"=midterm, "shortterm"=shortterm))
  } else{
    all_plots <- patchwork::wrap_plots(trend_plot , midterm_seasonality_plot, ncol=1)
    print(all_plots)

    return(list("longterm"=longterm, "midterm"=midterm))
  }
}
