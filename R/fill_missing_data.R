#' Replace missing values in the load data set
#'
#' The purpose of this function is to substitute missing values with the corresponding values from the dataset, exactly one week prior at the same time point. For example, if there is no load value available for May 12th at 20:00, the value recorded on May 7th at 20:00 will be used as a replacement.
#' This function is primarily designed to handle minor gaps in the dataset acquired using the "get_entsoE_data()" function. To use this function with other datasets, it is important that the input data frame adheres to the required column naming conventions.

#' @param load_data Data Frame with load data. Data Frame must contain the following columns:
#' "date" consisting of the datetime values,
#' "load" consisting of the load values,
#' "unit" indicating the measured unit (e.g. MW),
#' "country" indicating the country's ISO2C code
#'
#' @return Data Frame with filled up load values, date, unit, year, time resolution, ISO2C Country Code
#' @export
#'
#' @examples
#' nrow(example_load_data)
#' example_load_data[858:862,]
#' no_missing_data_example <- fill_missing_data(example_load_data)
#' nrow(no_missing_data_example)
#' no_missing_data_example[858:876,]

fill_missing_data <- function(load_data){

  timepoint <- seq(as.POSIXct(paste0(as.character(min(unique(lubridate::year(load_data$date)))),'-01-01 00:00'),tz="UTC"),
                   as.POSIXct(paste0(as.character(max(unique(lubridate::year(load_data$date)))),'-12-31 23:00'),tz="UTC"),
                   by=difftime(load_data$date[2],load_data$date[1]))

  complete_data <- as.data.frame(timepoint)
  colnames(complete_data)<- "date"

  complete_data$load <- 0
  complete_data$load[complete_data$date %in% load_data$date] <- load_data$load
  missing_data_index <- as.numeric(row.names(complete_data[which(complete_data$load==0),]))
  interval_minutes <- as.numeric(difftime(load_data$date[2],load_data$date[1],units="mins"))
  interval_one_week_ago <- 60/interval_minutes*24*7
  complete_data$load[missing_data_index]<- complete_data$load[missing_data_index - interval_one_week_ago]
  if ("unit" %in% colnames(load_data)){
  complete_data$unit <- unique(load_data$unit)}
  complete_data$year <- lubridate::year(complete_data$date)
  complete_data$time_interval <- difftime(complete_data$date[2],complete_data$date[1],units = "mins")
  complete_data$country <- unique(load_data$country)
  country <- unique(load_data$country)
  if (! file.exists(country)){
    dir.create(country)}
  if (! file.exists(paste0("./",country,"/data"))){
    dir.create(paste0("./",country,"/data"))}

  write.csv(complete_data,paste0("./",country,"/data/filled_load_data.csv"),row.names = F)


  return (complete_data)
}


