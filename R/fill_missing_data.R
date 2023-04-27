#' Replace NA's in the load data set
#'
#' This function replaces NA values with the values of the data set one week ago at the same time point.
#' The function is mainly written to deal with small gaps in the data set obtained with get_entsoE_data(). If the function is to be used with other data sets
#' the input needs to contain the necessary data and comply with the column naming.
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
#' fill_missing_data("data_set_obtained_from_get_entsoE_data()_with_missing_values")
#'
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

