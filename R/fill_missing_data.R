#' Title
#'
#' @param entsoe_data
#'
#' @return
#' @export
#'
#' @examples
fill_missing_data <- function(entsoe_data){

  timepoint <- seq(as.POSIXct(paste0(as.character(min(unique(entsoe_data$year))),'-01-01 00:00'),tz="UTC"),
                   as.POSIXct(paste0(as.character(max(unique(entsoe_data$year))),'-12-31 23:00'),tz="UTC"),by=unique(entsoe_data$time_interval),)

  complete_data <- as.data.frame(timepoint)
  colnames(complete_data)<- "Date"

  complete_data$load <- 0
  complete_data$load[complete_data$Date %in% entsoe_data$Date] <- entsoe_data$load
  missing_data_index <- as.numeric(row.names(complete_data[which(complete_data$load==0),]))
  interval_minutes <- as.integer(substr(unique(entsoe_data$time_interval), 1, 2))
  interval_one_week_ago <- 60/interval_minutes*24*7
  complete_data$load[missing_data_index]<- complete_data$load[missing_data_index - interval_one_week_ago]
  complete_data$unit <- unique(entsoe_data$unit)
  complete_data$year <- lubridate::year(complete_data$Date)
  complete_data$time_interval <- unique(entsoe_data$time_interval)
  complete_data$country <- unique(entsoe_data$country)
  country <- unique(entsoe_data$country)
  if (! file.exists(country)){
    dir.create(country)}
  if (! file.exists(paste0("./",country,"/data"))){
    dir.create(paste0("./",country,"/data"))}

  write.csv(complete_data,paste0("./",country,"/data/entsoE_load_data.csv"),row.names = F)


  return (complete_data)
}
