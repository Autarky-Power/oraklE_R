#' Replace missing values in the load data set
#'
#' This function substitutes missing values with the corresponding values at the same hour exactly one week prior. For example, if there is no load value available for May 12th at 20:00, the value recorded on May 7th at 20:00 will be used as a replacement.
#' This function is primarily designed to handle minor gaps in the dataset acquired using the "get_entsoE_data()" function. To use this function with other datasets, it is important that the input data frame adheres to the required column naming conventions.

#' @param load_data Data Frame with load data. Data Frame must contain the following columns:
#' \item{date} consisting of the datetime values,
#' \item{load} consisting of the load values,
#' \item{unit} indicating the measured unit (e.g. MW),
#' \item{country} indicating the country's ISO2C code
#'
#' @return Data Frame with completed load values, date, unit, year, time resolution, ISO2C Country Code
#' @export
#'
#' @examples
#' library(ggplot2)
#' no_missing_data_example <- fill_missing_data(example_load_data)
#' example_df<- as.data.frame(seq.POSIXt(example_load_data$date[841],example_load_data$date[870],"hour"))
#' example_df$before <- NA
#' example_df$before[example_df[,1] %in% example_load_data$date] <- example_load_data$load[example_load_data$date %in% example_df[,1]]
#' example_df$after <-   no_missing_data_example$load[no_missing_data_example$date %in% example_df[,1]]
#' ggplot(example_df,aes(x=example_df[,1]))+ geom_line(aes(y=after,colour="after data filling"))+
#' geom_line(aes(y=before,colour="before data filling"))+
#'  xlab("\nHour")+ylab("Load [MW]\n")+theme(legend.title = element_blank())+
#'  scale_x_continuous(breaks = c(example_df[1,1],example_df[25,1]),labels = c(as.Date(example_df[1,1]),as.Date(example_df[25,1])))




fill_missing_data <- function(load_data){

  min_year <- min(unique(lubridate::year(load_data$date)))
  min_month <- min(unique(lubridate::month(load_data$date[load_data$year==min_year])))
  timepoint <- seq(as.POSIXct(paste0(as.character(min(unique(lubridate::year(load_data$date)))),'-',min_month,'-01 00:00'),tz="UTC"),
                   as.POSIXct(paste0(as.character(max(unique(lubridate::year(load_data$date)))),'-12-31 23:59'),tz="UTC"),
                   by=difftime(load_data$date[2],load_data$date[1]))

  complete_data <- as.data.frame(timepoint)
  colnames(complete_data)<- "date"

  complete_data$load <- 0
  complete_data$load[complete_data$date %in% load_data$date] <- load_data$load
  interval_minutes <- as.numeric(difftime(load_data$date[2],load_data$date[1],units="mins"))
  interval_one_week_ago <- 60/interval_minutes*24*7
  while (length(as.numeric(row.names(complete_data[which(complete_data$load==0),])))>0){
    missing_data_index <- as.numeric(row.names(complete_data[which(complete_data$load==0),]))
    complete_data$load[missing_data_index]<- complete_data$load[missing_data_index - interval_one_week_ago]
  }


  if ("unit" %in% colnames(load_data)){
  complete_data$unit <- unique(load_data$unit)}
  complete_data$year <- lubridate::year(complete_data$date)
  complete_data$time_interval <- difftime(complete_data$date[2],complete_data$date[1],units = "mins")
  complete_data$country <- unique(load_data$country)
  country <- unique(load_data$country)
  complete_data <- zoo::na.trim(complete_data)
  if (! file.exists(country)){
    dir.create(country)}
  if (! file.exists(paste0("./",country,"/data"))){
    dir.create(paste0("./",country,"/data"))}
  complete_data
  utils::write.csv(complete_data,paste0("./",country,"/data/filled_load_data.csv"),row.names = F)


  return (complete_data)
}


