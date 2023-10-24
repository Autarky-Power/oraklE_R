#' Get weather data for the mid-term model
#' 
#' This function loads weather data which is used to forecast the mid-term data. The weather data is downloaded from https://wft-geo-db.p.rapidapi.com for a provided country and time period.
#'
#' @param midterm The mid-term data series resulting from the function \code{\link{decompose_load_data}.  
#'
#' @return A list containing the mid-term data and temperature data.
#' @export
#'
#' @examples
#' working_directory <- getwd()
#' setwd(tempdir())
#' midterm_all_example <- get_weather_data(midterm_holidays_example)
#' midterm_all_example$midterm
#' midterm_all_example$temperature_data
#' setwd(working_directory)
get_weather_data <- function(midterm){

  country=unique(midterm$country)
  start_year=min(unique(midterm$year))
  end_year=max(unique(midterm$year))

  rAPI_keys <-  c('78373349ffmsh25400c35b068c97p141de2jsnc9c3cb289eeb',
                  "1a5b8f0e09mshf53aea987d0438ap15877ajsnb38876b3416f",
                  '7bcc8e6611msh036ad704bd92ab7p1c117bjsn85b388ab36c2')

  key_integer <- sample(1:3, 1 )
  rAPI_key <- rAPI_keys[key_integer]

  cities1<- httr::GET(paste0("https://wft-geo-db.p.rapidapi.com/v1/geo/cities?countryIds=",country,"&sort=-population&offset=0&limit=10&types=CITY"),
                      httr::accept_json(),
                      httr::add_headers("x-rapidapi-host" = "wft-geo-db.p.rapidapi.com",
                                        "x-rapidapi-key" = rAPI_key))

  Sys.sleep(2)
  cities2<- httr::GET(paste0("https://wft-geo-db.p.rapidapi.com/v1/geo/cities?countryIds=",country,"&sort=-population&offset=10&limit=10&types=CITY"),
                      httr::accept_json(),
                      httr::add_headers("x-rapidapi-host" = "wft-geo-db.p.rapidapi.com",
                                        "x-rapidapi-key" = rAPI_key))


  big_cities <- rbind(jsonlite::fromJSON(rawToChar(cities1$content))$data,jsonlite::fromJSON(rawToChar(cities2$content))$data)
  big_cities$weather_station <- 0




  for (i in 1:nrow(big_cities)){
    lon=round(big_cities$longitude[i],digits=4 )
    lat=round(big_cities$latitude[i],digits=4)
    stations <- httr::GET(paste0("https://meteostat.p.rapidapi.com/stations/nearby?lat=",lat,"&lon=",lon),
                          httr::accept_json(),
                          httr::add_headers("x-rapidapi-host" = "meteostat.p.rapidapi.com",
                                            "x-rapidapi-key" = rAPI_key))

    stations_list <- jsonlite::fromJSON(rawToChar(stations$content))$data
    big_cities$weather_station[i] <- stations_list$id[1]
    Sys.sleep(0.5)
  }

  ts_date <- seq(as.Date(paste0(start_year,"-01-01")),as.Date(paste0(end_year,"-12-31")),by="d")
  temp_df <- data.frame(matrix(nrow=length(ts_date),ncol=(1+nrow(big_cities))))
  colnames(temp_df)[1] <- "date"
  temp_df$date <- ts_date
  temp_df$weighted_mean_temperature <- 0
  for (i in 1:nrow(big_cities)){
    tryCatch({
      station_id <- big_cities$weather_station[i]
      download.file(paste0("https://bulk.meteostat.net/v2/daily/",station_id,".csv.gz"),
                    destfile = "temp.csv.gz")

      R.utils::gunzip("temp.csv.gz")
      temp_data <- read.csv("temp.csv")
      colnames(temp_data)[c(1,2)] <- c("date","daily_avg_temp")
      temp_data$date <- as.Date(temp_data$date, format="%Y-%m-%d")
      temp_data <- temp_data[(lubridate::year(temp_data$date)>=start_year)&
                               (lubridate::year(temp_data$date)<=end_year),1:2]
      temp_df[,(i+1)][temp_df$date %in% temp_data$date]<- temp_data[,2]
      file.remove("temp.csv")
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }

  population <- big_cities$population

  for (i in 1:nrow(temp_df)){
    weighted_vector <- population[!is.na(temp_df[i,2:(ncol(temp_df)-1)])]/sum(population[!is.na(temp_df[i,2:(ncol(temp_df)-1)])])
    temp_df$weighted_mean_temperature[i]<- sum(temp_df[i,2:(ncol(temp_df)-1)][!is.na(temp_df[i,2:(ncol(temp_df)-1)])]*weighted_vector)
  }

  colnames(temp_df)[2:(ncol(temp_df)-1)] <- big_cities$name
  midterm$weighted_temperature <- 0

  midterm$weighted_temperature<- temp_df$weighted_mean_temperature[temp_df$date %in% midterm$date]

  if (! file.exists(country)){
    dir.create(country)}
  if (! file.exists(paste0("./",country,"/data"))){
    dir.create(paste0("./",country,"/data"))}
  write.csv(temp_df,paste0("./",country,"/data/temperatures.csv"),row.names = F)
  write.csv(midterm,paste0("./",country,"/data/midterm_data.csv"),row.names = F)

  return(list("midterm"=midterm, "temperature_data"=temp_df))

}
