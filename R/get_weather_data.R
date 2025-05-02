#' Load weather data via API
#'
#' This function loads weather data which is used to forecast the mid-term load seasonalities.
#' First the 20 most populated areas in the country are obtained from https://wft-geo-db.p.rapidapi.com . Then the closest weather stations of each area are identified and average daily temperature values are downloaded from https://meteostat.p.rapidapi.com  for the provided time period.
#' From this data a weighted daily average temperature based on population is calculated for the provided country.
#'
#' @param midterm_demand_data Dataframe. The mid-term data series from \code{\link{decompose_load_data}} with added holidays resulting from the function \code{\link{add_holidays_mid_term}}.
#' @param api_key Character. A valid API key from rapidapi that is subscribed to wft-geo-db and meteostat. If set to "default", one of the deposited keys will be used.
#' @param data_directory The path to the directory where the data will be saved.
#' @return A list containing the mid-term data and temperature data.
#' @export
#'
#' @seealso See function \code{\link{decompose_load_data}} for the generation of the mid-term series.
#'
#' @examples
#' example_midterm_demand_and_weather_data <- get_weather_data(example_midterm_demand_data,
#'   api_key = "default"
#' )
#' head(example_midterm_demand_and_weather_data$demand)
#' head(example_midterm_demand_and_weather_data$temperature_data)
get_weather_data <- function(midterm_demand_data, api_key = "default", data_directory = tempdir()) {
  if ("example" %in% colnames(midterm_demand_data)) {
    if (unique(midterm_demand_data$example) == TRUE) {
      station_id <- "07480"
      start_year <- 2018
      end_year <- 2018
      suppressWarnings(
        utils::download.file(paste0("https://bulk.meteostat.net/v2/daily/", station_id, ".csv.gz"),
          destfile = "temp.csv.gz"
        )
      )

      R.utils::gunzip("temp.csv.gz")
      temp_data <- utils::read.csv("temp.csv")
      colnames(temp_data)[c(1, 2)] <- c("date", "daily_avg_temp")
      temp_data$date <- as.Date(temp_data$date, format = "%Y-%m-%d")
      temp_data <- temp_data[(lubridate::year(temp_data$date) >= start_year) &
        (lubridate::year(temp_data$date) <= end_year), 1:2]
      file.remove("temp.csv")
      expected_temp_data <- c(8.2, 7.6, 10.2, 11.1, 12.4, 10.6, 8.1, 7.1, 7.5, 9.2)

      if (identical(temp_data[1:10, 2], expected_temp_data)) {
        return(oRaklE::example_midterm_demand_and_weather_data)
      } else {
        stop("The example in get_weather_data() failed. Please contact the package maintainer at schwenzer@europa-uni.de")
      }
    }
  }

  if (grepl("Rtmp", data_directory)) {
    message(paste(
      "\nThis function will try to save the load and weather data to a folder called", unique(midterm_demand_data$country),
      "\nin the current data directory:", data_directory
    ))
    message("\nIt is recommended to save the data in a directory other than a tempdir, so that it is available after you finish the R Session.")

    message("\nPlease choose an option:")
    message("\n1: Keep it as a tempdir")
    message(paste("2: Save data in the current working directory (", getwd(), ")", sep = ""))
    message("3: Set the directory manually\n")

    choice <- readline(prompt = "Enter the option number (1, 2, or 3): ")


    if (choice == "1") {
      message("\nData will be saved in a temporary directory and cleaned up when R is shut down.")
    } else if (choice == "2") {
      data_directory <- getwd()
      message(paste0("\nData will be saved in the current working directory in ", data_directory, "/", unique(midterm_demand_data$country), "/data"))
      message("\nYou can specify the *data_directory* parameter in the following functions as ", data_directory)
    } else if (choice == "3") {
      new_dir <- readline(prompt = "Enter the full path of the directory where you want to save the data: ")
      data_directory <- new_dir
      if (!dir.exists(data_directory)) {
        stop("The specified data_directory does not exist: ", data_directory, "\nPlease run the function again.")
      }
      message("\nData will be saved in the specified directory: ", data_directory, "/", unique(midterm_demand_data$country), "/data")
    } else {
      message("Invalid input. Keeping the temporary directory.\nData will be cleaned up when R is shut down.")
    }
  } else {
    if (!dir.exists(data_directory)) {
      stop("The specified data_directory does not exist: ", data_directory, "\nPlease run the function again.")
    }
    message("\nData will be saved in the specified working directory in ", data_directory, "/", unique(midterm_demand_data$country), "/data")
  }

  midterm <- midterm_demand_data
  country <- unique(midterm$country)
  start_year <- min(unique(midterm$year))
  end_year <- max(unique(midterm$year))

  rAPI_keys <- c(
    "78373349ffmsh25400c35b068c97p141de2jsnc9c3cb289eeb",
    "1a5b8f0e09mshf53aea987d0438ap15877ajsnb38876b3416f",
    "7bcc8e6611msh036ad704bd92ab7p1c117bjsn85b388ab36c2",
    "ff83e65c19mshe1a5a3cade9b307p12c6abjsnc5ca238dd93f",
    "a1ca87a51fmsh084a0420e192fb3p1e80dejsn751dfe70bbd9",
    "3a789ffa80msha4a27ded5ad6bf6p17156ajsn65dde6edcd82",
    "3f1e8156e8msh77ecdc22a46a6dcp16f7afjsn0f8652f0a598",
    "a2fa81f7c0msh99fc6bfcd50c2c1p11f186jsn6acb9eb0881b",
    "9e5732f249mshf4d3ca6064fd56ep1c0303jsnad9722dc52f8",
    "994e4b90abmshafbba71e8e92722p14f3d1jsn06ff01f2b6ca",
    "200439098fmsh508a44ddd5102eap196ed0jsn92487da6e4d6",
    "85bc618d2cmsh2df7162687e459cp11af9cjsncd9308af6007",
    "78a5abe631msh672c79b2280107ep1060d7jsn7dd1dda63e55",
    "73835b8d7emsha0dd3db1fbc8c45p1435e7jsn402be5ac38f9"
  )

  key_integer <- sample(1:14, 1)
  if (api_key == "default") {
    rAPI_key <- rAPI_keys[key_integer]
  } else {
    rAPI_key <- api_key
  }

  tryCatch(
    {
      cities1 <- httr::GET(
        paste0("https://wft-geo-db.p.rapidapi.com/v1/geo/cities?countryIds=", country, "&sort=-population&offset=0&limit=10&types=CITY"),
        httr::accept_json(),
        httr::add_headers(
          "x-rapidapi-host" = "wft-geo-db.p.rapidapi.com",
          "x-rapidapi-key" = rAPI_key
        )
      )

      Sys.sleep(2)
      cities2 <- httr::GET(
        paste0("https://wft-geo-db.p.rapidapi.com/v1/geo/cities?countryIds=", country, "&sort=-population&offset=10&limit=10&types=CITY"),
        httr::accept_json(),
        httr::add_headers(
          "x-rapidapi-host" = "wft-geo-db.p.rapidapi.com",
          "x-rapidapi-key" = rAPI_key
        )
      )
    },
    error = function(e) {
      stop("Error during GET request from wft-geo-db.p.rapidapi.com: ", e$message,
        "\nAre you connected to the internet?",
        call. = FALSE
      )
    }
  )

  if (cities1$status_code == 403 || cities2$status_code == 403) {
    stop("The used API key for wft-geo-db.p.rapidapi.com is either invalid or has reached it's monthly limit.\nYou can try again and a different API key will be used or you can provide your own.", call. = FALSE)
  }
  if (httr::http_error(cities1)) {
    status <- httr::status_code(cities1)
    error_info <- httr::http_status(cities1)$message
    stop("HTTP request failed from wft-geo-db.p.rapidapi.com (status ", status, "): ", error_info, call. = FALSE)
  }

  if (httr::http_error(cities2)) {
    status <- httr::status_code(cities2)
    error_info <- httr::http_status(cities2)$message
    stop("HTTP request failed from wft-geo-db.p.rapidapi.com (status ", status, "): ", error_info, call. = FALSE)
  }


  big_cities <- rbind(jsonlite::fromJSON(rawToChar(cities1$content))$data, jsonlite::fromJSON(rawToChar(cities2$content))$data)
  big_cities$weather_station <- 0




  for (i in 1:nrow(big_cities)) {
    lon <- round(big_cities$longitude[i], digits = 4)
    lat <- round(big_cities$latitude[i], digits = 4)

    tryCatch(
      {
        stations <- httr::GET(
          paste0("https://meteostat.p.rapidapi.com/stations/nearby?lat=", lat, "&lon=", lon),
          httr::accept_json(),
          httr::add_headers(
            "x-rapidapi-host" = "meteostat.p.rapidapi.com",
            "x-rapidapi-key" = rAPI_key
          )
        )
      },
      error = function(e) {
        stop("Error during GET request from meteostat.p.rapidapi: ", e$message,
          "\nAre you connected to the internet?",
          call. = FALSE
        )
      }
    )

    if (stations$status_code == 403) {
      stop("The used API key for meteostat.p.rapidapi.com is either invalid or has reached it's monthly limit.\nYou can try again and a different API key will be used or you can provide your own.", call. = FALSE)
    }

    if (httr::http_error(stations)) {
      status <- httr::status_code(stations)
      error_info <- httr::http_status(stations)$message
      stop("HTTP request failed from meteostat.p.rapidapi.com (status ", status, "): ", error_info, call. = FALSE)
    }
    stations_list <- jsonlite::fromJSON(rawToChar(stations$content))$data
    big_cities$weather_station[i] <- stations_list$id[1]
    Sys.sleep(0.5)
  }

  ts_date <- seq(as.Date(paste0(start_year, "-01-01")), as.Date(paste0(end_year, "-12-31")), by = "d")
  temp_df <- data.frame(matrix(nrow = length(ts_date), ncol = (1 + nrow(big_cities))))
  colnames(temp_df)[1] <- "date"
  temp_df$date <- ts_date
  temp_df$weighted_mean_temperature <- 0
  for (i in 1:nrow(big_cities)) {
    tryCatch(
      {
        station_id <- big_cities$weather_station[i]
        suppressWarnings(
          utils::download.file(paste0("https://bulk.meteostat.net/v2/daily/", station_id, ".csv.gz"),
            destfile = "temp.csv.gz"
          )
        )
        R.utils::gunzip("temp.csv.gz")
        temp_data <- utils::read.csv("temp.csv")
        colnames(temp_data)[c(1, 2)] <- c("date", "daily_avg_temp")
        temp_data$date <- as.Date(temp_data$date, format = "%Y-%m-%d")
        temp_data <- temp_data[(lubridate::year(temp_data$date) >= start_year) &
          (lubridate::year(temp_data$date) <= end_year), 1:2]
        temp_df[, (i + 1)][temp_df$date %in% temp_data$date] <- temp_data[, 2]
        file.remove("temp.csv")
      },
      error = function(e) {

      }
    )
  }

  population <- big_cities$population

  for (i in 1:nrow(temp_df)) {
    weighted_vector <- population[!is.na(temp_df[i, 2:(ncol(temp_df) - 1)])] / sum(population[!is.na(temp_df[i, 2:(ncol(temp_df) - 1)])])
    temp_df$weighted_mean_temperature[i] <- sum(temp_df[i, 2:(ncol(temp_df) - 1)][!is.na(temp_df[i, 2:(ncol(temp_df) - 1)])] * weighted_vector)
  }

  colnames(temp_df)[2:(ncol(temp_df) - 1)] <- big_cities$name
  midterm$weighted_temperature <- 0

  midterm$weighted_temperature <- temp_df$weighted_mean_temperature[temp_df$date %in% midterm$date]

  if (!file.exists(paste0(data_directory, "/", country))) {
    dir.create(paste0(data_directory, "/", country))
  }

  if (!file.exists(paste0(data_directory, "/", country, "/data"))) {
    dir.create(paste0(data_directory, "/", country, "/data"))
  }
  utils::write.csv(temp_df, paste0(data_directory, "/", country, "/data/temperatures.csv"), row.names = FALSE)
  utils::write.csv(midterm, paste0(data_directory, "/", country, "/data/midterm_data.csv"), row.names = FALSE)


  return(list("demand" = midterm, "temperature_data" = temp_df))
}
