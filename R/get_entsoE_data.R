#' Load data from the ENTSO-E Transparency Platform
#'
#' This function makes various API requests to the Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/) and
#' stores the downloaded load data in a data frame. The earliest possible year for the requested load time series is 2017.
#'
#' @param start_year  Numeric. The starting year for which load data will be requested.
#' @param end_year  Numeric. The final year for which load data will be requested.
#' @param country  Character. The country name for which load data will be requested provided as the English name of the country.
#' @param api_key  Character. A valid API key for the ENTSO-E Transparency Platform. If set to "default", one of the deposited keys will be used.
#' @param dry_run  Boolean. Defaults to FALSE. This is only set to TRUE for the example run.
#'
#' @return A Data Frame with the following columns
#' \describe{
#'   \item{date}{The series of dates, POSIXct format.}
#'   \item{load}{The series of load data, numeric}
#'   \item{unit}{The series of units in which the load data is provided, character.}
#'   \item{year}{The year of each load data point, numeric}
#'   \item{time_interval}{The time resolution of each load data point, character}
#'   \item{country}{The ISO2C Country Code, character}
#' }
#' @export
#'
#' @examples
#' example_demand_data <- get_entsoE_data(2017, 2021, "France", api_key = "default", dry_run = TRUE)
#' print(example_demand_data[1:20, ])
#'
get_entsoE_data <- function(start_year, end_year, country, api_key = "default", dry_run = FALSE) {
  if (dry_run == TRUE) {
    dummy_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<GL_MarketDocument>
  <TimeSeries>
    <Period>
      <timeInterval>
        <start>201801010000</start>
        <end>201801010100</end>
      </timeInterval>
      <resolution>PT15M</resolution>
      <quantity>100</quantity>
      <quantity>105</quantity>
      <quantity>102</quantity>
      <quantity>110</quantity>
    </Period>
    <quantity_Measure_Unit.name>MW</quantity_Measure_Unit.name>
  </TimeSeries>
</GL_MarketDocument>'


    dummy_doc <- xml2::read_xml(dummy_xml)
    entso_content_list <- xml2::as_list(dummy_doc)
    entso_timeseries <- entso_content_list$GL_MarketDocument[
      names(entso_content_list$GL_MarketDocument) == "TimeSeries"
    ]

    ts_list <- list()
    for (j in seq_along(entso_timeseries)) {
      ts <- entso_timeseries[[j]]
      period <- ts$Period
      load_vals <- as.numeric(period$quantity)

      start_date <- lubridate::ymd_hm(period$timeInterval$start, tz = "UTC")
      end_date <- lubridate::ymd_hm(period$timeInterval$end, tz = "UTC")

      time_resolution <- period$resolution
      time_minutes <- as.integer(gsub("PT([0-9]+)M", "\\1", time_resolution))
      time_resolution_minutes <- paste(time_minutes, "mins")

      dates <- seq(start_date, end_date, by = time_resolution_minutes)
      if (length(dates) > 1) dates <- dates[-length(dates)]

      ts_data <- data.frame(date = dates, load = load_vals)
      ts_list[[j]] <- ts_data
    }


    all_ts_data <- do.call(rbind, ts_list)
    all_ts_data$unit <- entso_timeseries[[1]]$quantity_Measure_Unit.name
    all_ts_data$year <- lubridate::year(all_ts_data$date)
    all_ts_data$time_interval <- time_resolution_minutes

    expected_df <- data.frame(
      date = as.POSIXct(c(
        "2018-01-01 00:00:00",
        "2018-01-01 00:15:00",
        "2018-01-01 00:30:00",
        "2018-01-01 00:45:00"
      ), tz = "UTC"),
      load = c(100, 100, 100, 100),
      unit = c("MW", "MW", "MW", "MW"),
      year = c(2018, 2018, 2018, 2018),
      time_interval = c("15 mins", "15 mins", "15 mins", "15 mins"),
      stringsAsFactors = FALSE
    )
    all_ts_data$unit <- as.character(unlist(all_ts_data$unit))

    if (identical(all_ts_data, expected_df)) {
      message("Getting data for each year from ENTSO-E Transparency Platform")
      return(oRaklE::example_demand_data)
    } else {
      stop("The example in get_entsoe_data() failed. Please contact the package maintainer at schwenzer@europa-uni.de")
    }
  }
  Sys.setlocale("LC_TIME", "English")
  # Convert country names to iso2c code ----
  if (country != "United Kingdom") {
    country <- countrycode::countrycode(country, "country.name", "iso2c")
  } else {
    country <- "UK"
  }
  start <- start_year
  end <- end_year


  # Generate dataframe with country name, country code and the respective API Domain code ----

  domain_codes <- c(
    "Albania", "AL", "10YAL-KESH-----5", "Estonia", "EE", "10Y1001A1001A39I", "Denmark", "DK", "10Y1001A1001A65H", "Germany", "DE", "10Y1001A1001A83F",
    "United Kingdom", "UK", "10Y1001A1001A92E",
    "Malta", "MT", "10Y1001A1001A93C",
    "Moldova", "MD", "10Y1001A1001A990",
    "Armenia", "AM", "10Y1001A1001B004",
    "Georgia", "GE", "10Y1001A1001B012",
    "Azerbaidjan", "AZ", "10Y1001A1001B05V",
    "Ukraine", "UA", "10Y1001C--00003F",
    "Kosovo", "XK", "10Y1001C--00100H",
    "Austria", "AT", "10YAT-APG------L",
    "Bosnia and Herz.", "BA", "10YBA-JPCC-----D",
    "Belgium", "BE", "10YBE----------2",
    "Bulgaria", "BG", "10YCA-BULGARIA-R",
    "Switzerland", "CH", "10YCH-SWISSGRIDZ",
    "Montenegro", "ME", "10YCS-CG-TSO---S",
    "Serbis", "RS", "10YCS-SERBIATSOV",
    "Cyprus", "CY", "10YCY-1001A0003J",
    "Czech Republic", "CZ", "10YCZ-CEPS-----N",
    "Spain", "ES", "10YES-REE------0",
    "Italy", "IT", "10YIT-GRTN-----B",
    "Finland", "FI", "10YFI-1--------U",
    "France", "FR", "10YFR-RTE------C",
    "Greece", "GR", "10YGR-HTSO-----Y",
    "Croatia", "HR", "10YHR-HEP------M",
    "Hungary", "HU", "10YHU-MAVIR----U",
    "Ireland", "IE", "10YIE-1001A00010",
    "Lithuania", "LT", "10YLT-1001A0008Q",
    "Luxembourg", "LU", "10YLU-CEGEDEL-NQ",
    "Latvia", "LV", "10YLV-1001A00074",
    "North Macedonia", "MK", "10YMK-MEPSO----8",
    "Netherlands", "NL", "10YNL----------L",
    "Norway", "NO", "10YNO-0--------C",
    "Poland", "PL", "10YPL-AREA-----S",
    "Portugal", "PT", "10YPT-REN------W",
    "Romania", "RO", "10YRO-TEL------P",
    "Sweden", "SE", "10YSE-1--------K",
    "Slovenia", "SI", "10YSI-ELES-----O",
    "Slovakia", "SK", "10YSK-SEPS-----K",
    "Turkey", "TR", "10YTR-TEIAS----W"
  )

  countrynames <- domain_codes[seq(1, length(domain_codes), 3)]
  country_codes <- domain_codes[seq(2, length(domain_codes), 3)]
  domains <- domain_codes[seq(3, length(domain_codes), 3)]
  domain_df <- data.frame(countrynames, country_codes, domains)


  domain <- domain_df$domains[domain_df$country_codes == country]


  # API call ----
  if (api_key == "default") {
    keys <- c(
      "38c78048-f4ae-4ec9-8ea6-983049e5db5d",
      "5ca5937c-7eae-4302-b444-5042ab55d8ef",
      "02f98e29-fe49-4343-87b6-42f2947d9004"
    )

    key_integer <- sample(1:3, 1)
    api_key <- keys[key_integer]
  }
  # Loop over every year

  data_list <- list()
  for (i in start:end) {
    starting_year <- i
    message(paste("Getting data for", i))
    tryCatch(
      {
        entso_response <- httr::GET(paste0("https://web-api.tp.entsoe.eu/api?securityToken=", api_key, "&documentType=A65&processType=A16&outBiddingZone_Domain=", domain, "&periodStart=", starting_year, "01010000&periodEnd=", (starting_year + 1), "01010000"))
      },
      error = function(e) {
        stop("Error during GET request for year ", i, ": ", e$message,
          "\nAre you connected to the internet?",
          call. = FALSE
        )
      }
    )

    if (entso_response$status_code == 503) {
      message("The ENTSO_E Transparency platform seems to be unavailable.\n
              Please check https://transparency.entsoe.eu/ and try again later.")
      return()
    }
    if (entso_response$status_code == 401) {
      message("Your API key seems to be invalid.")
      return()
    }
    if (httr::http_error(entso_response)) {
      status <- httr::status_code(entso_response)
      error_info <- httr::http_status(entso_response)$message
      stop("HTTP request failed for year ", i, " (status ", status, "): ", error_info)
    }


    entso_content <- httr::content(entso_response, encoding = "UTF-8")
    entso_content_list <- xml2::as_list(entso_content)

    entso_timeseries <- entso_content_list$GL_MarketDocument[names(entso_content_list$GL_MarketDocument) == "TimeSeries"]

    # The response sends unpredictable numbers of time series, therefore we have to loop
    # over each one.

    ts_list <- list()
    for (j in 1:length(entso_timeseries)) {
      ts <- entso_timeseries[j]
      load <- as.numeric(unlist(purrr::map(ts$TimeSeries$Period, "quantity")))
      start_date <- lubridate::ymd_hm(ts$TimeSeries$Period$timeInterval$start[[1]], tz = "UTC")
      end_date <- lubridate::ymd_hm(ts$TimeSeries$Period$timeInterval$end[[1]], tz = "UTC")
      time_resolution <- ts$TimeSeries$Period$resolution
      time_resolution_minutes <- paste(as.integer(substr(time_resolution, 3, 4)), "mins")

      ts_data <- as.data.frame(seq(start_date, end_date, by = time_resolution_minutes))
      colnames(ts_data) <- "date"
      ts_data <- ts_data[-length(ts_data$date), , drop = F]
      ts_data$load <- load

      ts_list[[j]] <- ts_data
    }

    # Combine all Timeseries data.
    all_ts_data <- do.call(what = rbind, args = ts_list)
    all_ts_data$unit <- entso_timeseries$TimeSeries$quantity_Measure_Unit.name[[1]]

    data_list[[(i - start + 1)]] <- all_ts_data
  }

  # Combine all years
  all_data <- do.call(what = rbind, args = data_list)
  all_data$year <- lubridate::year(all_data$date)
  all_data$time_interval <- time_resolution_minutes
  all_data$country <- country

  # check number of observations per year
  for (i in start:end) {
    message(paste("year:", i, "number of datapoints:", nrow(all_data[all_data$year == i, ])))
  }
  if (country == "UK") {
    all_data$country <- "GB"
  }
  return(all_data)
}
