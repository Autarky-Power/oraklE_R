#' Load data from the ENTSO-E Transparency Platform
#'
#' This function makes various API requests to the Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/) and 
#' stores the downloaded load data in a data frame. The earliest year for the requested load time series is 2017. 
#'
#' @param start_year  Numeric. The starting year for which load data will be requested.
#' @param end_year  Numeric. The final year for which load data will be requested.
#' @param country  Character. The country name for which load data will be requested provided as the English name of the country.
#' @param api_key  Character. A valid API key for the ENTSO-E Transparency Platform. If none is provided, one of the deposited keys will be used.
#'
#' @return A Data Frame with the 
#' \item{date} The series of dates in POSIXct format.
#' \item{load} The series of load data.
#' \item{unit} The series of units in which the load data is provided, character.
#' \item{year} The year of each load data point. 
#' \item{time_interval} The time resolution of reach load data point.
#' \item{country} The ISO2C Country Code.
#' @export
#'
#' @examples
#' example_load_data <- get_entsoE_data(2017,2021,"France")


get_entsoE_data <- function(start_year,end_year,country,api_key="5ca5937c-7eae-4302-b444-5042ab55d8ef"){

  Sys.setlocale("LC_TIME", "English")
  # Convert country names to iso2c code ----
  if (country != "United Kingdom"){
    country=countrycode::countrycode(country, "country.name","iso2c")
  }else{country="UK"}
  start=start_year
  end=end_year


  # Generate dataframe with country name, country code and the respective API Domain code ----

  domain_codes<-c("Albania","AL","10YAL-KESH-----5","Estonia","EE","10Y1001A1001A39I","Denmark","DK","10Y1001A1001A65H","Germany","DE","10Y1001A1001A83F",
                  "United Kingdom","UK","10Y1001A1001A92E",
                  "Malta","MT","10Y1001A1001A93C",
                  "Moldova","MD","10Y1001A1001A990",
                  "Armenia","AM","10Y1001A1001B004",
                  "Georgia","GE","10Y1001A1001B012",
                  "Azerbaidjan","AZ","10Y1001A1001B05V",
                  "Ukraine","UA","10Y1001C--00003F",
                  "Kosovo","XK","10Y1001C--00100H",
                  "Austria","AT","10YAT-APG------L",
                  "Bosnia and Herz.","BA","10YBA-JPCC-----D",
                  "Belgium","BE","10YBE----------2",
                  "Bulgaria","BG","10YCA-BULGARIA-R",
                  "Switzerland","CH","10YCH-SWISSGRIDZ",
                  "Montenegro","ME","10YCS-CG-TSO---S",
                  "Serbis","RS","10YCS-SERBIATSOV",
                  "Cyprus","CY","10YCY-1001A0003J",
                  "Czech Republic","CZ","10YCZ-CEPS-----N",
                  "Spain","ES","10YES-REE------0",
                  "Italy","IT","10YIT-GRTN-----B",
                  "Finland","FI","10YFI-1--------U",
                  "France","FR","10YFR-RTE------C",
                  "Greece","GR","10YGR-HTSO-----Y",
                  "Croatia","HR","10YHR-HEP------M",
                  "Hungary","HU","10YHU-MAVIR----U",
                  "Ireland","IE","10YIE-1001A00010",
                  "Lithuania","LT","10YLT-1001A0008Q",
                  "Luxembourg","LU","10YLU-CEGEDEL-NQ",
                  "Latvia","LV","10YLV-1001A00074",
                  "North Macedonia","MK","10YMK-MEPSO----8",
                  "Netherlands","NL","10YNL----------L",
                  "Norway","NO","10YNO-0--------C",
                  "Poland","PL","10YPL-AREA-----S",
                  "Portugal","PT","10YPT-REN------W",
                  "Romania","RO","10YRO-TEL------P",
                  "Sweden","SE","10YSE-1--------K",
                  "Slovenia","SI","10YSI-ELES-----O",
                  "Slovakia","SK","10YSK-SEPS-----K",
                  "Turkey","TR","10YTR-TEIAS----W")

  countrynames <- domain_codes[seq(1,length(domain_codes),3)]
  country_codes <-domain_codes[seq(2,length(domain_codes),3)]
  domains <- domain_codes[seq(3,length(domain_codes),3)]
  domain_df <- data.frame(countrynames,country_codes,domains)


  domain=domain_df$domains[domain_df$country_codes==country]


  # API call ----
  # Loop over every year

  data_list <- list()
  for (i in start:end){
    starting_year=i
    print(paste("Getting data for",i))
    entso_response = httr::GET(paste0("https://web-api.tp.entsoe.eu/api?securityToken=",api_key,"&documentType=A65&processType=A16&outBiddingZone_Domain=",domain,"&periodStart=",starting_year,"01010000&periodEnd=",(starting_year+1),"01010000"))
    entso_content <- httr::content(entso_response, encoding = "UTF-8")
    entso_content_list <- xml2::as_list(entso_content)

    entso_timeseries <- entso_content_list$GL_MarketDocument[names(entso_content_list$GL_MarketDocument) == "TimeSeries"]

    # The response sends unpredictable numbers of time series, therefore we have to loop
    # over each one.

    ts_list <- list()
    for (j in 1:length(entso_timeseries)){

      ts<-entso_timeseries[j]
      load <- as.numeric(unlist(purrr::map(ts$TimeSeries$Period, "quantity")))
      start_date <- lubridate::ymd_hm(ts$TimeSeries$Period$timeInterval$start[[1]], tz = "UTC")
      end_date <- lubridate::ymd_hm(ts$TimeSeries$Period$timeInterval$end[[1]], tz = "UTC")
      time_resolution <- ts$TimeSeries$Period$resolution
      time_resolution_minutes <- paste(as.integer(substr(time_resolution, 3, 4)),"mins")

      ts_data <- as.data.frame(seq(start_date,end_date,by=time_resolution_minutes))
      colnames(ts_data) <- "date"
      ts_data <- ts_data[-length(ts_data$date),,drop=F]
      ts_data$load <- load

      ts_list[[j]] <- ts_data
    }

    # Combine all Timeseries data.
    all_ts_data = do.call(what = rbind, args = ts_list)
    all_ts_data$unit <- entso_timeseries$TimeSeries$quantity_Measure_Unit.name[[1]]

    data_list[[(i-start+1)]] <- all_ts_data

  }

  # Combine all years
  all_data = do.call(what = rbind, args = data_list)
  all_data$year <- lubridate::year(all_data$date)
  all_data$time_interval <- time_resolution_minutes
  all_data$country <- country

  # check number of observations per year
  for (i in start:end){
    print(paste("year:",i,"number of datapoints:",nrow(all_data[all_data$year==i,])))


  }
  if (country=="UK"){
    all_data$country <- "GB"
  }
  return(all_data)
}
