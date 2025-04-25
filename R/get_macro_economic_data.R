#' Load a list of macroeconomic data from WDI
#'
#' This function downloads a set of ten macroeconomic variables via API from the World Development Indicators (WDI) of the World Bank (https://databank.worldbank.org/source/world-development-indicators).
#' The variables are suspected to have a predictive capacity for the load data.
#'
#' @param longterm_data The long-term data series resulting from the function \code{\link{decompose_load_data}}. Contains information on country (longterm$country) and years (longterm$year).
#'
#' @return Data frame with the original time series and 10 additional columns with macroeconomic indicators.
#' @export
#'
#' @seealso See function \code{\link{decompose_load_data}} for the generation of the long-term series and \code{\link{long_term_lm}} for the selection of covariates.
#'
#' @examples
#' example_longterm_and_macro_data <- get_macro_economic_data(example_longterm_data)
#' print("Macro economic variables are added from the World Bank Developer Indicators:")
#' example_longterm_and_macro_data
#'
get_macro_economic_data <- function(longterm_data) {
  if ("example" %in% colnames(longterm_data)) {
    if (unique(longterm_data$example) == TRUE) {
      return(oRaklE::example_longterm_and_macro_data)
    }
  }


  longterm <- longterm_data
  country <- unique(longterm$country)
  start_year <- min(longterm$year)
  end_year <- max(longterm$year)

  tryCatch(
    {
      res_pop <- httr::GET(paste0("https://api.worldbank.org/v2/country/", country, "/indicator/SP.POP.TOTL?date=", start_year, ":", end_year, "&format=json"))
    },
    error = function(e) {
      stop("Error during GET request to api.worldbank.org", e$message,
        "\nAre you connected to the internet?",
        call. = FALSE
      )
    }
  )

  if (res_pop$status_code == 502) {
    message("The World Development Indicator database of the World Bank is currently unreachable. Please try again in a minute.")
    return()
  }
  if (httr::http_error(res_pop)) {
    status <- httr::status_code(res_pop)
    error_info <- httr::http_status(res_pop)$message
    stop("HTTP request to api.worldbank.org failed (status ", status, "): ", error_info)
  }

  data_pop <- jsonlite::fromJSON(rawToChar(res_pop$content))
  df_pop <- as.data.frame(data_pop[2])
  df_pop <- df_pop[order(df_pop$date), ]
  longterm$population <- rep(NA, nrow(longterm))
  for (i in seq_len(nrow(df_pop))) {
    longterm$population[i] <- df_pop$value[i]
  }
  tryCatch(
    {
      res_gdp <- httr::GET(paste0("https://api.worldbank.org/v2/country/", country, "/indicator/NY.GDP.MKTP.KD?date=", start_year, ":", end_year, "&format=json"))
    },
    error = function(e) {
      stop("Error during GET request to api.worldbank.org", e$message,
        "\nAre you connected to the internet?",
        call. = FALSE
      )
    }
  )
  data_gdp <- jsonlite::fromJSON(rawToChar(res_gdp$content))
  df_gdp <- as.data.frame(data_gdp[2])
  df_gdp <- df_gdp[order(df_gdp$date), ]
  longterm$GDP <- rep(NA, nrow(longterm))
  for (i in seq_len(nrow(df_gdp))) {
    longterm$GDP[i] <- df_gdp$value[i]
  }

  tryCatch(
    {
      res_ind <- httr::GET(paste0("https://api.worldbank.org/v2/country/", country, "/indicator/NV.IND.TOTL.ZS?date=", start_year, ":", end_year, "&format=json"))
    },
    error = function(e) {
      stop("Error during GET request to api.worldbank.org", e$message,
        "\nAre you connected to the internet?",
        call. = FALSE
      )
    }
  )
  data_ind <- jsonlite::fromJSON(rawToChar(res_ind$content))
  df_ind <- as.data.frame(data_ind[2])
  df_ind <- df_ind[order(df_ind$date), ]

  longterm$industrial_value_added <- rep(NA, nrow(longterm))
  for (i in seq_len(nrow(df_ind))) {
    longterm$industrial_value_added[i] <- df_ind$value[i]
  }

  tryCatch(
    {
      res_man <- httr::GET(paste0("https://api.worldbank.org/v2/country/", country, "/indicator/NV.IND.MANF.ZS?date=", start_year, ":", end_year, "&format=json"))
    },
    error = function(e) {
      stop("Error during GET request to api.worldbank.org", e$message,
        "\nAre you connected to the internet?",
        call. = FALSE
      )
    }
  )
  data_man <- jsonlite::fromJSON(rawToChar(res_man$content))
  df_man <- as.data.frame(data_man[2])
  df_man <- df_man[order(df_man$date), ]

  longterm$manufacturing_value_added <- rep(NA, nrow(longterm))
  for (i in seq_len(nrow(df_man))) {
    longterm$manufacturing_value_added[i] <- df_man$value[i]
  }

  tryCatch(
    {
      res_gro <- httr::GET(paste0("https://api.worldbank.org/v2/country/", country, "/indicator/NY.GDP.MKTP.KD.ZG?date=", start_year, ":", end_year, "&format=json"))
    },
    error = function(e) {
      stop("Error during GET request to api.worldbank.org", e$message,
        "\nAre you connected to the internet?",
        call. = FALSE
      )
    }
  )
  data_gro <- jsonlite::fromJSON(rawToChar(res_gro$content))
  df_gro <- as.data.frame(data_gro[2])
  df_gro <- df_gro[order(df_gro$date), ]

  longterm$GDP_growth <- rep(NA, nrow(longterm))
  for (i in seq_len(nrow(df_gro))) {
    longterm$GDP_growth[i] <- df_gro$value[i]
  }


  tryCatch(
    {
      res_gdp_defl <- httr::GET(paste0("https://api.worldbank.org/v2/country/", country, "/indicator/NY.GDP.DEFL.KD.ZG?date=", start_year, ":", end_year, "&format=json"))
    },
    error = function(e) {
      stop("Error during GET request to api.worldbank.org", e$message,
        "\nAre you connected to the internet?",
        call. = FALSE
      )
    }
  )
  data_gdp_defl <- jsonlite::fromJSON(rawToChar(res_gdp_defl$content))
  df_gdp_defl <- as.data.frame(data_gdp_defl[2])
  df_gdp_defl <- df_gdp_defl[order(df_gdp_defl$date), ]

  longterm$GDP_deflator <- rep(NA, nrow(longterm))
  for (i in seq_len(nrow(df_gdp_defl))) {
    longterm$GDP_deflator[i] <- df_gdp_defl$value[i]
  }


  tryCatch(
    {
      res_serv <- httr::GET(paste0("https://api.worldbank.org/v2/country/", country, "/indicator/NV.SRV.TOTL.ZS?date=", start_year, ":", end_year, "&format=json"))
    },
    error = function(e) {
      stop("Error during GET request to api.worldbank.org", e$message,
        "\nAre you connected to the internet?",
        call. = FALSE
      )
    }
  )
  data_serv <- jsonlite::fromJSON(rawToChar(res_serv$content))
  df_serv <- as.data.frame(data_serv[2])
  df_serv <- df_serv[order(df_serv$date), ]


  longterm$service_value_added <- rep(NA, nrow(longterm))
  for (i in seq_len(nrow(df_serv))) {
    longterm$service_value_added[i] <- df_serv$value[i]
  }


  tryCatch(
    {
      res_gni <- httr::GET(paste0("https://api.worldbank.org/v2/country/", country, "/indicator/NY.GNP.MKTP.KD?date=", start_year, ":", end_year, "&format=json"))
    },
    error = function(e) {
      stop("Error during GET request to api.worldbank.org", e$message,
        "\nAre you connected to the internet?",
        call. = FALSE
      )
    }
  )
  data_gni <- jsonlite::fromJSON(rawToChar(res_gni$content))
  df_gni <- as.data.frame(data_gni[2])
  df_gni <- df_gni[order(df_gni$date), ]

  longterm$GNI <- rep(NA, nrow(longterm))
  for (i in seq_len(nrow(df_gni))) {
    longterm$GNI[i] <- df_gni$value[i]
  }


  tryCatch(
    {
      res_hou <- httr::GET(paste0("https://api.worldbank.org/v2/country/", country, "/indicator/NE.CON.PRVT.ZS?date=", start_year, ":", end_year, "&format=json"))

      data_hou <- jsonlite::fromJSON(rawToChar(res_hou$content))
      df_hou <- as.data.frame(data_hou[2])
      df_hou <- df_hou[order(df_hou$date), ]
    },
    error = function(e) {
      stop("Error during GET request to api.worldbank.org", e$message,
        "\nAre you connected to the internet?",
        call. = FALSE
      )
    }
  )

  longterm$household_consumption_expenditure <- rep(NA, nrow(longterm))
  for (i in seq_len(nrow(df_hou))) {
    longterm$household_consumption_expenditure[i] <- df_hou$value[i]
  }

  tryCatch(
    {
      res_rural <- httr::GET(paste0("https://api.worldbank.org/v2/country/", country, "/indicator/SP.RUR.TOTL?date=", start_year, ":", end_year, "&format=json"))
    },
    error = function(e) {
      stop("Error during GET request to api.worldbank.org", e$message,
        "\nAre you connected to the internet?",
        call. = FALSE
      )
    }
  )
  data_rural <- jsonlite::fromJSON(rawToChar(res_rural$content))
  df_rural <- as.data.frame(data_rural[2])
  df_rural <- df_rural[order(df_rural$date), ]


  longterm$rural_population <- rep(NA, nrow(longterm))
  for (i in seq_len(nrow(df_serv))) {
    longterm$rural_population[i] <- df_rural$value[i]
  }

  # consumer_price_inflation_pct= httr::GET(paste0("https://api.worldbank.org/v2/country/",country,"/indicator/FP.CPI.TOTL.ZG?date=",start_year,":",end_year,"&format=json"))
  #
  # data_cpi=jsonlite::fromJSON(rawToChar(consumer_price_inflation_pct$content))
  # df_cpi<- as.data.frame(data_cpi[2])
  # df_cpi<- df_cpi[order(df_cpi$date),]
  #
  # longterm$consumer_price_inflation_pct <- df_cpi$value




  return(longterm)
}
