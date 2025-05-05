#' Add holidays to the short-term series
#'
#' This function adds a dummy variable for holidays to the short-term data series. Information on the holidays is retrieved from "https://date.nager.at/api/v3/publicholidays/".
#'
#' @param shortterm The short-term data series resulting from \code{\link{decompose_load_data}}
#'
#' @return The short-term series with an additional column of holiday dummies.
#' @export
#'
#' @examples
#' example_shortterm_demand_data <- add_holidays_short_term(example_decomposed_data$shortterm)
#' example_shortterm_demand_data[1:5, c(1, 2, 11)]
add_holidays_short_term <- function(shortterm) {
  if ("example" %in% colnames(shortterm)) {
    if (unique(shortterm$example) == TRUE) {
      year <- 2017
      country <- "FR"
      holiday_list <- list()
      tryCatch(
        {
          Sys.sleep(1.5)
          response <- jsonlite::fromJSON(paste0(
            "https://date.nager.at/api/v3/publicholidays/",
            year, "/", country
          ))
          holiday_list[[1]] <- response$date
        },
        error = function(e) {
          stop("Error during JSON request to date.nager.at : ", e$message, call. = FALSE)
        }
      )

      if (holiday_list[[1]][1] == "2017-01-01") {
        return(oRaklE::example_shortterm_demand_data)
      }
    } else {
      stop("The example in add_holidays_short_term() failed. Please contact the package maintainer at schwenzer@europa-uni.de")
    }
  }
  years <- unique(shortterm$year)
  country <- (unique(shortterm$country))

  holiday_list <- list()
  for (i in 1:length(years)) {
    year <- years[i]
    tryCatch(
      {
        response <- jsonlite::fromJSON(paste0(
          "https://date.nager.at/api/v3/publicholidays/",
          year, "/", country
        ))
        holiday_list[[i]] <- response$date
      },
      error = function(e) {
        i=i-1
        Sys.sleep(5)
        #stop("Error during JSON request to date.nager.at : ", e$message, call. = FALSE)
      }
    )
  }


  holidays <- unlist(holiday_list)
  holidays <- as.Date(holidays)

  shortterm$holiday <- ifelse(as.Date(shortterm$date, tz = "CET") %in% holidays, 1, 0)

  return(shortterm)
}
