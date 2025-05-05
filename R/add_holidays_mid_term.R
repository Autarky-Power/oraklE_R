#' Add holidays to the mid-term series
#'
#' This function adds a dummy variable for holidays to the mid-term data series. Information on the holidays is retrieved from "https://date.nager.at/api/v3/publicholidays/".
#'
#' @param midterm_data The mid-term data series resulting from the function \code{\link{decompose_load_data}}.
#'
#' @return The mid-term series with an additional column of holiday dummies.
#' @export
#'
#' @seealso See also \code{\link{mid_term_lm}} for the prediction model.
#'
#' @examples
#' example_midterm_demand_data <- add_holidays_mid_term(example_decomposed_data$midterm)
#' head(example_midterm_demand_data)
add_holidays_mid_term <- function(midterm_data) {
  if ("example" %in% colnames(midterm_data)) {
    if (unique(midterm_data$example) == TRUE) {
      year <- 2017
      country <- "FR"
      holiday_list <- list()
      tryCatch(
        {
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
        return(oRaklE::example_midterm_demand_data)
      }
    } else {
      stop("The example in add_holidays_mid_term() failed. Please contact the package maintainer at schwenzer@europa-uni.de")
    }
  }
  midterm <- midterm_data

  years <- unique(midterm$year)
  country <- (unique(midterm$country))

  holiday_list <- list()
  for (i in 1:length(years)) {
    year <- years[i]
    tryCatch(
      {
        Sys.sleep(1.5)
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

  midterm$holiday <- 0
  midterm$holiday[midterm$date %in% holidays] <- 1


  return(midterm)
}
