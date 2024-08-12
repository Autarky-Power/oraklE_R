#' Add holidays to the short-term series
#'
#' This function adds a dummy variable for holidays to the short-term data series. Information on the holidays is retrieved from "https://date.nager.at/api/v3/publicholidays/".
#'
#' @param shortterm The short-term data series resulting from the function decompose_load_data()
#'
#' @return The short-term series with an additional column of holiday dummies.
#' @export
#'
#' @examples
#' shortterm_holidays_example <- add_holidays_short_term(decomposed_load_example$shortterm)
#' shortterm_holidays_example[,c(1,2,11)]
add_holidays_short_term<- function(shortterm){

  years=unique(shortterm$year)
  country= (unique(shortterm$country))

  holiday_list <- list()
  for (i in 1:length(years)){
    year= years[i]
    response = jsonlite::fromJSON(paste0("https://date.nager.at/api/v3/publicholidays/"
                                         ,year,"/",country) )
    holiday_list[[i]] <- response$date
  }

  holidays = unlist(holiday_list)
  holidays = as.Date(holidays)

  shortterm$holiday <- ifelse(as.Date(shortterm$date, tz = "CET") %in% holidays, 1, 0)

  return(shortterm)
}
