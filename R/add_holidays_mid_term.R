#' Add holidays to the mid-term series
#' 
#' This function adds a dummy variable for holidays to the mid-term data series. Information on the holidays is retrieved from "https://date.nager.at/api/v3/publicholidays/".   
#'
#' @param midterm The mid-term data series resulting from the function decompose_load_data()  
#'
#' @return The mid-term series with an additional column of holiday dummies.
#' @export
#'
#' @examples
#' midterm_holidays_example <- add_holidays_mid_term(decomposed_load_example$midterm)
#' midterm_holidays_example
add_holidays_mid_term<- function(midterm){

  years=unique(midterm$year)
  country= (unique(midterm$country))

  holiday_list <- list()
  for (i in 1:length(years)){
    year= years[i]
    response = jsonlite::fromJSON(paste0("https://date.nager.at/api/v3/publicholidays/"
                                         ,year,"/",country) )
    holiday_list[[i]] <- response$date
  }

  holidays = unlist(holiday_list)
  holidays = as.Date(holidays)

  midterm$holiday <- 0
  midterm$holiday[midterm$date %in% holidays] <- 1


  return(midterm)
}
