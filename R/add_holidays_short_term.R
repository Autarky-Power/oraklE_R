#' Title
#'
#' @param shortterm
#'
#' @return
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

  shortterm$place_holder <- as.Date(shortterm$date,tz="CET")
  shortterm$holiday <- 0
  shortterm$holiday[shortterm$place_holder %in% holidays] <- 1
  shortterm <- subset(shortterm, select = -c(place_holder))

  return(shortterm)
}
