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
#' \dontrun{
#' example_midterm_demand_data <- add_holidays_mid_term(example_decomposed_data$midterm)
#' example_midterm_demand_data
#' }
add_holidays_mid_term<- function(midterm_data){
  midterm <- midterm_data

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
