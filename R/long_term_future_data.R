#' Title
#'
#' @param longterm_predictions
#' @param end_year
#'
#' @return
#' @export
#'
#' @examples
long_term_future_data <- function(longterm_predictions,end_year){

  weo_data_set <- oRaklE::weo_data
  for (i in 1:3){
    model_path= paste0("./", unique(longterm_predictions$country),"/models/longterm/best_lm_model",i,".Rdata")
    load(model_path)
    attr(mod$terms , "term.labels")
  }
  country<- countrycode::countrycode(unique(longterm_predictions$country),origin = "iso2c",destination = "country.name")
  country_subset <-  weo_data_set[weo_data_set$Country==country, ]
  colnames(country_subset)[10:58] <- 1980:2028


  new_data_length <- end_year-max(longterm_predictions$year)
  new_rows <- as.data.frame(matrix(nrow=new_data_length,ncol = ncol(longterm_predictions) ))
  colnames(new_rows) <- colnames(longterm_predictions)
  longterm_predictions = rbind(longterm_predictions, new_rows)

  first_year <- min(longterm_predictions$year,na.rm = T)
  start_year <- max(longterm_predictions$year,na.rm = T)+1
  col_year_first <- which(colnames(country_subset)==as.character(first_year))
  col_year_start <- which(colnames(country_subset)==as.character(start_year))
  col_year_end   <- which(colnames(country_subset)==as.character(end_year))
  longterm_predictions$GDP_growth_weo <- 0
  longterm_predictions$GDP_deflator_weo <- 0
  longterm_predictions$population_growth_weo <-0

  ngdp_r <- as.numeric(country_subset[1,col_year_first:col_year_end])
  ngdp_d <- as.numeric(country_subset[2,col_year_first:col_year_end])
  pl     <- as.numeric(country_subset[3,col_year_first:col_year_end])
  #pcpipch <- as.numeric(country_subset[4,col_year_first:col_year_end])
  #longterm_predictions$consumer_price_pct_change_weo <-pcpipch


  for (i in 2:nrow(longterm_predictions)){
    longterm_predictions$GDP_growth_weo[i] <- (ngdp_r[i]/ngdp_r[(i-1)]-1)*100
    longterm_predictions$GDP_deflator_weo[i] <- (ngdp_d[i]/ngdp_d[(i-1)]-1)*100
    longterm_predictions$population_growth_weo[i] <- (pl[i]/pl[(i-1)]-1)*100
  }

  new_row_start <- (which(longterm_predictions$year==max(longterm_predictions$year,na.rm = T))+1)
  longterm_predictions$year[new_row_start:nrow(longterm_predictions)] <- (max(longterm_predictions$year,na.rm = T)+1):end_year
  longterm_predictions$country <- unique(longterm_predictions$country)[1]
  longterm_predictions$test_set_steps <- unique(longterm_predictions$test_set_steps)[1]
  #longterm_predictions$consumer_price_inflation_pct[new_row_start:nrow(longterm_predictions)] <- longterm_predictions$consumer_price_pct_change_weo[new_row_start:nrow(longterm_predictions)]

  longterm_predictions$GDP_deflator[new_row_start:nrow(longterm_predictions)] <- longterm_predictions$GDP_deflator_weo[new_row_start:nrow(longterm_predictions)]
  longterm_predictions$GDP_growth[new_row_start:nrow(longterm_predictions)] <- longterm_predictions$GDP_growth_weo[new_row_start:nrow(longterm_predictions)]

  for (i in new_row_start:nrow(longterm_predictions)){
    longterm_predictions$GDP[i] <- longterm_predictions$GDP[(i-1)]*(1+longterm_predictions$GDP_growth_weo[i]/100)
    longterm_predictions$population[i] <- longterm_predictions$population[(i-1)]*(1+longterm_predictions$population_growth_weo[i]/100)
  }

  for (i in new_row_start:nrow(longterm_predictions)){
    longterm_predictions$industrial_value_added[i] <-  mean(longterm_predictions$industrial_value_added[(i-3):(i-1)]/longterm_predictions$GDP[(i-3):(i-1)]) * longterm_predictions$GDP[i]
    longterm_predictions$manufacturing_value_added[i] <-       mean(longterm_predictions$manufacturing_value_added[(i-3):(i-1)]/longterm_predictions$GDP[(i-3):(i-1)]) * longterm_predictions$GDP[i]
    longterm_predictions$service_value_added[i] <-  mean(longterm_predictions$service_value_added[(i-3):(i-1)]/longterm_predictions$GDP[(i-3):(i-1)]) * longterm_predictions$GDP[i]
    longterm_predictions$household_consumption_expenditure[i] <-  mean(longterm_predictions$household_consumption_expenditure[(i-3):(i-1)]/longterm_predictions$GDP[(i-3):(i-1)]) * longterm_predictions$GDP[i]
    longterm_predictions$GNI[i] <-  mean(longterm_predictions$GNI[(i-3):(i-1)]/longterm_predictions$GDP[(i-3):(i-1)]) * longterm_predictions$GDP[i]
  }

  rural_model <- stats::lm(rural_population~ year, data = longterm_predictions[1:(new_row_start-1),])
  longterm_predictions$rural_population[new_row_start:nrow(longterm_predictions)]<-stats::predict(rural_model,newdata = longterm_predictions[new_row_start:nrow(longterm_predictions),] )

  return(longterm_predictions)

}
