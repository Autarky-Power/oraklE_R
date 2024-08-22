
#' Title
#'
#' @param start_year Specifies the starting year for which predictions and models will be generated.
#' @param end_year_data Specifies the final year for which data from the Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/) is retrieved and for which models will be generated.
#' @param end_year Specifies the final year for which future predictions will be generated.
#' @param country String. Specifies the country.
#' @param test_set_steps Integer. Specifies how many years are used for generating the test/validation set for the model selection.
#' @param future String. Option to enable or disable the future forecasts. If set to "Yes" forecasts will be made until the specified end_year. If set to
#' anything else, forecasts will only be generated until the specified end_year_data value.
#'
#' #' @seealso See also functions \code{\link{long_term_lm}}, \code{\link{mid_term_lm}}, and \code{\link{short_term_lm}} for the prediction models
#' and \code{\link{long_term_future}}, \code{\link{mid_term_future}}, and \code{\link{short_term_future}} for the future prediction models.
#'
#' @return Returns a dataframe with the full prediction results. All models, plots and data is saved to a folder with the corresponding
#' iso2c country code.
#' @export
#'
#' @examples
#' \dontrun{
#'   start_year <- 2017
#'   end_year <- 2023
#'   country <- "France"
#'   test_set_steps <- 2
#'   forecast_data <- full_forecast(start_year, end_year, country, test_set_steps)
#' }
full_forecast <- function(start_year, end_year_data, country, test_set_steps=2, future="Yes", end_year=2028){

demand_data <- oRaklE::get_entsoE_data(start_year,end_year_data,country)
demand_data_filled <- oRaklE::fill_missing_data(demand_data)
decomposed_data <- oRaklE::decompose_load_data(demand_data_filled)
longterm <- oRaklE::get_historic_load_data(decomposed_data$longterm)
longterm_all_data <- oRaklE::get_macro_economic_data(longterm)
longterm_all_data_predicted <- oRaklE::long_term_lm(longterm_all_data,  test_set_steps=test_set_steps)

midterm <- oRaklE::add_holidays_mid_term(decomposed_data$midterm)
midterm_all <- oRaklE::get_weather_data(midterm)
midterm_all_data_predicted <- oRaklE::mid_term_lm(midterm_all$midterm,  test_set_steps=test_set_steps*365)

shortterm <- oRaklE::add_holidays_short_term(decomposed_data$shortterm)
short_term_data_predicted <- oRaklE::short_term_lm(shortterm, test_set_steps=test_set_steps*8760)

combined_model_results <- oRaklE::combine_models(longterm_all_data_predicted,midterm_all_data_predicted,short_term_data_predicted,longterm_model_number =1)

if(future=="Yes"){
  longterm_future_macro_data <- oRaklE::long_term_future_data(longterm_all_data_predicted, end_year = end_year, dataset = "WEO")
  longterm_future_predictions <- oRaklE::long_term_future(longterm_future_macro_data)

  midterm_future_predictions = mid_term_future(midterm_all_data_predicted, end_year = end_year)
  shortterm_future_predictions = short_term_future(short_term_data_predicted,end_year = end_year)

  full_model_future_predictions <- combine_models_future(longterm_future_predictions,midterm_future_predictions,
                                                         shortterm_future_predictions,longterm_model_number =1)
  return(full_model_future_predictions)
}
else{
return(combined_model_results)
}
}



