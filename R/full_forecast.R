
#' Title
#'
#' @param start_year
#' @param end_year
#' @param country
#'
#' @return
#' @export
#'
#' @examples
full_forecast <- function(start_year, end_year, country){

demand_data <- oRaklE::get_entsoE_data(start_year,end_year,country)
demand_data_filled <- oRaklE::fill_missing_data(demand_data)
decomposed_data <- oRaklE::decompose_load_data(demand_data_filled)
longterm <- oRaklE::get_historic_load_data(decomposed_data$longterm)
longterm_all_data <- oRaklE::get_macro_economic_data(longterm)
longterm_all_data_predicted <- oRaklE::long_term_lm(longterm_all_data)

midterm <- oRaklE::add_holidays_mid_term(decomposed_data$midterm)
midterm_all <- oRaklE::get_weather_data(midterm)
midterm_all_data_predicted <- oRaklE::mid_term_lm(midterm_all$midterm)

shortterm <- oRaklE::add_holidays_short_term(decomposed_data$shortterm)
short_term_data_predicted <- oRaklE::short_term_lm(shortterm)

combined_model_results <- oRaklE::combine_models(longterm_all_data_predicted,midterm_all_data_predicted,short_term_data_predicted)

return(combined_model_results)
}



