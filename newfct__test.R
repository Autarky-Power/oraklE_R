library(devtools)


start_year = 2017
end_year = 2021
country= "Germany"
full_forecast <- function(start_year, end_year, country){
library(oRaklE)
demand_data <- get_entsoE_data(start_year,end_year,country)
demand_data_filled <- fill_missing_data(demand_data)
decomposed_data <- decompose_load_data(demand_data_filled)
longterm <- get_historic_load_data(decomposed_data$longterm)
longterm_all_data <- get_macro_economic_data(longterm)
longterm_all_data_predicted <- long_term_lm(longterm_all_data)

midterm <- add_holidays_mid_term(decomposed_data$midterm)
midterm_all <- get_weather_data(midterm)
midterm_all_data_predicted <- mid_term_lm(midterm_all$midterm)

shortterm= add_holidays_short_term(decomposed_data$shortterm)
short_term_data_predicted <- short_term_lm(shortterm)

combined_model_results <- combine_models(longterm_all_data_predicted,midterm_all_data_predicted,short_term_data_predicted)
}

full_forecast(2017,2021,"Germany")
