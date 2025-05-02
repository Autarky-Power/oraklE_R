#' Example Demand Data
#'
#' This dataset contains the hourly electricity demand data of France from 2017 until 2021.
#'
#' @format A data frame with 43,769 rows and 7 columns:
#' \describe{
#'   \item{\code{date}}{The date and time of the demand measurement (in `YYYY-MM-DD HH:MM:SS` format).}
#'   \item{\code{load}}{The electricity demand.}
#'   \item{\code{unit}}{The unit of measurement (MW).}
#'   \item{\code{year}}{The year of the respective timepoint.}
#'   \item{\code{time_interval}}{The time interval at which the demand was reported (e.g., `60 mins`).}
#'   \item{\code{country}}{The country, represented by the ISO2C country code (e.g., `FR` for France).}
#'   \item{\code{example}}{A boolean indicator to mark this dataset as an example dataset.}
#' }
#' @source  Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/)
#' @keywords dataset electricity demand
"example_demand_data"

#' Example Demand Data Filled
#'
#' This dataset contains the same data as \code{\link{example_demand_data}} with any missing values filled.
#'
#' @format A data frame with 43,824 rows and 7 columns:
#' \describe{
#'   \item{\code{date}}{The date and time of the demand measurement (in `YYYY-MM-DD HH:MM:SS` format).}
#'   \item{\code{load}}{The electricity demand.}
#'   \item{\code{unit}}{The unit of measurement (MW).}
#'   \item{\code{year}}{The year of the respective timepoint.}
#'   \item{\code{time_interval}}{The time interval at which the demand was reported (e.g., `60 mins`).}
#'   \item{\code{country}}{The country, represented by the ISO2C country code (e.g., `FR` for France).}
#'   \item{\code{example}}{A boolean indicator to mark this dataset as an example dataset.}
#' }
#' @source  Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/)
#' @keywords dataset demand
"example_demand_data_filled"

#' Example Decomposed Data
#'
#' This dataset contains the decomposed data of \code{\link{example_demand_data_filled}} into a long-term trend, a mid-term seasonality, and a short-term seasonality. It contains a list of three dataframes, one for each component.
#'
#' @format A list with three dataframes:
#' \describe{
#' \item{\code{longterm}}{A dataframe with long-term trend specific data}
#' \item{\code{midterm}}{A dataframe with mid-term seasonality specific data}
#' \item{\code{shortterm}}{A dataframe with short-term seasonality specific data}
#' }
#'  **Longterm Data Frame**:
#' \describe{
#'   \item{\code{longterm.country}}{The country, represented by the ISO2C country code (e.g., `FR` for France).}
#'   \item{\code{longterm.year}}{The respective year.}
#'   \item{\code{longterm.avg_hourly_demand}}{The average hourly electricity demand (in megawatts) for each year.}
#'   \item{\code{example}}{A boolean indicator to mark this dataset as an example dataset.}
#'   }
#'  **Midterm Data Frame**:
#'  \describe{
#'   \item{\code{midterm.country}}{The country, represented by the ISO2C country code (e.g., `FR` for France).}
#'   \item{\code{midterm.date}}{The date of the demand measurement (in `YYYY-MM-DD` format).}
#'   \item{\code{midterm.year}}{The respective year.}
#'   \item{\code{midterm.month}}{The respective month.}
#'   \item{\code{midterm.day}}{The respective day.}
#'   \item{\code{midterm.wday}}{The type of weekday (e.g., `Sun`, `Mon`)}
#'   \item{\code{midterm.avg_hourly_demand}}{The average hourly electricity demand (in megawatts) for each day.}
#'   \item{\code{midterm.seasonal_avg_hourly_demand}}{The seasonal mid-term component of the demand (in megawatts).}
#'   \item{\code{example}}{A boolean indicator to mark this dataset as an example dataset.}
#'   }
#'   **Shortterm Data Frame**
#'   \describe{
#'   \item{\code{shortterm.country}}{The country, represented by the ISO2C country code (e.g., `FR` for France).}
#'   \item{\code{shortterm.date}}{The date of the demand measurement (in `YYYY-MM-DD HH:MM:SS` format).}
#'   \item{\code{shortterm.year}}{The respective year.}
#'   \item{\code{shortterm.month}}{The respective month.}
#'   \item{\code{shortterm.day}}{The respective day.}
#'   \item{\code{shortterm.wday}}{The type of weekday (e.g., `Sun`, `Mon`)}
#'   \item{\code{shortterm.hour}}{The respective hour (from 0 to 23).}
#'   \item{\code{shortterm.hourly_demand}}{The actual hourly electricity demand (in megawatts) for each hour.}
#'   \item{\code{shortterm.hourly_demand_trend_corrected}}{The demand substracted by the long-term trend.}
#'   \item{\code{shortterm.yearly}}{The yearly average electricity demand in the respective year.}
#'   \item{\code{shortterm.daily}}{The daily average electricity demand in the respective day.}
#'   \item{\code{shortterm.hourly_demand_trend_and_season_corrected}}{The short-term seasonal component which is the hourly demand, substracted by both the long-term trend and the mid-term seasonality.}
#'   \item{\code{example}}{A boolean indicator to mark this dataset as an example dataset.}
#' }
#' @source Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/)
#' @keywords dataset decomposed
"example_decomposed_data"

#' Example Longterm Data
#'
#' This dataset contains the average hourly electricity demand per year for France from 2006 until 2021.
#' It is an extension of the long-term component of \code{\link{example_decomposed_data}} with historical data from the ENTSO-E Power Stats archive (https://www.entsoe.eu/data/power-stats/).
#'
#' @format A data frame with 16 rows and 4 columns:
#' \describe{
#'   \item{\code{country}}{The country, represented by the ISO2C country code (e.g., `FR` for France).}
#'   \item{\code{year}}{The year of the observation.}
#'   \item{\code{avg_hourly_demand}}{The average hourly electricity demand (in megawatts) for each year.}
#'   \item{\code{example}}{A boolean indicator to mark this dataset as an example dataset.}
#' }
#' @source Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/) ; ENTSO-E Power Stats Archive (https://www.entsoe.eu/data/power-stats/)
#' @keywords dataset longterm
"example_longterm_data"

#' Example Longterm and Macro Data
#'
#' This dataset extends the long-term average hourly electricity demand data from \code{\link{example_longterm_data}} with ten macro-economic indicators. The macro-economic data is taken from the World Development Indicators (WDI) of the World Bank (https://databank.worldbank.org/source/world-development-indicators).
#'
#' @format A data frame with 16 rows and 14 columns:
#' \describe{
#'   \item{\code{country}}{The country, represented by the ISO2C country code (e.g., `FR` for France).}
#'   \item{\code{year}}{The year of the observation.}
#'   \item{\code{avg_hourly_demand}}{The average hourly electricity demand (in megawatts) for the respective year.}
#'   \item{\code{population}}{The total population in the respective year.}
#'   \item{\code{GDP}}{Gross Domestic Product (in constant 2015 USD) in the respective year.}
#'   \item{\code{industrial_value_added}}{The percentage of GDP attributed to industrial value-added activities.}
#'   \item{\code{manufacturing_value_added}}{The percentage of GDP attributed to manufacturing value-added activities.}
#'   \item{\code{GDP_growth}}{The GDP growth rate (in percentage) for the respective year.}
#'   \item{\code{GDP_deflator}}{The GDP deflator (in percentage), which measures price inflation or deflation.}
#'   \item{\code{service_value_added}}{The percentage of GDP attributed to service sector value-added activities.}
#'   \item{\code{GNI}}{Gross National Income (in constant 2015 USD) in the respective year.}
#'   \item{\code{household_consumption_expenditure}}{The percentage of GDP attributed to household consumption expenditure.}
#'   \item{\code{rural_population}}{The rural population in the respective year.}
#'   \item{\code{example}}{A boolean indicator to mark this dataset as an example dataset.}
#' }
#' @source World Development Indicators (WDI) of the World Bank (https://databank.worldbank.org/source/world-development-indicators); Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/); ENTSO-E Power Stats Archive (https://www.entsoe.eu/data/power-stats/)
#' @keywords dataset longterm macro-economic
"example_longterm_and_macro_data"

#' Example Longterm Predictions Data
#'
#' This dataset extends the long-term trend component and the macro-economic data from \code{\link{example_longterm_and_macro_data}} with the prediction results of the three best derived trend models.
#'
#' @format A data frame with 16 rows and 18 columns:
#' \describe{
#'   \item{\code{country}}{The country, represented by the ISO2C country code (e.g., `FR` for France).}
#'   \item{\code{year}}{The year of the observation.}
#'   \item{\code{avg_hourly_demand}}{The average hourly electricity demand (in megawatts) for the respective year.}
#'   \item{\code{population}}{The total population in the respective year.}
#'   \item{\code{GDP}}{Gross Domestic Product (in constant 2015 USD) in the respective year.}
#'   \item{\code{industrial_value_added}}{The percentage of GDP attributed to industrial value-added activities.}
#'   \item{\code{manufacturing_value_added}}{The percentage of GDP attributed to manufacturing value-added activities.}
#'   \item{\code{GDP_growth}}{The GDP growth rate (in percentage) for the respective year.}
#'   \item{\code{GDP_deflator}}{The GDP deflator (in percentage), which measures price inflation or deflation.}
#'   \item{\code{service_value_added}}{The percentage of GDP attributed to service sector value-added activities.}
#'   \item{\code{GNI}}{Gross National Income (in constant 2015 USD) in the respective year.}
#'   \item{\code{household_consumption_expenditure}}{The percentage of GDP attributed to household consumption expenditure.}
#'   \item{\code{rural_population}}{The rural population in the respective year.}
#'   \item{\code{longterm_model_predictions1}}{Predictions for the long-term trend component of electricity demand based on Model 1.}
#'   \item{\code{longterm_model_predictions2}}{Predictions for the long-term trend component of electricity demand based on Model 2.}
#'   \item{\code{longterm_model_predictions3}}{Predictions for the long-term trend component of electricity demand based on Model 3.}
#'   \item{\code{test_set_steps}}{The number of years used in the test or validation set for the model evaluation.}
#'   \item{\code{example}}{A boolean indicator to mark this dataset as an example dataset.}
#' }
#' @source Demand predictions: \code{\link{long_term_lm}} ;World Development Indicators (WDI) of the World Bank (https://databank.worldbank.org/source/world-development-indicators); Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/); ENTSO-E Power Stats Archive (https://www.entsoe.eu/data/power-stats/)
#' @keywords dataset longterm prediction
"example_longterm_predictions"

#' Example Longterm Future Macro Data
#'
#' This dataset extends the macro-economic data from \code{\link{example_longterm_predictions}} until the year 2028. The macro-econmic data for the years 2023 until 2028 is derived from the World Economic Outlook Database (April 2023 edition) of the International Monetary Fund (IMF) (https://www.imf.org/en/Publications/WEO/weo-database/2023/October).
#'
#' @format A data frame with 23 rows and 18 columns:
#' \describe{
#'   \item{\code{country}}{The country, represented by the ISO2C country code (e.g., `FR` for France).}
#'   \item{\code{year}}{The year of the observation.}
#'   \item{\code{avg_hourly_demand}}{The average hourly electricity demand (in megawatts) for the respective year.}
#'   \item{\code{population}}{The total population in the respective year.}
#'   \item{\code{GDP}}{Gross Domestic Product (in constant 2015 USD) in the respective year.}
#'   \item{\code{industrial_value_added}}{The percentage of GDP attributed to industrial value-added activities.}
#'   \item{\code{manufacturing_value_added}}{The percentage of GDP attributed to manufacturing value-added activities.}
#'   \item{\code{GDP_growth}}{The GDP growth rate (in percentage) for the respective year.}
#'   \item{\code{GDP_deflator}}{The GDP deflator (in percentage), which measures price inflation or deflation.}
#'   \item{\code{service_value_added}}{The percentage of GDP attributed to service sector value-added activities.}
#'   \item{\code{GNI}}{Gross National Income (in constant 2015 USD) in the respective year.}
#'   \item{\code{household_consumption_expenditure}}{The percentage of GDP attributed to household consumption expenditure.}
#'   \item{\code{rural_population}}{The rural population in the respective year.}
#'   \item{\code{longterm_model_predictions1}}{Predictions for the long-term trend component of electricity demand based on Model 1.}
#'   \item{\code{longterm_model_predictions2}}{Predictions for the long-term trend component of electricity demand based on Model 2.}
#'   \item{\code{longterm_model_predictions3}}{Predictions for the long-term trend component of electricity demand based on Model 3.}
#'   \item{\code{test_set_steps}}{The number of years used in the test or validation set for the model evaluation.}
#'   \item{\code{example}}{A boolean indicator to mark this dataset as an example dataset.}
#' }
#' @source World Economic Outlook Database (April 2023 edition) of the International Monetary Fund (IMF) (https://www.imf.org/en/Publications/WEO/weo-database/2023/October); World Development Indicators (WDI) of the World Bank (https://databank.worldbank.org/source/world-development-indicators); Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/); ENTSO-E Power Stats Archive of https://www.entsoe.eu/data/power-stats/
#' @keywords dataset longterm future macro-economic
"example_longterm_future_macro_data"

#' Example Longterm Future Predictions Data
#'
#' This dataset extends the long-term trend from \code{\link{example_longterm_predictions}} until the year 2028.
#'
#' @format A data frame with 23 rows and 18 columns:
#' \describe{
#'   \item{\code{country}}{The country, represented by the ISO2C country code (e.g., `FR` for France).}
#'   \item{\code{year}}{The year of the observation.}
#'   \item{\code{avg_hourly_demand}}{The average hourly electricity demand (in megawatts) for the respective year.}
#'   \item{\code{population}}{The total population in the respective year.}
#'   \item{\code{GDP}}{Gross Domestic Product (in constant 2015 USD) in the respective year.}
#'   \item{\code{industrial_value_added}}{The percentage of GDP attributed to industrial value-added activities.}
#'   \item{\code{manufacturing_value_added}}{The percentage of GDP attributed to manufacturing value-added activities.}
#'   \item{\code{GDP_growth}}{The GDP growth rate (in percentage) for the respective year.}
#'   \item{\code{GDP_deflator}}{The GDP deflator (in percentage), which measures price inflation or deflation.}
#'   \item{\code{service_value_added}}{The percentage of GDP attributed to service sector value-added activities.}
#'   \item{\code{GNI}}{Gross National Income (in constant 2015 USD) in the respective year.}
#'   \item{\code{household_consumption_expenditure}}{The percentage of GDP attributed to household consumption expenditure.}
#'   \item{\code{rural_population}}{The rural population in the respective year.}
#'   \item{\code{longterm_model_predictions1}}{Predictions for the long-term trend component of electricity demand based on Model 1.}
#'   \item{\code{longterm_model_predictions2}}{Predictions for the long-term trend component of electricity demand based on Model 2.}
#'   \item{\code{longterm_model_predictions3}}{Predictions for the long-term trend component of electricity demand based on Model 3.}
#'   \item{\code{test_set_steps}}{The number of years used in the test or validation set for the model evaluation.}
#'   \item{\code{example}}{A boolean indicator to mark this dataset as an example dataset.}
#' }
#'  @source Demand predictions until 2021: \code{\link{long_term_lm}}; Demand predictions from 2022-2028: \code{\link{long_term_future}} ;World Economic Outlook Database (April 2023 edition) of the International Monetary Fund (IMF) (https://www.imf.org/en/Publications/WEO/weo-database/2023/October); World Development Indicators (WDI) of the World Bank (https://databank.worldbank.org/source/world-development-indicators); Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/); ENTSO-E Power Stats Archive of https://www.entsoe.eu/data/power-stats/
#' @keywords dataset longterm prediction
"example_longterm_future_predictions"

#' Example Midterm Demand Data
#' This dataset contains the seasonal mid-term demand (the difference between the yearly average hourly electricity demand and the daily average hourly electricity demand)
#' for each day for France from 2017 until 2021.
#'
#' @format A data frame with 1,825 rows and 10 columns:
#' \describe{
#'   \item{\code{country}}{The country, represented by the ISO2C country code (e.g., `FR` for France).}
#'   \item{\code{date}}{The date of the mid-term demand measurement (in `YYYY-MM-DD` format).}
#'   \item{\code{year}}{The year of the observation.}
#'   \item{\code{month}}{The month of the observation.}
#'   \item{\code{day}}{The day of the month for the observation.}
#'   \item{\code{wday}}{The day of the week for the observation (where 1 represents Sunday and 7 represents Saturday).}
#'   \item{\code{avg_hourly_demand}}{The average hourly electricity demand (in megawatts) for the day.}
#'   \item{\code{seasonal_avg_hourly_demand}}{The seasonal average hourly demand (in megawatts) for the day.}
#'   \item{\code{holiday}}{Indicates whether the day is a public holiday (`1` for holiday, `0` for non-holiday).}
#'   \item{\code{example}}{A boolean indicator to mark this dataset as an example dataset.}
#' }
#' @source demand data: Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/);
#' holidays: https://date.nager.at/api/v3/publicholidays/
#' @keywords dataset midterm-demand weather-variables
"example_midterm_demand_data"

#' Example Midterm Demand and Weather Data
#'
#' This dataset extends the \code{\link{example_midterm_demand_data}} by adding a weighted average temperature column.
#' The dataset is divided into two parts: `demand` and `temperature_data`. The `demand` dataframe contains the added
#' weighted average temperature column and the other demand related data. The `temperature_data` dataframe contains the daily temperature observations
#' for the 20 most populated regions. This data is provided to show from which locations the weather data was taken.
#'
#' @format A list containing two data frames:
#' \describe{
#'   \item{\code{demand}}{A data frame with 1,825 rows and 10 columns, representing mid-term electricity demand data.}
#'   \item{\code{temperature_data}}{A data frame with 1,826 rows and 22 columns, representing temperature measurements across multiple cities.}
#' }
#'
#' **Demand Data Frame**:
#'  \describe{
#'   \item{\code{demand.country}}{The country, represented by the ISO2C country code (e.g., `FR` for France).}
#'   \item{\code{demand.date}}{The date of the demand measurement (in `YYYY-MM-DD` format).}
#'   \item{\code{demand.year}}{The respective year.}
#'   \item{\code{demand.month}}{The respective month.}
#'   \item{\code{demand.day}}{The respective day.}
#'   \item{\code{demand.wday}}{The type of weekday (e.g., `Sun`, `Mon`)}
#'   \item{\code{demand.avg_hourly_demand}}{The average hourly electricity demand (in megawatts) for each day.}
#'   \item{\code{demand.seasonal_avg_hourly_demand}}{The seasonal mid-term component of the demand (in megawatts).}
#'   \item{\code{demand.weighted_temperature}}{The weighted average temperature for France on that day (in degrees Celsius).}
#'   \item{\code{example}}{A boolean indicator to mark this dataset as an example dataset.}
#' }
#'
#' **Temperature Data Frame**:
#' \describe{
#'   \item{\code{date}}{The date of the temperature observation (in `YYYY-MM-DD` format).}
#'   \item{\code{Paris}, \code{Marseille}, \code{Lyon}, \code{Toulouse}, \code{Nice}, \code{Nantes}, \code{Montpellier}, \code{Strasbourg}, \code{Bordeaux},\code{Cergy-Pontoise},\code{Toulon},\code{Reims}, \code{Lille},\code{15th arrondissement of Paris}, \code{20th arrondissement of Paris}, \code{18th arrondissement of Paris}, \code{19th arrondissement of Paris}, \code{13th arrondissement of Paris}}{The daily average temperature readings (in degrees Celsius) for various cities or city districts on that date.}
#'   \item{\code{weighted_mean_temperature}}{The weighted (by share of population) mean temperature across the country for the respective date.}
#' }
#'
#' @source demand data: Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/);
#' holidays: https://date.nager.at/api/v3/publicholidays/ ; area population: https://wft-geo-db.p.rapidapi.com ; daily average temperatures: https://meteostat.p.rapidapi.com;
#' @keywords dataset midterm demand weather-data
"example_midterm_demand_and_weather_data"


#' Example Midterm Predictions Data
#'
#' This dataset extends the `demand` dataframe from \code{\link{example_midterm_demand_and_weather_data}} with the prediction results from the best derived mid-term seasonality model.
#' It also includes all used covariates for the model selection process.
#'
#' @format A data frame with 1,825 rows and 46 columns:
#' \describe{
#'   \item{\code{country}}{The country, represented by the ISO2C country code (e.g., `FR` for France).}
#'   \item{\code{date}}{The date (in `YYYY-MM-DD` format).}
#'   \item{\code{year}}{The respective year.}
#'   \item{\code{month}}{The respective month.}
#'   \item{\code{day}}{The respective day.}
#'   \item{\code{wday}}{The type of weekday (e.g., `Sun`, `Mon`)}
#'   \item{\code{avg_hourly_demand}}{The average hourly electricity demand (in megawatts) for the day.}
#'   \item{\code{seasonal_avg_hourly_demand}}{The seasonal average hourly demand (in megawatts) for the day.}
#'   \item{\code{holiday}}{Indicates whether the day is a public holiday (`1` for holiday, `0` for non-holiday).}
#'   \item{\code{weighted_temperature}}{The weighted average temperature for France on that day (in degrees Celsius).}
#'   \item{\code{Jan}, \code{Feb}, \code{Mar}, \code{Apr}, \code{May}, \code{Jun}, \code{Jul}, \code{Aug}, \code{Sep}, \code{Nov}, \code{Dec}}{Monthly dummy variables for January through December, indicating the respective month (`1` if the date belongs to the month, `0` otherwise).}
#'   \item{\code{Sun}, \code{Mon}, \code{Tue}, \code{Wed}, \code{Thu}, \code{Fri}, \code{Sat}}{Weekly dummy variables for Sunday through Saturday, indicating the respective weekday (`1` if the date is the specific weekday, `0` otherwise).}
#'   \item{\code{HD}}{The weighted temperature converted to heating degree days.}
#'   \item{\code{CD}}{The weighted temperature converted to cooling degree days.}
#'   \item{\code{HD2}}{The squared heating degree days (HD).}
#'   \item{\code{HD3}}{The cubed heating degree days (HD).}
#'   \item{\code{CD2}}{The squared cooling degree days (CD).}
#'   \item{\code{CD3}}{The cubed cooling degree days (CD).}
#'   \item{\code{weighted_temperature2}}{The squared weighted temperature.}
#'   \item{\code{weighted_temperature3}}{The cubed weighted temperature.}
#'   \item{\code{HDlag1}}{Lagged value of heating degree days (1 day).}
#'   \item{\code{HDlag2}}{Lagged value of heating degree days (2 days).}
#'   \item{\code{CDlag1}}{Lagged value of cooling degree days (1 day).}
#'   \item{\code{CDlag2}}{Lagged value of cooling degree days (2 days).}
#'   \item{\code{weighted_temperaturelag1}}{Lagged weighted temperature (1 day).}
#'   \item{\code{weighted_temperaturelag2}}{Lagged weighted temperature (2 days).}
#'   \item{\code{midterm_model_fit}}{model predictions for the seasonal mid-term component.}
#'   \item{\code{end_of_year}}{Binary dummy variable to account for lower demand between Christmas and New Year's Evening. Starts at 22nd December.}
#'   \item{\code{test_set_steps}}{Number of days used in the test set for model evaluation.}
#'   \item{\code{example}}{A boolean indicator to mark this dataset as an example dataset.}
#' }
#' @source demand data: Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/);
#' holidays: https://date.nager.at/api/v3/publicholidays/ ; area population: https://wft-geo-db.p.rapidapi.com ; daily average temperatures: https://meteostat.p.rapidapi.com;
#' @keywords dataset midterm prediction
"example_midterm_predictions"

#' Example Midterm Future Predictions Data
#'
#' This dataset extends the mid-term electricity demand predictions from \code{\link{example_midterm_predictions}} until the year 2028.
#'
#' @format A data frame with 4,380 rows and 46 columns:
#' \describe{
#'   \item{\code{country}}{The country, represented by the ISO2C country code (e.g., `FR` for France).}
#'   \item{\code{date}}{The date (in `YYYY-MM-DD` format).}
#'   \item{\code{year}}{The respective year.}
#'   \item{\code{month}}{The respective month.}
#'   \item{\code{day}}{The respective day.}
#'   \item{\code{wday}}{The type of weekday (e.g., `Sun`, `Mon`)}
#'   \item{\code{avg_hourly_demand}}{The average hourly electricity demand (in megawatts) for the day.}
#'   \item{\code{seasonal_avg_hourly_demand}}{The seasonal average hourly demand (in megawatts) for the day.}
#'   \item{\code{holiday}}{Indicates whether the day is a public holiday (`1` for holiday, `0` for non-holiday).}
#'   \item{\code{weighted_temperature}}{The weighted average temperature for France on that day (in degrees Celsius).}
#'   \item{\code{Jan}, \code{Feb}, \code{Mar}, \code{Apr}, \code{May}, \code{Jun}, \code{Jul}, \code{Aug}, \code{Sep}, \code{Nov}, \code{Dec}}{Monthly dummy variables for January through December, indicating the respective month (`1` if the date belongs to the month, `0` otherwise).}
#'   \item{\code{Sun}, \code{Mon}, \code{Tue}, \code{Wed}, \code{Thu}, \code{Fri}, \code{Sat}}{Weekly dummy variables for Sunday through Saturday, indicating the respective weekday (`1` if the date is the specific weekday, `0` otherwise).}
#'   \item{\code{HD}}{The weighted temperature converted to heating degree days.}
#'   \item{\code{CD}}{The weighted temperature converted to cooling degree days.}
#'   \item{\code{HD2}}{The squared heating degree days (HD).}
#'   \item{\code{HD3}}{The cubed heating degree days (HD).}
#'   \item{\code{CD2}}{The squared cooling degree days (CD).}
#'   \item{\code{CD3}}{The cubed cooling degree days (CD).}
#'   \item{\code{weighted_temperature2}}{The squared weighted temperature.}
#'   \item{\code{weighted_temperature3}}{The cubed weighted temperature.}
#'   \item{\code{HDlag1}}{Lagged value of heating degree days (1 day).}
#'   \item{\code{HDlag2}}{Lagged value of heating degree days (2 days).}
#'   \item{\code{CDlag1}}{Lagged value of cooling degree days (1 day).}
#'   \item{\code{CDlag2}}{Lagged value of cooling degree days (2 days).}
#'   \item{\code{weighted_temperaturelag1}}{Lagged weighted temperature (1 day).}
#'   \item{\code{weighted_temperaturelag2}}{Lagged weighted temperature (2 days).}
#'   \item{\code{midterm_model_fit}}{model predictions for the seasonal mid-term component.}
#'   \item{\code{end_of_year}}{Binary dummy variable to account for lower demand between Christmas and New Year's Evening. Starts at 22nd December.}
#'   \item{\code{test_set_steps}}{Number of days used in the test set for model evaluation.}
#'   \item{\code{example}}{A boolean indicator to mark this dataset as an example dataset.}
#' }
#' @source demand data: Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/);
#' holidays: https://date.nager.at/api/v3/publicholidays/ ; area population: https://wft-geo-db.p.rapidapi.com ; daily average temperatures: https://meteostat.p.rapidapi.com;
#' @keywords dataset midterm prediction future
"example_midterm_future_predictions"

#' Example Short-term Demand Data
#'
#' This dataset contains the seasonal short-term demand (the difference between the measured hourly demand and the yearly average hourly electricity demand minus the daily average hourly electricity demand).
#' for each hour for France from 2017 until 2021. The short-term seasonality corresponds to the intra-day pattern.
#' @format A data frame with 43,800 rows and 14 columns:
#' \describe{
#'   \item{\code{country}}{The country, represented by the ISO2C country code (e.g., `FR` for France).}
#'   \item{\code{date}}{The date (in `YYYY-MM-DD` format).}
#'   \item{\code{year}}{The respective year.}
#'   \item{\code{month}}{The respective month.}
#'   \item{\code{day}}{The respective day.}
#'   \item{\code{wday}}{The type of weekday (e.g., `Sun`, `Mon`)}
#'   \item{\code{hour}}{The respective hour of the day.}
#'   \item{\code{hourly_demand}}{The actual hourly electricity demand (in megawatts).}
#'   \item{\code{hourly_demand_trend_corrected}}{The hourly demand corrected for long-term trends.}
#'   \item{\code{yearly}}{The yearly average electricity demand.}
#'   \item{\code{daily}}{The daily average electricity demand.}
#'   \item{\code{hourly_demand_trend_and_season_corrected}}{The hourly demand corrected for both long-term trends and seasonal variations.}
#'   \item{\code{holiday}}{A binary indicator for whether the day is a public holiday (`1` for holiday, `0` for non-holiday).}
#'   \item{\code{example}}{A boolean indicator to mark this dataset as an example dataset.}
#' }
#' @source demand data: Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/);
#' holidays: https://date.nager.at/api/v3/publicholidays/
#' @keywords dataset shortterm demand
#'
"example_shortterm_demand_data"

#' Example Shortterm Predictions Data
#'
#' This dataset extends the data from \code{\link{example_shortterm_demand_data}} with the prediction results from the best derived short-term seasonality model.
#' The only covariates used for the model selection process are the hour of the day and an indicator if its a holiday or not.
#'
#' @format A data frame with 43,800 rows and 40 columns:
#' \describe{
#'   \item{\code{country}}{The country, represented by the ISO2C country code (e.g., `FR` for France).}
#'   \item{\code{date}}{The date (in `YYYY-MM-DD` format).}
#'   \item{\code{year}}{The respective year.}
#'   \item{\code{month}}{The respective month.}
#'   \item{\code{day}}{The respective day.}
#'   \item{\code{wday}}{The type of weekday (e.g., `Sun`, `Mon`)}
#'   \item{\code{hour}}{The respective hour of the day.}
#'   \item{\code{hourly_demand}}{The actual hourly electricity demand (in megawatts).}
#'   \item{\code{hourly_demand_trend_corrected}}{The hourly demand corrected for long-term trends.}
#'   \item{\code{yearly}}{The yearly average electricity demand.}
#'   \item{\code{daily}}{The daily average electricity demand.}
#'   \item{\code{hourly_demand_trend_and_season_corrected}}{The hourly demand corrected for both long-term trends and seasonal variations.}
#'   \item{\code{holiday}}{A binary indicator for whether the day is a public holiday (`1` for holiday, `0` for non-holiday).}
#'   \item{\code{Hour0}, \code{Hour1}, \code{Hour2}, \code{Hour3}, \code{Hour4}, \code{Hour5}, \code{Hour6}, \code{Hour7}, \code{Hour8}, \code{Hour9}, \code{Hour10}, \code{Hour11}, \code{Hour12}, \code{Hour13}, \code{Hour14}, \code{Hour15}, \code{Hour16}, \code{Hour17}, \code{Hour18}, \code{Hour19}, \code{Hour20}, \code{Hour21}, \code{Hour22}, \code{Hour23}}{Binary variables indicating the hour of the day, where each variable represents a specific hour (e.g., \code{Hour0} for 00:00 to 00:59, \code{Hour1} for 01:00 to 01:59, and so on up to \code{Hour23} for 23:00 to 23:59).}
#'   \item{\code{short_term_lm_model_predictions}}{Model predictions for the short-term seasonality (the intraday pattern).}
#'   \item{\code{test_set_steps}}{Number of hours used in the test set for model evaluation.}
#'   \item{\code{example}}{A boolean indicator to mark this dataset as an example dataset.}
#' }
#' @source demand data: Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/);
#' holidays: https://date.nager.at/api/v3/publicholidays/
#' @keywords dataset shortterm prediction
"example_shortterm_predictions"

#' Example Shortterm Future Predictions Data
#'
#' This dataset extends the short-term electricity demand predictions from \code{\link{example_shortterm_predictions}} until the year 2028.
#'
#' @format A data frame with 105,120 rows and 40 columns:
#' \describe{
#'     \item{\code{country}}{The country, represented by the ISO2C country code (e.g., `FR` for France).}
#'   \item{\code{date}}{The date (in `YYYY-MM-DD` format).}
#'   \item{\code{year}}{The respective year.}
#'   \item{\code{month}}{The respective month.}
#'   \item{\code{day}}{The respective day.}
#'   \item{\code{wday}}{The type of weekday (e.g., `Sun`, `Mon`)}
#'   \item{\code{hour}}{The respective hour of the day.}
#'   \item{\code{hourly_demand}}{The actual hourly electricity demand (in megawatts).}
#'   \item{\code{hourly_demand_trend_corrected}}{The hourly demand corrected for long-term trends.}
#'   \item{\code{yearly}}{The yearly average electricity demand.}
#'   \item{\code{daily}}{The daily average electricity demand.}
#'   \item{\code{hourly_demand_trend_and_season_corrected}}{The hourly demand corrected for both long-term trends and seasonal variations.}
#'   \item{\code{holiday}}{A binary indicator for whether the day is a public holiday (`1` for holiday, `0` for non-holiday).}
#'   \item{\code{Hour0}, \code{Hour1}, \code{Hour2}, \code{Hour3}, \code{Hour4}, \code{Hour5}, \code{Hour6}, \code{Hour7}, \code{Hour8}, \code{Hour9}, \code{Hour10}, \code{Hour11}, \code{Hour12}, \code{Hour13}, \code{Hour14}, \code{Hour15}, \code{Hour16}, \code{Hour17}, \code{Hour18}, \code{Hour19}, \code{Hour20}, \code{Hour21}, \code{Hour22}, \code{Hour23}}{Binary variables indicating the hour of the day, where each variable represents a specific hour (e.g., \code{Hour0} for 00:00 to 00:59, \code{Hour1} for 01:00 to 01:59, and so on up to \code{Hour23} for 23:00 to 23:59).}
#'   \item{\code{short_term_lm_model_predictions}}{Model predictions for the short-term seasonality (the intraday pattern).}
#'   \item{\code{test_set_steps}}{Number of hours used in the test set for model evaluation.}
#'   \item{\code{example}}{A boolean indicator to mark this dataset as an example dataset.}
#' }
#' @source demand data: Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/);
#' holidays: https://date.nager.at/api/v3/publicholidays/
#' @keywords dataset shortterm future prediction
"example_shortterm_future_predictions"

#' Example Full Model Future Predictions Data
#'
#' This dataset extends the full model predictions from \code{\link{example_full_model_predictions}} until the year 2028.
#' @format A data frame with 43800 rows and 12 columns:
#' \describe{
#'  \item{\code{country}}{The country, represented by the ISO2C country code (e.g., `FR` for France).}
#'   \item{\code{date}}{The date (in `YYYY-MM-DD` format).}
#'   \item{\code{year}}{The respective year.}
#'   \item{\code{month}}{The respective month.}
#'   \item{\code{day}}{The respective day.}
#'   \item{\code{wday}}{The type of weekday (e.g., `Sun`, `Mon`)}
#'   \item{\code{hour}}{The respective hour of the day.}
#'   \item{\code{hourly_demand}}{The actual hourly electricity demand (in megawatts).}
#'   \item{\code{long_term_model}}{The predicted long-term trend (yearly average of hourly demand) from the best long-term forecasting model.}
#'   \item{\code{mid_term_model}}{The predicted mid-term seasonality (daily minus yearly average of hourly demand) from the best mid-term forecasting model.}
#'   \item{\code{short_term_model}}{The predicted short-term seasonality (actual hourly demand minus the long-term trend minus the mid-term seasonality) from the best short-term forecasting model.}
#'   \item{\code{complete_model}}{Final predicted electricity demand for each hour. Derived by adding the results from the long-, mid-, and short-term components.}
#' }
#' @source demand data: Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/);
#' holidays: https://date.nager.at/api/v3/publicholidays/
#' @keywords dataset final-predictions future
"example_full_model_future_predictions"

#' Example Full Model Predictions Data
#'
#' This dataset combines the results from \code{\link{long_term_lm}}, \code{\link{mid_term_lm}}, and \code{\link{short_term_lm}} into the final predictions of hourly electricity demand for France from 2017 until 2021.
#'
#' @format A data frame with 43800 rows and 12 columns:
#' \describe{
#'  \item{\code{country}}{The country, represented by the ISO2C country code (e.g., `FR` for France).}
#'   \item{\code{date}}{The date (in `YYYY-MM-DD` format).}
#'   \item{\code{year}}{The respective year.}
#'   \item{\code{month}}{The respective month.}
#'   \item{\code{day}}{The respective day.}
#'   \item{\code{wday}}{The type of weekday (e.g., `Sun`, `Mon`)}
#'   \item{\code{hour}}{The respective hour of the day.}
#'   \item{\code{hourly_demand}}{The actual hourly electricity demand (in megawatts).}
#'   \item{\code{long_term_model}}{The predicted long-term trend (yearly average of hourly demand) from the best long-term forecasting model.}
#'   \item{\code{mid_term_model}}{The predicted mid-term seasonality (daily minus yearly average of hourly demand) from the best mid-term forecasting model.}
#'   \item{\code{short_term_model}}{The predicted short-term seasonality (actual hourly demand minus the long-term trend minus the mid-term seasonality) from the best short-term forecasting model.}
#'   \item{\code{complete_model}}{Final predicted electricity demand for each hour. Derived by adding the results from the long-, mid-, and short-term components.}
#' }
#' @source demand data: Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/);
#' holidays: https://date.nager.at/api/v3/publicholidays/
#' @keywords dataset final-predictions
"example_full_model_predictions"


#' World Economic Outlook (WEO) Data
#'
#' This dataset contains macroeconomic data and projections from the International Monetary Fund (IMF) World Economic Outlook (WEO). It includes annual data for multiple countries and economic indicators, spanning from 1980 to 2030.
#'
#' @format A data frame with 588 rows and 61 columns:
#' \describe{
#'   \item{\code{WEO Country Code}}{Unique code assigned to each country by the WEO database.}
#'   \item{\code{ISO}}{The ISO3 country code (e.g., `FRA` for France).}
#'   \item{\code{WEO Subject Code}}{Unique code representing the economic indicator or subject in the WEO database.}
#'   \item{\code{Country}}{The name of the country.}
#'   \item{\code{Subject Descriptor}}{Description of the economic indicator.}
#'   \item{\code{Subject Notes}}{Additional notes or details about the economic indicator.}
#'   \item{\code{Units}}{The unit of measurement for the indicator (e.g., percentage, persons, national currency).}
#'   \item{\code{Scale}}{The scaling factor for the indicator values (e.g., "Billions").}
#'   \item{\code{Country/Series-specific Notes}}{Country-specific notes about the data series.}
#'   \item{\code{1980}, \code{1981}, \code{1982}, \code{1983}, \code{1984},
#'         \code{1985}, \code{1986}, \code{1987}, \code{1988}, \code{1989},
#'         \code{1990}, \code{1991}, \code{1992}, \code{1993}, \code{1994},
#'         \code{1995}, \code{1996}, \code{1997}, \code{1998}, \code{1999},
#'         \code{2000}, \code{2001}, \code{2002}, \code{2003}, \code{2004},
#'         \code{2005}, \code{2006}, \code{2007}, \code{2008}, \code{2009},
#'         \code{2010}, \code{2011}, \code{2012}, \code{2013}, \code{2014},
#'         \code{2015}, \code{2016}, \code{2017}, \code{2018}, \code{2019},
#'         \code{2020}, \code{2021}, \code{2022}, \code{2023}, \code{2024},
#'         \code{2025}, \code{2026}, \code{2027}, \code{2028},
#'         \code{2029}, \code{2030}}{The annual value of the economic indicator for the respective year, starting from 1980 (\code{1980}) to 2028 (\code{2028}).}
#'   \item{\code{Estimates Start After}}{The year after which data values are based on projections instead of past values.}
#' }
#'
#' @source World Economic Outlook Database (April 2025 edition) of the International Monetary Fund (IMF) (https://www.imf.org/en/Publications/WEO/weo-database/2023/October)
#' @keywords dataset macroeconomics IMF WEO projections
"weo_data"
