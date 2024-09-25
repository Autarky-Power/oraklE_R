# oraklE_R

R package for Long-term Electricity Demand Forecasting in hourly resolution on a country wide scale.




[![Github All Releases](https://img.shields.io/github/downloads/Autarky-Power/orakle/total.svg)]()

## Flowchart for package use

The functions included in the package can be used separately or combined in the function full_forecast()

<br>

<div align="center">
<img src="https://github.com/Autarky-Power/oraklE_R/assets/45041403/c166e930-876a-4e90-873a-3f4bcda249a7)https://github.com/Autarky-Power/oraklE_R/assets/45041403/c166e930-876a-4e90-873a-3f4bcda249a7" width="600">
</div>

<br>

## Installation
(Aspirational) Once on CRAN:
```r
install.packages("oRaklE")
```

Alternatively, install the development version


```r
install.packages("devtools")
devtools::install_github("Autarky-Power/oraklE_R")
```

Install requirements:
```r
packages <- c("caret","countrycode","doParallel","dplyr","ggplot2","ggthemes","glmnet","httr",
             "jsonlite","lubridate","MLmetrics","MuMIn","parallel","patchwork","purrr","R.utils",
              "readxl", "xml2")
install.packages(setdiff(packages, rownames(installed.packages())))
```

```r
library("oRaklE")
```

## Usage

### Get and prepare the data
As a first step, the package gets the load data from the Transparency Platform of the European Network of
Transmission System Operators for Electricity (ENTSO-E) [https://transparency.entsoe.eu/](https://transparency.entsoe.eu/). If the country is not a member of the ENTSO-E a dataset needs to be supplied manually. An example with South African load data is included at the end of this section. 

```r
# Get initial Data
demand_data = get_entsoE_data(2017,2021,"France")
```

![Load](https://github.com/user-attachments/assets/24450818-c869-4142-a5b3-cb2a2ec7ff36)

The next step checks for missing data and replaces NaN values with the load data one week prior at the same time. So a missing value on a Thursday at 8 pm will be filled with the load value at 8 pm the week before. It also adjusts the dataset to account for changes in time resolution, ensuring that the data is standardized to an hourly resolution. For example, French load data is reported at an hourly resolution from 2017 to 2021 and at a half-hourly resolution from 2022 onwards. Therefore, it is recommended to run this function, even if there are no missing values.

```r
# Fill missing values
demand_data_filled = fill_missing_data(demand_data)
```

After the data is downloaded and standardized, the load time series is decomposed into three components: a yearly long-term trend, a daily mid-term seasonality, and an hourly short-term seasonality. If the data is available only at a daily resolution, the calculation of hourly seasonality is skipped. 


```r
# Decompose the load data
demand_data_filled = fill_missing_data(demand_data)
```

![Decomposed_load](https://github.com/user-attachments/assets/e5db1014-6e2b-4632-ab00-5923e8414553)

The function returns a list of three dataframes —one for each time series component:

- `demand_data$longterm`
- `demand_data$midterm`
- `demand_data$shortterm`
  
In the following steps, each time series will be modelled individually.


### Calculate and show the best long-term model
First historical data from the ENTSO-E archive starting from 2006 is added to the series of the long-term trend.

```r
# Get historical data for the respective country
longterm <- get_historic_load_data(decomposed_data$longterm)
```

```r
longterm_all_data <- get_macro_economic_data(longterm)
longterm_predictions <- long_term_lm(longterm_all_data,test_set_steps = 2)
longterm_future_macro_data <- long_term_future_data(longterm_predictions, end_year = 2028, dataset = "WEO")
longterm_future_predictions <- long_term_future(longterm_future_macro_data)
```

```r
# Midterm model
midterm_demand_data = add_holidays_mid_term(decomposed_data$midterm)
midterm_demand_and_weather_data = get_weather_data(midterm_demand_data)
midterm_predictions = mid_term_lm(midterm_demand_and_weather_data$demand, Tref = 18, method = "temperature transformation")
midterm_future_predictions = mid_term_future(midterm_predictions, end_year = 2028)

# Shortterm model
shortterm_demand_data= add_holidays_short_term(decomposed_data$shortterm)
shortterm_predictions <- short_term_lm(shortterm_demand_data)
shortterm_future_predictions = short_term_future(shortterm_predictions,end_year = 2028)

# Combine all models
full_model_predictions <- combine_models(example_longterm_predictions,example_midterm_predictions,example_shortterm_predictions,longterm_model_number =1)
full_model_future_predictions <- combine_models_future(longterm_future_predictions,midterm_future_predictions,
                                                       shortterm_future_predictions,longterm_model_number =1)
```

