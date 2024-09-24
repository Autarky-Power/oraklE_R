# oraklE_R

R package for Long-term Electricity Demand Forecasting




[![Github All Releases](https://img.shields.io/github/downloads/Autarky-Power/orakle/total.svg)]()

## Flowchart for package use

The functions included in the package can be used separately or combined in the function full_forecast()

<img src="https://github.com/Autarky-Power/oraklE_R/assets/45041403/c166e930-876a-4e90-873a-3f4bcda249a7)https://github.com/Autarky-Power/oraklE_R/assets/45041403/c166e930-876a-4e90-873a-3f4bcda249a7" width="600">


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



```r
# Initial Data
demand_data = get_entsoE_data(2023,2024,"France")
demand_data_filled = fill_missing_data(demand_data)
decomposed_data = decompose_load_data(demand_data_filled)

# Longterm model
longterm <- get_historic_load_data(decomposed_data$longterm)
longterm_all_data <- get_macro_economic_data(longterm)
longterm_predictions <- long_term_lm(longterm_all_data,test_set_steps = 2)
longterm_future_macro_data <- long_term_future_data(longterm_predictions, end_year = 2028, dataset = "WEO")
longterm_future_predictions <- long_term_future(longterm_future_macro_data)

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

