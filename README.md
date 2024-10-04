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

<br>

### Calculate and show the best long-term component models
First, historical data from the ENTSO-E archive, starting from 2006, is added to the long-term trend series.
A dataset with historic yearly demand data for each ENTSOE-E member country is included in the library.

```r
# Get historical data for the respective country
longterm <- get_historic_load_data(decomposed_data$longterm)
```

The long-term electricity demand trend is estimated with different regression algorithms based on macro-economic covariates. 10 macro-economic indicators are fetched via an API call to the [Worldbank Development Indicators](https://databank.worldbank.org/source/world-development-indicators). Additional macro-economic covariates can be added manually.

```r
# Get macro-economic covariates for the respective country
longterm_all_data <- get_macro_economic_data(longterm)

head(longterm_all_data)
 country   year  avg_hourly_demand    population        GDP         industrial_value_added   ...
  FR       2006        54417.12        63628261     2.279283e+12           19.28408
  FR       2007        54769.12        64021737     2.334550e+12           19.13674
  FR       2008        56214.75        64379696     2.340502e+12           18.81383
  FR       2009        55409.97        64710879     2.273252e+12           18.30484
  ...
```
It should be noted that the average hourly demand (*avg_hourly_demand*) refers to the average demand over each hour of the respective year (typically in MW). To calculate the total annual demand, multiply *avg_hourly_demand* by 8760 hours.


After the dataset is fully prepared the best long-term prediction models are derived with multiple linear regression and k-fold cross-validation. Details on the mathematical approach are specified in the accompanying paper. The variable for *test_set_steps* defines how many years are used for the test set (also commonly referred to as validation set). The *testquant* variable defines how many of the initial best models are subjected to cross-validation.

```r
# Calculate the best prediction models
longterm_predictions <- long_term_lm(longterm_all_data,test_set_steps = 2, testquant = 500)
```

The three best models as well as plots for each model are generated and saved.

![Long_term_results1](https://github.com/user-attachments/assets/3facab6e-5e7c-4f6a-8e13-c53d6d9591a0)

Once the best models are derived, future predictions can be made. Since these models rely on macroeconomic covariates, which are unknown for future years, forecasts for these indicators are needed. The library can obtain these forecasts from the [World Economic Outlook Database](https://www.imf.org/en/Publications/WEO/weo-database/2023/October) of the International Monetary Fund, or users can manually include predictions from other sources or specific scenarios.

The *end_year* variable specifies until which year the predictions will be made. If the *WEO* dataset is used, predictions can only be made up to 2028. If the dataset variable is set to any option other than WEO, the function will prepare the data frame structure and list the required macroeconomic indicators. 

```r
## Prepare dataset for future predictions
# With the dataset option set to the default ("WEO")
longterm_future_macro_data <- long_term_future_data(longterm_predictions, end_year = 2028, dataset = "WEO")
tail(longterm_future_macro_data)

country   year   avg_hourly_demand     population          GDP           industrial_value_added   ...
  FR      2021          53225.29        67764304      2.575192e+12               16.39563
  FR      2022             NA           71764951      2.640147e+12               17.56864
  FR      2023             NA           75808393      2.665256e+12               17.73190
  FR      2024             NA           77673096      2.701124e+12               17.71412
  FR      2025             NA           79192297      2.750043e+12               18.21028
  FR      2026             NA           80760586      2.795947e+12               18.48387
  FR      2027             NA           82230070      2.838581e+12               18.72595
  FR      2028             NA           83540334      2.879402e+12               19.03256

# With the dataset option set to anything else (any string works)
longterm_future_macro_data <- long_term_future_data(longterm_predictions, end_year = 2028, dataset = "manual")

Output:
If you want to use your own dataset you will need predictions for the following macro-economic variables:

 GDP, GNI, industrial_value_added, rural_population for model 1 

 GDP_growth, household_consumption_expenditure, rural_population, service_value_added for model 2 

 GDP, industrial_value_added, rural_population, service_value_added for model 3 
```

After the dataset for the future predictions is prepared, long-term forecasts until the designated time period can be made. One forecast with each of the three best models is calculated and the results are saved and plotted.

```r
# Generate future long-term component forecasts
longterm_future_predictions <- long_term_future(longterm_future_macro_data)
```

![Long_term_results_future](https://github.com/user-attachments/assets/6ea76ae8-5aea-4d3b-9c48-132da3ebc269)

### Calculate and show the best mid-term seasonality models

The mid-term component is the difference between the yearly hourly average demand and the daily average hourly demand of the respective day:

$$D_M(y,d) = D_m(d)-D_L(y)$$

where $D_M(y,d)$ refers to the mid-term component at day $d$ and year $y$, $D_m(d)$ refers to the average daily hourly load of day $d$, and $D_L(y)$ refers to the yearly average hourly load of year $y$.

The mid-term time series is modelled using seasonal, calendar, and temperature-related variables. Seasonal covariates include the month (January-December), day of the week (Sunday-Saturday), and a dummy variable indicating whether the day is a holiday or a workday. Information about public holidays is retrieved from [https://date.nager.at/api/v3/publicholidays/](https://date.nager.at/api/v3/publicholidays/).

```r
# Get all national holidays within the respective time period
midterm_demand_data = add_holidays_mid_term(decomposed_data$midterm)
```

Daily temperature values are obtained by first retrieving the 20 most populated regions of the respective country or area from [https://wft-geo-db.p.rapidapi.com](https://wft-geo-db.p.rapidapi.com)). Next, the nearest weather station for each region is identified using [https://meteostat.p.rapidapi.com](https://meteostat.p.rapidapi.com) and the daily temperature data is downloaded. A weighted daily average temperature based on population is calculated for the provided country.

```r
midterm_demand_and_weather_data = get_weather_data(midterm_demand_data)
```

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

