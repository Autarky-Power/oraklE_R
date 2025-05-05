# oraklE_R

R package for Long-term Electricity Demand Forecasting in hourly resolution on a country wide scale.




![Github All Releases](https://img.shields.io/github/downloads/Autarky-Power/orakle/total.svg)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/oRaklE)](https://cran.r-project.org/package=oRaklE) 
[![CRAN downloads – total](https://cranlogs.r-pkg.org/badges/grand-total/oRaklE)](https://CRAN.R-project.org/package=oRaklE)

## Flowchart for package use

The functions included in the package can be used separately or combined in the function full_forecast()



<br> <div align="center"> <img src="https://github.com/user-attachments/assets/11965c6d-df53-466b-bd0e-33bc8f4acd9d" width="700"> </div> <br> 
## Installation
From CRAN:
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
### Usage recommendation and things to consider
The package was originally designed to save the downloaded data, the models, the plots, and the predictions in a newly created folder with the country name in your working directory. The default option is to save the data in a temporal directory which will be deleted if R is shutdown.
However, it is recommended to save the results in a permanent folder or your working directory. To do that you could specify the *data_directory* variable as:

```r
# Specify the current working directory as data directory
data_directory = getwd()

# Specify a designated folder as data directory
data_directory = "some/path/demand_predictions"
```
If you keep the *data_directory* variable as the default, each function will display a prompt, asking if you want to keep the data directory as a tempdir, use your current working directory or choose a directory.

It is also recommended to set the *verbose* argument to TRUE (it is FALSE by default) if a function uses it.  This way you can see a more detailed report of what's happening and see the resulting plots automatically.
The plots will also be in the function output but will throw a warning when you open them (*Use of x$y is discouraged. Use y instead.*). You can safely ignore this message; nothing is wrong—it's only due to a necessity such that CRAN checks will pass. 

### Get and prepare the data
As a first step, the package gets the load data from the Transparency Platform of the European Network of
Transmission System Operators for Electricity (ENTSO-E) [https://transparency.entsoe.eu/](https://transparency.entsoe.eu/). If the country is not a member of the ENTSO-E a dataset needs to be supplied manually. An example with South African load data is included at the end of this section. 
You can either supply your own ENTSO-E Transparency Platform API key or use one of the deposited keys if *api_key* is set to "default".

```r
# Get initial Data
demand_data = get_entsoE_data(2017,2021, "France", api_key="default")
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
demand_data_decomposed = fill_missing_data(demand_data_filled, data_directory=getwd(), verbose=TRUE )
```

![Decomposed_load](https://github.com/user-attachments/assets/e5db1014-6e2b-4632-ab00-5923e8414553)

The function returns a list of three dataframes —one for each time series component:

- `demand_data_decomposed$longterm`
- `demand_data_decomposed$midterm`
- `demand_data_decomposed$shortterm`
  
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


After the dataset is fully prepared the best long-term prediction models are derived with multiple linear regression and k-fold cross-validation. Details on the mathematical approach are specified in the accompanying paper. The variable for *test_set_steps* defines how many years are used for the test set (also commonly referred to as validation set). The *testquant* variable defines how many of the initial best models are subjected to cross-validation. The random seed used for cross validation can be fixed with *rdm_seed*. It is randomly generated by default.

```r
# Calculate the best long-term trend prediction models
longterm_predictions <- long_term_lm(longterm_all_data,test_set_steps = 2, testquant = 500, rdm_seed=421 ,data_directory=getwd(), verbose=TRUE)
```

The three best models as well as plots for each model are generated and saved.

![Long_term_results1](https://github.com/user-attachments/assets/3facab6e-5e7c-4f6a-8e13-c53d6d9591a0)

Once the best models are derived, future predictions can be made. Since these models rely on macroeconomic covariates, which are unknown for future years, forecasts for these indicators are needed. The library can obtain these forecasts from the [World Economic Outlook Database](https://www.imf.org/en/Publications/WEO/weo-database/2025/April) of the International Monetary Fund, or users can manually include predictions from other sources or specific scenarios.

The *end_year* variable specifies until which year the predictions will be made. If the *WEO* dataset is used, predictions can only be made up to 2030. If the dataset variable is set to any option other than WEO, the function will prepare the data frame structure and list the required macroeconomic indicators. The *long_term_lm()* function will output a list with the data and the used models. You can simply put this list directly into the *long_term_future_data()* function. If you only supply the dataframe, either a list with the used models has to be specified for the *model_list* variable (NULL by default) or the *data_directory* needs to contain the used models as .R file.

```r
## Prepare dataset for future predictions
# With the dataset option set to the default ("WEO")
longterm_future_macro_data <- long_term_future_data(longterm_predictions, end_year = 2028, dataset = "WEO", data_directory = getwd())
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

After the dataset for the future predictions is prepared, long-term forecasts until the designated time period can be made. One forecast with each of the three best models is calculated and the results are saved and plotted. A list with the models derived by the *long_term_lm()* function has to be supplied in the *model_list* variable (NULL by default). Alternatively you can pass the *data_directory* which was used in the *long_term_lm()* function.

```r
# Generate future long-term component forecasts
longterm_future_predictions <- long_term_future(longterm_future_macro_data,data_directory = getwd(), model_list = longterm_predictions$longterm_models, verbose = TRUE)
```

![Long_term_results_future](https://github.com/user-attachments/assets/6ea76ae8-5aea-4d3b-9c48-132da3ebc269)

<br>

### Calculate the best mid-term seasonality models

The mid-term component is the difference between the yearly hourly average demand and the daily average hourly demand of the respective day:

$$D_M(y,d) = D_m(d)-D_L(y)$$

where $D_M(y,d)$ refers to the mid-term component at day $d$ and year $y$, $D_m(d)$ refers to the average daily hourly load of day $d$, and $D_L(y)$ refers to the yearly average hourly load of year $y$.

The mid-term time series is modelled using seasonal, calendar, and temperature-related variables. Seasonal covariates include the month (January-December), day of the week (Sunday-Saturday), and a dummy variable indicating whether the day is a holiday or a workday. Information about public holidays is retrieved from [https://date.nager.at](https://date.nager.at).

```r
# Get all national holidays within the respective time period
midterm_demand_data = add_holidays_mid_term(decomposed_data$midterm, api_key = "default")
```

Daily temperature values are obtained by first retrieving the 20 most populated regions of the respective country or area from [https://rapidapi.com/wirefreethought/api/geodb-cities](https://rapidapi.com/wirefreethought/api/geodb-cities). Next, the nearest weather station for each region is identified using [https://rapidapi.com/meteostat/api/meteostat](https://rapidapi.com/meteostat/api/meteostat) and the daily temperature data is downloaded. A weighted daily average temperature for the country is then calculated, using the population share and temperature values of the 20 regions. You can either supply your own API key (from  [https://rapidapi.com/](https://rapidapi.com/)) that is subscribed to geo-db cities and meteostat or use one of the deposited keys if *api_key* is set to "default".

```r
# Get daily average temperature values
midterm_demand_and_weather_data = get_weather_data(midterm_demand_data, api_key="default")
```

This function returns a list of two dataframes —one with the prepared demand data for the models and a dataframe with the daily temperature values for the 20 regions:

- `midterm_demand_and_weather_data$demand`
- `midterm_demand_and_weather_data$temperature_data`

The mid-term seasonality can then be modelled using the *midterm_demand_and_weather_data$demand* data frame. 
Similar to the long-term trend models, the medium-term component is modelled using different regression techniques. The relationship between electricity demand and temperature is non-linear, the library implements two different options to account for this non-linearity: 

1.) If *method = "temperature transformation"*

The daily temperature values are transformed into heating and cooling degree days (HD and CD):

$$HD = \max \lbrace T_{\text{Ref}} - T, 0 \rbrace \quad CD = \max \lbrace T - T_{\text{Ref}}, 0 \rbrace$$

where $T_{\text{Ref}}$ is the reference temperature (usually 18 °C for Central European countries). The reference temperature can be set with the *Tref* option.
Squared, cubed, and up to two-day lagged values of the calculated heating and cooling degree day values, along with the original daily temperatures, are also included as covariates. The covariate selection is then done by a LASSO method (cite).

2.) If *method = "spline"*

A spline regression is instead used without the transformation of the temperature data.

The variable for *test_set_steps* defines how many days are used for the test set. It should be consistent between all three model components. Because we set the test set steps for the long-term model to 2 years it should be set to 730  in the mid-term model (2 years × 365 days). 

```r
# Calculate the best mid-term seasonality prediction model
midterm_predictions = mid_term_lm(midterm_demand_and_weather_data$demand, test_set_steps=730, Tref = 18, method = "temperature transformation", data_directory = getwd(), verbose = TRUE)
```

![Mid_term_results](https://github.com/user-attachments/assets/9bed2a15-5e3b-40f8-a1a9-a20ee57b0d8e)


After identifying the best mid-term model, future seasonality predictions can be generated. While the future calendar and holiday covariates are deterministic, the future daily average temperatures are impossible to know with certainty. To estimate these, the library calculates the average of the past three observations for the same day. For example, the average temperature for July 22, 2025, would be estimated by averaging the temperatures observed on July 22 in 2022, 2023, and 2024. The variable *end_year* specifies the final year up to which future predictions will be made and should be consistent over all three model components (long-, medium-, and short-term).
The *mid_term_lm()* function will output a list with the data and the used models. You can simply put this list directly into the *mid_term_future()* function. If you only supply the dataframe, either a list with the used models has to be specified for the *model_list* variable (NULL by default) or the *data_directory* needs to contain the used models as .R file.

```r
# Generate future mid-term component forecasts
midterm_future_predictions = mid_term_future(midterm_predictions, end_year = 2028, data_directory = getwd(), verbose = TRUE)
```

![mid_term_results_future](https://github.com/user-attachments/assets/018d1623-7cfe-4579-8373-246fdc87b078)

<br>

### Calculate the best short-term seasonality models

The short-term  component $D_S(y,d,h)$ corresponds to the respective intra-day patterns. These are isolated by subtracting the yearly and daily averages $D_L(y)$, $D_m(y,d)$ from the hourly load data of year $y$, day $d$, and hour $h$ :

$$D_S(y,d,h) =D_s(h) - D_m(y,d)-D_L(y)$$

The short-term time series is modelled with multiple regression using only the type of hour and a holiday indicator as covariates. Information about public holidays is again retrieved from [https://date.nager.at](https://date.nager.at) via API.

```r
# Get all national holidays within the respective time period
shortterm_demand_data= add_holidays_short_term(decomposed_data$shortterm)
```

A separate short-term model is generated for each combination of day type and month. This results in a total of 84 models (12 months × 7 days). This approach has proven to yield better results compared to using a single model for the entire time series.
The variable for *test_set_steps* defines how many hours are used for the test set. It should be consistent between all three model components. Because we set the test set steps for the long-term model to 2 years it should be set to 17520 in the short-term model (2 years × 8760 hours). 

```r
# Calculate the best short-term seasonality models
shortterm_predictions <- short_term_lm(shortterm_demand_data, test_set_steps=17520, data_directory = getwd(), verbose = TRUE)
```

<br>

<p align="center">
  <img src="https://github.com/user-attachments/assets/b77ed5c0-0021-4189-ae07-c142a17221bd" alt="short_term_results_sample_weeks" width="45%" style="display: inline-block;" />
  <img src="https://github.com/user-attachments/assets/3bc98573-4c5b-4057-8b21-3db3dfe37c5f" alt="short_term_results" width="45%" style="display: inline-block;" />
</p>

<br>

Because all used covariates are deterministic the future short-term seasonality forecast can easily be generated. The *short_term_lm()* function will output a list with the data and the used model. You can simply put this list directly into the *short_term_future()* function. If you only supply the dataframe, either a list with the used models has to be specified for the *model_list* variable (NULL by default) or the *data_directory* needs to contain the used models as .R file.

```r
# Generate future short-term component forecasts
shortterm_future_predictions = short_term_future(shortterm_predictions, end_year = 2028, data_directory = getwd(), verbose = TRUE)
```

![short_term_results_future](https://github.com/user-attachments/assets/2d1bc6b9-70e3-47ea-9d68-9259cb02c849)

<br>

### Combine all models

After all three components have been modelled successfully, the predictions can be combined into the full demand series. The variables *longterm_predictions*, *midterm_predictions*, and *shortterm_predictions* correspond to the dataframes output by the *long_term_lm()*, *mid_term_lm()*, and *short_term_lm()* functions respectively. If you put in the complete output list of each function, the necessary dataframe will be extracted automatically. 
The *longterm_model_number* option specifies which of the three best long-term models will be used. The function also prints four statistical metrics: MAPE, RMSE, Accuracy, and the R²-value of both the training and the validation set.

```r
# Combine all model predictions and evaluate the accuracy
full_model_predictions <- combine_models(longterm_predictions$longterm_predictions, midterm_predictions$midterm_predictions, shortterm_predictions$shortterm_predictions, longterm_model_number =1, data_directory = getwd(), verbose = TRUE)

Output:
*** Final Model Metrics ***

    MAPE
Training Set: 0.0267 
Test Set:     0.0384 

    RSQUARE
Training Set: 0.9754 
Test Set:     0.9474 

    ACCURACY
Training Set: 97.33 %
Test Set:     96.16 %

    RMSE
Training Set: 1863.2 MW
Test Set:     2635.4 MW
```

Note that the algorithm reaches a MAPE below 4% over the validation set of 17520 hours (2 years) for French demand data.

<br>

<p align="center">
  <img src="https://github.com/user-attachments/assets/af339a58-beeb-4a84-ae0c-9169e7c2708b" alt="complete_model_sample_weeks" width="45%" style="display: inline-block;" />
  <img src="https://github.com/user-attachments/assets/9d99c117-9dc0-475d-a6b8-a8c027b8f758" alt="complete_model_results" width="45%" style="display: inline-block;" />
</p>

<br>

Similarly, full future predictions up to the specified end year can be generated.

```r
# Generate full future demand predictions in hourly resolution
full_model_future_predictions <- combine_models_future(longterm_future_predictions$longterm_future_predictions, midterm_future_predictions$midterm_future_predictions,
                                                       shortterm_future_predictions$shortterm_future_predictions, longterm_model_number =1, data_directory = getwd(), verbose = TRUE)
```

<br>

![complete_model_results2](https://github.com/user-attachments/assets/ca5c1ba8-15e3-4016-9c16-ee112f204128)

<br>


### All-in-One function

The *full_forecast* function automatically performs all the steps described above and generates a complete forecast for a specified country. 
The *future* option determines whether future forecasts should be made, and if so, up to which *end_year*.
NOTE: If you want to use this function instead of the individual forecasting functions, it is strongly recommended to set the data_directory to something other than a tempdir. Otherwise user prompts will be needed and the advantage of only specifying a country and a timeframe and getting a forecast without additional steps after 15-20 minutes won't be there.

```r
forecast_data <- full_forecast(start_year=2017, end_year_data=2021, country="France", test_set_steps=2,
   future="yes", end_year=2028)
```

