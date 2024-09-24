# oraklE_R

R package for Long-term Electricity Demand Forecasting

Installing the development version:

```library(devtools)```

```install_github("Autarky-Power/oraklE_R")```

Install requirements:
```
packages <- c("caret","countrycode","doParallel","dplyr","ggplot2","ggthemes","glmnet","httr",
             "jsonlite","lubridate","MLmetrics","MuMIn","parallel","patchwork","purrr","R.utils",
              "readxl", "xml2")
install.packages(setdiff(packages, rownames(installed.packages())))
```


[![Github All Releases](https://img.shields.io/github/downloads/Autarky-Power/orakle/total.svg)]()

## Flowchart for package use

The functions included in the package can be used separately or combined in the function full_forecast()

<img src="https://github.com/Autarky-Power/oraklE_R/assets/45041403/c166e930-876a-4e90-873a-3f4bcda249a7)https://github.com/Autarky-Power/oraklE_R/assets/45041403/c166e930-876a-4e90-873a-3f4bcda249a7" width="600">


## Installation

```r
install.packages("svars")
```

Alternatively, install the development version


```r
install.packages("devtools")
devtools::install_github("alexanderlange53/svars")
```


```r
library("svars")
```

## Usage

To get started, use the example data set which is included in the package. The data set consists of three U.S. macroeconomic time series, i.e. output gap (x), inflation (pi) and interest rates (r). More details on the data set are provided in the description file `?USA`.

```r
library("ggplot2")
library("ggfortify")
autoplot(USA, facet = TRUE) + theme_bw()
```

![](man/figures/data_viz.png)

First, the reduced form VAR needs to be estimated, for instance using the vars package, and the user needs to store the resulting object. Subsequently, the user chooses a method from the svars package to determine the structural matrix. The choice of the method usually depends on the data structure, for more details see the help file `help(svars)`. For illustration, we use the identification by means of non-Gaussian maximum likelihood. 

```r
reduced.form <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
structural.form <- id.ngml(reduced.form)
summary(structural.form)
