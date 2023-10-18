#' Retrieve a list of macroeconomic data from WDI
#' 
#' This function downloads a set of ten macroeconomic variables via API from the World Development Indicators (WDI). The variables are suspected to have a predictive capacity for the load data.   
#'
#' @param longterm Data frame containing information on country (longterm$country) and years (e.g., longterm$year).
#'
#' @return Data frame with the original time series and 10 macroeconomic indicators.
#' @export
#' 
#' @seealso See also function \code{\link{long_term_lm}} for the selection of covariates. 
#'
#' @examples
#' longterm_all_data_example <- get_macro_economic_data(longterm_example)
#' longterm_all_data_example
get_macro_economic_data <- function(longterm){

  country=unique(longterm$country)
  start_year= min(longterm$year)
  end_year= max(longterm$year)

  res_pop = httr::GET(paste0("http://api.worldbank.org/v2/country/",country,"/indicator/SP.POP.TOTL?date=",start_year,":",end_year,"&format=json"))
  data_pop=jsonlite::fromJSON(rawToChar(res_pop$content))
  df_pop<- as.data.frame(data_pop[2])
  df_pop<- df_pop[order(df_pop$date),]
  longterm$population<-df_pop$value

  res_gdp= httr::GET(paste0("http://api.worldbank.org/v2/country/",country,"/indicator/NY.GDP.MKTP.KD?date=",start_year,":",end_year,"&format=json"))

  data_gdp=jsonlite::fromJSON(rawToChar(res_gdp$content))
  df_gdp<- as.data.frame(data_gdp[2])
  df_gdp<- df_gdp[order(df_gdp$date),]
  longterm$GDP<- df_gdp$value

  res_ind= httr::GET(paste0("http://api.worldbank.org/v2/country/",country,"/indicator/NV.IND.TOTL.ZS?date=",start_year,":",end_year,"&format=json"))

  data_ind=jsonlite::fromJSON(rawToChar(res_ind$content))
  df_ind<- as.data.frame(data_ind[2])
  df_ind<- df_ind[order(df_ind$date),]
  longterm$industrial_value_added<- longterm$GDP*df_ind$value/100


  res_man= httr::GET(paste0("http://api.worldbank.org/v2/country/",country,"/indicator/NV.IND.MANF.ZS?date=",start_year,":",end_year,"&format=json"))

  data_man=jsonlite::fromJSON(rawToChar(res_man$content))
  df_man<- as.data.frame(data_man[2])
  df_man<- df_man[order(df_man$date),]
  longterm$manufacturing_value_added<- longterm$GDP*df_man$value/100


  res_gro= httr::GET(paste0("http://api.worldbank.org/v2/country/",country,"/indicator/NY.GDP.MKTP.KD.ZG?date=",start_year,":",end_year,"&format=json"))

  data_gro=jsonlite::fromJSON(rawToChar(res_gro$content))
  df_gro<- as.data.frame(data_gro[2])
  df_gro<- df_gro[order(df_gro$date),]
  longterm$GDP_growth<- df_gro$value



  res_gdp_defl= httr::GET(paste0("http://api.worldbank.org/v2/country/",country,"/indicator/NY.GDP.DEFL.KD.ZG?date=",start_year,":",end_year,"&format=json"))

  data_gdp_defl=jsonlite::fromJSON(rawToChar(res_gdp_defl$content))
  df_gdp_defl<- as.data.frame(data_gdp_defl[2])
  df_gdp_defl<- df_gdp_defl[order(df_gdp_defl$date),]
  longterm$GDP_deflator<- df_gdp_defl$value


  res_serv= httr::GET(paste0("http://api.worldbank.org/v2/country/",country,"/indicator/NV.SRV.TOTL.ZS?date=",start_year,":",end_year,"&format=json"))

  data_serv=jsonlite::fromJSON(rawToChar(res_serv$content))
  df_serv<- as.data.frame(data_serv[2])
  df_serv<- df_serv[order(df_serv$date),]

  longterm$service_value_added<- longterm$GDP*   df_serv$value/100


  res_gni= httr::GET(paste0("http://api.worldbank.org/v2/country/",country,"/indicator/NY.GNP.MKTP.KD?date=",start_year,":",end_year,"&format=json"))

  data_gni=jsonlite::fromJSON(rawToChar(res_gni$content))
  df_gni<- as.data.frame(data_gni[2])
  df_gni<- df_gni[order(df_gni$date),]

  longterm$GNI <- df_gni$value


  res_hou= httr::GET(paste0("http://api.worldbank.org/v2/country/",country,"/indicator/NE.CON.PRVT.ZS?date=",start_year,":",end_year,"&format=json"))

  data_hou=jsonlite::fromJSON(rawToChar(res_hou$content))
  df_hou<- as.data.frame(data_hou[2])
  df_hou<- df_hou[order(df_gro$date),]

  longterm$household_consumption_expenditure <- longterm$GDP*df_hou$value/100


  res_rural= httr::GET(paste0("http://api.worldbank.org/v2/country/",country,"/indicator/SP.RUR.TOTL?date=",start_year,":",end_year,"&format=json"))

  data_rural=jsonlite::fromJSON(rawToChar(res_rural$content))
  df_rural<- as.data.frame(data_rural[2])
  df_rural<- df_rural[order(df_rural$date),]

  longterm$rural_population <- df_rural$value

  return(longterm)
}
