#' Mid-term forecast
#'
#' The mid-term load series is forecasted based on the provided load time series and weather data. The prediction is either based on the (lagged) temperature data in combination with transformed variables for heating and cooling days or on a spline regression applied on the temperature data to account for non-linear effects.
#'
#' @param midterm_all_data Dataframe. Containing the mid-term load data, the holidays and weather data obtained from \code{\link{get_weather_data}}.
#' @param test_set_steps Integer. Number of time periods in the test set.
#' @param Tref Numeric. Reference temperature as basis for the calculation of cooling and heating days.
#' @param method String. Indicates which model selection process is used. If method="temperature transformation", the temperature values are transformed to heating and cooling
#' degree days to capture the non-linear relationship of temperature and electricity demand. If the method is set to "spline" a spline regression is instead used without
#' the transformation of the temperature data.
#' @return The forecast of the best model fit is stored and the results are displayed in a plot.
#' @export
#'
#' @examples
#' \dontrun{
#' working_directory <- getwd()
#' setwd(tempdir())
#' midterm_model_data_example <- mid_term_lm(midterm_all_data_example$midterm)
#' setwd(working_directory)
#' }
mid_term_lm <- function(midterm_all_data,Tref=18, test_set_steps=730, method="temperature transformation"){
  month_list=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Nov","Dec")

  for (i in 1:length(month_list)){
    midterm_all_data[month_list[i]]=0
    midterm_all_data[midterm_all_data$month==i,month_list[i]]<- 1
  }

  midterm_all_data$wday<- lubridate::wday(midterm_all_data$date,label = T)
  weekday_list=as.character(unique(midterm_all_data$wday))

  for (i in 1:length(weekday_list)){
    midterm_all_data[weekday_list[i]]=0
    midterm_all_data[midterm_all_data$wday==weekday_list[i],weekday_list[i]]<- 1
  }

  if(method=="temperature transformation"){

  midterm_all_data$HD <-0
  midterm_all_data$CD <-0

  for (i in 1:nrow(midterm_all_data)){
    if (midterm_all_data$weighted_temperature[i] < Tref){
      midterm_all_data$HD[i]<- Tref-midterm_all_data$weighted_temperature[i]
    }else{  midterm_all_data$CD[i]<- midterm_all_data$weighted_temperature[i] -Tref
    }
  }

  midterm_all_data$HD2 <- midterm_all_data$HD^2
  midterm_all_data$HD3 <- midterm_all_data$HD^3
  midterm_all_data$CD2 <- midterm_all_data$CD^2
  midterm_all_data$CD3 <- midterm_all_data$CD^3
  midterm_all_data$weighted_temperature2 <- midterm_all_data$weighted_temperature^2
  midterm_all_data$weighted_temperature3 <- midterm_all_data$weighted_temperature^3

  midterm_all_data$HDlag1 <- dplyr::lag(midterm_all_data$HD, n = 1)
  midterm_all_data$HDlag1[1]<- midterm_all_data$HD[1]
  midterm_all_data$HDlag2 <- dplyr::lag(midterm_all_data$HD, n = 2)
  midterm_all_data$HDlag2[1:2]<- midterm_all_data$HDlag1[1:2]

  midterm_all_data$CDlag1 <- dplyr::lag(midterm_all_data$CD, n = 1)
  midterm_all_data$CDlag1[1]<- midterm_all_data$CD[1]
  midterm_all_data$CDlag2 <- dplyr::lag(midterm_all_data$CD, n = 2)
  midterm_all_data$CDlag2[1:2]<- midterm_all_data$CDlag1[1:2]

  midterm_all_data$weighted_temperaturelag1 <- dplyr::lag(midterm_all_data$weighted_temperature, n = 1)
  midterm_all_data$weighted_temperaturelag1[1]<- midterm_all_data$weighted_temperature[1]
  midterm_all_data$weighted_temperaturelag2 <- dplyr::lag(midterm_all_data$weighted_temperature, n = 2)
  midterm_all_data$weighted_temperaturelag2[1:2]<- midterm_all_data$weighted_temperaturelag1[1:2]

  midterm_all_data$end_of_year <- 0
  midterm_all_data$end_of_year[midterm_all_data$month==12 & midterm_all_data$day>22] <-1

  training_set=nrow(midterm_all_data)- test_set_steps
  training_data=midterm_all_data[1:training_set,]
  test_data=midterm_all_data[(training_set+1):nrow(midterm_all_data),]


  variables <- colnames(midterm_all_data)[c(9:(ncol(midterm_all_data)))]


  f <- stats:: as.formula(  paste("seasonal_avg_hourly_demand",paste(variables, collapse = " + "),
                          sep = " ~ "))

  globalmodel <-stats:: lm(f , data=training_data, na.action = "na.omit")


  y <- training_data$seasonal_avg_hourly_demand
  y_all <- midterm_all_data$seasonal_avg_hourly_demand
  y_test <- test_data$seasonal_avg_hourly_demand
  x <- data.matrix(training_data[, 9:ncol(midterm_all_data)])
  x_all <- data.matrix(midterm_all_data[, 9:ncol(midterm_all_data)])
  x_test<- data.matrix(test_data[, 9:ncol(training_data)])

  cv_model <- glmnet::cv.glmnet(x, y, alpha = 1)

  best_lambda <- cv_model$lambda.min
  best_model <- glmnet::glmnet(x, y, alpha = 1, lambda = best_lambda)



  testlasso<-stats::predict(best_model, s = best_lambda, newx = x_test)
  suppressWarnings(
    testlm<-stats::predict(globalmodel, newdata=test_data)
  )

  country = unique(midterm_all_data$country)
  if (! file.exists(country)){
    dir.create(country)}
  if (! file.exists(paste0("./",country,"/models"))){
    dir.create(paste0("./",country,"/models"))}
  if (! file.exists(paste0("./",country,"/data"))){
    dir.create(paste0("./",country,"/data"))}
  if (! file.exists(paste0("./",country,"/plots"))){
    dir.create(paste0("./",country,"/plots"))}
  if (! file.exists(paste0("./",country,"/models/midterm"))){
    dir.create(paste0("./",country,"/models/midterm"))}
  suppressWarnings(
    if(MLmetrics::RMSE(testlasso,y_test) < MLmetrics::RMSE(testlm,y_test)){
      midterm_all_data$midterm_model_fit <- stats:: predict(best_model, s = best_lambda, newx = x_all)
      save(best_model,file=paste0("./",country,"/models/midterm/best_model.Rdata"))
    }else{
      midterm_all_data$midterm_model_fit <- stats:: predict(globalmodel, newdata = midterm_all_data)
      save(globalmodel,file=paste0("./",country,"/models/midterm/best_model.Rdata"))
    })
  midterm_all_data$test_set_steps <- test_set_steps
  years <- unique(midterm_all_data$year)
  index <- 1:length(years)
  for (i in 1:length(years)){
    index[i] <- min(as.numeric(rownames(midterm_all_data[midterm_all_data$year==years[i],])))
  }

  lowest_real_values <- min(midterm_all_data$seasonal_avg_hourly_demand)
  midterm_all_data$midterm_model_fit[midterm_all_data$midterm_model_fit<lowest_real_values] <- lowest_real_values


  }else if(method=="spline"){


    midterm_all_data$weighted_temperaturelag1 <- dplyr::lag(midterm_all_data$weighted_temperature, n = 1)
    midterm_all_data$weighted_temperaturelag1[1]<- midterm_all_data$weighted_temperature[1]
    midterm_all_data$weighted_temperaturelag2 <- dplyr::lag(midterm_all_data$weighted_temperature, n = 2)
    midterm_all_data$weighted_temperaturelag2[1:2]<- midterm_all_data$weighted_temperaturelag1[1:2]


    midterm_all_data$end_of_year <- 0
    midterm_all_data$end_of_year[midterm_all_data$month==12 & midterm_all_data$day>22] <-1

    training_set=nrow(midterm_all_data)- test_set_steps
    training_data=midterm_all_data[1:training_set,]
    test_data=midterm_all_data[(training_set+1):nrow(midterm_all_data),]

    variables <- colnames(midterm_all_data)[c(9:(ncol(midterm_all_data)))]

    spline_vars <- c( "weighted_temperature", "weighted_temperaturelag1", "weighted_temperaturelag2")

    formula_str <- paste("seasonal_avg_hourly_demand ~",
                         paste(lapply(variables, function(v) {
                           if (v %in% spline_vars) {
                             return(paste("s(", v, ", bs='cs')", sep = ""))
                           } else {
                             return(v)
                           }
                         }), collapse = " + "))


    f <- stats:: as.formula(formula_str)


    globalmodel <- mgcv::gam(f , data=training_data, method="REML")


    y <- training_data$seasonal_avg_hourly_demand
    y_all <- midterm_all_data$seasonal_avg_hourly_demand
    y_test <- test_data$seasonal_avg_hourly_demand
    x <- data.matrix(training_data[, 9:ncol(midterm_all_data)])
    x_all <- data.matrix(midterm_all_data[, 9:ncol(midterm_all_data)])
    x_test<- data.matrix(test_data[, 9:ncol(training_data)])

    cv_model <- glmnet::cv.glmnet(x, y, alpha = 1)

    best_lambda <- cv_model$lambda.min
    best_model <- glmnet::glmnet(x, y, alpha = 1, lambda = best_lambda)



    testlasso<-stats::predict(best_model, s = best_lambda, newx = x_test)
    suppressWarnings(
      testlm<-stats::predict(globalmodel, newdata=test_data)
    )
    country = unique(midterm_all_data$country)
    if (! file.exists(country)){
      dir.create(country)}
    if (! file.exists(paste0("./",country,"/models"))){
      dir.create(paste0("./",country,"/models"))}
    if (! file.exists(paste0("./",country,"/data"))){
      dir.create(paste0("./",country,"/data"))}
    if (! file.exists(paste0("./",country,"/plots"))){
      dir.create(paste0("./",country,"/plots"))}
    if (! file.exists(paste0("./",country,"/models/midterm"))){
      dir.create(paste0("./",country,"/models/midterm"))}
    suppressWarnings(
      if(MLmetrics::RMSE(testlasso,y_test) < MLmetrics::RMSE(testlm,y_test)){
        midterm_all_data$midterm_model_fit <- stats::predict(best_model, s = best_lambda, newx = x_all)
        save(best_model,file=paste0("./",country,"/models/midterm/best_model.Rdata"))
      }else{
        midterm_all_data$midterm_model_fit <- stats::predict(globalmodel, newdata = midterm_all_data)
        save(globalmodel,file=paste0("./",country,"/models/midterm/best_model.Rdata"))
      })
    midterm_all_data$test_set_steps <- test_set_steps
    years <- unique(midterm_all_data$year)
    index <- 1:length(years)
    for (i in 1:length(years)){
      index[i] <- min(as.numeric(rownames(midterm_all_data[midterm_all_data$year==years[i],])))
    }

    lowest_real_values <- min(midterm_all_data$seasonal_avg_hourly_demand)
    midterm_all_data$midterm_model_fit[midterm_all_data$midterm_model_fit<lowest_real_values] <- lowest_real_values

  }


  mt_plot <- ggplot(midterm_all_data)+geom_line(aes(1:nrow(midterm_all_data),midterm_all_data$seasonal_avg_hourly_demand,color="actual"))+
    geom_line(aes(1:nrow(midterm_all_data),midterm_all_data$midterm_model_fit,color="fitted"))+
    geom_vline(xintercept=training_set,linetype=2)+
    ggthemes::theme_foundation(base_size=14, base_family="sans")+
    xlab("\nDay")+ylab("Change in avg. Hourly Demand\n p. Day [MW]\n")+
    ggtitle(paste("Mid Term Model Results -",country,"\n"))+
    theme(plot.title = element_text(face = "bold",
                                    size = rel(1.2), hjust = 0.5),
          text = element_text(),
          panel.background = element_rect(colour = NA),
          plot.background = element_rect(colour = NA),
          panel.border = element_rect(colour = NA),
          axis.title = element_text(face = "bold",size = rel(1)),
          axis.title.y = element_text(angle=90,vjust =2),
          axis.title.x = element_text(vjust = -0.2),
          axis.text = element_text(),
          axis.line.x = element_line(colour="black"),
          axis.line.y = element_line(colour="black"),
          axis.ticks = element_line(),
          panel.grid.major = element_line(colour="#f0f0f0"),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = NA),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.size= unit(0.2, "cm"),
          plot.margin=unit(c(10,5,5,5),"mm"),
          strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
          strip.text = element_text(face="bold"))+
    theme(legend.title = element_blank())+
    scale_x_continuous(breaks = index,labels = years)+guides(color = guide_legend(override.aes = list(linewidth = 2)))


  mt_plot2 <- ggplot(midterm_all_data)+geom_line(aes(1:nrow(midterm_all_data),midterm_all_data$seasonal_avg_hourly_demand,color="actual"))+
    geom_line(aes(1:nrow(midterm_all_data),midterm_all_data$midterm_model_fit,color="fitted"))+
    geom_vline(xintercept=training_set,linetype=2)+
    ggthemes::theme_foundation(base_size=14, base_family="sans")+
    xlab("\nDay")+ylab("Change in avg. Hourly Demand\n p. Day [MW]\n")+
    ggtitle(paste("Mid Term Model Results -",country,"\n"))+
    theme(plot.title = element_text(face = "bold",
                                    size = rel(1.2), hjust = 0.5),
          text = element_text(),
          panel.background = element_rect(colour = NA),
          plot.background = element_rect(colour = NA),
          panel.border = element_rect(colour = NA),
          axis.title = element_text(face = "bold",size = rel(1)),
          axis.title.y = element_text(angle=90,vjust =2),
          axis.title.x = element_text(vjust = -0.2),
          axis.text = element_text(),
          axis.line.x = element_line(colour="black"),
          axis.line.y = element_line(colour="black"),
          axis.ticks = element_line(),
          panel.grid.major = element_line(colour="#f0f0f0"),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = NA),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.size= unit(0.2, "cm"),
          plot.margin=unit(c(10,5,5,5),"mm"),
          strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
          strip.text = element_text(face="bold"))+
    theme(legend.title = element_blank())+
    theme(axis.title=element_text(size=23))+
    theme(legend.text=element_text(size=23))+
    theme(axis.text=element_text(size=20))+
    theme(plot.title = element_text(size=26))+
    scale_x_continuous(breaks = index,labels = years)+guides(color = guide_legend(override.aes = list(linewidth = 2)))



  ggsave(filename=paste0("./",country,"/plots/Mid_term_results.png"), plot=mt_plot2, width=12, height=8)
suppressWarnings(
  print(mt_plot)
)

  return(midterm_all_data)
}

