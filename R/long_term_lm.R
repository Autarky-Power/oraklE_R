#' Long-term forecast
#' 
#' This function predicts the long-term load data. For provided time series data and a set of macroeconomic variables. The load data is regressed on the macroeconomic indicators. For all possible model combinations, the best model is chosen based on AIC (?) and k-fold cross-validation. 
#'
#' @param longterm_all_data
#' @param training_set_ratio
#' @param testquant
#'
#' @return The forecast of the best model fit is stored and the results are displayed in a plot. 
#' @export
#' 
#' @seealso See also function \code{\link{mid_term_lm}} and \code{\link{short_term_lm}} for the other prediction models and \code{\link{get_macro_economic_data}} for the covariate download. 
#'
#' @examples
#' working_directory <- getwd()
#' setwd(tempdir())
#' #longterm_model_data_example <- long_term_lm(longterm_all_data_example)
#' setwd(working_directory)

long_term_lm<- function(longterm_all_data,training_set_ratio=0.1,testquant = 500){

  if(! "avg_hourly_demand" %in% colnames(longterm_all_data)){
    stop("No column named \"avg_hourly_demand\"")
  }
  if(FALSE %in%  lapply(longterm_all_data[,4:ncol(longterm_all_data)],is.numeric)){
    stop("Not all macroeconomic variables are numeric")
  }

  longterm_all_data <- longterm_all_data[mean(longterm_all_data$avg_hourly_demand)/longterm_all_data$avg_hourly_demand <150,]

  training_set=nrow(longterm_all_data)- round(nrow(longterm_all_data)*training_set_ratio)
  training_data=longterm_all_data[1:training_set,]
  test_data=longterm_all_data[(training_set+1):nrow(longterm_all_data),]
  variables <- colnames(longterm_all_data)[4:(ncol(longterm_all_data))]

  f <- as.formula(
    paste("avg_hourly_demand",
          paste(variables, collapse = " + "),
          sep = " ~ "))


  globalmodel <- lm(f , data=training_data, na.action = "na.fail")

  message("Getting all possible combinations, this might take a while.")

  suppressMessages(combinations <- MuMIn::dredge(globalmodel))

  for (i in 1:nrow(combinations)){
    if (all(is.na(combinations[i,2:(ncol(combinations)-5)]))){
      combinations<-combinations[-i,]
    }}
  row.names(combinations) <- NULL

  if (nrow(combinations) < testquant){
    testquant <- nrow(combinations)
  }

  rdm=421
  ctrl <- caret::trainControl(method = "repeatedcv", number = 5, repeats = 5)


  message(paste("Cross-validating the best",testquant,"models."))
  cross_val <- function(i){
    tryCatch({
      predictor_names=combinations[i,2:(ncol(combinations)-5)]
      model_variables=colnames(predictor_names)[complete.cases(t(predictor_names))]
      lm_formula <- as.formula(paste("avg_hourly_demand", paste(model_variables, collapse = " + "),
                                     sep = " ~ "))

      set.seed(rdm)
      Kfold <- caret::train(lm_formula, data = training_data,
                            trControl = ctrl, method = "lm")

    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    return(c(Kfold$results$RMSE,Kfold$results$Rsquared,Kfold$results$MAE,i))
  }
  # Parallel Processing
  no_cores <- parallel::detectCores(logical = TRUE)

  used_cores <- floor(no_cores*0.75)
  if (used_cores > 10) {used_cores=10}

  cl <- parallel::makeCluster(used_cores)
  doParallel::registerDoParallel(cl)

  parallel::clusterExport(cl,list("combinations","ctrl","training_data","rdm"),envir=environment())

  results_list <-  parallel::parLapply(cl,1:testquant,cross_val)

  doParallel::stopImplicitCluster()
  parallel::stopCluster(cl)


  results <-as.data.frame(do.call(rbind, results_list))
  colnames(results)= c("RMSE_k_fold","Rsquare_k_fold","MAE_k_fold","index")


  best_index_RMSE = results$index[results["RMSE_k_fold"] == min(results["RMSE_k_fold"],na.rm = T)]
  best_value_RMSE = results[best_index_RMSE, "RMSE_k_fold"]
  limit_RMSE = best_value_RMSE * 1.5


  best_index_MAE = results$index[results["MAE_k_fold"] == min(results["MAE_k_fold"],na.rm = T)]
  best_value_MAE = results[best_index_MAE, "MAE_k_fold"]
  limit_MAE = best_value_MAE * 1.5


  best_index_Rsquare = results$index[results["Rsquare_k_fold"] == max(results["Rsquare_k_fold"],na.rm = T)]

  best_value_Rsquare = results[best_index_Rsquare, "Rsquare_k_fold"]
  limit_Rsquare = best_value_Rsquare / 1.3


  mask_RMSE = results["RMSE_k_fold"] <= limit_RMSE
  mask_MAE = results["MAE_k_fold"] <= limit_MAE
  mask_Rsquare = results["Rsquare_k_fold"] >= limit_Rsquare

  candidates = results[mask_Rsquare & mask_RMSE & mask_MAE,]
  dist <- data.frame(matrix(nrow=nrow(candidates),ncol=3))
  colnames(dist) <- c("distance","model_no","max_single_distance")

  for (i in (1:nrow(candidates))){
    tryCatch({
      ind=candidates$index[i]
      x=combinations[ind,2:(ncol(combinations)-5)]
      variables=colnames(x)[complete.cases(t(x))]


      f <- as.formula(paste("avg_hourly_demand",paste(variables, collapse = " + "),
                            sep = " ~ "))

      model <- lm(f, data = training_data)

      LT <- predict(model,longterm_all_data)

      dist[i,1]<- sum(abs(test_data$avg_hourly_demand- LT[(training_set+1):nrow(longterm_all_data)]))
      dist[i,2]<- ind
      dist[i,3]<- max(abs(longterm_all_data$avg_hourly_demand- LT))
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }

  ordered_dist <- dist[order(dist$distance),]

  if (testquant>500){
    best_model_num <- ordered_dist$model_no[which.min(ordered_dist$max_single_distance[1:50])]
    best_model1 <- ordered_dist$model_no[ordered_dist$max_single_distance==sort(ordered_dist$max_single_distance[1:50], FALSE)[1]]
    best_model2 <- ordered_dist$model_no[ordered_dist$max_single_distance==sort(ordered_dist$max_single_distance[1:50], FALSE)[2]]
    best_model3 <- ordered_dist$model_no[ordered_dist$max_single_distance==sort(ordered_dist$max_single_distance[1:50], FALSE)[3]]
    best_models <-c(best_model1,best_model2,best_model3)
  }else{
    best_model1 <- ordered_dist$model_no[ordered_dist$max_single_distance==sort(ordered_dist$max_single_distance[1:5], FALSE)[1]]
    best_model2 <- ordered_dist$model_no[ordered_dist$max_single_distance==sort(ordered_dist$max_single_distance[1:5], FALSE)[2]]
    best_model3 <- ordered_dist$model_no[ordered_dist$max_single_distance==sort(ordered_dist$max_single_distance[1:5], FALSE)[3]]
    best_models <-c(best_model1,best_model2,best_model3)
  }

  longterm_all_data$longterm_model_predictions1 <- 0
  longterm_all_data$longterm_model_predictions2 <- 0
  longterm_all_data$longterm_model_predictions3 <- 0

  i = 1
  for (model in best_models){
    x=combinations[model,]
    x=x[,2:(ncol(combinations)-5)]
    variables=colnames(x)[complete.cases(t(x))]
    f <- as.formula(paste("avg_hourly_demand", paste(variables, collapse = " + "),
                          sep = " ~ "))
    print_vars <- paste(variables,collapse = ", ")
    print(paste("Best model",i,"depends on:",print_vars))


  best_lm_model<- lm(f,data=training_data)
  results<- predict(best_lm_model,longterm_all_data)
  longterm_all_data[,(ncol(longterm_all_data)-3+i)] <- results
  country<- unique(longterm_all_data$country)

  lt_plot <- ggplot(longterm_all_data)+geom_line(aes(year,avg_hourly_demand,color="actual"))+
    geom_line(aes(year,results,color="fitted"))+xlab("\nYear")+ylab("Avg Hourly Demand\n [MW]\n")+
    geom_vline(xintercept=longterm_all_data$year[training_set],linetype=2)+
    ggthemes::theme_foundation(base_size=14, base_family="sans")+
    xlab("\nYear")+ylab("Avg Hourly Demand\n [MW]\n")+
    ggtitle(paste("Long Term Model Results -",country),subtitle = paste("Model",i,"\n"))+
    theme(plot.title = element_text(face = "bold",
                                    size = rel(1.2), hjust = 0.5),
          plot.subtitle = element_text(size = rel(1), hjust = 0.5),
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
    theme(legend.title = element_blank())

  lt_plot2 <- ggplot(longterm_all_data)+geom_line(aes(year,avg_hourly_demand,color="actual"))+
    geom_line(aes(year,results,color="fitted"))+xlab("\nYear")+ylab("Avg Hourly Demand\n [MW]\n")+
    geom_vline(xintercept=longterm_all_data$year[training_set],linetype=2)+
    ggthemes::theme_foundation(base_size=14, base_family="sans")+
    xlab("\nYear")+ylab("Avg Hourly Demand\n [MW]\n")+
    ggtitle(paste("Long Term Model Results -",country),subtitle = paste("Model",i,"\n"))+
    theme(plot.title = element_text(face = "bold",
                                    size = rel(1.2), hjust = 0.5),
          plot.subtitle = element_text(size = rel(1), hjust = 0.5),
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
    theme(plot.title = element_text(size=26))

  print(lt_plot)


  if (! file.exists(country)){
    dir.create(country)}
  if (! file.exists(paste0("./",country,"/models"))){
    dir.create(paste0("./",country,"/models"))}
  if (! file.exists(paste0("./",country,"/data"))){
    dir.create(paste0("./",country,"/data"))}
  if (! file.exists(paste0("./",country,"/plots"))){
    dir.create(paste0("./",country,"/plots"))}
  if (! file.exists(paste0("./",country,"/models/longterm"))){
    dir.create(paste0("./",country,"/models/longterm"))}
  save(best_lm_model,file=paste0("./",country,"/models/longterm/best_lm_model",i,".Rdata"))

  ggsave(file=paste0("./",country,"/plots/Long_term_results",i,".png"), plot=lt_plot2, width=12, height=8)

  i=i+1
  }
  longterm_all_data$training_set_end = max(training_data$year,na.rm = T)
  write.csv(longterm_all_data,paste0("./",country,"/data/long_term_all_data.csv"),row.names = F)


  return(longterm_all_data)
}
