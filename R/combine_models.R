#' Combine forecast models
#'
#' This function combines the three separate forecasts for the low, mid and high frequency model. The three separate forecasts need to be run first.
#'
#' @param longterm_all_data_predicted Dataframe. The object resulting from function \code{\link{long_term_lm}}.
#' @param midterm_all_data_predicted Dataframe. The object resulting from function \code{\link{mid_term_lm}}.
#' @param short_term_data_predicted Dataframe. The object resulting from function \code{\link{short_term_lm}}.
#' @param longterm_model_number Integer. Specifies which of the 3 best long-term models should be used.
#' @return The combined model results.
#' @export
#'
#' @examples
#' \dontrun{
#' combined_model_results <- long_term_lm(longterm_all_data_example,
#' test_set_steps=2,testquant = 500)
#' }
#'

combine_models <- function(longterm_all_data_predicted,midterm_all_data_predicted,short_term_data_predicted, longterm_model_number=1){

  combined_model_results <- short_term_data_predicted[,1:8]
  country = unique(longterm_all_data_predicted$country)
  combined_model_results$long_term_model <- 0

  for (year in unique(combined_model_results$year)){
  if (longterm_model_number==1){
    combined_model_results$long_term_model[combined_model_results$year==year]  <-
      longterm_all_data_predicted$longterm_model_predictions1[longterm_all_data_predicted$year==year]
  } else if (longterm_model_number==2){

    combined_model_results$long_term_model[combined_model_results$year==year]  <-
      longterm_all_data_predicted$longterm_model_predictions2[longterm_all_data_predicted$year==year]
  }else{
    combined_model_results$long_term_model[combined_model_results$year==year]  <-
      longterm_all_data_predicted$longterm_model_predictions3[longterm_all_data_predicted$year==year]
      }
  }
  combined_model_results$mid_term_model <- 0


  for (i in 1:nrow(midterm_all_data_predicted)){
    combined_model_results$mid_term_model[((i-1)*24+1):(i*24)] <-
      midterm_all_data_predicted$midterm_model_fit[i]
  }

  combined_model_results$short_term_model <- short_term_data_predicted$short_term_lm_model_predictions

  combined_model_results$complete_model <- combined_model_results$long_term_model+
    combined_model_results$mid_term_model + combined_model_results$short_term_model


  test_set_steps <- unique(longterm_all_data_predicted$test_set_steps)
  year_training_set=nrow(longterm_all_data_predicted)- test_set_steps
  end_of_training_set=max(which(combined_model_results$year== longterm_all_data_predicted$year[year_training_set]))



  training_mape=MLmetrics::MAPE(combined_model_results$complete_model[1:end_of_training_set],combined_model_results$hourly_demand[1:end_of_training_set])
  test_mape=MLmetrics::MAPE(combined_model_results$complete_model[(end_of_training_set+1):nrow(combined_model_results)],combined_model_results$hourly_demand[(end_of_training_set+1):nrow(combined_model_results)])

  RSQUARE_training = stats::cor(combined_model_results$hourly_demand[1:end_of_training_set],combined_model_results$complete_model[1:end_of_training_set])^2
  RSQUARE_test = stats::cor(combined_model_results$hourly_demand[(end_of_training_set+1):nrow(combined_model_results)],combined_model_results$complete_model[(end_of_training_set+1):nrow(combined_model_results)])^2

  training_rmse=MLmetrics::RMSE(combined_model_results$complete_model[1:end_of_training_set],combined_model_results$hourly_demand[1:end_of_training_set])
  test_rmse=MLmetrics::RMSE(combined_model_results$complete_model[(end_of_training_set+1):nrow(combined_model_results)],combined_model_results$hourly_demand[(end_of_training_set+1):nrow(combined_model_results)])

  cat("\n*** Final Model Metrics ***\n
    MAPE\nTraining Set:",round(training_mape,4),"\nTest Set:    ",round(test_mape,4),"\n
    RSQUARE\nTraining Set:",round(RSQUARE_training,4),"\nTest Set:    ",round(RSQUARE_test,4),"\n
    ACCURACY\nTraining Set:",round((1-training_mape)*100,2),"%\nTest Set:    ",round((1-test_mape)*100,2),"%\n
    RMSE\nTraining Set:",round(training_rmse,1),"MW\nTest Set:    ",round(test_rmse,1),"MW\n\n")

  results <- as.data.frame(matrix(nrow=2,ncol = 4))
  colnames(results)<- c("MAPE","RSQUARE","ACCURACY","RMSE")
  rownames(results)<- c("training set","test set")
  results[1,] <- c(round(training_mape,4),round(RSQUARE_training,4),round((1-training_mape)*100,2),round(training_rmse,1))
  results[2,] <- c(round(test_mape,4),round(RSQUARE_test,4),round((1-test_mape)*100,2),round(test_rmse,1))


  full_plot <- ggplot(combined_model_results)+geom_line(aes(date,combined_model_results$hourly_demand,color="actual"))+
    geom_line(aes(date,combined_model_results$complete_model,color="fitted"))+xlab("\nYear")+ylab("Hourly Demand\n [MW]\n")+
    geom_vline(xintercept=combined_model_results$date[end_of_training_set],linetype=2)+
    ggthemes::theme_foundation(base_size=14, base_family="sans")+
    xlab("\nHour")+ylab("Hourly Demand\n [MW]\n")+
    ggtitle(paste("Complete Model Results -",country,"\n"))+
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
    theme(legend.title = element_blank())



  full_plot2 <- ggplot(combined_model_results)+geom_line(aes(date,combined_model_results$hourly_demand,color="actual"))+
    geom_line(aes(date,combined_model_results$complete_model,color="fitted"))+xlab("\nYear")+ylab("Hourly Demand\n [MW]\n")+
    geom_vline(xintercept=combined_model_results$date[end_of_training_set],linetype=2)+
    ggthemes::theme_foundation(base_size=14, base_family="sans")+
    xlab("\nHour")+ylab("Hourly Demand\n [MW]\n")+
    ggtitle(paste("Complete Model Results -",country,"\n"))+
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
    theme(plot.title = element_text(size=26))


  if (! file.exists(country)){
    dir.create(country)}
  if (! file.exists(paste0("./",country,"/data"))){
    dir.create(paste0("./",country,"/data"))}
  utils::write.csv(results,paste0("./",country,"/data/final_model_metrics.csv"))

  if (! file.exists(paste0("./",country,"/plots"))){
    dir.create(paste0("./",country,"/plots"))}
suppressWarnings(
  ggsave(filename=paste0("./",country,"/plots/complete_model_results.png"), plot=full_plot2, width=12, height=8)
)
  ###
  sample_week_index <-nrow(combined_model_results)-(nrow(combined_model_results)-end_of_training_set)*0.45

  week_start <- which(combined_model_results$wday[sample_week_index:(sample_week_index+200)]==
                        "Mon")[1]+sample_week_index
  sample_year <- combined_model_results$year[week_start]

  full_plot_sample_week <- ggplot(combined_model_results[week_start:(week_start+335),])+geom_line(aes(date,combined_model_results$hourly_demand[week_start:(week_start+335)],color="actual"))+
    geom_line(aes(date,combined_model_results$complete_model[week_start:(week_start+335)],color="fitted"))+
    ggthemes::theme_foundation(base_size=14, base_family="sans")+
    xlab("\nHour")+ylab("[MW]\n")+
    ggtitle(paste("Complete Model Results -",country),subtitle = paste("2 sample weeks in",sample_year,"\n") )+
    theme(plot.title = element_text(face = "bold",
                                    size = rel(1.2), hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
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




  full_plot_sample_week2 <- ggplot(combined_model_results[week_start:(week_start+335),])+geom_line(aes(date,combined_model_results$hourly_demand[week_start:(week_start+335)],color="actual"))+
    geom_line(aes(date,combined_model_results$complete_model[week_start:(week_start+335)],color="fitted"))+
    ggthemes::theme_foundation(base_size=14, base_family="sans")+
    xlab("\nHour")+ylab("[MW]\n")+
    ggtitle(paste("Complete Model Results -",country),subtitle = paste("2 sample weeks in",sample_year,"\n") )+
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
    theme(plot.subtitle = element_text(size=20,hjust = 0.5))
suppressWarnings(
  ggsave(filename=paste0("./",country,"/plots/complete_model_sample_weeks.png"), plot=full_plot_sample_week2, width=12, height=8)
)
suppressWarnings(
  stacked_plots <- patchwork::wrap_plots(full_plot, full_plot_sample_week, ncol = 1)
)
  suppressWarnings(
    print(stacked_plots)
  )
  suppressWarnings(
  print(full_plot_sample_week)
  )
  suppressWarnings(
  print(full_plot)
)
  return(combined_model_results)
}
