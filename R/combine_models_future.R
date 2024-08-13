#' Combine forecast models
#'
#' This function combines the three separate forecasts for the low, mid and high frequency model. The three separate forecasts need to be run first.
#'
#' @param longterm_future_predictions Dataframe. The object resulting from function \code{\link{long_term_lm}}.
#' @param midterm_future_predictions Dataframe. The object resulting from function \code{\link{mid_term_lm}}.
#' @param short_term_future_predictions Dataframe. The object resulting from function \code{\link{short_term_lm}}.
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

combine_models_future <- function(longterm_future_predictions, midterm_future_predictions
                                  ,short_term_future_predictions, longterm_model_number=1){

  combined_model_results <- short_term_future_predictions[,1:8]
  country = unique(longterm_future_predictions$country)
  combined_model_results$long_term_model <- 0

  for (year in unique(combined_model_results$year)){
    if (longterm_model_number==1){
      combined_model_results$long_term_model[combined_model_results$year==year]  <-
        longterm_future_predictions$longterm_model_predictions1[longterm_future_predictions$year==year]
    } else if (longterm_model_number==2){

      combined_model_results$long_term_model[combined_model_results$year==year]  <-
        longterm_future_predictions$longterm_model_predictions2[longterm_future_predictions$year==year]
    }else{
      combined_model_results$long_term_model[combined_model_results$year==year]  <-
        longterm_future_predictions$longterm_model_predictions3[longterm_future_predictions$year==year]
    }
  }
  combined_model_results$mid_term_model <- 0


  for (i in 1:nrow(midterm_future_predictions)){
    combined_model_results$mid_term_model[((i-1)*24+1):(i*24)] <-
      midterm_future_predictions$midterm_model_fit[i]
  }

  combined_model_results$short_term_model <- short_term_future_predictions$short_term_lm_model_predictions

  combined_model_results$complete_model <- combined_model_results$long_term_model+
    combined_model_results$mid_term_model + combined_model_results$short_term_model


  test_set_steps <- unique(longterm_future_predictions$test_set_steps)
  unknown_set_start <- min(as.numeric(rownames(longterm_future_predictions[is.na(longterm_future_predictions$avg_hourly_demand), ])))
  year_training_set=unknown_set_start-1 - test_set_steps
  end_of_training_set=max(which(combined_model_results$year== longterm_future_predictions$year[year_training_set]))
  year_test_set=unknown_set_start-1
  end_of_test_set=max(which(combined_model_results$year== longterm_future_predictions$year[year_test_set]))

  years <- unique(combined_model_results$year)
  index <- 1:length(years)
  for (i in 1:length(years)){
    index[i] <- min(as.numeric(rownames(combined_model_results[combined_model_results$year==years[i],])))
  }
  max_value <- max(c(max(combined_model_results$complete_model),max(combined_model_results$hourly_demand,na.rm = T)))

  full_plot <- ggplot(combined_model_results)+geom_line(aes(1:nrow(combined_model_results),combined_model_results$hourly_demand,color="actual"))+
    geom_line(aes(1:nrow(combined_model_results),combined_model_results$complete_model,color="fitted"))+xlab("\nYear")+ylab("Hourly Demand\n [MW]\n")+
    geom_vline(xintercept=end_of_training_set,linetype=2)+
    geom_vline(xintercept=end_of_test_set,linetype=3)+
    ggthemes::theme_foundation(base_size=14, base_family="sans")+
    xlab("\nHour")+ylab("Hourly Demand\n [MW]\n")+
    scale_y_continuous(labels = scales::label_number(scalar = 1))+
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
    scale_x_continuous(breaks = index,labels = years)+
    annotate("text", x = (end_of_training_set/2), y =   (max_value+max_value*0.05), label = "Training", size = 4, hjust = 0.5, vjust = 0)+
    annotate("text", x = (end_of_training_set+(end_of_test_set-end_of_training_set)/2), y =    (max_value+max_value*0.05), label = "Test", size = 4, hjust = 0.5, vjust = 0)+
    annotate("text", x = (end_of_test_set+(nrow(combined_model_results)-end_of_test_set)/2), y =      (max_value+max_value*0.05), label = "Unknown", size = 4, hjust = 0.5, vjust = 0)



  full_plot2 <- ggplot(combined_model_results)+geom_line(aes(1:nrow(combined_model_results),combined_model_results$hourly_demand,color="actual"))+
    geom_line(aes(1:nrow(combined_model_results),combined_model_results$complete_model,color="fitted"))+xlab("\nYear")+ylab("Hourly Demand\n [MW]\n")+
    geom_vline(xintercept=end_of_training_set,linetype=2)+
    geom_vline(xintercept=end_of_test_set,linetype=3)+
    ggthemes::theme_foundation(base_size=14, base_family="sans")+
    xlab("\nHour")+ylab("Hourly Demand\n [MW]\n")+
    scale_y_continuous(labels = scales::label_number(scalar = 1))+
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
    theme(plot.title = element_text(size=26))+
    scale_x_continuous(breaks = index,labels = years)+
    annotate("text", x = (end_of_training_set/2), y =   (max_value+max_value*0.05), label = "Training", size = 4, hjust = 0.5, vjust = 0)+
    annotate("text", x = (end_of_training_set+(end_of_test_set-end_of_training_set)/2), y =    (max_value+max_value*0.05), label = "Test", size = 4, hjust = 0.5, vjust = 0)+
    annotate("text", x = (end_of_test_set+(nrow(combined_model_results)-end_of_test_set)/2), y =      (max_value+max_value*0.05), label = "Unknown", size = 4, hjust = 0.5, vjust = 0)


  if (! file.exists(country)){
    dir.create(country)}
  if (! file.exists(paste0("./",country,"/data"))){
    dir.create(paste0("./",country,"/data"))}
  utils::write.csv(combined_model_results,paste0("./",country,"/data/complete_model_future.csv"))

  if (! file.exists(paste0("./",country,"/plots"))){
    dir.create(paste0("./",country,"/plots"))}
  suppressWarnings(
    ggsave(filename=paste0("./",country,"/plots/complete_model_results.png"), plot=full_plot2, width=12, height=8)
  )
  ###
  sample_week_index <-nrow(combined_model_results)-(nrow(combined_model_results)-end_of_test_set)*0.8

  week_start <- which(combined_model_results$wday[sample_week_index:(sample_week_index+200)]==
                        "Mon")[1]+sample_week_index
  sample_year <- combined_model_results$year[week_start]

  combined_model_results$date <- as.POSIXct(combined_model_results$date, format = "%Y-%m-%d %H:%M:%S")

  full_plot_sample_week <- ggplot(combined_model_results[week_start:(week_start+335),])+
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




  full_plot_sample_week2 <- ggplot(combined_model_results[week_start:(week_start+335),])+
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
