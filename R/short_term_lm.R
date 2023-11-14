#' Short-term forecast
#'
#' The short-term load series is forecasted based on the data.
#'
#' @param short_term_data
#' @param test_set_steps
#'
#' @return
#' @export
#'
#' @examples
#' working_directory <- getwd()
#' setwd(tempdir())
#' shortterm_model_data_example <- short_term_lm(shortterm_holidays_example)
#' setwd(working_directory)
short_term_lm <- function(short_term_data, test_set_steps=17520){


  columns_original_df <- ncol(short_term_data)
  short_term_data[,c((columns_original_df+1):(columns_original_df+24))] <- 0
  for (i in (0:23)){
    colnames(short_term_data)[(columns_original_df+1+i)] <- paste0("Hour",i)
    short_term_data[short_term_data$hour==i,(columns_original_df+1+i)] <- 1
  }

  country=unique(short_term_data$country)
  wday <- as.character(unique(short_term_data$wday))
  # preparation of matrix, where model is stored
  model_st <- data.frame(matrix(ncol = 14, nrow = 168))
  colnames(model_st) <- c("wday","hour","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  model_st[,1] <- c(rep(wday[1],24),rep(wday[2],24),rep(wday[3],24),rep(wday[4],24),rep(wday[5],24),rep(wday[6],24),rep(wday[7],24))
  model_st[,2] <- 1:24


  # data frame for adding all model fits together:
  model.st <- data.frame(matrix(ncol = 4, nrow = 2016)) # 2016 = 24 hours*7 days*12 month
  colnames(model.st) <- c("month","wday","hour","modelfit")
  model.st[,1] <- c(rep(1,168), rep(2,168),rep(3,168),rep(4,168),rep(5,168),rep(6,168),
                    rep(7,168),rep(8,168),rep(9,168),rep(10,168),rep(11,168),rep(12,168))

  model.st[,2] <- c(rep(wday[1],24),rep(wday[2],24),
                    rep(wday[3],24),rep(wday[4],24),
                    rep(wday[5],24),rep(wday[6],24),
                    rep(wday[7],24))
  model.st[,3] <- 1:24

  # define training and test set
  training_set=nrow(short_term_data)- test_set_steps
  training_data=short_term_data[1:training_set,]
  test_data=short_term_data[(training_set+1):nrow(short_term_data),]

  # compute models and store results
  if (! file.exists(country)){
    dir.create(country)}
  if (! file.exists(paste0("./",country,"/models"))){
    dir.create(paste0("./",country,"/models"))}
  if (! file.exists(paste0("./",country,"./models/shortterm_lm"))){
    dir.create(paste0("./",country,"./models/shortterm_lm"))}

  k = 1
  l = 1
  for (i in 1:12){
    for (j in 1:7){
      cat(paste("Processing model:",7*(i-1)+j,"of",12*7))

      x <- training_data[which(training_data$month == i & training_data$wday == wday[j]),]
      xreg <- as.matrix(x[,c((columns_original_df):(columns_original_df+24))])

      fit1 <- stats:: lm(hourly_demand_trend_and_season_corrected ~ xreg[,1:25], data=x)

      name=paste0("month",i,wday[j])
      modelname=paste0("./",country,"/models/shortterm_lm/",name,".Rdata")
      save(fit1,file=modelname)
      fv <- data.frame(fit1$fitted.values)
      model_st[k:(k+23),(i+2)] <- fv[1:24,]
      model.st[l:(l+23),4] <- fv[1:24,]
      k = k+24
      l = l+24
      if (7*(i-1)+j == 12*7) cat('\n Done')
      else cat('\014')
    }

    k = 1
  }





  ## combine the results
  short_term_data$short_term_lm_model_predictions <- vector(length=nrow(short_term_data))

  suppressWarnings(
    for (i in 1:12){
      for(j in 1:7){
        short_term_data$short_term_lm_model_predictions[which(short_term_data$month == i & short_term_data$wday == wday[j])] <-
          model.st[which(model.st$month == i & model.st$wday == wday[j]),4]
      }
    })
  short_term_data$test_set_steps <- test_set_steps
  if (! file.exists(paste0("./",country,"./data/"))){
    dir.create(paste0("./",country,"./data/"))}
  utils::write.csv(short_term_data,paste0("./",country,"./data/short_term_data.csv"),row.names = F)

  colnames(short_term_data) <- make.unique(names(short_term_data))

  st_plot <- ggplot(short_term_data)+geom_line(aes(date,hourly_demand_trend_and_season_corrected,color="actual"))+
    geom_line(aes(date,short_term_lm_model_predictions,color="fitted"))+
    geom_vline(xintercept=test_data$date[1],linetype=2)+
    ggthemes::theme_foundation(base_size=14, base_family="sans")+
    xlab("\nHour")+ylab("[MW]\n")+
    ggtitle(paste("Short Term Model Results -",country,"\n"))+
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

  print(st_plot)

  week_start <- which(short_term_data$wday[(nrow(short_term_data)/2):(nrow(short_term_data)/2+200)]==
                        "Mon")[1]+(nrow(short_term_data)/2)
  sample_year <- short_term_data$year[week_start]

  st_plot_sample_week <- ggplot(short_term_data[week_start:(week_start+335),])+geom_line(aes(date,hourly_demand_trend_and_season_corrected,color="actual"))+
    geom_line(aes(date,short_term_lm_model_predictions,color="fitted"))+
    ggthemes::theme_foundation(base_size=14, base_family="sans")+
    xlab("\nHour")+ylab("[MW]\n")+
    ggtitle(paste("Short Term Model Results -",country),subtitle = paste("2 sample weeks in",sample_year,"\n") )+
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

  print(st_plot_sample_week)

  if (! file.exists(paste0("./",country,"/plots"))){
    dir.create(paste0("./",country,"/plots"))}

  st_plot2 <-ggplot(short_term_data)+geom_line(aes(date,hourly_demand_trend_and_season_corrected,color="actual"))+
    geom_line(aes(date,short_term_lm_model_predictions,color="fitted"))+
    geom_vline(xintercept=test_data$date[1],linetype=2)+
    ggthemes::theme_foundation(base_size=14, base_family="sans")+
    xlab("\nHour")+ylab("[MW]\n")+
    ggtitle(paste("Short Term Model Results -",country,"\n"))+
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

  ggsave(file=paste0("./",country,"/plots/short_term_results.png"), plot=st_plot2, width=12, height=8)

  st_plot_sample_week2 <- ggplot(short_term_data[week_start:(week_start+335),])+geom_line(aes(date,hourly_demand_trend_and_season_corrected,color="actual"))+
    geom_line(aes(date,short_term_lm_model_predictions,color="fitted"))+
    ggthemes::theme_foundation(base_size=14, base_family="sans")+
    xlab("\nHour")+ylab("[MW]\n")+
    ggtitle(paste("Short Term Model Results -",country),subtitle = paste("2 sample weeks in",sample_year,"\n") )+
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



  ggsave(file=paste0("./",country,"/plots/short_term_results_sample_weeks.png"), plot=st_plot_sample_week2, width=12, height=8)


  return (short_term_data)
}
