

#' Title
#'
#' @param shortterm_predictions
#' @param end_year
#'
#' @return
#' @export
#'
#' @examples
short_term_future <- function(shortterm_predictions,end_year){

  short_df <- shortterm_predictions

  start_year = max(short_df$year)+1

  timepoint <- seq(as.POSIXct(paste0(as.character(start_year),'-01-01 00:00')),
                   as.POSIXct(paste0(as.character(end_year),'-12-31 23:00')),by="hour")


  new_rows <- as.data.frame(matrix(nrow=length(timepoint),ncol = ncol(short_df) ))
  colnames(new_rows) <- colnames(short_df)

  new_rows$date <- timepoint

  #new_rows$date <- as.character(timepoint)
  first_row = nrow(short_df)+1

  new_rows$year <- lubridate::year(new_rows$date)
  new_rows$month <- lubridate::month(new_rows$date)
  new_rows$day <- lubridate::day(new_rows$date)
  new_rows$wday <- lubridate::wday(new_rows$date,label = T,locale = "English")
  new_rows$hour <- lubridate::hour(new_rows$date)#0:23

  for (i in (0:23)){
    col_name <- paste0("Hour",i)
    new_rows[,col_name] <- 0
    new_rows[new_rows$hour==i,col_name] <- 1
  }

  new_rows$country <- unique(short_df$country)

  years=unique(new_rows$year)
  country= (unique(new_rows$country))

  holiday_list <- list()
  for (i in 1:length(years)){
    year= years[i]
    response = jsonlite::fromJSON(paste0("https://date.nager.at/api/v3/publicholidays/"
                                         ,year,"/",country) )
    holiday_list[[i]] <- response$date
  }

  holidays = unlist(holiday_list)
  holidays = as.Date(holidays)

  new_rows$place_holder <- as.Date(new_rows$date,tz="CET")
  new_rows$holiday <- 0
  new_rows$holiday[new_rows$place_holder %in% holidays] <- 1
  new_rows <- subset(new_rows, select = -c(place_holder))


  wday <- as.character(unique(new_rows$wday))
  suppressWarnings(
    for (i in 1:12){
      for (j in 1:7){


        x <- new_rows[which(new_rows$month == i & new_rows$wday == wday[j]),]


        name=paste0("month",i,wday[j])
        load(paste0("./",country,"/models/shortterm_lm/",name,".Rdata"))

        new_rows$short_term_lm_model_predictions[which(new_rows$month == i & new_rows$wday == wday[j])] <-
          predict(fit1,newdata = x)


      }
    })



  new_rows$test_set_steps = unique(short_df$test_set_steps)
  future_short_term = rbind(short_df, new_rows)


  st_plot <- ggplot(future_short_term)+geom_line(aes(date,hourly_demand_trend_and_season_corrected,color="actual"))+
    geom_line(aes(date,short_term_lm_model_predictions,color="fitted"))+
    geom_vline(xintercept=short_df$date[(nrow(short_df)-unique(short_df$test_set_steps))],linetype=2)+
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
    theme(legend.title = element_blank())+guides(color = guide_legend(override.aes = list(linewidth = 2)))
  suppressWarnings(
    print(st_plot)
  )
  ggsave(file=paste0("./",country,"/plots/short_term_results_future.png"), plot=st_plot, width=12, height=8)

  return(future_short_term)

}
