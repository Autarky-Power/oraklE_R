#' Decomposing the load data into long-, mid- and short-term component
#'
#' This function decomposes the load data into three components: a yearly long-term trend, a daily mid-term seasonality, and an hourly short-term seasonality. If the data is available only at a daily resolution, the calculation of hourly seasonality is skipped. The results of the decomposition are returned as a list of dataframes. The series are plotted additionally.
#'
#' @param load_data A data frame object with "load", "date", "unit", and "country" columns
#' \describe{
#'   \item{load}{Consisting of the load values, numeric.}
#'   \item{date}{Consisting of the datetime values, datetime (e.g. POSIXct).}
#'   \item{unit}{Indicating the unit, e.g. MW, character.}
#'   \item{country}{Indicating the country's ISO2C code, character.}
#' }
#'
#' @return A list of three data frames with
#' \describe{
#'   \item{longterm}{A data frame of the long-term trend, including columns for country, year, and yearly average hourly demand.}
#'   \item{midterm}{A data frame of the mid-term component, including country, date, year, month, day, weekday, average hourly demand, and seasonal average hourly demand. Where seasonal average hourly demand corresponds to the difference between the yearly average demand per hour and the daily average demand per hour of the respective day.}
#'   \item{shortterm}{A data frame of the short-term component, including country, date, year, month, day, weekday, hour, hourly demand, and hourly demand trend and trend and season corrected. Where hourly demand trend and season corrected corresponds to the difference between the daily average demand per hour and the actual demand in the respective hour, effectively showing the intra-day pattern.}
#' }
#' @export
#' @import ggplot2
#'
#' @examples
#'
#' example_decomposed_data <- decompose_load_data(example_demand_data_filled)
decompose_load_data <- function(load_data) {
  if ("example" %in% colnames(load_data)) {
    if (unique(load_data$example) == TRUE) {
      message("Decomposing the load data into a long-term trend component, a mid-term seasonality component and a short-term seasonality component.")
      return(oRaklE::example_decomposed_data)
    }
  }
  resolution <- as.numeric(difftime(load_data$date[2], load_data$date[1], units = "hours"))
  if (resolution <= 1) {
    timepoint <- seq(as.POSIXct(paste0(as.character(min(unique(load_data$year))), "-01-01 00:00"), tz = "UTC"),
      as.POSIXct(paste0(as.character(max(unique(load_data$year))), "-12-31 23:00"), tz = "UTC"),
      by = "hour"
    )
  } else {
    timepoint <- seq(as.POSIXct(paste0(as.character(min(unique(load_data$year))), "-01-01"), tz = "UTC"),
      as.POSIXct(paste0(as.character(max(unique(load_data$year))), "-12-31"), tz = "UTC"),
      by = "day"
    )
  }


  if (is.character(load_data$time_interval)) {
    intervals <- as.numeric(sub(" mins", "", load_data$time_interval))
    load_data$time_interval <- as.difftime(intervals, units = "mins")
  }


  ordered_data <- as.data.frame(timepoint)
  colnames(ordered_data) <- "date"
  ordered_data$year <- lubridate::year(ordered_data$date)
  ordered_data$month <- lubridate::month(ordered_data$date)
  ordered_data$day <- lubridate::day(ordered_data$date)
  ordered_data$wday <- lubridate::wday(ordered_data$date, label = T, locale = "en_US.UTF-8")
  ordered_data$load <- 0
  years <- unique(load_data$year)
  suppressWarnings(
    if (resolution <= 1) {
      ordered_data$hour <- lubridate::hour(ordered_data$date)
      for (year in years) {
        intervals <- unique(load_data$time_interval[load_data$year == year])
        if (length(intervals) > 1) {
          data <- load_data[load_data$year == year, ]
          year_load <- c()
          for (i in intervals) {
            end_index <- max(which(data$time_interval == i))
            start_index <- min(which(data$time_interval == i))
            load_mean <- colMeans(matrix(data$load[start_index:end_index], nrow = 60 / i))
            year_load <- c(year_load, load_mean)
          }
        } else {
          year_load <- colMeans(matrix(load_data$load[load_data$year == year], nrow = 60 / as.numeric(intervals[1])))
        }
        ordered_data$load[ordered_data$year == year] <- year_load
      }
    } else {
      ordered_data$load <- load_data$load
    }
  )
  if ("unit" %in% colnames(load_data)) {
    ordered_data$unit <- unique(load_data$unit)
  }

  ordered_data$country <- unique(load_data$country)

  all_data <- ordered_data
  all_data <- all_data[!(all_data$month == 2 & all_data$day == 29), ]

  longterm <- data.frame(matrix(nrow = length(unique(all_data$year)), ncol = 3))
  colnames(longterm) <- c("country", "year", "avg_hourly_demand")
  longterm$year <- unique(all_data$year)
  country <- unique(all_data$country)
  longterm$country <- country
  for (i in (min(longterm$year):max(longterm$year))) {
    longterm$avg_hourly_demand[longterm$year == i] <- mean(all_data$load[all_data$year == i], na.rm = T)
  }
  if (resolution <= 1) {
    midterm <- data.frame(matrix(nrow = (length(unique(all_data$year)) * 365), ncol = 7))
    colnames(midterm) <- c("country", "date", "year", "month", "day", "wday", "avg_hourly_demand")

    for (i in 1:length(unique(all_data$year))) {
      midterm$year[((i - 1) * 365 + 1):(i * 365)] <- unique(all_data$year)[i]
    }

    for (i in 1:nrow(midterm)) {
      midterm$date[i] <- all_data$date[((i - 1) * 24 + 1)]
      midterm$month[i] <- all_data$month[((i - 1) * 24 + 1)]
      midterm$day[i] <- all_data$day[((i - 1) * 24 + 1)]
      midterm$wday[i] <- all_data$wday[((i - 1) * 24 + 1)]
      midterm$avg_hourly_demand[i] <- mean(all_data$load[((i - 1) * 24 + 1):(i * 24)], na.rm = T)
    }
    midterm$date <- as.POSIXct(midterm$date, format = "%Y-%m-%d", origin = "1970-01-01")
    midterm$date <- as.Date(midterm$date, format = "%Y-%m-%d", "UTC")
    midterm$country <- country
  } else {
    midterm <- data.frame(matrix(nrow = nrow(ordered_data), ncol = 7))
    midterm[, 1:7] <- ordered_data[, c(8, 1:6)]
    colnames(midterm) <- c("country", "date", "year", "month", "day", "wday", "avg_hourly_demand")
  }

  midterm$seasonal_avg_hourly_demand <- 0
  for (i in min(all_data$year):max(all_data$year)) {
    midterm$seasonal_avg_hourly_demand[midterm$year == i] <- midterm$avg_hourly_demand[midterm$year == i] -
      longterm$avg_hourly_demand[longterm$year == i]
  }


  if (resolution <= 1) {
    shortterm <- data.frame(matrix(nrow = (nrow(all_data)), ncol = 1))
    colnames(shortterm) <- "country"

    shortterm$country <- country
    shortterm$date <- all_data$date
    shortterm$year <- all_data$year
    shortterm$month <- all_data$month
    shortterm$day <- all_data$day
    shortterm$wday <- all_data$wday
    shortterm$hour <- all_data$hour
    shortterm$hourly_demand <- all_data$load
    shortterm$hourly_demand_trend_corrected <- 0

    #
    shortterm$yearly <- 0
    shortterm$daily <- 0
    for (i in min(all_data$year):max(all_data$year)) {
      shortterm$yearly[shortterm$year == i] <- longterm$avg_hourly_demand[longterm$year == i]
      shortterm$hourly_demand_trend_corrected[shortterm$year == i] <- shortterm$hourly_demand[shortterm$year == i] -
        longterm$avg_hourly_demand[longterm$year == i]
    }

    shortterm$hourly_demand_trend_and_season_corrected <- 0


    for (i in seq_len(nrow(midterm))) {
      shortterm$daily[((i - 1) * 24 + 1):(i * 24)] <- midterm$seasonal_avg_hourly_demand[i]
      shortterm$hourly_demand_trend_and_season_corrected[((i - 1) * 24 + 1):(i * 24)] <-
        shortterm$hourly_demand_trend_corrected[((i - 1) * 24 + 1):(i * 24)] - midterm$seasonal_avg_hourly_demand[i]
    }
    suppressWarnings(
      shortterm_seasonality_plot <- ggplot(shortterm) +
        geom_line(aes(1:nrow(shortterm), shortterm$hourly_demand_trend_and_season_corrected, color = "Average hourly demand"), linewidth = 1.1) +
        theme(legend.title = element_blank()) +
        ggtitle("Short-term seasonality \n") +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
        ylab("MW") +
        xlab("Hour\n") +
        scale_color_manual(values = c("#ff7f24"))
    )
  }
  suppressWarnings(
    trend_plot <- ggplot(longterm) +
      geom_line(aes(year, longterm$avg_hourly_demand, color = "Average hourly demand"), linewidth = 1.1) +
      theme(legend.title = element_blank()) +
      ggtitle("Long-term trend \n") +
      xlab("Year\n") +
      theme(plot.title = element_text(hjust = 0.5)) +
      ylab("MW") +
      theme(legend.position = "none")
  )
  suppressWarnings(
    midterm_seasonality_plot <- ggplot(midterm) +
      geom_line(aes(1:nrow(midterm), midterm$seasonal_avg_hourly_demand, color = "Average hourly demand"), linewidth = 1.1) +
      theme(legend.title = element_blank()) +
      ggtitle("Mid-term seasonality \n") +
      theme(plot.title = element_text(hjust = 0.5)) +
      ylab("MW") +
      xlab("Day\n") +
      theme(legend.position = "none") +
      scale_color_manual(values = c("#4d0272"))
  )
  if (!file.exists(country)) {
    dir.create(country)
  }
  if (!file.exists(paste0("./", country, "/models"))) {
    dir.create(paste0("./", country, "/models"))
  }
  if (!file.exists(paste0("./", country, "/data"))) {
    dir.create(paste0("./", country, "/data"))
  }
  if (!file.exists(paste0("./", country, "/plots"))) {
    dir.create(paste0("./", country, "/plots"))
  }

  if (resolution <= 1) {
    suppressWarnings(
      all_plots <- patchwork::wrap_plots(trend_plot, midterm_seasonality_plot, shortterm_seasonality_plot, ncol = 1)
    )
    suppressWarnings(
      print(all_plots)
    )
    suppressWarnings(
      ggsave(filename = paste0("./", country, "/plots/Decomposed_load.png"), plot = all_plots, width = 12, height = 8)
    )
    return(list("longterm" = longterm, "midterm" = midterm, "shortterm" = shortterm))
  } else {
    all_plots <- patchwork::wrap_plots(trend_plot, midterm_seasonality_plot, ncol = 1)
    print(all_plots)
    ggsave(filename = paste0("./", country, "/plots/Decomposed_load.png"), plot = all_plots, width = 12, height = 8)

    return(list("longterm" = longterm, "midterm" = midterm))
  }
}
