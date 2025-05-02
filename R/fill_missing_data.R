#' Replace missing values in the load data set
#'
#' This function substitutes missing values with the corresponding values at the same hour exactly one week prior. For example, if there is no load value available for May 12th at 20:00, the value recorded on May 7th at 20:00 will be used as a replacement.
#' This function is primarily designed to handle minor gaps in the dataset acquired using \code{\link{get_entsoE_data}}. To use this function with other datasets, it is important that the input data frame adheres to the required column naming conventions.

#' @param load_data Data Frame with load data. Data Frame must contain the following columns:
#' \describe{
#'   \item{date}{Consisting of the datetime values, date formatted (e.g. POSIXct).}
#'   \item{load}{Consisting of the load values, numeric.}
#'   \item{unit}{Indicating the measured unit (e.g., MW), character.}
#'   \item{country}{Indicating the country's ISO2C code, character.}
#' }
#' @param data_directory The path to the directory where the data will be saved. The default is set to a temporary directory.
#' @return Data Frame with completed load values, date, unit, year, time resolution, ISO2C Country Code
#' @export
#'
#' @examples
#'
#' suppressWarnings(
#'   library(ggplot2)
#' )
#' example_demand_data_filled <- fill_missing_data(example_demand_data)
#' example_df <- as.data.frame(seq.POSIXt(
#'   example_demand_data$date[841],
#'   example_demand_data$date[870], "hour"
#' ))
#' example_df$before <- NA
#' example_df$before[example_df[, 1] %in% example_demand_data$date] <-
#'   example_demand_data$load[example_demand_data$date %in% example_df[, 1]]
#' example_df$after <- example_demand_data_filled$load[example_demand_data_filled$date
#'   %in% example_df[, 1]]
#' ggplot(example_df, aes(x = example_df[, 1])) +
#'   geom_line(aes(y = after, colour = "after data filling")) +
#'   geom_line(aes(y = before, colour = "before data filling")) +
#'   xlab("\nHour") +
#'   ylab("Load [MW]\n") +
#'   theme(legend.title = element_blank()) +
#'   scale_x_continuous(
#'     breaks = c(example_df[1, 1], example_df[25, 1]),
#'     labels = c(as.Date(example_df[1, 1]), as.Date(example_df[25, 1]))
#'   )
fill_missing_data <- function(load_data, data_directory = tempdir()) {
  if ("example" %in% colnames(load_data)) {
    if (unique(load_data$example) == TRUE) {
      return(oRaklE::example_demand_data_filled)
    }
  }

  if (grepl("Rtmp", data_directory)) {
    message(paste(
      "\nThis function will try to save the load series (as .csv) to a folder called", unique(load_data$country),
      "\nin the current data directory:", data_directory
    ))
    message("\nIt is recommended to save the data in a directory other than a tempdir, so that it is available after you finish the R Session.")

    message("\nPlease choose an option:")
    message("\n1: Keep it as a tempdir")
    message(paste("2: (Recommended) Save data in the current working directory (", getwd(), ")", sep = ""))
    message("3: Set the directory manually\n")

    choice <- readline(prompt = "Enter the option number (1, 2, or 3): ")


    if (choice == "1") {
      message("\nData will be saved in a temporary directory and cleaned up when R is shut down.")
      # data_directory remains unchanged.
    } else if (choice == "2") {
      data_directory <- getwd()
      message(paste0("\nData will be saved in the current working directory in ", data_directory, "/", unique(load_data$country), "/data"))
      message("\nYou can specify the *data_directory* parameter in the following functions as ", data_directory)
    } else if (choice == "3") {
      new_dir <- readline(prompt = "Enter the full path of the directory where you want to save the data: ")
      data_directory <- new_dir
      if (!dir.exists(data_directory)) {
        stop("The specified data_directory does not exist: ", data_directory, "\nPlease run the function again.")
      }
      message("\nData will be saved in the specified directory: ", data_directory, "/", unique(load_data$country), "/data")
      message("\nYou can specify the *data_directory* parameter in the following functions as ", data_directory)
    } else {
      message("Invalid input. Keeping the temporary directory.\nData will be cleaned up when R is shut down.")
    }
  } else {
    if (!dir.exists(data_directory)) {
      stop("The specified data_directory does not exist: ", data_directory, "\nPlease run the function again.")
    }
    message("\nData will be saved in the specified working directory in ", data_directory, "/", unique(load_data$country), "/data")
  }

  years <- unique(lubridate::year(load_data$date))
  min_year <- min(unique(lubridate::year(load_data$date)))
  max_year <- max(unique(lubridate::year(load_data$date)))
  if (max_year > min_year) {
    file_name_years <- paste0(min_year, "_", max_year)
  } else {
    file_name_years <- min_year
  }
  min_month <- min(unique(lubridate::month(load_data$date[load_data$year == min_year])))
  all_timepoints <- as.POSIXct(NULL, tz = "UTC")
  for (year in years) {
    if (year == min_year) {
      min_month <- min(unique(lubridate::month(load_data$date[load_data$year == min_year])))
    } else {
      min_month <- 1
    }
    min_index <- which(load_data$year == year)[1]
    resolution <- difftime(load_data$date[(min_index + 1)], load_data$date[(min_index)])
    original_data <- load_data$date[load_data$year == year]

    time_diffs <- difftime(original_data[-1], original_data[-length(original_data)])

    time_diff1 <- difftime(original_data[2], original_data[1])
    resolution_consistent <- all(time_diffs == time_diffs[1])

    # Note: This is to check if there has been a change in reporting resolution to the ENTSO-E during the year
    real_time_diff <- any(time_diffs < time_diff1)

    if (!resolution_consistent & real_time_diff) {
      inconsistent_index <- which(diff(as.numeric(time_diffs)) != 0 & diff(as.numeric(time_diffs)) < time_diff1)[1]
      start1 <- as.POSIXct(paste0(year, "-", min_month, "-01 00:00"), tz = "UTC")
      end1 <- as.POSIXct(original_data[inconsistent_index], tz = "UTC")
      time_diff1 <- difftime(original_data[2], original_data[1])
      timepoint1 <- seq(from = start1, to = end1, by = time_diff1)

      start2 <- as.POSIXct(original_data[(inconsistent_index + 1)], tz = "UTC")
      end2 <- as.POSIXct(original_data[length(original_data)], tz = "UTC")

      relevant_data <- original_data[which(original_data >= start2 & original_data <= end2)]
      time_diffs_subset <- difftime(relevant_data[-1], relevant_data[-length(relevant_data)])
      time_diffs_freq <- table(time_diffs_subset)
      time_diff2 <- as.difftime(as.numeric(names(which.max(time_diffs_freq))), units = "mins")

      timepoint2 <- seq(from = start2, to = end2, by = time_diff2)
      timepoint <- c(timepoint1, timepoint2)
    } else {
      timepoint <- seq(as.POSIXct(paste0(as.character(year), "-", min_month, "-01 00:00"), tz = "UTC"),
        as.POSIXct(paste0(as.character(year), "-12-31 23:59"), tz = "UTC"),
        by = difftime(load_data$date[(min_index + 1)], load_data$date[(min_index)])
      )
    }

    all_timepoints <- c(all_timepoints, timepoint)
  }


  complete_data <- as.data.frame(all_timepoints)
  colnames(complete_data) <- "date"

  complete_data$load <- 0
  complete_data$load[complete_data$date %in% load_data$date] <- load_data$load


  while (length(as.numeric(row.names(complete_data[which(complete_data$load == 0), ]))) > 0) {
    missing_data_index <- as.numeric(row.names(complete_data[which(complete_data$load == 0), ]))
    for (i in missing_data_index) {
      interval_minutes <- as.numeric(difftime(complete_data$date[(i + 1)], complete_data$date[i], units = "mins"))
      interval_one_week_ago <- 60 / interval_minutes * 24 * 7
      complete_data$load[i] <- complete_data$load[(i - interval_one_week_ago)]
    }
  }


  if ("unit" %in% colnames(load_data)) {
    complete_data$unit <- unique(load_data$unit)
  }
  complete_data$year <- lubridate::year(complete_data$date)
  complete_data$time_interval <- c(
    difftime(complete_data$date[-1], complete_data$date[-length(complete_data$date)], units = "mins"),
    difftime(complete_data$date[length(complete_data$date)], complete_data$date[(length(complete_data$date) - 1)], units = "mins")
  )

  complete_data$country <- unique(load_data$country)
  country <- unique(load_data$country)

  na_end_count <- sum(cumprod(rev(is.na(complete_data))) == 1)
  if (na_end_count > 0) {
    warning(paste("Trimmed", na_end_count, "timepoints at the end because of missing values."))
  }
  complete_data <- zoo::na.trim(complete_data)

  if (!file.exists(paste0(data_directory, "/", country))) {
    dir.create(paste0(data_directory, "/", country))
  }
  if (!file.exists(paste0(data_directory, "/", country, "/data"))) {
    dir.create(paste0(data_directory, "/", country, "/data"))
  }

  utils::write.csv(complete_data, paste0(data_directory, "/", country, "/data/filled_load_data_", file_name_years, ".csv"), row.names = F)


  return(complete_data)
}
