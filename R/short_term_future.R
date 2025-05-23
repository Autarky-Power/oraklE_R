#' Generate future short-term demand predictions
#'
#' This function extends the short-term demand predictions generated by \code{\link{short_term_lm}} until a specified future year.
#' The function also produces and saves visualizations of the actual and the predicted demand over the training, test, and future periods.
#' @param shortterm_predictions Dataframe. Generated by \code{\link{short_term_lm}}
#' @param end_year Integer. Specifies the final year for which future predictions will be generated
#' @param data_directory The path to the directory where the data will be saved and where the function will look for
#' the short-term models from \code{\link{short_term_lm}}. The default is set to a temporary directory.
#' @param model_list A list with the models from \code{\link{short_term_lm}}. Only needs to be specified if the models
#' are not in the data directory.
#' @param verbose A boolean value indicating if you want the generated plots to be shown (set to TRUE if yes).
#' @return The extended initial dataframe with the future predictions for the short term model.
#' @return A list with the extended initial dataframe with the future predictions for the short term model. And the plot with the shortterm seasonality future forecast.
#' The dataset and the plot are saved in the respective folder for the country.
#' \describe{
#'   \item{shortterm_future_predictions}{A dataframe with the input and prediction data for the future short-term seasonality.}
#'   \item{shortterm_future_plot}{A plot with the prediction results.}
#' }
#' @export
#' @seealso See also function \code{\link{long_term_future}} and \code{\link{mid_term_future}} for the other prediction models.
#' @export
#'
#' @examples
#' example_shortterm_future_predictions <- short_term_future(example_shortterm_predictions,
#'   end_year = 2028
#' )
short_term_future <- function(shortterm_predictions, end_year, data_directory = tempdir(), model_list = NULL, verbose = FALSE) {
  if (inherits(shortterm_predictions, "list") && names(shortterm_predictions)[1] == "shortterm_predictions") {
    model_list <- shortterm_predictions$shortterm_model
    short_df <- shortterm_predictions$shortterm_predictions
  } else {
    short_df <- shortterm_predictions
  }

  if ("example" %in% colnames(shortterm_predictions)) {
    if (unique(shortterm_predictions$example) == TRUE) {
      message("Extending the short-term seasonality model predictions until the year 2028.")
      example_data <- oRaklE::example_shortterm_future_predictions
      training_set <- 26280
      training_data <- example_data[1:training_set, ]
      test_data <- example_data[(training_set + 1):nrow(example_data), ]

      x <- training_data[which(training_data$month == 1 & training_data$wday == "Mon"), ]
      x_test <- test_data[which(test_data$month == 1 & test_data$wday == "Mon"), ]

      variables <- colnames(example_data)[13:37]
      f <- stats::as.formula(
        paste("hourly_demand_trend_and_season_corrected",
          paste(variables, collapse = " + "),
          sep = " ~ "
        )
      )

      fit1 <- stats::lm(f, data = x)
      suppressWarnings(
        example_prediction <- stats::predict(fit1, newdata = x_test)
      )

      test_sum <- sum(example_prediction - test_data$short_term_lm_model_predictions[which(test_data$month == 1 & test_data$wday == "Mon")])
      if (test_sum < 1) {
        return(oRaklE::example_shortterm_future_predictions)
      } else {
        stop("The example in short_term_future() failed. Please contact the package maintainer at schwenzer@europa-uni.de")
      }
    }
  }
  if (grepl("Rtmp", data_directory)) {
    message(paste(
      "\nThis function will try to save the plots and find the short-term models in a folder called", unique(short_df$country),
      "\nin the current data directory:", data_directory
    ))
    message("\nIf the short-term models are not found, a list with the models has to be passed in the *model_list* argument.")

    message("\nPlease choose an option:")
    message("\n1: Keep it as a tempdir")
    message(paste("2: Save data and look for the models in the current working directory (", getwd(), ")", sep = ""))
    message("3: Set the directory manually\n")

    choice <- readline(prompt = "Enter the option number (1, 2, or 3): ")


    if (choice == "1") {
      message("\nData will be saved in a temporary directory and cleaned up when R is shut down.\n")
    } else if (choice == "2") {
      data_directory <- getwd()
      message(paste0("\nData will be saved in the current working directory in ", data_directory, "/", unique(short_df$country), "/data"))
      message("\nYou can specify the *data_directory* parameter in the following functions as ", data_directory)
    } else if (choice == "3") {
      new_dir <- readline(prompt = "Enter the full path of the directory where you want to save the data: ")
      data_directory <- new_dir
      if (!dir.exists(data_directory)) {
        stop("The specified data_directory does not exist: ", data_directory, "\nPlease run the function again.")
      }
      message("\nData will be saved in the specified directory: ", data_directory, "/", unique(short_df$country), "/data")
    } else {
      message("Invalid input. Keeping the temporary directory.\nData will be cleaned up when R is shut down.\n")
    }
  } else {
    if (!dir.exists(data_directory)) {
      stop("The specified data_directory does not exist: ", data_directory, "\nPlease run the function again.")
    }
    message("\nData will be saved in the specified working directory in ", data_directory, "/", unique(short_df$country), "/data")
  }


  start_year <- max(short_df$year) + 1

  timepoint <- seq(as.POSIXct(paste0(as.character(start_year), "-01-01 00:00")),
    as.POSIXct(paste0(as.character(end_year), "-12-31 23:00")),
    by = "hour"
  )


  new_rows <- as.data.frame(matrix(nrow = length(timepoint), ncol = ncol(short_df)))
  colnames(new_rows) <- colnames(short_df)

  new_rows$date <- timepoint
  new_rows <- new_rows[!(format(new_rows$date, "%m-%d") == "02-29"), ]

  # new_rows$date <- as.character(timepoint)
  first_row <- nrow(short_df) + 1

  new_rows$year <- lubridate::year(new_rows$date)
  new_rows$month <- lubridate::month(new_rows$date)
  new_rows$day <- lubridate::day(new_rows$date)
  new_rows$wday <- lubridate::wday(new_rows$date, label = T, locale = "en_US.UTF-8")
  new_rows$hour <- lubridate::hour(new_rows$date) # 0:23

  for (i in (0:23)) {
    col_name <- paste0("Hour", i)
    new_rows[, col_name] <- 0
    new_rows[new_rows$hour == i, col_name] <- 1
  }

  new_rows$country <- unique(short_df$country)

  years <- unique(new_rows$year)
  country <- (unique(new_rows$country))

  holiday_list <- list()
  for (i in 1:length(years)) {
    year <- years[i]
    tryCatch(
      {
        Sys.sleep(1.5)
        response <- jsonlite::fromJSON(paste0(
          "https://date.nager.at/api/v3/publicholidays/",
          year, "/", country
        ))
      },
      error = function(e) {
        i=i-1
        Sys.sleep(5)
        #stop("Error during JSON request to date.nager.at : ", e$message, call. = FALSE)
      }
    )
    holiday_list[[i]] <- response$date
  }

  holidays <- unlist(holiday_list)
  holidays <- as.Date(holidays)


  new_rows$holiday <- ifelse(as.Date(new_rows$date, tz = "CET") %in% holidays, 1, 0)



  wday <- as.character(unique(new_rows$wday))
  fit1 <- NULL

  suppressWarnings(
    for (i in 1:12) {
      for (j in 1:7) {
        x <- new_rows[which(new_rows$month == i & new_rows$wday == wday[j]), ]

        name <- paste0("month", i, wday[j])
        model_path <- paste0(data_directory, "/", country, "/models/shortterm_lm/", name, ".Rdata")
        if (name %in% names(model_list)) {
          fit1 <- model_list[[name]]
        } else if (file.exists(model_path)) {
          load(paste0(data_directory, "/", country, "/models/shortterm_lm/", name, ".Rdata"))
        } else {
          stop("\nModel not found. Please either specify the base path where the country data is saved (e.g. the current working directory or supply a list with models for the *model_list* variable.")
        }
        new_rows$short_term_lm_model_predictions[which(new_rows$month == i & new_rows$wday == wday[j])] <-
          stats::predict(fit1, newdata = x)
      }
    }
  )

  new_rows$test_set_steps <- unique(short_df$test_set_steps)
  future_short_term <- rbind(short_df, new_rows)

  training_set_end <- nrow(short_df) - unique(short_df$test_set_steps)
  test_set_end <- training_set_end + unique(short_df$test_set_steps)
  future_set <- nrow(future_short_term) - test_set_end
  max_value <- max(c(max(future_short_term$short_term_lm_model_predictions), max(future_short_term$hourly_demand_trend_and_season_corrected, na.rm = T)))

  suppressWarnings(
    st_plot <- ggplot(future_short_term) +
      geom_line(aes(date, future_short_term$hourly_demand_trend_and_season_corrected, color = "actual")) +
      geom_line(aes(date, future_short_term$short_term_lm_model_predictions, color = "fitted")) +
      geom_vline(xintercept = short_df$date[(nrow(short_df) - unique(short_df$test_set_steps))], linetype = 2) +
      geom_vline(xintercept = short_df$date[test_set_end], linetype = 3) +
      ggthemes::theme_foundation(base_size = 14, base_family = "sans") +
      xlab("\nHour") +
      ylab("Change in avg. Hourly Demand\n[MW]\n") +
      ggtitle(paste("Short Term Model Results -", country, "\n")) +
      theme(
        plot.title = element_text(
          face = "bold",
          size = rel(1.2), hjust = 0.5
        ),
        text = element_text(),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        panel.border = element_rect(colour = NA),
        axis.title = element_text(face = "bold", size = rel(1)),
        axis.title.y = element_text(angle = 90, vjust = 2),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.ticks = element_line(),
        panel.grid.major = element_line(colour = "#f0f0f0"),
        panel.grid.minor = element_blank(),
        legend.key = element_rect(colour = NA),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(0.2, "cm"),
        plot.margin = unit(c(10, 5, 5, 5), "mm"),
        strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
        strip.text = element_text(face = "bold")
      ) +
      theme(legend.title = element_blank()) +
      guides(color = guide_legend(override.aes = list(linewidth = 2))) +
      annotate("text", x = future_short_term$date[(training_set_end / 2)], y = (max_value + max_value * 0.1), label = "Training", size = 4, hjust = 0.5, vjust = 0) +
      annotate("text", x = future_short_term$date[(training_set_end + unique(short_df$test_set_steps) / 2)], y = (max_value + max_value * 0.1), label = "Test", size = 4, hjust = 0.5, vjust = 0) +
      annotate("text", x = future_short_term$date[(nrow(short_df) + future_set / 2)], y = (max_value + max_value * 0.1), label = "Unknown", size = 4, hjust = 0.5, vjust = 0)
  )

  if (verbose == FALSE) {
    message("\nVerbose is set to FALSE. Set to TRUE if you want to see the generated plot automatically. The plot is saved in the output under *shortterm_future_plot* and in the plots folder in ", data_directory)
  } else {
    suppressWarnings(
      print(st_plot)
    )
  }

  suppressWarnings(
    ggsave(filename = paste0(data_directory, "/", country, "/plots/short_term_results_future.png"), plot = st_plot, width = 12, height = 8)
  )
  return(list("shortterm_future_predictions" = future_short_term, "shortterm_future_plot" = st_plot))
}
