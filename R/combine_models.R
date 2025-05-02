#' Combine forecast models
#'
#' This function combines the three separate forecasts for the low, mid and high frequency model. The three separate forecasts need to be run first.
#'
#' @param longterm_predictions Dataframe. The object resulting from function \code{\link{long_term_lm}}.
#' @param midterm_predictions Dataframe. The object resulting from function \code{\link{mid_term_lm}}.
#' @param shortterm_predictions Dataframe. The object resulting from function \code{\link{short_term_lm}}.
#' @param longterm_model_number Integer. Specifies which of the 3 best long-term models should be used.
#' @param data_directory The path to the directory where the results, metrics and plots will be saved. The default is set to a temporary directory.
#' @param verbose A boolean value indicating if you want the generated plots and final result metrics to be shown (set to TRUE if yes).
#' @return A list with the dataframe with the combined model results. A dataframe with selected model metrics. And a list with the plotted results.
#' The combined model predictions, plots, and metrics are saved in the respective folder for the country.
#' \describe{
#'   \item{combined_model_predictions}{A dataframe with the combined model results.}
#'   \item{combined_model_metrics}{A dataframe with model metrics.}
#'   \item{combined_model_plots}{A list with the plot for the full timeseries, a plot with two sample weeks, and a stacked plot with both.}
#' }
#' @export
#' @examples
#' example_full_model_predictions <- combine_models(example_longterm_predictions,
#'   example_midterm_predictions, example_shortterm_predictions,
#'   longterm_model_number = 1
#' )
combine_models <- function(longterm_predictions, midterm_predictions, shortterm_predictions, longterm_model_number = 1,
                           data_directory = tempdir(), verbose = FALSE) {

  if (inherits(longterm_predictions, "list") && names(longterm_predictions)[1] == "longterm_predictions") {

    longterm_predictions <- longterm_predictions$longterm_predictions
  }
  if (inherits(midterm_predictions, "list") && names(midterm_predictions)[1] == "midterm_predictions") {

    midterm_predictions <- midterm_predictions$midterm_predictions
  }
  if (inherits(shortterm_predictions, "list") && names(shortterm_predictions)[1] == "shortterm_predictions") {

    shortterm_predictions <- shortterm_predictions$shortterm_predictions
  }

   if ("example" %in% colnames(shortterm_predictions) &&
    "example" %in% colnames(midterm_predictions) &&
    "example" %in% colnames(longterm_predictions)) {
    if (unique(shortterm_predictions$example) == TRUE &&
      unique(shortterm_predictions$example) == TRUE &&
      unique(shortterm_predictions$example) == TRUE) {
      message("Combining the long-term, mid-term and short-term seasonality into the final forecast.")

      combined_model_results <- shortterm_predictions[1:8760, 1:8]
      combined_model_results$long_term_model <- 0
      year <- 2017
      combined_model_results$long_term_model[combined_model_results$year == year] <-
        longterm_predictions$longterm_model_predictions1[longterm_predictions$year == year]

      combined_model_results$mid_term_model <- 0
      for (i in 1:365) {
        combined_model_results$mid_term_model[((i - 1) * 24 + 1):(i * 24)] <-
          midterm_predictions$midterm_model_fit[i]
      }

      combined_model_results$short_term_model <- shortterm_predictions$short_term_lm_model_predictions[1:8760]

      combined_model_results$complete_model <- combined_model_results$long_term_model +
        combined_model_results$mid_term_model + combined_model_results$short_term_model

      eval_data <- oRaklE::example_full_model_predictions
      eval_sum <- sum(combined_model_results$complete_model - eval_data$complete_model[1:8760])
      if (eval_sum < 1) {
        return(oRaklE::example_full_model_predictions)
      } else {
        stop("The example in combine_models() failed. Please contact the package maintainer at schwenzer@europa-uni.de")
      }
    }
  }


  if (grepl("Rtmp", data_directory)) {
    message(paste(
      "\nThis function will try to save the results, metrics and plots to a folder called", unique(longterm_predictions$country),
      "\nin the current data directory:", data_directory
    ))
    message("\nIt is recommended to save the data in a directory other than a tempdir, so that it is available after you finish the R Session.")

    message("\nPlease choose an option:")
    message("\n1: Keep it as a tempdir")
    message(paste("2: Save data in the current working directory (", getwd(), ")", sep = ""))
    message("3: Set the directory manually\n")

    choice <- readline(prompt = "Enter the option number (1, 2, or 3): ")


    if (choice == "1") {
      message("\nData will be saved in a temporary directory and cleaned up when R is shut down.")
      # data_directory remains unchanged.
    } else if (choice == "2") {
      data_directory <- getwd()
      message(paste0("\nResults, metrics, and plots will be saved in the current working directory in ", data_directory, "/", unique(longterm_predictions$country)))
    } else if (choice == "3") {
      new_dir <- readline(prompt = "Enter the full path of the directory where you want to save the data: ")
      data_directory <- new_dir
      if (!dir.exists(data_directory)) {
        stop("The specified data_directory does not exist: ", data_directory, "\nPlease run the function again.")
      }
      message("\nResults, metrics, and plots will be saved in the specified directory: ", data_directory, "/", unique(longterm_predictions$country))
    } else {
      message("Invalid input. Keeping the temporary directory.\nData will be cleaned up when R is shut down.")
    }
  } else {
    if (!dir.exists(data_directory)) {
      stop("The specified data_directory does not exist: ", data_directory, "\nPlease run the function again.")
    }
    message("\nResults, metrics, and plots will be saved in the specified working directory in ", data_directory, "/", unique(longterm_predictions$country))
  }

  combined_model_results <- shortterm_predictions[, 1:8]
  country <- unique(longterm_predictions$country)
  combined_model_results$long_term_model <- 0

  for (year in unique(combined_model_results$year)) {
    if (longterm_model_number == 1) {
      combined_model_results$long_term_model[combined_model_results$year == year] <-
        longterm_predictions$longterm_model_predictions1[longterm_predictions$year == year]
    } else if (longterm_model_number == 2) {
      combined_model_results$long_term_model[combined_model_results$year == year] <-
        longterm_predictions$longterm_model_predictions2[longterm_predictions$year == year]
    } else {
      combined_model_results$long_term_model[combined_model_results$year == year] <-
        longterm_predictions$longterm_model_predictions3[longterm_predictions$year == year]
    }
  }
  combined_model_results$mid_term_model <- 0


  for (i in 1:nrow(midterm_predictions)) {
    combined_model_results$mid_term_model[((i - 1) * 24 + 1):(i * 24)] <-
      midterm_predictions$midterm_model_fit[i]
  }

  combined_model_results$short_term_model <- shortterm_predictions$short_term_lm_model_predictions

  combined_model_results$complete_model <- combined_model_results$long_term_model +
    combined_model_results$mid_term_model + combined_model_results$short_term_model


  test_set_steps <- unique(longterm_predictions$test_set_steps)
  year_training_set <- nrow(longterm_predictions) - test_set_steps
  end_of_training_set <- max(which(combined_model_results$year == longterm_predictions$year[year_training_set]))



  training_mape <- MLmetrics::MAPE(combined_model_results$complete_model[1:end_of_training_set], combined_model_results$hourly_demand[1:end_of_training_set])
  test_mape <- MLmetrics::MAPE(combined_model_results$complete_model[(end_of_training_set + 1):nrow(combined_model_results)], combined_model_results$hourly_demand[(end_of_training_set + 1):nrow(combined_model_results)])

  RSQUARE_training <- stats::cor(combined_model_results$hourly_demand[1:end_of_training_set], combined_model_results$complete_model[1:end_of_training_set])^2
  RSQUARE_test <- stats::cor(combined_model_results$hourly_demand[(end_of_training_set + 1):nrow(combined_model_results)], combined_model_results$complete_model[(end_of_training_set + 1):nrow(combined_model_results)])^2

  training_rmse <- MLmetrics::RMSE(combined_model_results$complete_model[1:end_of_training_set], combined_model_results$hourly_demand[1:end_of_training_set])
  test_rmse <- MLmetrics::RMSE(combined_model_results$complete_model[(end_of_training_set + 1):nrow(combined_model_results)], combined_model_results$hourly_demand[(end_of_training_set + 1):nrow(combined_model_results)])

  if (!verbose) {
    message("Verbose is set to FALSE. Set to TRUE if you want to see the final model metrics and the generated plots automatically. The plots and metrics are saved in the output under *combined_model_plots* and *combined_model_metrics*
            in the plots and data folder in ", data_directory)
  } else {
    cat("\n*** Final Model Metrics ***\n
    MAPE\nTraining Set:", round(training_mape, 4), "\nTest Set:    ", round(test_mape, 4), "\n
    RSQUARE\nTraining Set:", round(RSQUARE_training, 4), "\nTest Set:    ", round(RSQUARE_test, 4), "\n
    ACCURACY\nTraining Set:", round((1 - training_mape) * 100, 2), "%\nTest Set:    ", round((1 - test_mape) * 100, 2), "%\n
    RMSE\nTraining Set:", round(training_rmse, 1), "MW\nTest Set:    ", round(test_rmse, 1), "MW\n\n")
  }
  results <- as.data.frame(matrix(nrow = 2, ncol = 4))
  colnames(results) <- c("MAPE", "RSQUARE", "ACCURACY", "RMSE")
  rownames(results) <- c("training set", "test set")
  results[1, ] <- c(round(training_mape, 4), round(RSQUARE_training, 4), round((1 - training_mape) * 100, 2), round(training_rmse, 1))
  results[2, ] <- c(round(test_mape, 4), round(RSQUARE_test, 4), round((1 - test_mape) * 100, 2), round(test_rmse, 1))


  full_plot <- ggplot(combined_model_results) +
    geom_line(aes(date, combined_model_results$hourly_demand, color = "actual")) +
    geom_line(aes(date, combined_model_results$complete_model, color = "fitted")) +
    xlab("\nYear") +
    ylab("Hourly Demand\n [MW]\n") +
    geom_vline(xintercept = combined_model_results$date[end_of_training_set], linetype = 2) +
    ggthemes::theme_foundation(base_size = 14, base_family = "sans") +
    xlab("\nHour") +
    ylab("Hourly Demand\n [MW]\n") +
    ggtitle(paste("Complete Model Results -", country, "\n")) +
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
    theme(legend.title = element_blank())



  full_plot2 <- ggplot(combined_model_results) +
    geom_line(aes(date, combined_model_results$hourly_demand, color = "actual")) +
    geom_line(aes(date, combined_model_results$complete_model, color = "fitted")) +
    xlab("\nYear") +
    ylab("Hourly Demand\n [MW]\n") +
    geom_vline(xintercept = combined_model_results$date[end_of_training_set], linetype = 2) +
    ggthemes::theme_foundation(base_size = 14, base_family = "sans") +
    xlab("\nHour") +
    ylab("Hourly Demand\n [MW]\n") +
    ggtitle(paste("Complete Model Results -", country, "\n")) +
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
    theme(axis.title = element_text(size = 23)) +
    theme(legend.text = element_text(size = 23)) +
    theme(axis.text = element_text(size = 20)) +
    theme(plot.title = element_text(size = 26))

  if (!file.exists(paste0(data_directory, "/", country))) {
    dir.create(paste0(data_directory, "/", country))
  }
  if (!file.exists(paste0(data_directory, "/", country, "/data"))) {
    dir.create(paste0(data_directory, "/", country, "/data"))
  }
  if (!file.exists(paste0(data_directory, "/", country, "/plots"))) {
    dir.create(paste0(data_directory, "/", country, "/plots"))
  }

  utils::write.csv(results, paste0(data_directory, "/", country, "/data/final_model_metrics.csv"))

  suppressWarnings(
    ggsave(filename = paste0(data_directory, "/", country, "/plots/complete_model_results.png"), plot = full_plot2, width = 12, height = 8)
  )
  ###
  sample_week_index <- nrow(combined_model_results) - (nrow(combined_model_results) - end_of_training_set) * 0.45

  week_start <- which(combined_model_results$wday[sample_week_index:(sample_week_index + 200)] ==
    "Mon")[1] + sample_week_index
  sample_year <- combined_model_results$year[week_start]

  full_plot_sample_week <- ggplot(combined_model_results[week_start:(week_start + 335), ]) +
    geom_line(aes(date, combined_model_results$hourly_demand[week_start:(week_start + 335)], color = "actual")) +
    geom_line(aes(date, combined_model_results$complete_model[week_start:(week_start + 335)], color = "fitted")) +
    ggthemes::theme_foundation(base_size = 14, base_family = "sans") +
    xlab("\nHour") +
    ylab("[MW]\n") +
    ggtitle(paste("Complete Model Results -", country), subtitle = paste("2 sample weeks in", sample_year, "\n")) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = rel(1.2), hjust = 0.5
      ),
      plot.subtitle = element_text(hjust = 0.5),
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
    theme(legend.title = element_blank())


  full_plot_sample_week2 <- ggplot(combined_model_results[week_start:(week_start + 335), ]) +
    geom_line(aes(date, combined_model_results$hourly_demand[week_start:(week_start + 335)], color = "actual")) +
    geom_line(aes(date, combined_model_results$complete_model[week_start:(week_start + 335)], color = "fitted")) +
    ggthemes::theme_foundation(base_size = 14, base_family = "sans") +
    xlab("\nHour") +
    ylab("[MW]\n") +
    ggtitle(paste("Complete Model Results -", country), subtitle = paste("2 sample weeks in", sample_year, "\n")) +
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
    theme(axis.title = element_text(size = 23)) +
    theme(legend.text = element_text(size = 23)) +
    theme(axis.text = element_text(size = 20)) +
    theme(plot.title = element_text(size = 26)) +
    theme(plot.subtitle = element_text(size = 20, hjust = 0.5))
  suppressWarnings(
    ggsave(filename = paste0(data_directory, "/", country, "/plots/complete_model_sample_weeks.png"), plot = full_plot_sample_week2, width = 12, height = 8)
  )
  suppressWarnings(
    stacked_plots <- patchwork::wrap_plots(full_plot, full_plot_sample_week, ncol = 1)
  )
  if (verbose) {
    suppressWarnings(
      print(stacked_plots)
    )
    suppressWarnings(
      print(full_plot_sample_week)
    )
    suppressWarnings(
      print(full_plot)
    )
  }

  all_plots <- list(
    full_model_plot = full_plot,
    sample_weeks = full_plot_sample_week,
    stacked = stacked_plots
  )
  utils::write.csv(combined_model_results, paste0(data_directory, "/", country, "/data/complete_model.csv"))
  return(list("combined_model_predictions" = combined_model_results, "combined_model_metrics" = results, "combined_model_plots" = all_plots))
}
