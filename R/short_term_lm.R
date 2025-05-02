#' Short-term forecast
#'
#' The short-term load series is forecasted based on the provided hourly load data.
#'
#' @param shortterm_demand_data Dataframe. Containing the short-term load data from \code{\link{decompose_load_data}} and the added holiday dummy resulting from \code{\link{add_holidays_short_term}}.
#' @param test_set_steps Integer. Number of hours used for the test set. The default value of 17520 equals two years (2 * 8760 hours).
#' @param data_directory The path to the directory where the data, plots, and models will be saved. The default is set to a temporary directory.
#' @param verbose A boolean value indicating if you want the generated plots to be shown (set to TRUE if yes).
#' @return A list with the dataframe with the input data and results. A list with the plotted result for the complete timeseries and two sample weeks.
#' And a list with the the best model for each type of month and type of day (84 in total with 12 different months times 7 types of weekdays).
#' The dataset, plots, and the models are saved in the respective folder for the country.
#' \describe{
#'   \item{shortterm_predictions}{A dataframe with the input data and additional columns for the respective hour, test_set_steps, and for the model predictions.}
#'   \item{shortterm_plots}{A list with the full plot and a plot for two sample weeks.}
#'   \item{shortterm_models}{A list with the respective models for each month and type of day.}
#' }
#' @export
#'
#' @examples
#' example_shortterm_predictions <- short_term_lm(example_shortterm_demand_data)
short_term_lm <- function(shortterm_demand_data, test_set_steps = 17520, data_directory = tempdir(), verbose = FALSE) {
  if ("example" %in% colnames(shortterm_demand_data)) {
    if (unique(shortterm_demand_data$example) == TRUE) {
      message("Calculating the best short-term model for each combination of type of month and type of day.")
      message("Calculating 84 models in total (12 months x 7 weekdays).")
      example_data <- oRaklE::example_shortterm_predictions
      training_set <- nrow(example_data) - test_set_steps
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
        return(oRaklE::example_shortterm_predictions)
      } else {
        stop("The example in short_term_lm() failed. Please contact the package maintainer at schwenzer@europa-uni.de")
      }
    }
  }
  short_term_data <- shortterm_demand_data

  if (grepl("Rtmp", data_directory)) {
    message(paste(
      "\nThis function will try to save the results, models and plots to a folder called", unique(short_term_data$country),
      "\nin the current data directory:", data_directory
    ))
    message("\nIt is recommended to save the data in a directory other than a tempdir, so that it is available after you finish the R Session.")

    message("\nPlease choose an option:")
    message("\n1: Keep it as a tempdir")
    message(paste("2: Save data in the current working directory (", getwd(), ")", sep = ""), "(recommended)")
    message("3: Set the directory manually\n")

    choice <- readline(prompt = "Enter the option number (1, 2, or 3): ")


    if (choice == "1") {
      message("\nData will be saved in a temporary directory and cleaned up when R is shut down.")
      # data_directory remains unchanged.
    } else if (choice == "2") {
      data_directory <- getwd()
      message(paste0("\nResults, models, and plots will be saved in the current working directory in ", data_directory, "/", unique(short_term_data$country)))
      message("\nYou can specify the *data_directory* parameter in the following functions as '", data_directory, "'")
    } else if (choice == "3") {
      new_dir <- readline(prompt = "Enter the full path of the directory where you want to save the data: ")
      data_directory <- new_dir
      if (!dir.exists(data_directory)) {
        stop("The specified data_directory does not exist: ", data_directory, "\nPlease run the function again.")
      }
      message("\nResults, models, and plots will be saved in the specified directory: ", data_directory, "/", unique(short_term_data$country))
    } else {
      message("Invalid input. Keeping the temporary directory.\nData will be cleaned up when R is shut down.")
    }
  } else {
    if (!dir.exists(data_directory)) {
      stop("The specified data_directory does not exist: ", data_directory, "\nPlease run the function again.")
    }
    message("\nData, models, and plots will be saved in the specified working directory in ", data_directory, "/", unique(short_term_data$country))
  }

  columns_original_df <- ncol(short_term_data)
  short_term_data[, c((columns_original_df + 1):(columns_original_df + 24))] <- 0
  for (i in (0:23)) {
    colnames(short_term_data)[(columns_original_df + 1 + i)] <- paste0("Hour", i)
    short_term_data[short_term_data$hour == i, (columns_original_df + 1 + i)] <- 1
  }

  country <- unique(short_term_data$country)
  wday <- as.character(unique(short_term_data$wday))

  # define training and test set
  training_set <- nrow(short_term_data) - test_set_steps
  training_data <- short_term_data[1:training_set, ]

  test_data <- short_term_data[(training_set + 1):nrow(short_term_data), ]


  country <- unique(short_term_data$country)
  if (!file.exists(paste0(data_directory, "/", country))) {
    dir.create(paste0(data_directory, "/", country))
  }
  if (!file.exists(paste0(data_directory, "/", country, "/models"))) {
    dir.create(paste0(data_directory, "/", country, "/models"))
  }
  if (!file.exists(paste0(data_directory, "/", country, "/data"))) {
    dir.create(paste0(data_directory, "/", country, "/data"))
  }
  if (!file.exists(paste0(data_directory, "/", country, "/plots"))) {
    dir.create(paste0(data_directory, "/", country, "/plots"))
  }
  if (!file.exists(paste0(data_directory, "/", country, "/models/shortterm_lm"))) {
    dir.create(paste0(data_directory, "/", country, "/models/shortterm_lm"))
  }
  variables <- colnames(training_data)[(columns_original_df):(columns_original_df + 24)]

  f <- stats::as.formula(
    paste("hourly_demand_trend_and_season_corrected",
      paste(variables, collapse = " + "),
      sep = " ~ "
    )
  )

  training_data$short_term_lm_model_predictions <- 0
  test_data$short_term_lm_model_predictions <- 0

  all_plots <- list()
  all_models <- list()
  suppressWarnings(
    for (i in 1:12) {
      for (j in 1:7) {
        if (verbose) {
          cat(paste("Processing model:", 7 * (i - 1) + j, "of", 12 * 7))
        } else {
          if (i == 1 && j == 1) {
            message("Verbose is set to FALSE. Set to TRUE if you want to see the model progress and generated plot automatically. The plot is saved in the output under *shortterm_plot* and in the plots folder in ", data_directory)
          }
        }
        x <- training_data[which(training_data$month == i & training_data$wday == wday[j]), ]
        x_test <- test_data[which(test_data$month == i & test_data$wday == wday[j]), ]


        fit1 <- stats::lm(f, data = x)

        name <- paste0("month", i, wday[j])
        modelname <- paste0(data_directory, "/", country, "/models/shortterm_lm/", name, ".Rdata")
        all_models[[name]] <- fit1
        save(fit1, file = modelname)
        training_data$short_term_lm_model_predictions[which(training_data$month == i & training_data$wday == wday[j])] <-
          fit1$fitted.values
        test_data$short_term_lm_model_predictions[which(test_data$month == i & test_data$wday == wday[j])] <-
          stats::predict(fit1, newdata = x_test)

        if (verbose) {
          if (7 * (i - 1) + j == 12 * 7) {
            cat("\n Done")
          } else {
            cat("\014")
          }
        }
      }
    }
  )


  ## combine the results

  short_term_data <- rbind(training_data, test_data)

  short_term_data$test_set_steps <- test_set_steps

  utils::write.csv(short_term_data, paste0(data_directory, "/", country, "/data/short_term_data.csv"), row.names = FALSE)

  colnames(short_term_data) <- make.unique(names(short_term_data))

  st_plot <- ggplot(short_term_data) +
    geom_line(aes(date, short_term_data$hourly_demand_trend_and_season_corrected, color = "actual")) +
    geom_line(aes(date, short_term_data$short_term_lm_model_predictions, color = "fitted")) +
    geom_vline(xintercept = test_data$date[1], linetype = 2) +
    ggthemes::theme_foundation(base_size = 14, base_family = "sans") +
    xlab("\nHour") +
    ylab("Avg Hourly Demand\n[MW]\n") +
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
    guides(color = guide_legend(override.aes = list(linewidth = 2)))
  if (verbose) {
    suppressWarnings(
      print(st_plot)
    )
  }
  week_start <- which(short_term_data$wday[(nrow(short_term_data) / 2):(nrow(short_term_data) / 2 + 200)] ==
    "Mon")[1] + (nrow(short_term_data) / 2)
  sample_year <- short_term_data$year[week_start]

  st_plot_sample_week <- ggplot(short_term_data[week_start:(week_start + 335), ]) +
    geom_line(aes(date, short_term_data$hourly_demand_trend_and_season_corrected[week_start:(week_start + 335)], color = "actual")) +
    geom_line(aes(date, short_term_data$short_term_lm_model_predictions[week_start:(week_start + 335)], color = "fitted")) +
    ggthemes::theme_foundation(base_size = 14, base_family = "sans") +
    xlab("\nHour") +
    ylab("Avg Hourly Demand\n[MW]\n") +
    ggtitle(paste("Short Term Model Results -", country), subtitle = paste("2 sample weeks in", sample_year, "\n")) +
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
    theme(legend.title = element_blank()) +
    guides(color = guide_legend(override.aes = list(linewidth = 2)))

  if (verbose) {
    suppressWarnings(
      print(st_plot_sample_week)
    )
  }

  st_plot2 <- ggplot(short_term_data) +
    geom_line(aes(date, short_term_data$hourly_demand_trend_and_season_corrected, color = "actual")) +
    geom_line(aes(date, short_term_data$short_term_lm_model_predictions, color = "fitted")) +
    geom_vline(xintercept = test_data$date[1], linetype = 2) +
    ggthemes::theme_foundation(base_size = 14, base_family = "sans") +
    xlab("\nHour") +
    ylab("Avg Hourly Demand\n[MW]\n") +
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
    theme(axis.title = element_text(size = 23)) +
    theme(legend.text = element_text(size = 23)) +
    theme(axis.text = element_text(size = 20)) +
    theme(plot.title = element_text(size = 26)) +
    guides(color = guide_legend(override.aes = list(linewidth = 2)))

  suppressWarnings(
    ggsave(filename = paste0(data_directory, "/", country, "/plots/short_term_results.png"), plot = st_plot2, width = 12, height = 8)
  )

  st_plot_sample_week2 <- ggplot(short_term_data[week_start:(week_start + 335), ]) +
    geom_line(aes(date, short_term_data$hourly_demand_trend_and_season_corrected[week_start:(week_start + 335)], color = "actual")) +
    geom_line(aes(date, short_term_data$short_term_lm_model_predictions[week_start:(week_start + 335)], color = "fitted")) +
    ggthemes::theme_foundation(base_size = 14, base_family = "sans") +
    xlab("\nHour") +
    ylab("Avg Hourly Demand\n[MW]\n") +
    ggtitle(paste("Short Term Model Results -", country), subtitle = paste("2 sample weeks in", sample_year, "\n")) +
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
    theme(plot.subtitle = element_text(size = 20, hjust = 0.5)) +
    guides(color = guide_legend(override.aes = list(linewidth = 2)))

  suppressWarnings(
    ggsave(filename = paste0(data_directory, "/", country, "/plots/short_term_results_sample_weeks.png"), plot = st_plot_sample_week2, width = 12, height = 8)
  )

  all_plots[["short_model_results"]] <- st_plot
  all_plots[["sample_weeks"]] <- st_plot_sample_week

  return(list("shortterm_predictions" = short_term_data, "shortterm_plots" = all_plots, "shortterm_models" = all_models))
}
