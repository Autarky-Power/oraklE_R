#' Mid-term forecast
#'
#' The mid-term load series is forecasted based on the provided load time series and weather data. The prediction is either based on the (lagged) temperature data in combination with transformed variables for heating and cooling days or on a spline regression applied on the temperature data to account for non-linear effects.
#'
#' @param demand_and_weather_data Dataframe. Containing the mid-term load data, the holidays and weather data obtained from \code{\link{get_weather_data}}.
#' @param test_set_steps Integer. Number of time periods in the test set.
#' @param Tref Numeric. Reference temperature as basis for the calculation of cooling and heating days.
#' @param method String. Indicates which model selection process is used. If method="temperature transformation", the temperature values are transformed to heating and cooling
#' degree days to capture the non-linear relationship of temperature and electricity demand. If the method is set to "spline" a spline regression is instead used without
#' the transformation of the temperature data.
#' @param data_directory The path to the directory where the data, plots, and models will be saved. The default is set to a temporary directory.
#' @param verbose A boolean value indicating if you want the generated plots to be shown (set to TRUE if yes).
#' @return A list with the dataframe with the input data and results. The plot with the midterm seasonality forecast. And the midterm model.
#' The dataset, the plot, and the model are saved in the respective folder for the country.
#' \describe{
#'   \item{midterm_predictions}{A dataframe with the input and prediction data for the mid-term seasonality.}
#'   \item{midterm_plot}{A plot with the prediction results.}
#'   \item{midterm_model}{The mid-term seasonality model.}
#' }
#' @export
#'
#' @examples
#'
#' example_midterm_predictions <- mid_term_lm(example_midterm_demand_and_weather_data$demand,
#'   Tref = 18, test_set_steps = 730, method = "temperature transformation"
#' )
#'
mid_term_lm <- function(demand_and_weather_data, Tref = 18, test_set_steps = 730, method = "temperature transformation",
                        data_directory = tempdir(), verbose = FALSE) {
  if ("example" %in% colnames(demand_and_weather_data)) {
    if (unique(demand_and_weather_data$example) == TRUE) {
      message("Transforming temperature values to heating- and cooling-degree day temperatures")
      midterm_all_data <- demand_and_weather_data
      month_list <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Nov", "Dec")

      for (i in 1:length(month_list)) {
        midterm_all_data[month_list[i]] <- 0
        midterm_all_data[midterm_all_data$month == i, month_list[i]] <- 1
      }

      midterm_all_data$wday <- lubridate::wday(midterm_all_data$date, label = T)
      weekday_list <- as.character(unique(midterm_all_data$wday))

      for (i in 1:length(weekday_list)) {
        midterm_all_data[weekday_list[i]] <- 0
        midterm_all_data[midterm_all_data$wday == weekday_list[i], weekday_list[i]] <- 1
      }

      midterm_all_data$HD <- 0
      midterm_all_data$CD <- 0

      for (i in 1:nrow(midterm_all_data)) {
        if (midterm_all_data$weighted_temperature[i] < Tref) {
          midterm_all_data$HD[i] <- Tref - midterm_all_data$weighted_temperature[i]
        } else {
          midterm_all_data$CD[i] <- midterm_all_data$weighted_temperature[i] - Tref
        }
      }

      midterm_all_data$HD2 <- midterm_all_data$HD^2
      midterm_all_data$HD3 <- midterm_all_data$HD^3
      midterm_all_data$CD2 <- midterm_all_data$CD^2
      midterm_all_data$CD3 <- midterm_all_data$CD^3
      midterm_all_data$weighted_temperature2 <- midterm_all_data$weighted_temperature^2
      midterm_all_data$weighted_temperature3 <- midterm_all_data$weighted_temperature^3

      midterm_all_data$HDlag1 <- dplyr::lag(midterm_all_data$HD, n = 1)
      midterm_all_data$HDlag1[1] <- midterm_all_data$HD[1]
      midterm_all_data$HDlag2 <- dplyr::lag(midterm_all_data$HD, n = 2)
      midterm_all_data$HDlag2[1:2] <- midterm_all_data$HDlag1[1:2]

      midterm_all_data$CDlag1 <- dplyr::lag(midterm_all_data$CD, n = 1)
      midterm_all_data$CDlag1[1] <- midterm_all_data$CD[1]
      midterm_all_data$CDlag2 <- dplyr::lag(midterm_all_data$CD, n = 2)
      midterm_all_data$CDlag2[1:2] <- midterm_all_data$CDlag1[1:2]

      midterm_all_data$weighted_temperaturelag1 <- dplyr::lag(midterm_all_data$weighted_temperature, n = 1)
      midterm_all_data$weighted_temperaturelag1[1] <- midterm_all_data$weighted_temperature[1]
      midterm_all_data$weighted_temperaturelag2 <- dplyr::lag(midterm_all_data$weighted_temperature, n = 2)
      midterm_all_data$weighted_temperaturelag2[1:2] <- midterm_all_data$weighted_temperaturelag1[1:2]

      midterm_all_data$end_of_year <- 0
      midterm_all_data$end_of_year[midterm_all_data$month == 12 & midterm_all_data$day > 22] <- 1

      message("Calculating the best mid-term model.")

      training_set <- nrow(midterm_all_data) - test_set_steps
      training_data <- midterm_all_data[1:training_set, ]
      test_data <- midterm_all_data[(training_set + 1):nrow(midterm_all_data), ]


      variables <- colnames(midterm_all_data)[c(9:(ncol(midterm_all_data)))]


      f <- stats::as.formula(paste("seasonal_avg_hourly_demand", paste(variables, collapse = " + "),
        sep = " ~ "
      ))

      globalmodel <- stats::lm(f, data = training_data, na.action = "na.omit")


      y <- training_data$seasonal_avg_hourly_demand
      y_all <- midterm_all_data$seasonal_avg_hourly_demand
      y_test <- test_data$seasonal_avg_hourly_demand
      x <- data.matrix(training_data[, 9:ncol(midterm_all_data)])
      x_all <- data.matrix(midterm_all_data[, 9:ncol(midterm_all_data)])
      x_test <- data.matrix(test_data[, 9:ncol(training_data)])

      cv_model <- glmnet::cv.glmnet(x, y, alpha = 1)

      best_lambda <- cv_model$lambda.min
      best_model <- glmnet::glmnet(x, y, alpha = 1, lambda = best_lambda)

      testlasso <- stats::predict(best_model, s = best_lambda, newx = x_test)
      suppressWarnings(
        testlm <- stats::predict(globalmodel, newdata = test_data)
      )

      suppressWarnings(
        if (MLmetrics::RMSE(testlasso, y_test) < MLmetrics::RMSE(testlm, y_test)) {
          return(oRaklE::example_midterm_predictions)
        } else {
          stop("The example in mid_term_lm() failed. Please contact the package maintainer at schwenzer@europa-uni.de")
        }
      )
    }
  }
  midterm_all_data <- demand_and_weather_data
  if (grepl("Rtmp", data_directory)) {
    message(paste(
      "\nThis function will try to save the results, models and plots to a folder called", unique(midterm_all_data$country),
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
      message(paste0("\nResults, models, and plots will be saved in the current working directory in ", data_directory, "/", unique(midterm_all_data$country)))
      message("\nYou can specify the *data_directory* parameter in the following functions as '", data_directory, "'")
    } else if (choice == "3") {
      new_dir <- readline(prompt = "Enter the full path of the directory where you want to save the data: ")
      data_directory <- new_dir
      if (!dir.exists(data_directory)) {
        stop("The specified data_directory does not exist: ", data_directory, "\nPlease run the function again.")
      }
      message("\nResults, models, and plots will be saved in the specified directory: ", data_directory, "/", unique(midterm_all_data$country))
    } else {
      message("Invalid input. Keeping the temporary directory.\nData will be cleaned up when R is shut down.")
    }
  } else {
    if (!dir.exists(data_directory)) {
      stop("The specified data_directory does not exist: ", data_directory, "\nPlease run the function again.")
    }
    message("\nData, models, and plots will be saved in the specified working directory in ", data_directory, "/", unique(midterm_all_data$country))
  }



  month_list <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Nov", "Dec")

  for (i in 1:length(month_list)) {
    midterm_all_data[month_list[i]] <- 0
    midterm_all_data[midterm_all_data$month == i, month_list[i]] <- 1
  }

  midterm_all_data$wday <- lubridate::wday(midterm_all_data$date, label = T)
  weekday_list <- as.character(unique(midterm_all_data$wday))

  for (i in 1:length(weekday_list)) {
    midterm_all_data[weekday_list[i]] <- 0
    midterm_all_data[midterm_all_data$wday == weekday_list[i], weekday_list[i]] <- 1
  }

  if (method == "temperature transformation") {
    midterm_all_data$HD <- 0
    midterm_all_data$CD <- 0

    for (i in 1:nrow(midterm_all_data)) {
      if (midterm_all_data$weighted_temperature[i] < Tref) {
        midterm_all_data$HD[i] <- Tref - midterm_all_data$weighted_temperature[i]
      } else {
        midterm_all_data$CD[i] <- midterm_all_data$weighted_temperature[i] - Tref
      }
    }

    midterm_all_data$HD2 <- midterm_all_data$HD^2
    midterm_all_data$HD3 <- midterm_all_data$HD^3
    midterm_all_data$CD2 <- midterm_all_data$CD^2
    midterm_all_data$CD3 <- midterm_all_data$CD^3
    midterm_all_data$weighted_temperature2 <- midterm_all_data$weighted_temperature^2
    midterm_all_data$weighted_temperature3 <- midterm_all_data$weighted_temperature^3

    midterm_all_data$HDlag1 <- dplyr::lag(midterm_all_data$HD, n = 1)
    midterm_all_data$HDlag1[1] <- midterm_all_data$HD[1]
    midterm_all_data$HDlag2 <- dplyr::lag(midterm_all_data$HD, n = 2)
    midterm_all_data$HDlag2[1:2] <- midterm_all_data$HDlag1[1:2]

    midterm_all_data$CDlag1 <- dplyr::lag(midterm_all_data$CD, n = 1)
    midterm_all_data$CDlag1[1] <- midterm_all_data$CD[1]
    midterm_all_data$CDlag2 <- dplyr::lag(midterm_all_data$CD, n = 2)
    midterm_all_data$CDlag2[1:2] <- midterm_all_data$CDlag1[1:2]

    midterm_all_data$weighted_temperaturelag1 <- dplyr::lag(midterm_all_data$weighted_temperature, n = 1)
    midterm_all_data$weighted_temperaturelag1[1] <- midterm_all_data$weighted_temperature[1]
    midterm_all_data$weighted_temperaturelag2 <- dplyr::lag(midterm_all_data$weighted_temperature, n = 2)
    midterm_all_data$weighted_temperaturelag2[1:2] <- midterm_all_data$weighted_temperaturelag1[1:2]

    midterm_all_data$end_of_year <- 0
    midterm_all_data$end_of_year[midterm_all_data$month == 12 & midterm_all_data$day > 22] <- 1

    training_set <- nrow(midterm_all_data) - test_set_steps
    training_data <- midterm_all_data[1:training_set, ]
    test_data <- midterm_all_data[(training_set + 1):nrow(midterm_all_data), ]


    variables <- colnames(midterm_all_data)[c(9:(ncol(midterm_all_data)))]


    f <- stats::as.formula(paste("seasonal_avg_hourly_demand", paste(variables, collapse = " + "),
      sep = " ~ "
    ))

    globalmodel <- stats::lm(f, data = training_data, na.action = "na.omit")


    y <- training_data$seasonal_avg_hourly_demand
    y_all <- midterm_all_data$seasonal_avg_hourly_demand
    y_test <- test_data$seasonal_avg_hourly_demand
    x <- data.matrix(training_data[, 9:ncol(midterm_all_data)])
    x_all <- data.matrix(midterm_all_data[, 9:ncol(midterm_all_data)])
    x_test <- data.matrix(test_data[, 9:ncol(training_data)])

    cv_model <- glmnet::cv.glmnet(x, y, alpha = 1)

    best_lambda <- cv_model$lambda.min
    best_model <- glmnet::glmnet(x, y, alpha = 1, lambda = best_lambda)



    testlasso <- stats::predict(best_model, s = best_lambda, newx = x_test)
    suppressWarnings(
      testlm <- stats::predict(globalmodel, newdata = test_data)
    )

    country <- unique(midterm_all_data$country)
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
    if (!file.exists(paste0(data_directory, "/", country, "/models/midterm"))) {
      dir.create(paste0(data_directory, "/", country, "/models/midterm"))
    }
    suppressWarnings(
      if (MLmetrics::RMSE(testlasso, y_test) < MLmetrics::RMSE(testlm, y_test)) {
        midterm_all_data$midterm_model_fit <- stats::predict(best_model, s = best_lambda, newx = x_all)
        save(best_model, file = paste0(data_directory, "/", country, "/models/midterm/best_model.Rdata"))
        midterm_model <- best_model
      } else {
        midterm_all_data$midterm_model_fit <- stats::predict(globalmodel, newdata = midterm_all_data)
        save(globalmodel, file = paste0(data_directory, "/", country, "/models/midterm/best_model.Rdata"))
        midterm_model <- globalmodel
      }
    )
    midterm_all_data$test_set_steps <- test_set_steps
    years <- unique(midterm_all_data$year)
    index <- 1:length(years)
    for (i in 1:length(years)) {
      index[i] <- min(as.numeric(rownames(midterm_all_data[midterm_all_data$year == years[i], ])))
    }

    lowest_real_values <- min(midterm_all_data$seasonal_avg_hourly_demand)
    midterm_all_data$midterm_model_fit[midterm_all_data$midterm_model_fit < lowest_real_values] <- lowest_real_values
  } else if (method == "spline") {
    midterm_all_data$weighted_temperaturelag1 <- dplyr::lag(midterm_all_data$weighted_temperature, n = 1)
    midterm_all_data$weighted_temperaturelag1[1] <- midterm_all_data$weighted_temperature[1]
    midterm_all_data$weighted_temperaturelag2 <- dplyr::lag(midterm_all_data$weighted_temperature, n = 2)
    midterm_all_data$weighted_temperaturelag2[1:2] <- midterm_all_data$weighted_temperaturelag1[1:2]


    midterm_all_data$end_of_year <- 0
    midterm_all_data$end_of_year[midterm_all_data$month == 12 & midterm_all_data$day > 22] <- 1

    training_set <- nrow(midterm_all_data) - test_set_steps
    training_data <- midterm_all_data[1:training_set, ]
    test_data <- midterm_all_data[(training_set + 1):nrow(midterm_all_data), ]

    variables <- colnames(midterm_all_data)[c(9:(ncol(midterm_all_data)))]

    spline_vars <- c("weighted_temperature", "weighted_temperaturelag1", "weighted_temperaturelag2")

    formula_str <- paste(
      "seasonal_avg_hourly_demand ~",
      paste(lapply(variables, function(v) {
        if (v %in% spline_vars) {
          return(paste("s(", v, ", bs='cs')", sep = ""))
        } else {
          return(v)
        }
      }), collapse = " + ")
    )


    f <- stats::as.formula(formula_str)


    globalmodel <- mgcv::gam(f, data = training_data, method = "REML")


    y <- training_data$seasonal_avg_hourly_demand
    y_all <- midterm_all_data$seasonal_avg_hourly_demand
    y_test <- test_data$seasonal_avg_hourly_demand
    x <- data.matrix(training_data[, 9:ncol(midterm_all_data)])
    x_all <- data.matrix(midterm_all_data[, 9:ncol(midterm_all_data)])
    x_test <- data.matrix(test_data[, 9:ncol(training_data)])

    cv_model <- glmnet::cv.glmnet(x, y, alpha = 1)

    best_lambda <- cv_model$lambda.min
    best_model <- glmnet::glmnet(x, y, alpha = 1, lambda = best_lambda)



    testlasso <- stats::predict(best_model, s = best_lambda, newx = x_test)
    suppressWarnings(
      testlm <- stats::predict(globalmodel, newdata = test_data)
    )
    country <- unique(midterm_all_data$country)
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
    if (!file.exists(paste0(data_directory, "/", country, "/models/midterm"))) {
      dir.create(paste0(data_directory, "/", country, "/models/midterm"))
    }
    suppressWarnings(
      if (MLmetrics::RMSE(testlasso, y_test) < MLmetrics::RMSE(testlm, y_test)) {
        midterm_all_data$midterm_model_fit <- stats::predict(best_model, s = best_lambda, newx = x_all)
        save(best_model, file = paste0(data_directory, "/", country, "/models/midterm/best_model.Rdata"))
        midterm_model <- best_model
      } else {
        midterm_all_data$midterm_model_fit <- stats::predict(globalmodel, newdata = midterm_all_data)
        save(globalmodel, file = paste0(data_directory, "/", country, "/models/midterm/best_model.Rdata"))
        midterm_model <- globalmodel
      }
    )
    midterm_all_data$test_set_steps <- test_set_steps
    years <- unique(midterm_all_data$year)
    index <- 1:length(years)
    for (i in 1:length(years)) {
      index[i] <- min(as.numeric(rownames(midterm_all_data[midterm_all_data$year == years[i], ])))
    }

    lowest_real_values <- min(midterm_all_data$seasonal_avg_hourly_demand)
    midterm_all_data$midterm_model_fit[midterm_all_data$midterm_model_fit < lowest_real_values] <- lowest_real_values
  }


  mt_plot <- ggplot(midterm_all_data) +
    geom_line(aes(1:nrow(midterm_all_data), midterm_all_data$seasonal_avg_hourly_demand, color = "actual")) +
    geom_line(aes(1:nrow(midterm_all_data), midterm_all_data$midterm_model_fit, color = "fitted")) +
    geom_vline(xintercept = training_set, linetype = 2) +
    ggthemes::theme_foundation(base_size = 14, base_family = "sans") +
    xlab("\nDay") +
    ylab("Change in avg. Hourly Demand\n p. Day [MW]\n") +
    ggtitle(paste("Mid Term Model Results -", country, "\n")) +
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
    scale_x_continuous(breaks = index, labels = years) +
    guides(color = guide_legend(override.aes = list(linewidth = 2)))


  mt_plot2 <- ggplot(midterm_all_data) +
    geom_line(aes(1:nrow(midterm_all_data), midterm_all_data$seasonal_avg_hourly_demand, color = "actual")) +
    geom_line(aes(1:nrow(midterm_all_data), midterm_all_data$midterm_model_fit, color = "fitted")) +
    geom_vline(xintercept = training_set, linetype = 2) +
    ggthemes::theme_foundation(base_size = 14, base_family = "sans") +
    xlab("\nDay") +
    ylab("Change in avg. Hourly Demand\n p. Day [MW]\n") +
    ggtitle(paste("Mid Term Model Results -", country, "\n")) +
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
    theme(axis.title = element_text(size = 18)) +
    theme(legend.text = element_text(size = 18)) +
    theme(axis.text = element_text(size = 16)) +
    theme(plot.title = element_text(size = 22)) +
    scale_x_continuous(breaks = index, labels = years) +
    guides(color = guide_legend(override.aes = list(linewidth = 2)))


  suppressWarnings(
    ggsave(filename = paste0(data_directory, "/", country, "/plots/Mid_term_results.png"), plot = mt_plot2, width = 12, height = 8)
  )
  if (verbose) {
    suppressWarnings(
      print(mt_plot)
    )
  }

  if (!verbose) {
    message("Verbose is set to FALSE. Set to TRUE if you want to see the generated plot automatically. The plot is saved in the output under *midterm_plot* and in the plots folder in ", data_directory)
  }

  return(list("midterm_predictions" = midterm_all_data, "midterm_plot" = mt_plot, "midterm_model" = midterm_model))
}
