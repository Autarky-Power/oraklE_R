#' Long-term forecast
#'
#' This function predicts the long-term load data based on the provided time series and a set of macroeconomic variables.
#'
#' The model corresponds to
#' \deqn{\bar{D}_L(t_L)=\beta_{L,1}+\beta_{L,2}x_1(t_L)+...+ \beta_{L,10}x_{10}(t_L) \epsilon_L(t_L).}
#' where the covariates correspond to the loaded macroeconomic variables from \code{\link{get_macro_economic_data}}. The three best models out of all possible covariate combinations are chosen and saved. The predicted and actual time series of the three best models are plotted and saved as well.
#'
#' @param longterm_and_macro_data Dataframe. Containing the load data and macroeconomic indicators derived from \code{\link{get_macro_economic_data}}.
#' @param test_set_steps Integer. Number of time periods in the test set.
#' @param testquant Integer. Determines how many of the best ranked models are evaluated with cross validation.
#' @param rdm_seed A random seed to keep results consistent
#' @param data_directory The path to the directory where the data, plots, and models will be saved. The default is set to a temporary directory.
#' @param verbose A boolean value indicating if you want the generated plots to be shown (set to TRUE if yes).
#' @return A list with the dataframe with the input data and results. A list with the plotted results of the three best models. And a list with the three best models.
#' The dataset, plots, and the models are saved in the respective folder for the country.
#' \describe{
#'   \item{longterm_predictions}{A dataframe with the input data and additional columns for test_set_steps and for best three models longterm_model_predictions1, longterm_model_predictions2 and longterm_model_predictions3.}
#'   \item{longterm_plots}{A list with the respective plots for each model.}
#'   \item{longterm_models}{A list with the three best models.}
#' }
#' @export
#' @seealso See also function \code{\link{mid_term_lm}} and \code{\link{short_term_lm}} for the other prediction models and \code{\link{get_macro_economic_data}} for the covariate download.
#' @import survival
#' @importFrom survival Surv is.Surv survfit
#' @examples
#' example_longterm_predictions <- long_term_lm(example_longterm_and_macro_data,
#'   test_set_steps = 2, testquant = 500, rdm_seed = 421
#' )
long_term_lm <- function(longterm_and_macro_data, test_set_steps = 2, testquant = 500, rdm_seed = sample(1:10000, 1), data_directory = tempdir(), verbose = FALSE) {
  if ("example" %in% colnames(longterm_and_macro_data)) {
    if (unique(longterm_and_macro_data$example) == TRUE) {
      test_data <- longterm_and_macro_data[, c(1, 2, 3, 6, 11, 13)]

      training_set <- nrow(test_data) - test_set_steps
      training_data <- test_data[1:training_set, ]
      test_set <- test_data[(training_set + 1):nrow(test_data), ]
      variables <- colnames(test_data)[4:(ncol(test_data))]

      f <- stats::as.formula(
        paste("avg_hourly_demand",
          paste(variables, collapse = " + "),
          sep = " ~ "
        )
      )

      globalmodel <- stats::lm(f, data = training_data, na.action = "na.fail")
      suppressMessages(combinations <- MuMIn::dredge(globalmodel))
      for (i in seq_len(nrow(combinations))) {
        if (all(is.na(combinations[i, 2:(ncol(combinations) - 5)]))) {
          combinations <- combinations[-i, ]
        }
      }
      row.names(combinations) <- NULL

      if (nrow(combinations) < testquant) {
        testquant <- nrow(combinations)
      }

      rdm <- rdm_seed
      ctrl <- caret::trainControl(method = "repeatedcv", number = 5, repeats = 5, allowParallel = FALSE)
      cross_val <- function(i) {
        tryCatch(
          {
            predictor_names <- combinations[i, 2:(ncol(combinations) - 5)]
            model_variables <- colnames(predictor_names)[stats::complete.cases(t(predictor_names))]
            lm_formula <- stats::as.formula(paste("avg_hourly_demand", paste(model_variables, collapse = " + "),
              sep = " ~ "
            ))

            set.seed(rdm)
            Kfold <- caret::train(lm_formula,
              data = training_data,
              trControl = ctrl, method = "lm"
            )
          },
          error = function(e) {
            cat("ERROR :", conditionMessage(e), "\n")
          }
        )
        return(c(Kfold$results$RMSE, Kfold$results$Rsquared, Kfold$results$MAE, i))
      }

      results_list <- lapply(1:testquant, cross_val)
      results <- as.data.frame(do.call(rbind, results_list))
      colnames(results) <- c("RMSE_k_fold", "Rsquare_k_fold", "MAE_k_fold", "index")
      candidates <- results
      dist <- data.frame(matrix(nrow = nrow(candidates), ncol = 3))
      colnames(dist) <- c("distance", "model_no", "max_single_distance")

      for (i in seq_len(nrow(candidates))) {
        tryCatch(
          {
            ind <- candidates$index[i]
            x <- combinations[ind, 2:(ncol(combinations) - 5)]
            variables <- colnames(x)[stats::complete.cases(t(x))]
            f <- stats::as.formula(paste("avg_hourly_demand", paste(variables, collapse = " + "),
              sep = " ~ "
            ))

            model <- stats::lm(f, data = training_data)

            LT <- stats::predict(model, test_data)

            dist[i, 1] <- sum(abs(test_set$avg_hourly_demand - LT[(training_set + 1):nrow(test_data)]))
            dist[i, 2] <- ind
            dist[i, 3] <- max(abs(test_data$avg_hourly_demand - LT))
          },
          error = function(e) {
            cat("ERROR :", conditionMessage(e), "\n")
          }
        )
      }

      dist_clean <- stats::na.omit(dist)
      ordered_dist <- dist_clean[order(dist_clean$distance), ]
      best_model <- ordered_dist$model_no[ordered_dist$max_single_distance == sort(ordered_dist$max_single_distance[1:5], FALSE)[1]]

      x <- combinations[best_model, ]
      x <- x[, 2:(ncol(combinations) - 5)]
      variables <- colnames(x)[stats::complete.cases(t(x))]
      expected_vars <- c("GNI", "industrial_value_added", "rural_population")


      if (identical(variables, expected_vars)) {
        return(oRaklE::example_longterm_predictions)
      } else {
        stop("The example in long_term_lm() failed. Please contact the package maintainer at schwenzer@europa-uni.de")
      }
    }
  }
  longterm_all_data <- longterm_and_macro_data
  if (!"avg_hourly_demand" %in% colnames(longterm_all_data)) {
    stop("No column named \"avg_hourly_demand\"")
  }
  if (FALSE %in% lapply(longterm_all_data[, 4:ncol(longterm_all_data)], is.numeric)) {
    stop("Not all macroeconomic variables are numeric")
  }

  if (grepl("Rtmp", data_directory)) {
    message(paste(
      "\nThis function will try to save the results, models and plots to a folder called", unique(longterm_all_data$country),
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
      message(paste0("\nResults, models, and plots will be saved in the current working directory in ", data_directory, "/", unique(longterm_all_data$country)))
      message("\nYou can specify the *data_directory* parameter in the following functions as '", data_directory, "'")
    } else if (choice == "3") {
      new_dir <- readline(prompt = "Enter the full path of the directory where you want to save the data: ")
      data_directory <- new_dir
      if (!dir.exists(data_directory)) {
        stop("The specified data_directory does not exist: ", data_directory, "\nPlease run the function again.")
      }
      message("\nResults, models, and plots will be saved in the specified directory: ", data_directory, "/", unique(longterm_all_data$country))
    } else {
      message("Invalid input. Keeping the temporary directory.\nData will be cleaned up when R is shut down.")
    }
  } else {
    if (!dir.exists(data_directory)) {
      stop("The specified data_directory does not exist: ", data_directory, "\nPlease run the function again.")
    }
    message("\nData, models, and plots will be saved in the specified working directory in ", data_directory, "/", unique(longterm_all_data$country))
  }


  longterm_all_data <- longterm_all_data[mean(longterm_all_data$avg_hourly_demand) / longterm_all_data$avg_hourly_demand < 150, ]

  training_set <- nrow(longterm_all_data) - test_set_steps
  training_data <- longterm_all_data[1:training_set, ]
  test_data <- longterm_all_data[(training_set + 1):nrow(longterm_all_data), ]
  variables <- colnames(longterm_all_data)[4:(ncol(longterm_all_data))]

  f <- stats::as.formula(
    paste("avg_hourly_demand",
      paste(variables, collapse = " + "),
      sep = " ~ "
    )
  )


  globalmodel <- stats::lm(f, data = training_data, na.action = "na.fail")

  message("\n[1/4] Getting all possible combinations.")

  suppressMessages(combinations <- MuMIn::dredge(globalmodel))

  combinations <- combinations[combinations$population >= 0 | is.na(combinations$population), ]
  for (i in seq_len(nrow(combinations))) {
    if (all(is.na(combinations[i, 2:(ncol(combinations) - 5)]))) {
      combinations <- combinations[-i, ]
    }
  }
  row.names(combinations) <- NULL

  if (nrow(combinations) < testquant) {
    testquant <- nrow(combinations)
  }

  rdm <- rdm_seed


  ctrl <- caret::trainControl(method = "repeatedcv", number = 5, repeats = 5)

  message("[2/4] Cross-validating the best ", testquant, " models.")
  message("Using random seed: ", rdm_seed)
  cross_val <- function(i) {
    tryCatch(
      {
        predictor_names <- combinations[i, 2:(ncol(combinations) - 5)]
        model_variables <- colnames(predictor_names)[stats::complete.cases(t(predictor_names))]
        lm_formula <- stats::as.formula(paste("avg_hourly_demand", paste(model_variables, collapse = " + "),
          sep = " ~ "
        ))

        set.seed(rdm)
        Kfold <- caret::train(lm_formula,
          data = training_data,
          trControl = ctrl, method = "lm"
        )
      },
      error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      }
    )
    return(c(Kfold$results$RMSE, Kfold$results$Rsquared, Kfold$results$MAE, i))
  }
  # Parallel Processing
  no_cores <- parallel::detectCores(logical = TRUE)

  used_cores <- floor(no_cores * 0.75)
  if (used_cores > 10) {
    used_cores <- 10
  }


  cl <- parallel::makeCluster(used_cores)
  doParallel::registerDoParallel(cl)
  parallel::clusterExport(cl, list("combinations", "ctrl", "training_data", "rdm"), envir = environment())

  results_list <- parallel::parLapply(cl, 1:testquant, cross_val)

  doParallel::stopImplicitCluster()
  parallel::stopCluster(cl)


  results <- as.data.frame(do.call(rbind, results_list))
  colnames(results) <- c("RMSE_k_fold", "Rsquare_k_fold", "MAE_k_fold", "index")


  best_index_RMSE <- results$index[results["RMSE_k_fold"] == min(results["RMSE_k_fold"], na.rm = T)]
  best_value_RMSE <- results[best_index_RMSE, "RMSE_k_fold"]
  limit_RMSE <- best_value_RMSE * 1.5


  best_index_MAE <- results$index[results["MAE_k_fold"] == min(results["MAE_k_fold"], na.rm = T)]
  best_value_MAE <- results[best_index_MAE, "MAE_k_fold"]
  limit_MAE <- best_value_MAE * 1.5


  best_index_Rsquare <- results$index[results["Rsquare_k_fold"] == max(results["Rsquare_k_fold"], na.rm = T)]

  best_value_Rsquare <- results[best_index_Rsquare, "Rsquare_k_fold"]
  limit_Rsquare <- best_value_Rsquare / 1.3


  mask_RMSE <- results["RMSE_k_fold"] <= limit_RMSE
  mask_MAE <- results["MAE_k_fold"] <= limit_MAE
  mask_Rsquare <- results["Rsquare_k_fold"] >= limit_Rsquare

  candidates <- results[mask_Rsquare & mask_RMSE & mask_MAE, ]
  dist <- data.frame(matrix(nrow = nrow(candidates), ncol = 3))
  colnames(dist) <- c("distance", "model_no", "max_single_distance")

  for (i in seq_len(nrow(candidates))) {
    tryCatch(
      {
        ind <- candidates$index[i]
        x <- combinations[ind, 2:(ncol(combinations) - 5)]
        variables <- colnames(x)[stats::complete.cases(t(x))]


        f <- stats::as.formula(paste("avg_hourly_demand", paste(variables, collapse = " + "),
          sep = " ~ "
        ))

        model <- stats::lm(f, data = training_data)

        LT <- stats::predict(model, longterm_all_data)

        dist[i, 1] <- sum(abs(test_data$avg_hourly_demand - LT[(training_set + 1):nrow(longterm_all_data)]))
        dist[i, 2] <- ind
        dist[i, 3] <- max(abs(longterm_all_data$avg_hourly_demand - LT))
      },
      error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      }
    )
  }

  dist_clean <- stats::na.omit(dist)

  ordered_dist <- dist_clean[order(dist_clean$distance), ]


  if (testquant > 500) {
    best_model1 <- ordered_dist$model_no[ordered_dist$max_single_distance == sort(ordered_dist$max_single_distance[1:50], FALSE)[1]]
    best_model2 <- ordered_dist$model_no[ordered_dist$max_single_distance == sort(ordered_dist$max_single_distance[1:50], FALSE)[2]]
    best_model3 <- ordered_dist$model_no[ordered_dist$max_single_distance == sort(ordered_dist$max_single_distance[1:50], FALSE)[3]]
    best_models <- c(best_model1, best_model2, best_model3)
  } else {
    best_model1 <- ordered_dist$model_no[ordered_dist$max_single_distance == sort(ordered_dist$max_single_distance[1:5], FALSE)[1]]
    best_model2 <- ordered_dist$model_no[ordered_dist$max_single_distance == sort(ordered_dist$max_single_distance[1:5], FALSE)[2]]
    best_model3 <- ordered_dist$model_no[ordered_dist$max_single_distance == sort(ordered_dist$max_single_distance[1:5], FALSE)[3]]
    best_models <- c(best_model1, best_model2, best_model3)
  }

  longterm_all_data$longterm_model_predictions1 <- 0
  longterm_all_data$longterm_model_predictions2 <- 0
  longterm_all_data$longterm_model_predictions3 <- 0

  if (!verbose) {
    message("\n[3/4] Verbose is set to FALSE. Set to TRUE if you want to see the generated plots automatically. The plots are saved in the output under *longterm_plots* and in the plots folder in ", data_directory)
  }
  if (verbose) {
    message("\n[3/4] Generating plots for the 3 best models.")
  }
  all_plots <- list()
  all_models <- list()
  i <- 1
  for (model in best_models) {
    x <- combinations[model, ]
    x <- x[, 2:(ncol(combinations) - 5)]
    variables <- colnames(x)[stats::complete.cases(t(x))]
    f <- stats::as.formula(paste("avg_hourly_demand", paste(variables, collapse = " + "),
      sep = " ~ "
    ))
    print_vars <- paste(variables, collapse = ", ")
    message("\nBest model ", i, " depends on: ", print_vars)


    best_lm_model <- stats::lm(f, data = training_data)
    results <- stats::predict(best_lm_model, longterm_all_data)
    longterm_all_data[, (ncol(longterm_all_data) - 3 + i)] <- results
    country <- unique(longterm_all_data$country)

    lt_plot <- ggplot(longterm_all_data) +
      geom_line(aes(longterm_all_data$year, longterm_all_data$avg_hourly_demand, color = "actual")) +
      geom_line(aes(longterm_all_data$year, results, color = "fitted")) +
      xlab("\nYear") +
      ylab("Avg Hourly Demand\n [MW]\n") +
      geom_vline(xintercept = longterm_all_data$year[training_set], linetype = 2) +
      ggthemes::theme_foundation(base_size = 14, base_family = "sans") +
      xlab("\nYear") +
      ylab("Avg Hourly Demand p. Year\n [MW]\n") +
      ggtitle(paste("Long Term Model Results -", country), subtitle = paste("Model", i, "\n")) +
      theme(
        plot.title = element_text(
          face = "bold",
          size = rel(1.2), hjust = 0.5
        ),
        plot.subtitle = element_text(size = rel(1), hjust = 0.5),
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

    lt_plot2 <- ggplot(longterm_all_data) +
      geom_line(aes(longterm_all_data$year, longterm_all_data$avg_hourly_demand, color = "actual")) +
      geom_line(aes(longterm_all_data$year, results, color = "fitted")) +
      xlab("\nYear") +
      ylab("Avg Hourly Demand\n [MW]\n") +
      geom_vline(xintercept = longterm_all_data$year[training_set], linetype = 2) +
      ggthemes::theme_foundation(base_size = 14, base_family = "sans") +
      xlab("\nYear") +
      ylab("Avg Hourly Demand p. Year\n [MW]\n") +
      ggtitle(paste("Long Term Model Results -", country), subtitle = paste("Model", i, "\n")) +
      theme(
        plot.title = element_text(
          face = "bold",
          size = rel(1.2), hjust = 0.5
        ),
        plot.subtitle = element_text(size = rel(1), hjust = 0.5),
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

    if (verbose) {
      suppressWarnings(
        print(lt_plot)
      )
    }

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
    if (!file.exists(paste0(data_directory, "/", country, "/models/longterm"))) {
      dir.create(paste0(data_directory, "/", country, "/models/longterm"))
    }
    save(best_lm_model, file = paste0(data_directory, "/", country, "/models/longterm/best_lm_model", i, ".Rdata"))
    suppressWarnings(
      ggsave(filename = paste0(data_directory, "/", country, "/plots/Long_term_results", i, ".png"), plot = lt_plot2, width = 12, height = 8)
    )

    plot_name <- paste0("longterm_model", i)
    all_plots[[plot_name]] <- lt_plot
    all_models[[plot_name]] <- best_lm_model
    i <- i + 1

    if (i == 4) {
      message("\n[4/4] Writing data to disc.")
    }
  }
  longterm_all_data$test_set_steps <- test_set_steps
  utils::write.csv(longterm_all_data, paste0(data_directory, "/", country, "/data/long_term_all_data.csv"), row.names = F)

  return(list("longterm_predictions" = longterm_all_data, "longterm_plots" = all_plots, "longterm_models" = all_models))
}
