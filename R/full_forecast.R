#' Title
#'
#' @param start_year Specifies the starting year for which predictions and models will be generated.
#' @param end_year_data Specifies the final year for which data from the Transparency Platform of the European Network of
#' Transmission System Operators for Electricity (ENTSO-E, https://transparency.entsoe.eu/) is retrieved and for which models will be generated.
#' @param end_year Specifies the final year for which future predictions will be generated.
#' @param country String. Specifies the country.
#' @param test_set_steps Integer. Specifies how many years are used for generating the test/validation set for the model selection.
#' @param future String. Option to enable or disable the future forecasts. If set to "yes" forecasts will be made until the specified end_year. If set to
#' anything else, forecasts will only be generated until the specified end_year_data value.
#' @param data_directory The path to the directory where the results and plots will be saved. The default is set to a temporary directory.
#' It is highly recommended to set it to something that is not a temporary directory if you want to use this function.
#' @param verbose A boolean value indicating if you want the generated plots and detailed status updates to be shown (set to TRUE if yes).
#' #' @seealso See also functions \code{\link{long_term_lm}}, \code{\link{mid_term_lm}}, and \code{\link{short_term_lm}} for the prediction models
#' and \code{\link{long_term_future}}, \code{\link{mid_term_future}}, and \code{\link{short_term_future}} for the future prediction models.
#'
#' @return Returns a list with the combined model results and plots. And a list with the results, models and plots for the long-term trend, mid-term seasonality, and
#' short-term seasonality respectively.
#' The combined model predictions and plots are saved in the respective folder for the country.
#' \describe{
#'   \item{full_model_predictions}{The combined model results and plots.}
#'   \item{longterm}{The long-term trend models, results, and plots.}
#'   \item{midterm}{The mid-term seasonality models, results, and plots.}
#'   \item{shortterm}{The short-term seasonality models, results, and plots.}
#' }
#' @export
#'
#' @examples
#' library(ggplot2)
#' ## Without future predictions
#' \donttest{
#' forecast_data <- full_forecast(
#'   start_year = 2017, end_year = 2021, country = "France", test_set_steps = 2,
#'   future = "no"
#' )
#' }
#' ggplot(example_full_model_predictions) +
#'   geom_line(aes(date, hourly_demand, color = "actual")) +
#'   geom_line(aes(date, complete_model, color = "fitted")) +
#'   xlab("\nYear") +
#'   ylab("Hourly Demand\n [MW]\n") +
#'   geom_vline(xintercept = example_full_model_predictions$date[26280], linetype = 2) +
#'   ggthemes::theme_foundation(base_size = 14, base_family = "sans") +
#'   xlab("\nHour") +
#'   ylab("Hourly Demand\n [MW]\n") +
#'   ggtitle(paste("Complete Model Results - FR\n")) +
#'   theme(
#'     plot.title = element_text(
#'       face = "bold",
#'       size = rel(1.2), hjust = 0.5
#'     ),
#'     text = element_text(),
#'     panel.background = element_rect(colour = NA),
#'     plot.background = element_rect(colour = NA),
#'     panel.border = element_rect(colour = NA),
#'     axis.title = element_text(face = "bold", size = rel(1)),
#'     axis.title.y = element_text(angle = 90, vjust = 2),
#'     axis.title.x = element_text(vjust = -0.2),
#'     axis.text = element_text(),
#'     axis.line.x = element_line(colour = "black"),
#'     axis.line.y = element_line(colour = "black"),
#'     axis.ticks = element_line(),
#'     panel.grid.major = element_line(colour = "#f0f0f0"),
#'     panel.grid.minor = element_blank(),
#'     legend.key = element_rect(colour = NA),
#'     legend.position = "bottom",
#'     legend.direction = "horizontal",
#'     legend.key.size = unit(0.2, "cm"),
#'     plot.margin = unit(c(10, 5, 5, 5), "mm"),
#'     strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
#'     strip.text = element_text(face = "bold")
#'   ) +
#'   theme(legend.title = element_blank())
#'
#' ## With future predictions
#' \donttest{
#' forecast_data <- full_forecast(
#'   start_year = 2017, end_year = 2021, country = "France", test_set_steps = 2,
#'   future = "yes", end_year = 2028
#' )
#' }
#' suppressWarnings(
#'   ggplot(example_full_model_future_predictions) +
#'     geom_line(aes(1:nrow(example_full_model_future_predictions),
#'       hourly_demand,
#'       color = "actual"
#'     )) +
#'     geom_line(aes(1:nrow(example_full_model_future_predictions), complete_model,
#'       color = "fitted"
#'     )) +
#'     xlab("\nYear") +
#'     ylab("Hourly Demand\n [MW]\n") +
#'     geom_vline(xintercept = 26280, linetype = 2) +
#'     geom_vline(xintercept = 43800, linetype = 3) +
#'     ggthemes::theme_foundation(base_size = 14, base_family = "sans") +
#'     xlab("\nHour") +
#'     ylab("Hourly Demand\n [MW]\n") +
#'     scale_y_continuous(labels = scales::label_number(scalar = 1)) +
#'     ggtitle(paste("Complete Model Results - FR\n")) +
#'     theme(
#'       plot.title = element_text(
#'         face = "bold",
#'         size = rel(1.2), hjust = 0.5
#'       ),
#'       text = element_text(),
#'       panel.background = element_rect(colour = NA),
#'       plot.background = element_rect(colour = NA),
#'       panel.border = element_rect(colour = NA),
#'       axis.title = element_text(face = "bold", size = rel(1)),
#'       axis.title.y = element_text(angle = 90, vjust = 2),
#'       axis.title.x = element_text(vjust = -0.2),
#'       axis.text = element_text(),
#'       axis.line.x = element_line(colour = "black"),
#'       axis.line.y = element_line(colour = "black"),
#'       axis.ticks = element_line(),
#'       panel.grid.major = element_line(colour = "#f0f0f0"),
#'       panel.grid.minor = element_blank(),
#'       legend.key = element_rect(colour = NA),
#'       legend.position = "bottom",
#'       legend.direction = "horizontal",
#'       legend.key.size = unit(0.2, "cm"),
#'       plot.margin = unit(c(10, 5, 5, 5), "mm"),
#'       strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
#'       strip.text = element_text(face = "bold")
#'     ) +
#'     theme(legend.title = element_blank()) +
#'     scale_x_continuous(
#'       breaks = c(1, 8761, 17521, 26281, 35041, 43801, 52561, 61321, 70081, 78841, 87601, 96361),
#'       labels = c(2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028)
#'     ) +
#'     annotate("text", x = 13140, y = 99216.6, label = "Training", size = 4, hjust = 0.5, vjust = 0) +
#'     annotate("text", x = 35040, y = 99216.6, label = "Test", size = 4, hjust = 0.5, vjust = 0) +
#'     annotate("text", x = 74460, y = 99216.6, label = "Unknown", size = 4, hjust = 0.5, vjust = 0)
#' )
full_forecast <- function(start_year, end_year_data, country, test_set_steps = 2, future = "yes", end_year = 2028, data_directory = tempdir(), verbose = FALSE) {
  country_abbreviation <- countrycode::countrycode(country, "country.name", "iso2c")
  if (grepl("Rtmp", data_directory)) {
    message(paste(
      "\nAll results, models and plots will be saved to a folder called", country_abbreviation,
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
      message("NOTE: YOU WILL BE ASKED MULTIPLE TIMES NOW DURING THE DIFFERENT FUNCTIONS. PLEASE ALWAYS STATE 1.")
    } else if (choice == "2") {
      data_directory <- getwd()
      message(paste0("\nResults, models, and plots will be saved in the current working directory in ", data_directory, "/", country_abbreviation))
    } else if (choice == "3") {
      new_dir <- readline(prompt = "Enter the full path of the directory where you want to save the data: ")
      data_directory <- new_dir
      if (!dir.exists(data_directory)) {
        stop("The specified data_directory does not exist: ", data_directory, "\nPlease run the function again.")
      }
      message("\nResults, models, and plots will be saved in the specified directory: ", data_directory, "/", country_abbreviation)
    } else {
      message("Invalid input. Keeping the temporary directory.\nData will be cleaned up when R is shut down.")
    }
  } else {
    if (!dir.exists(data_directory)) {
      stop("The specified data_directory does not exist: ", data_directory, "\nPlease run the function again.")
    }
    message("\nData, models, and plots will be saved in the specified working directory in ", data_directory, "/", country_abbreviation)
  }
  if (!verbose) {
    message("Verbose is set to false. Set to true if you want to see the generated plots and more detailed progress information.")
  }
  message("\nStarting the algorithm. This might take up to 15 minutes.")
  message("\nData preparation.")

  demand_data <- oRaklE::get_entsoE_data(start_year, end_year_data, country)
  suppressMessages(
    demand_data_filled <- oRaklE::fill_missing_data(demand_data, data_directory = data_directory)
  )
  suppressMessages(
    decomposed_data <- oRaklE::decompose_load_data(demand_data_filled, data_directory = data_directory, verbose = verbose)
  )
  message("\nCalculating long-term trend models.")
  longterm <- oRaklE::get_historic_load_data(decomposed_data$longterm)
  longterm_all_data <- oRaklE::get_macro_economic_data(longterm)
  longterm_all_data_predicted <- oRaklE::long_term_lm(longterm_all_data, test_set_steps = test_set_steps, data_directory = data_directory, verbose = verbose)
  message("\nCalculating mid-term trend models.")
  midterm <- oRaklE::add_holidays_mid_term(decomposed_data$midterm)
  suppressMessages(
    midterm_all <- oRaklE::get_weather_data(midterm, data_directory = data_directory)
  )
  midterm_all_data_predicted <- oRaklE::mid_term_lm(midterm_all$demand, test_set_steps = test_set_steps * 365, data_directory = data_directory, verbose = verbose)
  message("\nCalculating short-term trend models.")
  shortterm <- oRaklE::add_holidays_short_term(decomposed_data$shortterm)
  short_term_data_predicted <- oRaklE::short_term_lm(shortterm, test_set_steps = test_set_steps * 8760, data_directory = data_directory, verbose = verbose)
  message("\nCombining models.")
  suppressMessages(
    combined_model_results <- oRaklE::combine_models(longterm_all_data_predicted$longterm_predictions, midterm_all_data_predicted$midterm_predictions, short_term_data_predicted$shortterm_predictions, longterm_model_number = 1, data_directory = data_directory, verbose = verbose)
  )
  if (future == "Yes"){
    future <- "yes"
  }
  if (future == "yes") {
    message("\nCalculating future forecasts.")
    longterm_future_macro_data <- oRaklE::long_term_future_data(longterm_all_data_predicted, end_year = end_year, dataset = "WEO")
    suppressMessages(
      longterm_future_predictions <- oRaklE::long_term_future(longterm_future_macro_data, data_directory = data_directory, verbose = verbose)
    )
    suppressMessages(
      midterm_future_predictions <- mid_term_future(midterm_all_data_predicted, end_year = end_year, data_directory = data_directory, verbose = verbose)
    )
    suppressMessages(
      shortterm_future_predictions <- short_term_future(short_term_data_predicted, end_year = end_year, data_directory = data_directory, verbose = verbose)
    )
    suppressMessages(
      full_model_future_predictions <- combine_models_future(longterm_future_predictions$longterm_future_predictions, midterm_future_predictions$midterm_future_predictions,
        shortterm_future_predictions$shortterm_future_predictions,
        longterm_model_number = 1, data_directory = data_directory, verbose = verbose
      )
    )
    training_timespan <- paste0(start_year, "_to_", end_year_data)
    lt_results <- list()
    lt_results[[training_timespan]] <- longterm_all_data_predicted
    lt_results[["future predictions"]] <- longterm_future_predictions

    mt_results <- list()
    mt_results[[training_timespan]] <- midterm_all_data_predicted
    mt_results[["future predictions"]] <- midterm_future_predictions
    st_results <- list()
    st_results[[training_timespan]] <- short_term_data_predicted
    st_results[["future predictions"]] <- shortterm_future_predictions

    return(list(
      "full_model_predictions" = full_model_future_predictions, "longterm" = lt_results, "midterm" = mt_results,
      "shortterm" = st_results
    ))
  } else {
    return(list(
      "full_model_predictions" = combined_model_results, "longterm" = longterm_all_data_predicted, "midterm" = midterm_all_data_predicted,
      "shortterm" = short_term_data_predicted
    ))
  }
}
