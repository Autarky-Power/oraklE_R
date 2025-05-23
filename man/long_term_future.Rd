% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/long_term_future.R
\name{long_term_future}
\alias{long_term_future}
\title{Long-term trend predictions for future years}
\usage{
long_term_future(
  longterm_future_macro_data,
  data_directory = tempdir(),
  model_list = NULL,
  verbose = FALSE
)
}
\arguments{
\item{longterm_future_macro_data}{Dataframe. Generated by \code{\link{long_term_future_data}}}

\item{data_directory}{The path to the directory where the data will be saved and where the function will look for
the long-term models from \code{\link{long_term_lm}}. The default is set to a temporary directory.}

\item{model_list}{A list with the models from \code{\link{long_term_lm}}. Only needs to be specified if the models
are not in the data directory.}

\item{verbose}{A boolean value indicating if you want the generated plots to be shown (set to TRUE if yes).}
}
\value{
A list with the extended initial dataframe with the future predictions for each of the 3 best long term models and one plot with the respective results
per model.
\describe{
\item{longterm_future_predictions}{A dataframe with the input data and additional columns for test_set_steps and for best three models longterm_model_predictions1, longterm_model_predictions2 and longterm_model_predictions3.}
\item{logterm_future_plots}{A list with the respective plots for each model.}
}
}
\description{
This function extends the long-term trend predictions generated by \code{\link{long_term_lm}} until a specified future year.
The unknown macro-economic covariates are either obtained from the WEO Outlook with \code{\link{long_term_future_data}} or can be supplied manually.
The function also produces and saves visualizations of the actual and the predicted demand over the training, test, and future periods.
}
\examples{
example_longterm_future_predictions <- long_term_future(example_longterm_future_macro_data)
}
\seealso{
See also function \code{\link{mid_term_future}} and \code{\link{short_term_future}} for the other prediction models and \code{\link{long_term_future_data}} for the covariate download.
}
