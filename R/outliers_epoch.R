#' Select outlier epochs
#'
#' @description
#' Function for selecting outlier epochs for one subject and one sensor in chosen time points. Epochs are marked as outliers based on one of the following criteria: interquartile range criterion, percentile approach or Hampel filter method.
#'
#' @param data A data frame, tibble or a database table with input data, required columns: subject, sensor, time, epoch and the column with EEG amplitude named as in \code{amplitude} parameter.
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values. Default is \code{"signal"}.
#' @param subject An integer or character ID of selected subject.
#' @param sensor An integer or character ID of selected sensor.
#' @param time A vector with time range for outliers detection. If not defined, the outliers are searched for all time points in the dataset.
#' @param method A character denoting the method used for outlier detection. The options are: \code{"iqr"} for interquartile range (IQR) criterion, \code{"percentile"} for percentile method and \code{"hampel"} for Hampel filter method. See details for further information about methods.
#' @param k_iqr A positive numeric value denoting the scaling factor used in the IQR criterion. Default value is `k_iqr = 1.5`.
#' @param k_mad A positive numeric value denoting the scaling factor used in the Hampel filter method. Default value is `k_mad = 3`.
#' @param p A probability value from \code{[0,1]} interval determining percentile to the percentile method (according to \code{probs} argument in \code{quantile()} function). The default value is set to 0.975 for the interval formed by the 2.5 and 97.5 percentiles.
#' @param print_tab Logical. Indicates, whether result table should be printed in console. Default is \code{TRUE}.
#'
#' @details
#' The input data frame or database table must contain at least following columns:
#' subject - a column with subject IDs,
#' sensor - a column with sensor labels,
#' time - a column with time point numbers,
#' signal (or other name specified in `amplitude` parameter) - a column with measured EEG signal values.
#'
#' The outlier detection method is chosen through \code{method} argument. The possibilities are
#' - \code{iqr}: The interquartile range criterion, values outside the interval \code{[lower quartile - k_iqr * IQR, upper quartile + k_iqr * IQR]} are considered as outliers. IQR denotes interquartile range and `k_iqr` the scaling factor.
#' - \code{percentile}: The percentile method, values outside the interval defined by the chosen percentiles are considered as outliers. Note: chosing small `p`leads to marking too many (or all) values.
#' - \code{hampel}: The Hampel filter method, values outside the interval \code{[median - k_mad * MAD, median + k_mad * MAD]} are considered as outliers. MAD denotes median absolute deviation and `k_mad` the scaling factor.
#' Each of the above methods operates independently per time point, not globally across time.
#'
#' @return A list with following components:
#' \item{epoch_table}{A data frame with epoch ID and the number of time points in which the epoch was evaluated as outlier. (Only epochs with occurrence of outliers in at least one time point are included.)}
#' \item{outliers_data}{A data frame with subset of data corresponding to the outliers found. (The full record for each flagged point from `epoch_table`.)}
#' With the setting \code{print_tab = TRUE}, the \code{epoch_table} is also printed to the console.
#'
#' @importFrom grDevices boxplot.stats
#' @importFrom stats lm mad median quantile sd
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # Outlier epoch detection for subject 2, electrode E45 for the whole time range with IQR method
#' outliers_epoch(epochdata, amplitude = "signal", subject = 2, sensor = "E45", method = "iqr")
#' # From the result table we see that epochs 14 and 15 were marked as outliers in 50 cases out of 50
#'
#' # Outlier epoch detection for subject 2, electrode E45 for the whole time range
#' # using percentile method with 1 and 99 percentiles
#' outliers_epoch(epochdata, amplitude = "signal", subject = 2, sensor = "E45",
#'  method = "percentile", p = 0.99)

outliers_epoch <- function(data,
                           amplitude = "signal",
                           subject = NULL,
                           sensor = NULL,
                           time = NULL,
                           method,
                           k_iqr = 1.5,
                           k_mad = 3,
                           p = 0.975,
                           print_tab = TRUE){

  amp_value <- {{ amplitude }}
  amp_name <- rlang::as_string(amp_value)

  if (!method %in% c("iqr", "percentile", "hampel")) {
    stop("Invalid method specified.")
  }

  if (!is.numeric(k_iqr) || k_iqr <= 0) {
    stop("'k_iqr' must be a positive number.")
  }

  if (!is.numeric(k_mad) || k_mad <= 0) {
    stop("'k_mad' must be a positive number.")
  }

  if (!(is.logical(print_tab))) {
    stop("Argument 'print_tab' has to be logical.")
  }

  if (!amplitude %in% colnames(data)) {
    stop(paste0("There is no column '", amplitude, "' in the input data."))
  }

  if (!all(c("subject", "sensor", "time", "epoch") %in% colnames(data))) {
    stop("Input data must contain 'subject', 'sensor', 'time', and 'epoch' columns.")
  }

  newdata <- pick_data(data, subject_rg = {{ subject }}, sensor_rg = {{ sensor }}, time_rg = {{ time }})

   newdata <- newdata |>
     dplyr::select(all_of(c("subject", "time", "epoch", "sensor", amplitude)))
   newdata <- dplyr::collect(newdata)
   newdata$epoch <- factor(newdata$epoch)

  if (method == "iqr") {
    outdata <- newdata |>
      dplyr::group_by(.data$time) |>
      dplyr::mutate(outliers = .data[[amplitude]] %in% boxplot.stats(.data[[amplitude]])$out) |>
      dplyr::filter(.data$outliers == TRUE) |>
      dplyr::select(all_of(c("time", "epoch", "sensor", amplitude)))
  }

  if (method == "percentile") {
    if (p > 1 | p < 0) {
      stop("The input 'p' is outside the interval [0,1].")
    }

    outdata <- newdata |>
      dplyr::group_by(.data$time) |>
      dplyr::mutate(outliers = .data[[amplitude]] < quantile(.data[[amplitude]], 1 - p) | .data[[amplitude]] > quantile(.data[[amplitude]], p)) |>
      dplyr::filter(.data$outliers == TRUE) |>
      dplyr::select(all_of(c("time", "epoch", "sensor", amplitude)))
  }

  if (method == "hampel") {
    outdata <- newdata |>
      dplyr::group_by(.data$time) |>
      dplyr::mutate(outliers = .data[[amplitude]] < median(.data[[amplitude]]) - 3 * mad(.data[[amplitude]], constant = 1) | .data[[amplitude]] > median(.data[[amplitude]]) + 3 * mad(.data[[amplitude]], constant = 1)) |>
      dplyr::filter(.data$outliers == TRUE) |>
      dplyr::select(all_of(c("time", "epoch", "sensor", amplitude)))
  }

  epoch_vec <- outdata$epoch
  epoch_tbl <- table(droplevels(epoch_vec))
  epoch_tbl <- as.data.frame(epoch_tbl)
  colnames(epoch_tbl) <- c("Epoch", "Count")
  if (print_tab == TRUE) {
    print(epoch_tbl)
  }

  invisible(list(epoch_table = epoch_tbl, outliers_data = outdata))
}
