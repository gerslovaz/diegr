#' Select outlier epochs
#'
#' @description
#' Function for selecting outlier epochs for one subject and one sensor in chosen time points. Epochs are marked as outliers based on one of the following criteria: interquartile range criterion, percentile approach or Hampel filter method.
#'
#' @param data A data frame, tibble or a database table with input data, required columns: subject, sensor, time, epoch and the column with EEG amplitude named as in \code{amplitude} parameter.
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values. Default is \code{"signal"}.
#' @param subject An integer or character ID of selected subject.
#' @param sensor An integer or character ID of selected sensor.
#' @param time A vector with time range for outliers detection. If not defined, the outliers are searching across all time points in the dataset.
#' @param method A character denoting the method used for outlier detection. The options are: \code{"iqr"} for interquartile range criterion, \code{"percentile"} for percentile method and \code{"hampel"} for Hampel filter method. See details for further information about methods.
#' @param p A probability value from \code{[0,1]} interval determining percentile to the percentile method (according to \code{probs} argument in \code{quantile()} function). The default value is set to 0.975 for the interval formed by the 2.5 and 97.5 percentiles.
#'
#' @details
#' The input data frame or database table must contain at least following columns:
#' subject - a column with subject IDs,
#' sensor - a column with sensor labels,
#' time - a column with time point numbers,
#' signal (or other name specified in `amplitude` parameter) - a column with measured EEG signal values.
#'
#' The outlier detection method is chosen through \code{method} argument. The possibilities are
#' - \code{iqr} for the interquartile range criterion, values outside the interval \code{[lower quartile - 1.5 * IQR, upper quartile + 1.5 * IQR]}, where IQR denotes interquartile range, are considered as outliers
#' - \code{percentile} for the percentile method, values outside the interval defined by the chosen percentiles are considered as outliers
#' - \code{hampel} for the Hampel filter method, values outside the interval \code{[median - 3 * MAD, median + 3 * MAD]}, where MAD denotes median absolute deviation, are considered as outliers
#'
#' @return A list with following components:
#' \item{epoch_table}{A data frame with epoch ID and the number of time points in which the epoch was evaluated as outlier. (Only epochs with occurrence of outliers in at least one time point are presented.)}
#' \item{outliers_data}{A data frame with subset of data corresponding to the outliers found.}
#'
#' @importFrom grDevices boxplot.stats
#' @importFrom stats lm mad median quantile sd
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # Outlier epoch detection for subject 2, electrode E45 for the whole time range with IQR method
#' outliers_epoch(epochdata, amplitude = "signal", subject = 2, sensor = "E45", method = "iqr")
#'
#' # Outlier epoch detection for subject 2, electrode E45 for the whole time range
#' # using percentile method with 1 and 99 percentiles
#' outliers_epoch(epochdata, amplitude = "signal", subject = 2, sensor = "E45",
#'  method = "percentile", p = 0.99)

outliers_epoch <- function(data, amplitude = "signal", subject = NULL, sensor = NULL, time = NULL,
                           method, p = 0.975){

  amp_value <- {{ amplitude }}
  amp_name <- rlang::as_string(amp_value)

  if (!amp_name %in% colnames(data)) {
    stop(paste0("There is no column '", amp_name, "' in the input data."))
  }

  newdata <- pick_data(data, subject_rg = {{ subject }}, sensor_rg = {{ sensor }}, time_rg = {{ time }})

   newdata <- newdata |>
     dplyr::select("subject", "time", "epoch", "sensor", amp_name)
   newdata <- dplyr::collect(newdata)
   newdata$epoch <- factor(newdata$epoch)

  if (method == "iqr") {
    outdata <- newdata |>
      dplyr::group_by(.data$time) |>
      dplyr::mutate(outliers = .data[[amp_name]] %in% boxplot.stats(.data[[amp_name]])$out) |>
      dplyr::filter(.data$outliers == TRUE) |>
      dplyr::select("time", "epoch", "sensor", amp_name)
  }

  if (method == "percentile") {
    if (p > 1 | p < 0) {
      p <- 0.975
      warning("The input 'p' is outside the interval [0,1]. The default value 0.975 is used instead.")
    }
    if (p < 0.5) {
      p <- 1 - p
    }
    outdata <- newdata |>
      dplyr::group_by(.data$time) |>
      dplyr::mutate(outliers = .data[[amp_name]] < quantile(.data[[amp_name]], 1 - p) | .data[[amp_name]] > quantile(.data[[amp_name]], p)) |>
      dplyr::filter(.data$outliers == TRUE) |>
      dplyr::select("time", "epoch", "sensor", amp_name)
  }

  if (method == "hampel") {
    outdata <- newdata |>
      dplyr::group_by(.data$time) |>
      dplyr::mutate(outliers = .data[[amp_name]] < median(.data[[amp_name]]) - 3 * mad(.data[[amp_name]], constant = 1) | .data[[amp_name]] > median(.data[[amp_name]]) + 3 * mad(.data[[amp_name]], constant = 1)) |>
      dplyr::filter(.data$outliers == TRUE) |>
      dplyr::select("time", "epoch", "sensor", amp_name)
  }

  epoch_vec <- outdata$epoch
  epoch_tbl <- table(droplevels(epoch_vec))
  epoch_tbl <- as.data.frame(epoch_tbl)
  colnames(epoch_tbl) <- c("Epoch", "Count")
  print(epoch_tbl)

  invisible(list(epoch_table = epoch_tbl, outliers_data = outdata))
}
