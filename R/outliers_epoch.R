#' Select outlier epochs
#'
#' @description
#' Function for selecting outlier epochs for one subject and one sensor in chosen time points. Epochs are marked as outliers based on one of the following criteria: interquartile range criterion, percentile approach or Hampel filter method.
#'
#' @param data A data frame, tibble or a database table with input data, required columns: subject, sensor, time, signal, epoch.
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
#' signal - a column with measured EEG signal values,
#' epoch - a column with epoch numbers.
#'
#' The outlier detection method is chosen through \code{method} argument. The possibilities are
#' - \code{iqr} for the interquartile range criterion, values outside the interval \code{[lower quartile - 1.5 * IQR, upper quartile + 1.5 * IQR]}, where IQR denotes interquartile range, are considered as outliers
#' - \code{percentile} for the percentile method, values outside the interval defined by the chosen percentiles are considered as outliers
#' - \code{hampel} for the Hampel filter method, values outside the interval \code{[median - 3 * MAD, median + 3 * MAD]}, where MAD denotes median absolute deviation, are considered as outliers
#'
#' @return A list with following components:
#' \item{epoch.table}{A data frame with epoch ID and the number of time points in which the epoch was evaluated as outlier. (Only epochs with occurrence of outliers in at least one time point are presented.)}
#' \item{outliers.data}{A data frame with subset of data corresponding to the outliers found.}
#'
#' @importFrom grDevices boxplot.stats
#' @importFrom stats lm mad median quantile sd
#' @export
#'
#' @examples
#' # Outlier epoch detection for subject 2, electrode E45 for the whole time range with IQR method
#' outliers_epoch(epochdata, subject = 2, sensor = "E45", method = "iqr")
#'
#' # Outlier epoch detection for subject 2, electrode E45 for the whole time range
#' # using percentile method with 1 and 99 percentiles
#' outliers_epoch(epochdata, subject = 2, sensor = "E45", method = "percentile", p = 0.99)

outliers_epoch <- function(data, subject = NULL, sensor = NULL, time = NULL, method, p = 0.975){

  if (is.null(time)) {
    newdata <- pick_data(data, subject.rg = {{ subject }}, sensor.rg = {{ sensor }})
  } else {
    newdata <- pick_data(data, subject.rg = {{ subject }}, sensor.rg = {{ sensor }}, time.rg = {{ time }})
  }
   newdata <- newdata |>
     dplyr::select(subject, time, signal, epoch, sensor)
   newdata <- dplyr::collect(newdata)
   newdata$epoch <- factor(newdata$epoch)

  if (method == "iqr") {
    outdata <- newdata |>
      dplyr::group_by(time) |>
      dplyr::mutate(outliers = signal %in% boxplot.stats(signal)$out) |>
      dplyr::filter(outliers == TRUE) |>
      dplyr::select(time, signal, epoch, sensor)
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
      dplyr::group_by(time) |>
      dplyr::mutate(outliers = signal < quantile(signal, 1 - p) | signal > quantile(signal, p)) |>
      dplyr::filter(outliers == TRUE) |>
      dplyr::select(time, signal, epoch, sensor)
  }

  if (method == "hampel") {
    outdata <- newdata |>
      dplyr::group_by(time) |>
      dplyr::mutate(outliers = signal < median(signal) - 3 * mad(signal, constant = 1) | signal > median(signal) + 3 * mad(signal, constant = 1)) |>
      dplyr::filter(outliers == TRUE) |>
      dplyr::select(time, signal, epoch, sensor)
  }

  epoch.vec <- outdata$epoch
  epoch.tbl <- table(droplevels(epoch.vec))
  epoch.tbl <- as.data.frame(epoch.tbl)
  colnames(epoch.tbl) <- c("Epoch", "Count")
  print(epoch.tbl)

  invisible(list(epoch.table = epoch.tbl, outliers.data = outdata))
}
