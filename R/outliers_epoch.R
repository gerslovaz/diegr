#' Select outlier epochs
#'
#' @param data A data frame, tibble or a database table with input data, must contain at least following columns: subject, sensor, time, signal, epoch.
#' @param subject An integer or character ID of selected subject.
#' @param sensor An integer or character ID of selected sensor.
#' @param time A vector with time range for outliers detection. If not defined, the outliers are searching across all time points in the dataset.
#' @param method A character denoting the method used for outlier detection. The options are: "iqr" for interquartile range criterion, "percentile" for percentile method and "hampel" for Hampel filter method.
#' @param p A probability value from [0,1] interval determining percentile to the percentile method (according to \code{probs} argument in \code{quantile()} function). The default value is set to 0.975 for the interval formed by the 2.5 and 97.5 percentiles.
#'
#'
#' @return A list with following components:
#' \item{epoch.table}{A data frame with epoch ID and the number of time points in which the epoch was evaluated as outlier. (Only epochs with occurrence of outliers in at least one time point are presented.)}
#' \item{outliers.data}{A data frame with subset of data corresponding to the outliers found.}

#' @export
#'
#' @examples
#' data("epochdata")
#' # Outlier epoch detection for subject 2, electrode E45 for the whole time range with IQR method
#' outliers_epoch(epochdata, subject = 2, sensor = "E45", method = "iqr")
#'
#' # Outlier epoch detection for subject 2, electrode E45 for the whole time range with percentile method with 1 and 99 percentiles
#' outliers_epoch(epochdata, subject = 2, sensor = "E45", method = "percentile", q = 0.99)

outliers_epoch <- function(data, subject = NULL, sensor = NULL, time = NULL, method, p = 0.975){

  newdata <- pick_data(data, subject = {{ subject }}, sensor = {{ sensor }}, time = {{ time }})
  if (method == "iqr") {
    outdata <- newdata |>
      dplyr::group_by(time) |>
      dplyr::mutate(outliers = signal %in% boxplot.stats(signal)$out) |>
      dplyr::filter(outliers == TRUE) |>
      dplyr::select(time, signal, epoch, sensor)
  }

  if (method == "percentile") { # zvazit osetreni q mezi 0 a 1, prip. nechat default warning - trochu neprehledny
    if (p > 1 | p < 0) {
      p <- 0.975
      warning("The input 'p' is outside the interval [0,1]. The default value 0.975 is used instead.")
    }
    if (p < 0.5) {
      p <- 1 - p
    }
    outdata <- newdata |>
      dplyr::group_by(time) |>
      dplyr::mutate(outliers = signal < quantile(signal, 1 - q) | signal > quantile(signal, q)) |>
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

  epoch.tbl <- table(droplevels(outdata$epoch))
  epoch.tbl <- as.data.frame(epoch.tbl)
  colnames(epoch.tbl) <- c("Epoch", "Count")
  print(epoch.tbl)

  invisible(list(epoch.table = epoch.tbl, outliers.data = outdata))
}
