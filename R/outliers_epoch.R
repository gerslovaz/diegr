#' Select outlier epochs
#'
#' @description
#' Function identifies epochs with outlier values in a numeric EEG amplitude variable in chosen time points.
#' Outliers are detected separately at each time point within the groups present in the data. The function then summarizes how many times each epoch was marked as an outlier
#' across all time points.
#'
#' Epochs are marked as outliers based on one of the following criteria: interquartile range criterion, percentile approach or Hampel filter method.
#'
#' @param data A data frame, tibble or a database table with input data, required columns: `time`, `epoch` and the column with EEG amplitude specified by `amplitude` parameter. Optional columns: `group`, `subject`, `sensor`, `condition`.
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values. Default is `"signal"`.
#' @param time A vector with time range for outliers detection. If not defined, the outliers are searched for all time points in the dataset.
#' @param method A character denoting the method used for outlier detection. The options are: `"iqr"` for interquartile range (IQR) criterion (default value), `"percentile"` for percentile method and `"hampel"` for Hampel filter method. See details for further information about methods.
#' @param k_iqr A positive numeric value denoting the scaling factor used in the IQR criterion. Default value is `k_iqr = 1.5`.
#' @param k_mad A positive numeric value denoting the scaling factor used in the Hampel filter method. Default value is `k_mad = 3`.
#' @param p A probability value from \code{[0,1]} interval determining percentile to the percentile method (according to `probs` argument in \code{quantile()} function). The default value is set to 0.975 for the interval formed by the 2.5 and 97.5 percentiles.
#' @param print_tab Logical. Indicates, whether result table should be printed in console. Default is `TRUE`.
#'
#' @details
#' The input data frame or database table must contain at least following columns:
#' `epoch` - a column with epoch numbers/labels,
#' `time` - a column with time point numbers,
#' signal (or other name specified in `amplitude` parameter) - a column with measured EEG signal values.
#'
#' The outlier detection method is chosen through `method` argument. The possibilities are
#' - `iqr`: The interquartile range criterion, values outside the interval \code{[lower quartile - k_iqr * IQR, upper quartile + k_iqr * IQR]} are considered as outliers. IQR denotes interquartile range and `k_iqr` the scaling factor.
#' - `percentile`: The percentile method, values outside the interval defined by the chosen percentiles are considered as outliers. Note: chosing small `p`leads to marking too many (or all) values.
#' - `hampel`: The Hampel filter method, values outside the interval \code{[median - k_mad * MAD, median + k_mad * MAD]} are considered as outliers. MAD denotes median absolute deviation and `k_mad` the scaling factor.
#' Each of the above methods operates independently per time point, not globally across time.
#'
#' Note: For large datasets, the calculation can be time-consuming. It is recommended to pre-filter or subset the data before using this function to reduce computation time.
#'
#' @return A list with following components:
#' \item{epoch_table}{A data frame with epoch ID and the number of time points in which the epoch was evaluated as outlier. (Only epochs with occurrence of outliers in at least one time point are included.)}
#' \item{outliers_data}{A data frame with subset of data corresponding to the outliers found. (The full record for each flagged point from `epoch_table`.)}
#' With the setting `print_tab = TRUE`, the `epoch_table` is also printed to the console.
#'
#' @importFrom grDevices boxplot.stats
#' @importFrom stats lm mad median quantile sd
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # 1. Outlier epoch detection for subject 2, electrode E45 for the whole time range with IQR method
#' epochdata |>
#' pick_data(subject_rg = 2, sensor_rg = "E45") |>
#' outliers_epoch(amplitude = "signal")
#' ## From the result table we see that epochs 14 and 15 were marked as outliers in 50 cases out of 50
#'
#' # 2. Outlier epoch detection for both subjects, electrode E45 for the whole time range
#' # using percentile method with 1 and 99 percentiles
#' outdata <- epochdata |>
#' pick_data(sensor_rg = "E45") |>
#' outliers_epoch(amplitude = "signal", method = "percentile", p = 0.99)
#' # see head of outliers data
#' head(outdata$outliers_data)

outliers_epoch <- function(data,
                           amplitude = "signal",
                           time = NULL,
                           method = c("iqr", "percentile", "hampel"),
                           k_iqr = 1.5,
                           k_mad = 3,
                           p = 0.975,
                           print_tab = TRUE){

  method <- match.arg(method)

  if (!is.numeric(k_iqr) || k_iqr <= 0) {
    stop("'k_iqr' must be a positive number.")
  }

  if (!is.numeric(k_mad) || k_mad <= 0) {
    stop("'k_mad' must be a positive number.")
  }

  if (!(is.logical(print_tab))) {
    stop("Argument 'print_tab' has to be logical.")
  }

  stop_if_missing_cols(data, required_cols = c(amplitude, "time", "epoch"))

  newdata <- pick_data(data, time_rg = {{ time }}) |>
    dplyr::collect()
  newdata$epoch <- factor(newdata$epoch)

  group_vars <- intersect(c("group", "subject", "sensor", "condition", "time"), names(data))
  check_grouping_vars(data, vars = group_vars, action = "warn")

  if (method == "iqr") {
    outdata <- newdata |>
      dplyr::group_by(dplyr::across(all_of(group_vars))) |>
      dplyr::mutate(outliers = .data[[amplitude]] %in% boxplot.stats(.data[[amplitude]])$out) |>
      dplyr::filter(.data$outliers == TRUE) |>
      dplyr::ungroup()
  }

  if (method == "percentile") {
    if (p > 1 | p < 0) {
      stop("The input 'p' is outside the interval [0,1].")
    }

    outdata <- newdata |>
      dplyr::group_by(dplyr::across(all_of(group_vars))) |>
      dplyr::mutate(outliers = .data[[amplitude]] < quantile(.data[[amplitude]], 1 - p) | .data[[amplitude]] > quantile(.data[[amplitude]], p)) |>
      dplyr::filter(.data$outliers == TRUE) |>
      dplyr::ungroup()
  }

  if (method == "hampel") {
    outdata <- newdata |>
      dplyr::group_by(dplyr::across(all_of(group_vars))) |>
      dplyr::mutate(outliers = .data[[amplitude]] < median(.data[[amplitude]]) - 3 * mad(.data[[amplitude]], constant = 1) | .data[[amplitude]] > median(.data[[amplitude]]) + 3 * mad(.data[[amplitude]], constant = 1)) |>
      dplyr::filter(.data$outliers == TRUE) |>
      dplyr::ungroup()
  }

   group_vars_epoch <- setdiff(group_vars, "time")

   epoch_tbl <- outdata |>
     dplyr::group_by(
       dplyr::across(all_of(c(group_vars_epoch, "epoch")))
     ) |>
     dplyr::summarise(
       count = dplyr::n(),
       .groups = "drop"
     )

  if (print_tab == TRUE) {
    print(epoch_tbl)
  }

   outdata <- outdata |>
     dplyr::select(-"outliers")

  invisible(list(epoch_table = epoch_tbl, outliers_data = outdata))
}
