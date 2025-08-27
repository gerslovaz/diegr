#'
#' Baseline correction
#'
#' @description
#' Function for computing amplitude corrected to the selected baseline.
#'
#'
#' @param data A data frame, tibble or a database table with input data, required columns: subject, sensor, time, signal, epoch.
#' @param baseline_range A numeric vector of time points used as the baseline (e.g., \code{baseline_range = 125:250}).
#' @param type A character indicating the type of baseline correction. Only the value \code{"absolute"} is allowed at this moment (other values will trigger an error).
#'
#' @details
#' If the values from `baseline_range` vector are out of the range of the `time` column, the baseline calculation proceeds as follows:
#' 1. If a part of the `baseline_range` vector is in the `time` column and part is outside its range, the baseline correction is computed only from the part inside a `time` range.
#' 2. If the whole `baseline_range` vector is out of the `time` range, the `baseline` and also the `signal_base` values of the output are `NA`'s.
#' In both cases the function returns a warning message along with the output data frame or tibble.
#'
#' Note: If there are `NA` values in the `signal` column, matching rows are ignored in the baseline calculation (which may bias the results) and the function prints a warning message.
#'
#' @return A data frame/tibble with added columns:
#' \item{signal_base}{Signal corrected by subtracting the baseline for each epoch.}
#' \item{baseline}{A baseline value used for correction.}
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' # Computing baseline correction on first 10 points, sensor "E1"
#' # a) Prepare data and compute
#' data01 <- epochdata |> dplyr::filter(.data$subject == 1 & .data$sensor == "E1")
#' basedata <- baseline_correction(data01, baseline_range = 1:10, type = "absolute")
#'
#' # b) Plot raw (black line) and corrected (red line) signal for epoch 1
#' epoch1 <- basedata |> dplyr::filter(.data$epoch == 1)
#' plot(epoch1$signal, type = "l", ylim = c(-20, 30), main = "Raw (black) vs Corrected (red) Signal",
#' xlab = "time point", ylab = "amplitude")
#' lines(epoch1$signal_base, col = "red")
#'
#' \dontrun{
#' # Set baseline_range outside of time range
#' # results in NA's in baseline and signal_base columns
#' # also returns a warning message
#' basedata <- baseline_correction(data01, baseline_range = 70:80, type = "absolute")
#' head(basedata)
#' }

baseline_correction <- function(data, baseline_range, type = "absolute") {

  if (!is.numeric(baseline_range)) {
    stop("'baseline_range' must be a numeric vector of time points.")
  }

  if (type != "absolute") {
    stop("Only 'absolute' baseline correction is implemented now.")
  }

  required_cols <- c("time", "signal", "epoch", "subject", "sensor")
  missing_cols <- setdiff(required_cols, colnames(data))

  if (length(missing_cols) > 0) {
    stop(paste("The following required columns in 'data' are missing:",
               paste(missing_cols, collapse = ", ")))
  }

  newdata <- data |>
    dplyr::select("subject", "sensor", "epoch", "time", "signal")
  newdata <- collect(newdata)

  if (any(is.na(newdata$signal))) {
    warning("There are NA's in `signal` column, these values are ignored in baseline computation.")
  }

  if (!all(baseline_range %in% newdata$time)) {
    warning("Some 'baseline_range' values are not present in the 'time' column.")
  }

  basel_data <- newdata |>
    dplyr::filter(.data$time %in% baseline_range) |>
    dplyr::group_by(.data$subject, .data$sensor, .data$epoch) |>
    dplyr::summarise(baseline = mean(.data$signal, na.rm = TRUE), .groups = "drop")

  newdata <- newdata |>
    dplyr::left_join(basel_data, by = c("subject", "sensor", "epoch")) |>
    dplyr::mutate(signal_base = .data$signal - .data$baseline) |>
    dplyr::ungroup()


  return(newdata)

}
