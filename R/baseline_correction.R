#'
#' Baseline correction
#'
#' @description
#' Function for computing amplitude corrected to the selected baseline.
#'
#'
#' @param data A data frame, tibble or a database table with input data, required columns: subject, sensor, time, signal, epoch.
#' @param base_int Numeric character vector including time points to use as a baseline. It must include all required values, so for baseline between time points 125 and 250 write \code{base_int = 125:250}.
#' @param type A character indicating the type of baseline correction. Only the value \code{"absolute"} is available at this moment.
#'
#' @return A data frame/tibble with added two columns:
#' \item{signal_base}{signal corrected on chosen baseline}
#' \item{baseline}{a value of baseline for the given epoch}
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' # Computing baseline correction on first 10 points, sensor "E1"
#' # a) Prepare data and compute
#' data01 <- epochdata |> dplyr::filter(.data$subject == 1 & .data$sensor == "E1")
#' basedata <- baseline_correction(data01, base_int = 1:10, type = "absolute")
#'
#' # b) Plot raw (black line) and corrected (red line) signal for epoch 1
#' plot(basedata$signal[1:50], type = "l", ylim = c(-20, 30), xlab = "time point", ylab = "amplitude")
#' lines(basedata$signal_base[1:50], col = "red")

baseline_correction <- function(data, base_int, type = 'absolute') {

  required_cols <- c("time", "signal", "epoch", "subject", "sensor")
  missing_cols <- setdiff(required_cols, colnames(data))

  if (length(missing_cols) > 0) {
    stop(paste("The following required columns in 'data' are missing:",
               paste(missing_cols, collapse = ", ")))
  }

  newdata <- data |>
    dplyr::select("subject", "sensor", "epoch", "time", "signal")
  newdata <- collect(newdata)

  basel_data <- newdata |>
    dplyr::filter(.data$time %in% base_int) |>
    dplyr::group_by(.data$subject, .data$sensor, .data$epoch) |>
    dplyr::summarise(baseline = mean(.data$signal, na.rm = TRUE), .groups = "drop")


  newdata <- newdata |>
    dplyr::left_join(basel_data, by = c("subject", "sensor", "epoch")) |>
    dplyr::mutate(signal_base = .data$signal - .data$baseline) |>
    dplyr::ungroup()

  return(newdata)

}
