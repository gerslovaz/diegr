#'
#' Baseline correction
#'
#' @description
#' Function for computing amplitude corrected to the selected baseline.
#'
#'
#' @param data A data frame, tibble or a database table with input data, required columns: subject, sensor, time, signal, epoch.
#' @param base.int Numeric character vector including time points to use as a baseline. It must include all required values, so for baseline between time points 125 and 250 write \code{base.int = 125:250}.
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
#' data01 <- epochdata |> dplyr::filter(subject == 1 & sensor == "E1")
#' basedata <- baseline_correction(data01, base.int = 1:10, type = "absolute")
#'
#' # b) Plot raw (black line) and corrected (red line) signal for epoch 1
#' plot(basedata$signal[1:50], type = "l", ylim = c(-20, 30), xlab = "time point", ylab = "amplitude")
#' lines(basedata$signal_base[1:50], col = "red")

baseline_correction <- function(data, base.int, type = 'absolute') {

  required_cols <- c("time", "signal", "epoch", "subject", "sensor")
  missing_cols <- setdiff(required_cols, colnames(data))

  if (length(missing_cols) > 0) {
    stop(paste("The following required columns in 'data' are missing:",
               paste(missing_cols, collapse = ", ")))
  }

  newdata <- data |>
    dplyr::select("subject", "sensor", "epoch", "time", "signal")
  newdata <- collect(newdata)

  newdata <- newdata |>
    dplyr::group_by(.data$subject, .data$sensor, .data$epoch) |>
    dplyr::mutate(baseline = mean(.data$signal[{ base.int }])) |>
    dplyr::mutate(signal_base = .data$signal - .data$baseline) |>
    dplyr::ungroup()

  return(newdata)

}

# E1 <- epochdata |>
#   dplyr::filter(sensor == "E1", subject == 1)
#
#
# d03 <- E12bothbase |>
#   filter(subject == 2 & sensor == "E1")
#
# epocha1 <- epochdata |>
#   filter(subject == 2 & sensor == "E1" & epoch == 1)
#
# E1bothbase |>
#   filter(subject == 2 & epoch == 1) |>
#   select(signal_base)
