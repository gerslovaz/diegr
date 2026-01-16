#'
#' Baseline correction
#'
#' @description
#' Compute amplitude values corrected to the selected baseline.
#'
#' The function computes a baseline value within each epoch and subtracts it from the signal.
#'
#'
#' @param data A data frame, tibble or a database table with input data, required columns: `time` and `signal`. Optional columns: `group`, `subject`, `sensor`, `condition` and `epoch`, if present, are included in the grouping structure.
#' @param baseline_range A numeric vector of time points used as the baseline (e.g., \code{baseline_range = 125:250}).
#' @param type A character specifying the type of baseline correction. Currently, only \code{"absolute"} is supported, any other value results in an error.
#'
#' @details
#' If the values from `baseline_range` vector extend beyond the range of the `time` column, the baseline computation proceeds as follows:
#' 1. If a part of the `baseline_range` vector is in the `time` column and part is outside its range, the baseline correction is computed only from the part inside a `time` range.
#' 2. If the whole `baseline_range` vector is out of the `time` range, the `baseline` and also the `signal_base` values of the output are `NA`'s.
#' In both cases, the function returns the output data along with a warning.
#'
#' Notes:
#' \itemize{
#'   \item Rows with \code{NA} values in the \code{signal} column are ignored
#'   when computing the baseline, and a warning is issued.
#'   \item If any grouping variable present in the data contains only
#'   \code{NA} values, a warning is issued, as this may lead to invalid or
#'   uninformative grouping.
#' }
#'
#' @return A data frame/tibble with added columns:
#' \item{signal_base}{Signal corrected by subtracting the baseline for each epoch.}
#' \item{baseline}{A baseline value used for correction.}
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' # Computing baseline correction for subject 1 on first 10 points, sensor "E1"
#' # a) Prepare data and compute
#' data01 <- epochdata |> dplyr::filter(.data$subject == 1 & .data$sensor == "E1")
#' basedata <- baseline_correction(data01, baseline_range = 1:10, type = "absolute")
#'
#' ## Note: You can also use baseline_correction() on the whole epochdata
#' ## and then filter selected subject and sensor, the results are the same,
#' ## the procedure above was chosen only for the speed of the example.
#'
#' # b) Plot raw (black line) and corrected (red line) signal for epoch 1
#' epoch1 <- basedata |> dplyr::filter(.data$epoch == 1)
#' plot(epoch1$signal, type = "l", ylim = c(-20, 30), main = "Raw (black) vs Corrected (red) Signal",
#' xlab = "time point", ylab = "amplitude")
#' lines(epoch1$signal_base, col = "red")
#'
#' \donttest{
#' # Set baseline_range outside of time range
#' # results in NA's in baseline and signal_base columns,
#' # also returns a warning message
#' basedata <- baseline_correction(data01, baseline_range = 70:80, type = "absolute")
#' head(basedata)
#' }

baseline_correction <- function(data,
                                 baseline_range,
                                 type = "absolute") {

  if (!is.numeric(baseline_range)) {
    stop("'baseline_range' must be a numeric vector of time points.")
  }

  if (type != "absolute") {
    stop("Only 'absolute' baseline correction is implemented now.")
  }

  stop_if_missing_cols(data, required_cols = c("time", "signal"))

  # check NA's in signal column
  na_check <- data |>
    dplyr::summarise(has_na = any(is.na(.data$signal))) |>
    dplyr::collect()

  if (na_check$has_na) {
    warning("There are NA's in `signal` column, these values are ignored in baseline computation.")
  }

  # check baseline time range
  existing_times <- data |>
    dplyr::filter(.data$time %in% !!baseline_range) |>
    dplyr::distinct(.data$time) |>
    dplyr::collect() |>
    dplyr::pull(.data$time)

  if (!all(baseline_range %in% existing_times)) {
    warning("Some 'baseline_range' values are not present in the 'time' column.")
  }

  potential_hierarchy <- c("group", "subject", "sensor", "condition", "epoch")
  group_vars <- intersect(potential_hierarchy, colnames(data))

  check_grouping_vars(data, vars = group_vars, action = "warn")

  basel_data <- data |>
    dplyr::filter(.data$time >= !!min(baseline_range, na.rm = TRUE),
                  .data$time <= !!max(baseline_range, na.rm = TRUE)) |>
    dplyr::group_by(across(all_of(group_vars))) |>
    dplyr::summarise(baseline = mean(.data$signal, na.rm = TRUE), .groups = "drop")

  newdata <- data |>
    dplyr::left_join(basel_data, by = group_vars) |>
    dplyr::mutate(signal_base = .data$signal - .data$baseline)

  return(dplyr::collect(newdata))

}
