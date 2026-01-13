#' Compute summary statistics of reaction times by subject
#'
#' @description
#' Calculates basic descriptive statistics of reaction time (RT) for each
#' subject in the input data. If condition is present, results are computed per subject and condition. Computed statistics include: the number of epochs, minimum,
#' maximum, median, mean, and standard deviation of RT.
#'
#'
#' @param data A data frame or a database table with response times dataset. Required columns: `subject`, `epoch`, `RT` (value of response time in ms).
#' Optional column: `condition` for computing summary statistics per condition.
#'
#' @returns A tibble with summary statistics of response times by subject and, if present, by condition, consisting of the following columns:
#' \describe{
#'   \item{subject}{Subject identifier.}
#'   \item{condition}{Experimental condition (only if present in the input data).}
#'   \item{n_epoch}{Number of epochs.}
#'   \item{min_rt}{Minimum reaction time.}
#'   \item{max_rt}{Maximum reaction time.}
#'   \item{median_rt}{Median reaction time.}
#'   \item{avg_rt}{Mean reaction time.}
#'   \item{sd_rt}{Standard deviation of reaction time.}
#' }
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' # Summary statistics for rtdata (two different subjects, no conditions)
#' summary_stats_rt(rtdata)
#'
#' # Summary statistics for data with conditions
#' # 1. create data
#' data_cond <- rtdata
#' data_cond$condition <- c(rep("a", 7), rep("b", 7), rep("a", 8), rep("b",7))
#' # 2. compute statistics
#' summary_stats_rt(data_cond)
summary_stats_rt <- function(data) {

  group_vars <- intersect(c("subject", "condition"), names(data))

  results <- data |>
    dplyr::group_by(dplyr::across(all_of(group_vars))) |>
    dplyr::summarize(
      n_epoch = n(),
      min_rt = min(.data$RT),
      max_rt = max(.data$RT),
      median_rt = median(.data$RT),
      avg_rt = mean(.data$RT),
      sd_rt = sd(.data$RT),
      .groups = "drop"
    )

  return(results)
}
