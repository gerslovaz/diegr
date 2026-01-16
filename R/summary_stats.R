#' Compute summary statistics of reaction times
#'
#' @description
#' Calculates basic descriptive statistics of reaction time (RT).
#' Statistics are computed separately for each combination of grouping variables present in the data (e.g., group, subject, condition).
#'
#' Computed statistics include: the number of epochs, minimum, maximum, median, mean, and standard deviation of RT.
#'
#'
#' @param data A data frame or a database table with reaction times dataset. Required columns are `epoch` and `RT` (value of reaction time in ms).
#' Optional columns: `group`, `subject`, `condition` for computing summary statistics per group/subject/condition.
#'
#' @returns A tibble with summary statistics of reaction times consisting of the following columns:
#' \describe{
#'   \item{group}{Group identifier (only if present in the input data).}
#'   \item{subject}{Subject identifier (only if present in the input data).}
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
#' # 1. Summary statistics for rtdata
#' # two different subjects, no group or conditions - results are computed per subject
#' summary_stats_rt(rtdata)
#'
#' # 2. Summary statistics for data with conditions
#' # a) create example data
#' data_cond <- rtdata
#' data_cond$condition <- c(rep("a", 7), rep("b", 7), rep("a", 8), rep("b",7))
#' # b) compute statistics per subject and condition
#' summary_stats_rt(data_cond)
#' # c) compute statistics per conditions regardless of subjects
#' # exclude "subject" column from computing
#' summary_stats_rt(data_cond[,-1])
summary_stats_rt <- function(data) {

  stop_if_missing_cols(data, required_cols = c("epoch", "RT"))

  group_vars <- intersect(c("group", "subject", "condition"), names(data))
  check_grouping_vars(data, vars = group_vars, action = "warn")

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
