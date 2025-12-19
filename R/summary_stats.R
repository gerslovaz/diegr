#' Compute summary statistics of reaction times by subject
#'
#' @description
#' Calculates basic descriptive statistics of reaction time (RT) for each
#' subject in the input data. Computed statistics include: the number of epochs, minimum,
#' maximum, median, mean, and standard deviation of RT.
#'
#'
#' @param data A data frame or a database table with response times dataset. Required columns: subject, epoch, RT (value of response time in ms).
#'
#' @returns A data frame with summary statistics of response times by subject, consisting of the following columns:
#' \describe{
#'   \item{subject}{Subject identifier.}
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
#' # Summary statistics for rtdata
#' summary_stats_rt(rtdata)
summary_stats_rt <- function(data) {
  sumdata <- data |>
    group_by(.data$subject) |>
    summarize(
      n_epoch = n(), avg_rt = mean(.data$RT),
      sd_rt = sd(.data$RT),
      median_rt = median(.data$RT),
      min_rt = min(.data$RT), max_rt = max(.data$RT),
      .groups = "drop"
    )

  results <- data.frame(
    subject = sumdata$subject,
    n_epoch = sumdata$n_epoch,
    min_rt = sumdata$min_rt,
    max_rt = sumdata$max_rt,
    median_rt = sumdata$median_rt,
    avg_rt = sumdata$avg_rt,
    sd_rt = sumdata$sd_rt
  )

  return(results)
}
