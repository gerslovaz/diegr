#' Calculate mean in temporal or spatial domain
#'
#' @description
#' Function calculates a pointwise or a jackknife (leave-one-out) average signal for chosen subject (or more subjects) in temporal or spatial domain together with bootstrap or jackknife pointwise confidence interval (CI) bounds. The function computes an average at subject, sensor/time point or epoch level (according to the `level` parameter).
#'
#'
#' @param data A data frame, tibble or a database table with input data, required columns: subject, sensor, epoch (only for epoch level), time and the column with the EEG amplitude specified in the argument \code{amplitude}.
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values. Default is \code{"signal_base"} for computing average from baseline corrected signal.
#' @param subject A vector of subject indices to be included in the average calculation. If NULL, all subjects present in input data are included.
#' @param channel A character vector specifying the channels to be averaged.  If NULL, all channels present in input data are included.
#' @param time A numeric vector specifying the time points used for computing the average signal. If NULL, the whole time interval present in the input data is included.
#' @param ex_epoch Optional. A vector of epoch indices to be excluded from the average calculation.
#' @param group A character specifying the grouping factor. Default is \code{"time"} for calculation of the average at individual time points, other possibility is \code{"space"} for calculation of the average at individual space points (sensors).
#' @param level A character specifying the level of average calculation. The possible values are \code{"epoch"}, \code{"sensor"} and \code{"subject"}. See details for more information.
#' @param type A character specifying the method of calculating the average, \code{"point"} for pointwise arithmetic average (default) and \code{"jack"} for jackknife leave-one-out average.
#' @param R The number of replications used in bootstrap interval calculation. Required only for computing pointwise mean. Default value is 1000.
#' @param alpha A number indicating confidence interval level. The default value is 0.95 for 95% confidence intervals.
#'
#' @details
#' The `level` parameter enables to choose the level at which the average is calculated:
#' - \code{"epoch"} means averaging on epoch level, the result is the average curve (from all included epochs) for individual sensors and subjects in the \code{group = "time"} case or the 2D sensor array average (from all included epochs) for individual time points and subjects in the \code{group = "space"} case.
#' - \code{"sensor"} means averaging on sensor or time point level, the result is the average curve from all included sensors for individual subjects in the \code{group = "time"} case or the average sensor array from all time points for individual subjects in the \code{group = "space"} case.
#' - \code{"subject"} means averaging on subject level, the result is the average curve or sensor array from all included subjects.
#' The function assumes input adapted to the desired level of averaging (i.e. for epoch level the epoch column must be present etc.).
#'
#' Computing standard error of the mean:
#' - for `type = "point"`, the standard error is computed as sample standard deviation divided by square root of the sample size
#' - for `type = "jack"`, the standard error is jackknife standard error of the mean (for the exact formula see Efron and Tibshirani 1994)
#'
#' Computing point confidence intervals:
#' For each average value, the upper and lower bounds of the point confidence interval are also available.
#' - Setting `type = "point"` and `R`: the bounds are computed using percentile method from bootstrapping with \code{R} replicates (can be slow for large amounts of data).
#' - Setting `type = "point"` without specifying `R`: the bounds are computed using standard error of the mean and approximation by the Student distribution.
#' - Setting `type = "jack"`: the bounds are computed using jackknife standard error of the mean and approximation by the Student t-distribution. Note: used method assumes equal variance and symmetric distribution, which may be problematic for very small samples.
#'
#' Note: If there are NA's in `amplitude` column, corresponding rows are ignored in the average calculation and function prints a warning message.
#'
#'
#' @return A tibble with resulting average and CI bounds according to the chosen `level`, `group` and `alpha` arguments. The statistics are saved in columns
#' - \code{average} for computed average amplitude value,
#' - \code{se} for standard error of the mean,
#' - \code{ci_low} for lower bound of the confidence interval and
#' - \code{ci_up} for upper bound of the confidence interval.
#'
#' @export
#'
#' @references Efron B., Tibshirani RJ. \emph{An Introduction to the Bootstrap.} Chapman & Hall/CRC; 1994.
#'
#' @importFrom rlang .data
#' @importFrom stats qt
#'
#' @examples
#' # Average (pointwise) raw signal for subject 1 and electrode E1
#' # without outlier epoch 14
#' avg_data <- compute_mean(epochdata, amplitude = "signal", subject = 1, channel = "E1",
#' level = "epoch", ex_epoch = 14)
#' str(avg_data)
#' \donttest{
#' # plot the result using interactive plot with pointwise CI
#' interactive_waveforms(data = avg_data, amplitude = "average", subject = 1, t0 = 10,
#' level = "sensor", avg = FALSE, CI = TRUE)
#' }
#'
#'
#' # Jackknife average signal for subject 1 in all electrodes in time point 11 with baseline correction
#' # on interval 1:10 (again without outlier epoch 14)
#' # a) prepare corrected data
#' data01 <- epochdata |> dplyr::filter(.data$subject == 1)
#' basedata <- baseline_correction(data01, baseline_range = 1:10, type = "absolute")
#' # b) compute the average in time point 11
#' avg_data <- compute_mean(basedata, amplitude = "signal_base", time = 11, level = "epoch",
#'  group = "space", type = "jack", ex_epoch = 14)
#' str(avg_data)
#' # c) plot the result with topo_plot()
#' topo_plot(data = avg_data, amplitude = "average")
#'
#' # Space average on subject level (average for all included subjects in time point 11)
#' # a) filter time point 11
#' data02 <- epochdata |> dplyr::filter(.data$time == 11)
#' # b) compute mean from all epochs for each subject
#' mean_epoch <- compute_mean(data02, amplitude = "signal", time = 11, level = "epoch",
#' group = "space", type = "point", ex_epoch = c(14,15))
#' # c) compute mean on subject level
#' mean_subjects <- compute_mean(mean_epoch, amplitude = "average", level = "subject",
#' group = "space", type = "point")
#' head(mean_subjects)
compute_mean <- function(data,
                         amplitude = "signal_base",
                         subject = NULL,
                         channel = NULL,
                         time = NULL,
                         ex_epoch = NULL,
                         group = "time",
                         level = "epoch",
                         type = "point",
                         R = NULL,
                         alpha = 0.95){

  if (!amplitude %in% colnames(data)) {
     stop(paste0("There is no column '", amplitude, "' in the input data."))
  }

  if (!is.null(ex_epoch)) {
    data <- exclude_epoch(data, ex_epoch = {{ ex_epoch }})
  }

  newdata <- pick_data(data, subject_rg = {{ subject }}, sensor_rg = {{ channel }}, time_rg = {{ time }}) # subset data

  if (inherits(newdata, "tbl_sql") || inherits(newdata, "tbl_dbi")) {
    newdata <- dplyr::collect(newdata) # collect data for DB table
  }

  if (any(is.na(newdata[[amplitude]]))) {
    warning("There are NA's in amplitude column, the resulting mean is computed without corresponding rows.")
  }


  if (type == "point") { # pointwise average
    output_df <- pointwise_mean(newdata, amp_name = amplitude, group = group, level = level,
                                R = {{ R }}, alpha = alpha)
  } else if (type == "jack") { # jackknife average
    output_df <- jackknife_mean(newdata, amp_name = amplitude, group = group, level = level,
                                alpha = alpha)
  } else {
    stop("Invalid 'type' argument.")
  }


  return(output_df)

}


#' Compute pointwise asymptotic or bootstrap average and CI
#'
#' @description
#' Computes the pointwise arithmetic mean and approximate Student or bootstrap confidence interval of EEG amplitude across time or space, depending on the specified grouping and averaging level.
#'
#'
#' @param data A data frame or tibble with EEG data filtered to the relevant subset.
#' @param amp_name Character. Name of the column with amplitude values.
#' @param group A character specifying the grouping factor. One of \code{"time"} or \code{"space"}.
#' @param level A character specifying the level of average calculation. One of \code{"epoch"}, \code{"sensor"}, or \code{"subject"}.
#' @param R Number of bootstrap replicates. If `NULL`, the approximate CI using Student distribution are computed.
#' @param alpha Confidence level (e.g., 0.95 for 95\% CI).
#'
#' @return A tibble with average amplitude and confidence bounds.
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom purrr map_dbl
#' @noRd
pointwise_mean <- function(data,
                           amp_name,
                           group = c("time", "space"),
                           level = c("epoch", "sensor", "subject"),
                           R,
                           alpha = 0.95) {

  level <- match.arg(level)
  group <- match.arg(group)

  group_vars <- set_group_vars_pointwise(group, level)

  avg_data_grouped <- data |>
    group_by(!!!group_vars) |>
    summarise(average_col = mean(.data[[amp_name]], na.rm = TRUE),
              sd_col = sd(.data[[amp_name]], na.rm = TRUE),
              n = length(.data[[amp_name]]),
              CI =
                if (!is.null(R)) {
                list(compute_CI_boot(.data[[amp_name]], R = R, alpha = alpha))
              } else {
                list(NA)
              },
              .groups = "drop")

  if (!is.null(R)) {
      avg_data <- avg_data_grouped |>
        mutate(
          se = .data$sd_col / sqrt(.data$n),
          ci_low = purrr::map_dbl(.data$CI, 1),
          ci_up = purrr::map_dbl(.data$CI, 2),
        ) |>
        select(-"CI", -"n")
  } else {
    avg_data <- avg_data_grouped |>
          mutate(
            se = .data$sd_col / sqrt(.data$n),
            ci_low = .data$average_col - qt(1 - (1 - alpha) / 2, .data$n - 1) * .data$sd_col / sqrt(.data$n),
            ci_up = .data$average_col + qt(1 - (1 - alpha) / 2, .data$n - 1) * .data$sd_col / sqrt(.data$n)
          ) |>
      select(-"CI", -"n")
  }

    avg_data <- avg_data |>
      mutate(average = .data$average_col,
             sd = .data$sd_col) |>
      select(-"average_col", -"sd_col")

  return(avg_data)
}

#' Sets grouping arguments for pointwise average
#'
#' @description Helper function for setting columns for group_by in pointwise computed average
#'
#' @param group A character specifying the grouping factor. One of \code{"time"} or \code{"space"}.
#' @param level A character specifying the level of average calculation. One of \code{"epoch"}, \code{"sensor"}, or \code{"subject"}.
#'
#' @return A list of symbols created with `rlang::syms()` representing the grouping variables.
#'
#' @importFrom rlang syms
#'
#' @noRd
set_group_vars_pointwise <- function(group, level) {

  if (group == "time") {
    group_vars <- switch(
      level,
      "epoch"  = syms(c("subject", "sensor", "time")),
      "sensor"  = syms(c("subject", "time")),
      "subject" = syms(c("time")),
      stop("Invalid 'level' argument.")
    ) # different levels of group_by for time mean
  } else if (group == "space") {
    group_vars <- switch(
      level,
      "epoch"  = syms(c("subject", "time", "sensor")),
      "sensor"  = syms(c("subject", "sensor")),
      "subject" = syms(c("sensor")),
      stop("Invalid 'level' argument.")
    ) # different levels of group_by for space mean
  }
  return(group_vars)
}

#' Leave-one-out mean and jackknife CI
#'
#' @description
#' Computes a jackknife (leave-one-out) estimate of the mean and its confidence interval (using Student distribution) for grouped observations.
#'
#' @param x Numeric vector of values.
#' @param id Vector of identifiers used for leave-one-out resampling.
#' @param alpha Confidence level (e.g., 0.95 for 95\% CI).
#'
#' @return A list with \code{average}, \code{ci_low}, and \code{ci_up}.
#'
#' @importFrom stats qt
#' @noRd
#'
leave_one_mean <- function(x, id, alpha) {

    vec_ids <- unique(id)
    n <- length(vec_ids)
    if (n <= 1) {
      stop("There is no jackknife mean for less than 2 elements.")
    }

    means <- vapply(vec_ids, function(i) {
      mean(x[id != i], na.rm = TRUE)
    }, numeric(1))

    jack_mean <- mean(means, na.rm = TRUE)

    jack_se <- sqrt((n - 1) / n * sum((means - jack_mean)^2))

    low <- (1 - alpha) / 2

    t_idx <- qt(1 - low, n - 1) # approx. Student t distribution
    ci_low <- jack_mean - t_idx * jack_se
    ci_up <- jack_mean + t_idx * jack_se

    return(list(average = jack_mean,
                se = jack_se,
                ci_low = ci_low,
                ci_up = ci_up))

}


#' Compute jackknife average and CI
#'
#' @description
#' Computes the leave-one-out jackknife mean and confidence interval for EEG amplitude data grouped by specified spatial or temporal resolution.
#'
#' @param data A tibble with EEG data.
#' @param amp_name Character. Name of the column with amplitude values.
#' @param group A character specifying the grouping factor. One of \code{"time"} or \code{"space"}.
#' @param level A character specifying the level of average calculation. One of \code{"epoch"}, \code{"sensor"}, or \code{"subject"}.
#' @param alpha Confidence level (e.g., 0.95 for 95\% CI).
#'
#' @return A tibble with jackknife mean and CI bounds.
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom purrr map_dbl
#'
#' @noRd
jackknife_mean <- function(data,
                           amp_name,
                           group = c("time", "space"),
                           level = c("epoch", "sensor", "subject"),
                           alpha) {

  level <- match.arg(level)
  group <- match.arg(group)

  group_vars <- set_group_vars_pointwise(group, level)

  if (group == "time") {
    id_sym <- switch(
      level,
      "epoch"  = sym("epoch"),
      "sensor" = sym("sensor"),
      "subject" = sym("subject")
    )
  } else if (group == "space") {
    id_sym <- switch(
      level,
      "epoch"  = sym("epoch"),
      "sensor" = sym("time"),
      "subject" = sym("subject")
    )
  }

  avg_data <- data |>
    group_by(!!!group_vars) |>
    summarise(
      jack_stats = list(leave_one_mean(.data[[amp_name]], id = !!id_sym, alpha = alpha)),
      .groups = "drop") |>
    mutate(
      average = purrr::map_dbl(.data$jack_stats, "average"),
      se = purrr::map_dbl(.data$jack_stats, "se"),
      ci_low = purrr::map_dbl(.data$jack_stats, "ci_low"),
      ci_up = purrr::map_dbl(.data$jack_stats, "ci_up")
    ) |>
    select(-"jack_stats")

  return(avg_data)
}


#' Bootstrap confidence interval for mean
#'
#' @description
#' Computes a non-parametric bootstrap confidence interval (using percentile method) for the mean of a numeric vector.
#'
#' @param x Numeric vector.
#' @param R Number of bootstrap samples.
#' @param alpha Confidence level (default is 0.95).
#'
#' @return A numeric vector of length 2: lower and upper confidence bounds.
#'
#' @importFrom stats quantile
#'
#' @noRd
compute_CI_boot <- function(x,
                            R,
                            alpha) {

  if (length(na.omit(x)) <= 1) {
      stop("Input for bootstrap is constant or contains only NA values.")
  }

  n <- length(x)
  boot_matrix <- replicate(R, sample(x, size = n, replace = TRUE))
  means <- colMeans(boot_matrix)
  low <- (1 - alpha) / 2
  quantile(means, probs = c(low, 1 - low))

}
