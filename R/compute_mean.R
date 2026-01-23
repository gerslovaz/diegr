#' Calculate mean in temporal or spatial domain
#'
#' @description
#' Calculate a pointwise or a jackknife (leave-one-out) average signal across temporal or spatial domain together with standard error and pointwise confidence interval (CI) bounds.
#' Pointwise averages can be computed in two ways: standard (un-weighted) by default, or weighted using the values in the column specified by `weights_col`.
#'
#' The function computes an average at group, subject, sensor/time point, condition or epoch level (according to the `level` parameter). For the option `level = "epoch"` the epochs are averaged etc. Function assumes pre-prepared data according to the chosen level.
#'
#'
#' @param data A data frame, tibble or a database table with input data, required columns: `time` or `sensor` (according to the selected domain), the column with the EEG amplitude specified in the argument `amplitude` and columns corresponding to the selected `level`.
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values. Default is `"signal_base"` for computing average from baseline corrected signal.
#' @param domain A character specifying the domain over which the average is computed. One of `"time"` or `"space"`. Option `"time"` computes a time-resolved average at each time point, whereas `"space"` computes a space-resolved average at each sensor.
#' @param level A character specifying the level of average calculation. The possible values are `"epoch"`,`"condition"`, `"sensor"`, `"subject"` and `"group"`. See Details for more information.
#' @param type A character specifying the method of calculating the average, `"point"` for pointwise average and `"jack"` for jackknife leave-one-out average.
#' @param weights_col A character specifying the name of the column containing observation weights. If `NULL`, un-weighted standard pointwise average is computed.
#' @param R The number of replications used in bootstrap interval calculation. Required only for computing pointwise mean. Default value is 1000.
#' @param alpha A number indicating confidence interval level. The default value is 0.95 for 95% confidence intervals.
#'
#' @details
#' The function supports averaging at different hierarchical levels of the data (using `level` argument):
#' - \code{"epoch"}: averaging across epochs. Returns the average curve (time domain) or sensor array (space domain) for each combination of other grouping variables.
#' - \code{"condition"}: averages across experimental conditions.
#' - \code{"sensor"}: averages across sensors (space domain) or time points (time domain).
#' - \code{"subject"}: averages across subjects.
#' - \code{"group"}: averages across groups of subjects (highest aggregation level).
#' The function assumes input adapted to the desired level of averaging (i.e. for `epoch` level the `epoch` column must be present in `data` etc.). For all levels higher than epochs, the averages of the lower level are assumed as the input data.
#'
#' Weighted vs un-weighted average (`type = "point"`):
#' - If `weights_col` is `NULL`, each observation is treated equally (with weight = 1), producing a standard un-weighted mean, standard errors (SE), and CI.
#' - If `weight_cols` is provided, a weighted average is computed using the values in the specified column as weights. SE and CI are computed based on the weighted variance.
#'
#' Computing standard error of the mean:
#' - For `type = "point"`, the standard error is computed as sample standard deviation divided by square root of the sample size for standard mean or its weighted alternative (if `weights_col` is specified).
#' - For `type = "jack"`, the standard error is jackknife standard error of the mean (for the exact formula see Efron and Tibshirani 1994).
#'
#' Computing point confidence intervals:
#' For each average value, the upper and lower bounds of the point confidence interval are also available.
#' - Setting `type = "point"` and `R`: the bounds are computed using percentile method from bootstrapping with `R` replicates (can be slow for large amounts of data).
#' - Setting `type = "point"` without specifying `R`: the bounds are computed using standard error of the mean and approximation by the Student distribution.
#' - Setting `type = "jack"`: the bounds are computed using jackknife standard error of the mean and approximation by the Student t-distribution. Note: used method assumes equal variance and symmetric distribution, which may be problematic for very small samples.
#'
#' Note: If there are `NA`'s in `amplitude` or `weights_col` columns, corresponding rows are ignored in the average calculation and function prints a warning message.
#'
#'
#' @return A tibble with resulting average and CI bounds according to the chosen `level`, `domain` and `alpha` arguments. The statistics are saved in columns
#' - `average` for computed average amplitude value,
#' - `n` for number of observations used in average computing,
#' - `se` for standard error of the mean,
#' - `ci_low` for lower bound of the confidence interval and
#' - `ci_up` for upper bound of the confidence interval.
#'
#' @export
#'
#' @references Efron B., Tibshirani RJ. \emph{An Introduction to the Bootstrap.} Chapman & Hall/CRC; 1994.
#'
#' @importFrom rlang .data
#' @importFrom stats qt
#' @importFrom tidyr drop_na
#' @import dplyr
#'
#'
#'
#' @examples
#' # Average (pointwise) raw signal for subject 1 and electrode E1
#' # without outlier epoch 14
#' avg_data <- epochdata |>
#' pick_data(subject_rg = 1, epoch_rg = 1:13, sensor_rg = "E1") |>
#' compute_mean(amplitude = "signal", level = "epoch", domain = "time")
#' str(avg_data)
#' \donttest{
#' # plot the result using interactive plot with pointwise CI
#' avg_data |>
#' pick_data(subject = 1) |>
#' interactive_waveforms(amplitude = "average", t0 = 10,
#' level = "sensor", avg = FALSE, CI = TRUE)
#' }
#'
#' # Jackknife average signal for subject 1 in all electrodes in time point 11 with baseline correction
#' # on interval 1:10 (again without outlier epoch 14)
#' # a) prepare corrected data
#' basedata <- pick_data(epochdata, subject_rg = 1) |>
#' baseline_correction(baseline_range = 1:10, type = "absolute")
#' # b) filter time point 11 (without epoch 14) and compute the average
#' avg_data <- pick_data(basedata, time_rg = 11, epoch_rg = 1:13) |>
#' compute_mean(amplitude = "signal_base", level = "epoch", domain = "space", type = "jack")
#' str(avg_data)
#' # c) plot the result with topo_plot()
#' topo_plot(data = avg_data, amplitude = "average")
#'
#' # Space average on subject level (average for all included subjects in time point 11)
#' # a) compute mean from all epochs for each subject
#' mean_epoch <- epochdata |>
#' pick_data(time_rg = 11, epoch_rg = 1:13) |>
#' compute_mean(amplitude = "signal", level = "epoch", domain = "space", type = "point")
#' # b) compute mean on subject level
#' mean_subjects <- compute_mean(mean_epoch, amplitude = "average", level = "subject",
#' domain = "space", type = "point")
#' head(mean_subjects)
#' # c) compute weighted mean with number of observations as weights
#' weighted_mean_subjects <- compute_mean(mean_epoch, amplitude = "average", level = "subject",
#' domain = "space", type = "point", weights_col = "n")
#' head(weighted_mean_subjects)
compute_mean <- function(data,
                         amplitude = "signal_base",
                         domain = c("time", "space"),
                         level = c("epoch", "condition", "sensor", "subject", "group"),
                         type = c("point", "jack"),
                         weights_col = NULL,
                         R = NULL,
                         alpha = 0.95){

  domain <- match.arg(domain)
  type <- match.arg(type)
  level <- match.arg(level)

  domain_col <- switch(
    domain,
    time  = "time",
    space = "sensor"
  )

  if (!is.null(weights_col)) {
    warn_if_na(data, weights_col)
    required_cols <- c(amplitude, domain_col, weights_col)
  } else {
    required_cols <- c(amplitude, domain_col)
  }

  stop_if_missing_cols(data, required_cols = required_cols)
  warn_if_na(data, amplitude)

  if (level == "sensor" && domain == "space") {
    id_sym <- rlang::sym("time")
  } else {
    id_sym <- rlang::sym(level)
  }

  group_vars <- set_group_vars(data, domain, level)
  check_grouping_vars(data, group_vars, action = "warn")

  if (type == "point") { # pointwise average
    if (is.null(weights_col)) {
      data <- data |>
        mutate(help_weights_col = 1L)

      weights_col <- "help_weights_col"
    }

    vars <- c(amplitude, weights_col)

    data_clean <- data |> # remove rows with NA
      tidyr::drop_na(any_of(vars))

    output_df <- pointwise_mean(data_clean, amp_name = amplitude,
                                group_vars = group_vars,
                                weights_col = weights_col,
                                R = R, alpha = alpha)
  } else if (type == "jack") { # jackknife average
    output_df <- jackknife_mean(data, amp_name = amplitude,
                                group_vars = group_vars,
                                id_sym = id_sym,
                                alpha = alpha)
  }

  return(output_df)

}


#' Pointwise averaging with asymptotic or bootstrap confidence intervals
#'
#' @description
#' Computes the pointwise (weighted or un-weighted) average and approximate Student or percentile bootstrap confidence intervals (CIs) of EEG amplitude across time or space, depending on the specified domain and averaging level.
#'
#'
#' @param data A data frame or tibble with EEG data filtered to the relevant subset.
#' @param amp_name Character. Name of the column with amplitude values.
#' @param group_vars A character vector with grouping variable names (according to the level of average).
#' @param weights_col Character. Name of the column with weights for weighted mean computing. If `NULL`, standard (un-weighted) mean is computed.
#' @param R Number of bootstrap replicates. If `NULL`, the approximate CI using Student distribution are computed.
#' @param alpha Confidence level (e.g., 0.95 for 95\% CI).
#'
#' @return A tibble with average amplitude and confidence bounds, containing columns:
#'   \itemize{
#'     \item `average` – frequency-weighted or un-weighted pointwise mean of `amp_name`,
#'     \item `n` – sample size (number of observations used for average computing),
#'     \item `se` – standard error of the mean,
#'     \item `ci_low`, `ci_up` – lower and upper confidence interval bounds.
#'   }
#'
#' @details
#' - If `weights_col` is set, the function computes a weighted mean using the column specified by `weights_col` as the weight. The effective sample size is taken as the sum of these weights.
#' - If `weights_col` is `NULL`, all observations are treated equally, and the function computes the standard un-weighted mean, SE, and CI.
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom purrr map_dbl
#' @noRd
pointwise_mean <- function(data,
                           amp_name,
                           group_vars,
                           weights_col,
                           R,
                           alpha = 0.95) {

  avg_data_grouped <- data |>
    summarise(
      n_eff = sum(.data[[weights_col]], na.rm = TRUE),
      avg_col = sum(.data[[amp_name]] * .data[[weights_col]], na.rm = TRUE) / sum(.data[[weights_col]], na.rm = TRUE),
      n_out = sum(!is.na(.data[[weights_col]])),
      se = {
        n_eff = sum(.data[[weights_col]], na.rm = TRUE)
        avg_col = sum(.data[[amp_name]] * .data[[weights_col]], na.rm = TRUE) / n_eff
        sqrt(
        sum(.data[[weights_col]] * (.data[[amp_name]] - avg_col)^2, na.rm = TRUE) / ((n_eff - 1) * n_eff)
        )
      },
      CI =
        if (!is.null(R)) {
          list(compute_CI_boot(
            x = .data[[amp_name]],
            w = .data[[weights_col]],
            R = R,
            alpha = alpha
          ))
         } else {
          list(NA_real_)
        },
      .by = any_of(group_vars)
    ) |>
    dplyr::collect()


  if (!is.null(R)) { # bootstrap CI
    avg_data <- avg_data_grouped |>
      mutate(
        ci_low = purrr::map_dbl(.data$CI, 1),
        ci_up = purrr::map_dbl(.data$CI, 2),
      )
  } else { # standard CI using Student distribution
    avg_data <- avg_data_grouped |>
      mutate(
        ci_low = .data$avg_col -
          qt(1 - (1 - alpha) / 2, .data$n_eff - 1) * .data$se,
        ci_up  = .data$avg_col +
          qt(1 - (1 - alpha) / 2, .data$n_eff - 1) * .data$se
      )
  }

  newnames <- c(n = "n_out", average = "avg_col")

  avg_data <- avg_data |>
    dplyr::select(-"n_eff", -"CI") |>
    dplyr::rename(all_of(newnames)) |>
    mutate(
      se = if_else(.data$n <= 1, NA_real_, .data$se),
      ci_low = if_else(.data$n <= 1, NA_real_, .data$ci_low),
      ci_up  = if_else(.data$n <= 1, NA_real_, .data$ci_up)
    )

  return(avg_data)
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
#' @return A list with `average`, `ci_low`, and `ci_up`.
#'
#' @importFrom stats qt
#' @noRd
#'
leave_one_mean <- function(x,
                           id,
                           alpha) {

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
#' @param group_vars A character vector with grouping variable names.
#' @param id_sym A symbol identifier for leave-one-out resampling.
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
                           group_vars,
                           id_sym,
                           alpha) {

  avg_data <- data |>
    summarise(
      jack_stats = list(leave_one_mean(.data[[amp_name]], id = !!id_sym, alpha = alpha)),
      .by = any_of(group_vars)
      ) |>
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
#' @param x Numeric vector (with signal values).
#' @param w Numeric vector with weights to compute weighted CI.
#' @param R Number of bootstrap samples.
#' @param alpha Confidence level (default is 0.95).
#'
#' @return A numeric vector of length 2: lower and upper confidence bounds.
#'
#' @import dplyr
#' @importFrom stats quantile
#'
#' @noRd
compute_CI_boot <- function(x,
                            w,
                            R,
                            alpha) {

  if (length(na.omit(x)) <= 1) {
      stop("Input for bootstrap is constant or contains only NA values.")
  }

  n <- length(x)
  idx <- replicate(R, sample.int(n, n, replace = TRUE)) # replicate indexes for weighted/standard mean

  means <- apply(idx, 2, function(i) {
    w_sum <- sum(w[i])
    if (w_sum == 0) { # control for division by zero
      return(NA_real_)
    }
    sum(x[i] * w[i]) / w_sum
  })

  n_na <- sum(is.na(means))

  if (n_na > 0) {
    warning(sprintf(
      "Bootstrap CI: %d out of %d replicates had zero total weight and were discarded.",
      n_na, R
    ), call. = FALSE)
  }

  means <- means[!is.na(means)]

  if (length(means) == 0) {
    return(c(NA_real_, NA_real_))
  }

  low <- (1 - alpha) / 2
  quantile(means, probs = c(low, 1 - low))

}



#' Sets grouping arguments for average computing
#'
#' @description Helper function for setting columns for group_by in pointwise computed average
#'
#' @param domain A character specifying the domain over which the average is computed. One of `"time"` or `"space"`.
#' @param level A character specifying the level of average calculation. One of `"epoch"`, `"condition"`, `"sensor"`, `"subject"` or `"group"`.
#'
#' @return A character vector representing the grouping variables.
#'
#' @noRd
set_group_vars <- function(data,
                           domain = c("time", "space"),
                           level) {

  domain <- match.arg(domain)

  hierarchy <- switch(
    domain,
    time = c("group", "subject", "sensor", "condition", "epoch"),
    space = c("group", "subject", "time", "condition", "epoch")
  )

  hierarchy <- intersect(hierarchy, colnames(data))

  keep <- switch(
    domain,
    time  = "time",
    space = "sensor"
  )

  level_idx <- match(level, hierarchy)

  if (is.na(level_idx)) {
    stop("Invalid level.")
  }

  c(hierarchy[seq_len(level_idx - 1)], keep)

}
