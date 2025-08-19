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
#' \dontrun{
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
#' \dontrun{
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
#' }
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

  #amp_value <- {{ amplitude }}
  #amp_name <- rlang::as_string(amp_value)

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
#' @import dbplyr
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
      select(-"average_col", -"sd_col") #|>
      #collect()
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
    select(-"jack_stats") #|>
    #collect()

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


#' Plot time curve of average EEG signal with confidence interval
#'
#' @description
#' Plot a time curve of the average EEG signal amplitude together with pointwise confidence intervals (CIs).
#'
#'
#' @param data A data frame, tibble or a database table with input data to plot. It should be an output from \code{\link{compute_mean}} function or an object with the same structure, containing columns: \code{time} with labels of time points and \code{average, ci_low, ci_up} with values of average signal and lower and upper CI bounds.
#' @param FS The sampling frequency. Default value is 250 Hz.
#' @param t0 Index of the zero time point, i.e. point, where 0 ms should be marked (most often time of the stimulus or time of the response).
#' @param color The color of line. Default value is `red`. The argument accepts standard R color names or hex codes.
#' @param fill The color of CI bound, default value is `lightsalmon`. The argument accepts standard R color names or hex codes.
#' @param transp A numeric value between 0 and 1 controlling the transparency of the confidence ribbon (corresponding to \code{alpha} parameter in \link[ggplot2]{geom_ribbon} function).
#' @param y_limits A numeric vector of length two, specifying the minimum and maximum y-axis limits. Defaults to `NULL`for plot limits determined according to input data.
#' @param label_0ms Character string for the annotation label at the 0ms mark. Default is `"stimulus"`.
#' @param label_offset A numeric vector of length two to offset the stimulus label. The first value indicates a horizontal shift, the second a vertical one. Default is `c(0,0)` for no shift.
#'
#' @details
#' The output in the form of a ggplot object allows to easily edit the result image properties. For interactive version of plot see \link{interactive_waveforms} function.
#'
#'
#' @returns A \code{ggplot} object showing the time course of the average EEG signal with pointwise confidence intervals for chosen sensor.
#' @export
#'
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @examples
#' # Plot average signal with CI bounds for subject 2 from the sensor E65
#' # excluding outlier epochs 14 and 15
#'
#' # a) preparing data
#' # a1) extract required data
#' edata <- epochdata |>
#' dplyr::filter(subject == 2 & sensor == "E65" & epoch %in% 1:13)
#' # a2) baseline correction
#' data_base <- baseline_correction(edata, baseline_range = 1:10)
#' # a3) average computing
#' data_mean <- compute_mean(data_base, amplitude = "signal_base", subject = 2, channel = "E65",
#'  type = "point")
#' # b) plotting the average line with default settings
#' plot_time_mean(data = data_mean, t0 = 10)

plot_time_mean <- function(data,
                           FS = 250,
                           t0 = 1,
                           color = "red",
                           fill = "lightsalmon",
                           transp = 0.4,
                           y_limits = NULL,
                           label_0ms = "stimulus",
                           label_offset = c(0,0)) {

  miss_data <- setdiff(c("average", "ci_low", "ci_up"), colnames(data))

  if (length(miss_data) > 0) {
    stop(paste("The following required columns in 'data' are missing:",
               paste(miss_data, collapse = ", ")))
  }

  if (!is.numeric(FS) || FS <= 0) {
    stop("'FS' must be a positive number.")
  }

  if (!is.numeric(t0) || length(t0) != 1) {
    stop("'t0' must be a numeric value.")
  }

  k <- 1000 / FS
  k0 <- t0 * k
  y0 <- min(data$ci_low)
  g <- ggplot(data, aes(x = .data$time * k - k0, y = .data$average)) +
    geom_ribbon(aes(ymin = .data$ci_low, ymax = .data$ci_up), fill = fill, alpha = transp) +
    geom_line(color = color, linewidth = 1) +
    geom_vline(xintercept =  t0 * k - k0, linetype = "dashed", color = "black") +
    annotate("text", x = t0 * k - k0 + label_offset[1], y = y0 + label_offset[2],
             label = label_0ms, vjust = -0.5, color = "black") +
    labs(
      x = "Time (ms)",
      y = expression(paste("Average amplitude (", mu, "V)"))
    ) +
    theme_minimal()

  if (!is.null(y_limits)) { # expand the limits
    g <- g + expand_limits(y = y_limits)
  }

  return(g)
}


#' Plot topographic map of average EEG signal
#'
#' @description
#' Plot a topographic circle or polygon map of the average EEG signal amplitude and its lower and upper confidence interval bounds using topographic colour scale.
#' The thin-plate spline interpolation model \eqn{\text{IM:}\; \mathbb{R}^2 \rightarrow \mathbb{R}} is used for signal interpolation between the sensor locations.
#' The output in the form of a `ggplot` object allows to easily edit the result image properties.
#'
#' @param data A data frame, tibble or a database table with input data to plot. It should be an output from \code{\link{compute_mean}} function or an object with the same structure, containing columns: \code{sensor} with sensor labels and \code{average, ci_low, ci_up} with values of average signal and its lower and upper CI bounds in one time point (or precomputed average of multiple time points).
#' @param mesh A \code{"mesh"} object (or a named list with the same structure) containing at least \code{D2} element with x and y coordinates of a point mesh used for computing IM model. If not defined, the point mesh with default settings from \code{\link{point_mesh}} function is used.
#' @param coords Sensor coordinates as a tibble or data frame with named \code{x}, \code{y} and \code{sensor} columns. The \code{sensor} labels must match the labels in sensor column in \code{data}. If not defined, the HCGSN256 template is used.
#' @param template The kind of sensor template montage used. Currently the only available option is \code{"HCGSN256"} denoting the 256-channel HydroCel Geodesic Sensor Net v.1.0, which is also a default setting.
#' @param col_range A vector with minimum and maximum value of the amplitude used in the colour palette for plotting. If not defined, the range of input data (average and CI bounds) is used.
#' @param col_scale Optionally, a colour scale to be utilised for plotting. It should be a list with \code{colors} and \code{breaks} components (usually created via \code{\link{create_scale}}). If not defined, it is computed from \code{col_range}.
#' @param contour Logical. Indicates, whether contours should be plotted in the graph. Default value is \code{FALSE}.
#' @param show_legend Logical. Indicates, whether legend should be displayed below the graph. Default value is \code{TRUE}.
#' @param label_sensors A logical value indicating whether the sensor labels should also be plotted (default value is \code{FALSE}).
#'
#' @details
#' The spline interpolation is done independently for each CI bound and average.
#'
#' Note: When specifying the `coords` and `template` at the same time, the `template` parameter takes precedence and the `coords` parameter is ignored.
#'
#'
#' @return A \code{ggplot} object showing the static topographic map of the signal divided into three panels: CI lower, mean, CI upper.
#' @export
#'
#' @seealso \code{\link{animate_topo_mean}}: animated version of the plot; \code{\link{topo_plot}}
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom grDevices hsv
#' @importFrom scales rescale
#' @importFrom stats influence
#' @importFrom rlang .data
#'
#' @examples
#' # Plot average topographic map with CI bounds of signal for subject 2 from the time point 10
#' # (the time of the stimulus) excluding outlier epochs 14 and 15
#'
#' # a) preparing data
#' # a1) extract required data
#' edata <- epochdata |>
#' dplyr::filter(subject == 2 & time %in% 1:10 & epoch %in% 1:13)
#' # a2) baseline correction (needed for suitable topographic map)
#' data_base <- baseline_correction(edata, baseline_range = 1:10)
#' # a3) average computing
#' data_mean <- compute_mean(data_base, amplitude = "signal_base", subject = 2, time = 10,
#'  type = "jack", group = "space")
#'
#'
#' # b) plotting the topographic map with legend
#' plot_topo_mean(data = data_mean, template = "HCGSN256", show_legend = TRUE)
plot_topo_mean <- function(data,
                           mesh,
                           coords = NULL,
                           template = NULL,
                           col_range = NULL,
                           col_scale = NULL,
                           contour = FALSE,
                           show_legend = TRUE,
                           label_sensors = FALSE) {

  if (!(is.logical(contour))) {
    stop("Argument 'contour' has to be logical.")
  }

  if (!(is.logical(show_legend))) {
    stop("Argument 'show_legend' has to be logical.")
  }

  if (!(is.logical(label_sensors))) {
    stop("Argument 'label_sensors' has to be logical.")
  }

  miss_data <- setdiff(c("average", "ci_low", "ci_up"), colnames(data))

  if (length(miss_data) > 0) {
    stop(paste("The following required columns in 'data' are missing:",
               paste(miss_data, collapse = ", ")))
  }

  if (!is.null(template) && !is.null(coords)) {
    warning("Both 'template' and 'coords' were specified. Using 'template' and ignoring 'coords'.")
  }

  if (is.null(template) && is.null(coords)) {
    # use HCGSN256 template
    template <- "HCGSN256"
  }

  if (!is.null(template)) {
    coords <- switch(template,
                     "HCGSN256" = diegr::HCGSN256$D2,
                     stop("Unknown template.")
                     )
  }

  required_cols <- c("x", "y", "sensor")
  missing_cols <- setdiff(required_cols, colnames(coords))

  if (length(missing_cols) > 0) {
    stop(paste("The following required columns in 'coords' are missing:",
               paste(missing_cols, collapse = ", ")))
  }

  if (missing(mesh)) {
    mesh <- point_mesh(dimension = 2, template = { template })
  }

  if (control_D2(mesh)) {
    mesh_mat <- mesh$D2
  }

  M <- max(max(mesh_mat[,2], na.rm = TRUE), max(coords[["y"]]))
  x0 <- mean(mesh_mat[,1], na.rm = TRUE)

  coords_df <- data.frame(x = coords[["x"]], y = coords[["y"]]) # coords as data.frame without sensor labels for IM function

  sensor_order <- as.factor(coords$sensor) # reorder data according to sensor
  data_order <- data |>
    mutate(sensor = factor(.data$sensor, levels = sensor_order)) |>
    arrange(.data$sensor)

  y_hat <- IM(coords_df, data_order[["average"]], mesh_mat)$Y_hat
  ycp_IM <- y_hat[1:dim(mesh_mat)[1]]
  y_hat_low <- IM(coords_df, data_order[["ci_low"]], mesh_mat)$Y_hat
  ycp_IM_low <- y_hat_low[1:dim(mesh_mat)[1]]
  y_hat_up <- IM(coords_df, data_order[["ci_up"]], mesh_mat)$Y_hat
  ycp_IM_up <- y_hat_up[1:dim(mesh_mat)[1]]

  interp_data <- data.frame(x = mesh_mat[,1], y = mesh_mat[,2],
                            ycp_avg = ycp_IM, ycp_low = ycp_IM_low, ycp_up = ycp_IM_up)
  interp_data <- interp_data |>
    tidyr::pivot_longer(
      cols = c("ycp_low", "ycp_avg", "ycp_up"),
      names_to = "stats",
      values_to = "stats_value"
    )

  interp_data$stats <- factor(
    interp_data$stats,
    levels = c("ycp_low", "ycp_avg", "ycp_up"),
    labels = c("CI, lower bound", "Average", "CI, upper bound")
  )

  if (is.null(col_range)) {
    col_range <- range(interp_data$stats_value)
  }
  if (is.null(col_scale)) {
    col_scale <- create_scale(col_range)
  }


  g <- ggplot(interp_data, aes(x = .data$x, y = .data$y)) +
    geom_raster(aes(fill = .data$stats_value)) +
    scale_fill_gradientn(
      colors = col_scale$colors,
      breaks = col_scale$breaks,
      limits = range(col_scale$breaks),
      labels = round(col_scale$breaks, 2),
      values = scales::rescale(col_scale$breaks)
    ) +
    facet_wrap(~ stats, ncol = 3) +
    coord_fixed(ratio = 1) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )

  if (show_legend == TRUE) {
    g <- g  +
      labs(fill = expression(paste("Amplitude (", mu, "V)"))) +
      guides(fill = guide_colorbar(barwidth = 20, barheight = 0.7)) +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 8)
      )
  } else {
    g <- g +
      theme(legend.position = "none")
  }

  if (contour == TRUE) {
    g <- g + geom_contour(aes(z = .data$stats_value), color = "gray", breaks = col_scale$breaks)
  }

  g <- g +
    geom_point(data = coords, aes(x = .data$x, y = .data$y), color = "black", cex = 0.7)

  if (label_sensors == TRUE) {
    g <- g + geom_text(data = coords, aes(x = .data$x, y = .data$y, label = rep(coords$sensor, 3)), size = 2, vjust = -0.9)
  }

  g +
    annotate("segment", x = x0, y = M + 0.07 * abs(M), xend = x0 - 0.08 * M, yend = M + 0.01 * abs(M), col = "gray40") +
    annotate("segment", x = x0, y = M + 0.07 * abs(M), xend = x0 + 0.08 * M, yend = M + 0.01 * abs(M), col = "gray40")

}
