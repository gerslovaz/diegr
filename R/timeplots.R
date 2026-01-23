#' Plot interactive waveform graph
#'
#' @description Function for plotting time series of EEG signal colour-coded by epoch, condition, channel, subject or group (depending on selected `level` parameter) an interactive `plotly` graph. The function assumes that the input data have already been filtered to the desired subset according to the `level`.
#' When using the function for plotting the average, there is an option to add a confidence band using the `CI` argument.
#' The output in \link[plotly]{plotly} format enables to easily edit the image layout.
#'
#' @param data A data frame, tibble or a database table with input data containing a `time` column and columns corresponding to the selected `amplitude` and `level` parameter (see Details).
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values. Default is `"signal"`.
#' @param FS The sampling frequency. Default value is 250 Hz.
#' @param t0 Index of the zero time point, i.e. point, where 0 ms should be marked (most often time of the stimulus or time of the response).
#' @param col_palette Optionally, a colour palette for plotting lines. If missing, the rainbow palette is used. The expected length is the same (or higher) as the number of unique levels (e.g. number of epochs for `level = "epoch"`).
#' @param level A character specifying the level of the time curves. The possible values are `"epoch"` (default option), `"condition"`, `"sensor"`, `"subject"` and `"group"`. See details for more information.
#' @param avg A logical value indicating, if the average black curve should be plotted. Default is `TRUE`.
#' @param CI A logical value indicating, if the confidence ribbon should be plotted. Default is `FALSE`. See Details for more information.
#' @param use_latex A logical value indicating whether to use LaTeX formatting for the y-axis title. The default is `TRUE`.
#'
#' @details
#' The input data frame or database table must contain column `time` (a column with time point numbers) and a column with the EEG amplitude (or average amplitude) specified in the argument `amplitude`.<br>
#' It must also contain at least one of the optional columns (according to the `level` parameter - for `"sensor"` level the column `sensor` is required etc.):<br>
#' `group` - a column with group identifiers,<br>
#' `subject` - a column with subject IDs,<br>
#' `sensor` - a column with sensor labels,<br>
#' `epoch` - a column with epoch numbers.
#'
#' Note: The average signals must be pre-aggregated before plotting at higher grouping levels, for example `sensor` level assumes a mean sensor signal in the `amplitude` column (the input data for individual epochs together with sensor level setting will result in a mess output).
#'
#' Plotting confidence ribbon:
#' To plot the confidence bands around the average lines (`CI = TRUE`), the input data must include the `ci_up` and `ci_low` columns (as in the output tibble from \code{\link{compute_mean}} function).
#'
#' @return A `plotly` object showing an interactive time series of the signal according to the chosen level.
#'
#' @import ggplot2
#' @rawNamespace import(plotly, except = last_plot)
#' @importFrom grDevices rainbow
#' @importFrom rlang .data
#' @importFrom stats as.formula setNames
#'
#' @export
#'
#' @examples
#' # 1) Plot epoch waveforms with average curve for subject 1 and electrode "E65"
#' # with 250 sampling frequency rate (default) and 10 as zero time point
#' epochdata |>
#' pick_data(subject_rg = 1, sensor_rg = "E65") |>
#' interactive_waveforms(amplitude = "signal", t0 = 10, level = "epoch")
#'
#' # 2) Plot sensor level waveforms with confidence bands for subject 1 and electrodes "E65" and "E182"
#' # a) preparing data
#' sendata <- epochdata |>
#' pick_data(subject_rg = 1, sensor_rg = c("E65", "E182")) |>
#' compute_mean(amplitude = "signal", domain = "time", level = "epoch")
#' # b) plot the waveforms without the average
#' interactive_waveforms(sendata, amplitude = "average", t0 = 10,
#' level = "sensor", avg = FALSE, CI = TRUE)
interactive_waveforms <- function(data,
                                  amplitude = "signal",
                                  FS = 250,
                                  t0 = NULL,
                                  col_palette,
                                  level = "epoch",
                                  avg = TRUE,
                                  CI = FALSE,
                                  use_latex = TRUE) {

  if (CI && (!all(c("ci_low", "ci_up") %in% colnames(data)))) {
    stop("To plot confidence bands, the data must include 'ci_low' and 'ci_up' columns.")
  }

  group_arg <- switch(
    level,
    "epoch" = sym("epoch"),
    "condition" = sym("condition"),
    "sensor" = sym("sensor"),
    "subject" = sym("subject"),
    "group" = sym("group"),
    stop("Invalid level argument.")
  )

  stop_if_missing_cols(data, required_cols = c(amplitude, level, "time"))


  if (avg == TRUE) {
    avgdata <- data |>
      group_by(.data$time) |>
      summarise(avg = mean(.data[[amplitude]], na.rm = TRUE), .groups = "drop")
  }


  if (!is.factor(data[[level]])) { # convert to factor
    data[[level]] <- factor(data[[level]])
  }

  group_levels <- unique(data[[level]])

  if (missing(col_palette)) {
    n <- length(group_levels)
    col_palette <- rainbow(n)
  }


  base_colors <- setNames(col_palette, group_levels)
  ribbon_colors <- setNames(alpha(col_palette, 0.4), group_levels)

  if (is.null(t0)) {
    t0 <- min(data$time)
    warning("The argument t0 was not specified. The minimal value of time was chosen as 0 ms time point.")
  }

  k <- 1000 / FS
  k0 <- t0 * k


  p <- plot_ly()

  for (grp in group_levels) {
    data_grp <- data |>
      filter(.data[[level]] == grp)

    if (CI == TRUE) { # add ribbons
      p <- p |>
        add_ribbons(
          data = data_grp,
          x = ~time * k - k0,
          ymin = ~ci_low,
          ymax = ~ci_up,
          fillcolor = ribbon_colors[[grp]],
          line = list(color = 'transparent'),
          showlegend = FALSE,
          name = paste0(grp, " CI")
        )
    }

    p <- p |> # add traces
      add_trace(
        data = data_grp,
        x = ~time * k - k0,
        y = as.formula(paste0("~.data$", amplitude)),
        type = "scatter",
        mode = "lines",
        line = list(color = base_colors[[grp]]),
        name = grp
      )
  }

  if (avg == TRUE) {
    p <- p |>
      add_trace(data = avgdata, x = ~time * k - k0, y = ~.data$avg, type = 'scatter', mode = 'lines',
                line = list(color = 'black'),
                name = 'Average')
  }

  if (use_latex) {
    y_label <- TeX("\\mu V")
    mathjax_config <- 'cdn'
  } else {
    y_label <- "Microvolts (uV)"
    mathjax_config <- NULL
  }

  p |> plotly::layout(xaxis = list(title = "Time (ms)"),
            yaxis = list(title = y_label),
            showlegend = FALSE
            ) |>
     config(mathjax = mathjax_config)

}


#' Plot time curve of average EEG signal with confidence interval
#'
#' @description
#' Plot a time course of the average EEG signal amplitude with pointwise confidence intervals (CIs), colour-coded by a user-defined grouping variable such as experimental condition, subject or group. If the `condition_column` is `NULL`, all observations are treated as a single condition.
#'
#'
#' @param data A data frame, tibble or a database table with input data to plot. It should be an output from \code{\link{compute_mean}} function or an object with the same structure, containing columns: `time` with labels of time points and `average`, `ci_low`, `ci_up` with values of average signal and lower and upper CI bounds.
#' @param condition_column Character string specifying the name of the column used to define conditions for plotting. If `NULL`, all observations are treated as a single condition.
#' @param FS The sampling frequency. Default value is 250 Hz.
#' @param t0 Index of the zero time point, i.e. point, where 0 ms should be marked (most often time of the stimulus or time of the response).
#' @param transp A numeric value between 0 and 1 controlling the transparency of the confidence ribbon (corresponding to `alpha` parameter in \link[ggplot2]{geom_ribbon} function).
#' @param y_limits A numeric vector of length two, specifying the minimum and maximum y-axis limits. Defaults to `NULL`for plot limits determined according to input data.
#' @param label_0ms Character string for the annotation label at the 0ms mark. Default is `"stimulus"`.
#' @param label_offset A numeric vector of length two to offset the stimulus label. The first value indicates a horizontal shift, the second a vertical one. Default is `c(0,0)` for no shift.
#' @param legend_title Character string specifying the legend title shown in the plot. Default is `"Condition"`. For all observations treated as a single condition (`condition_column = NULL`) is plotted no legend.
#'
#' @details
#' The output in the form of a ggplot object allows to easily edit the result image properties.
#'
#' @seealso \code{\link{compute_mean}}, interactive version of time plot: \code{\link{interactive_waveforms}}
#'
#'
#' @returns A `ggplot` object showing the time course of the average EEG signal with pointwise confidence intervals.
#' @export
#'
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @examples
#' # Plot average signal with CI bounds from the sensor E65 excluding outlier epochs (14 and 15)
#' # for subject 2 - part b) and for the both subjects treated as conditions - part c)
#'
#' # a) preparing data
#' # a1) extract required data
#' edata <- pick_data(epochdata, sensor_rg = c("E65"), epoch_rg = 1:13)
#' # a2) baseline correction
#' data_base <- baseline_correction(edata, baseline_range = 1:10)
#' # a3) average computing
#' data_mean <- compute_mean(data_base, amplitude = "signal_base", type = "point")
#'
#' # b) filter subject 2 and plot the average line with default settings
#' # (the whole dataset treated as one condition, no legend plotted)
#' data_mean2 <- data_mean |>
#' dplyr::filter(subject == 2) # or use pick_data(data_mean, subject_rg = 2)
#' plot_time_mean(data = data_mean2, t0 = 10)
#'
#' # c) plot the time course by subject (treated as a condition)
#' plot_time_mean(data = data_mean, condition_column = "subject", t0 = 10, legend_title = "Subject")
#'
#' # Plot average signal with CI bounds for subject 1 from three chosen sensors
#' # preparing data
#' edata <- pick_data(epochdata, subject_rg = 1, sensor_rg = c("E5", "E35" ,"E65"),
#'                  epoch_rg = 1:13)
#' data_base <- baseline_correction(edata, baseline_range = 1:10)
#' data_mean <- compute_mean(data_base, amplitude = "signal_base", type = "point")
#' # plot the time course by sensor (channel)
#' plot_time_mean(data = data_mean, condition_column = "sensor", t0 = 10, legend_title = "Channel")
plot_time_mean <- function(data,
                           condition_column = NULL,
                           FS = 250,
                           t0 = 1,
                           transp = 0.4,
                           y_limits = NULL,
                           label_0ms = "stimulus",
                           label_offset = c(0,0),
                           legend_title = "Condition") {

  stop_if_missing_cols(data, required_cols = c("time", "average", "ci_low", "ci_up"))

  if (inherits(data, "tbl_sql")) {
    data <- dplyr::collect(data)
  }

  if (is.null(condition_column)) {
    data$condition <- factor("all")
  } else {
    if (!condition_column %in% names(data)) {
      stop(sprintf("Column '%s' does not exist in the data.", condition_column))
    }
    data$condition <- factor(data[[condition_column]])
  }

  if (any(is.na(data[["average"]]))) {
    warning("There are NA's in the 'average' column, these values are ignored in the plot.")
  }

  if (any(is.na(data[["average"]]))) {
    warning("There are NA's in the 'average' column, these values are ignored in the plot.")
  }

  if (any(is.na(data[["ci_low"]]))) {
    warning("There are NA's in the 'ci_low' column, these values are ignored in the plot.")
  }
  if (any(is.na(data[["ci_up"]]))) {
    warning("There are NA's in the 'ci_up' column, these values are ignored in the plot.")
  }

  if (!is.numeric(FS) || FS <= 0) {
    stop("'FS' must be a positive number.")
  }

  if (!is.numeric(t0) || length(t0) != 1) {
    stop("'t0' must be a numeric value.")
  }

  stopifnot(length(label_offset) == 2)

  k <- 1000 / FS
  k0 <- t0 * k
  y0 <- min(data$ci_low)
  n_cond <- dplyr::n_distinct(data$condition)

  if (n_cond > 1) {
    leg_pos <- "right"
  } else  {
    leg_pos <- "none"
  }

  g <- ggplot(data, aes(x = .data$time * k - k0, y = .data$average, colour = .data$condition, fill = .data$condition)) +
    geom_ribbon(aes(ymin = .data$ci_low, ymax = .data$ci_up), alpha = transp) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept =  t0 * k - k0, linetype = "dashed", color = "black") +
    annotate("text", x = t0 * k - k0 + label_offset[1], y = y0 + label_offset[2],
             label = label_0ms, vjust = -0.5, color = "black") +
    labs(
      x = "Time (ms)",
      y = expression(paste("Average amplitude (", mu, "V)")),
      colour = legend_title,
      fill = legend_title
    ) +
    theme_minimal() +
    theme(
      legend.position = leg_pos
    )

  if (!is.null(y_limits)) { # expand the limits
    g <- g + coord_cartesian(ylim = y_limits)
  }

  return(g)
}

