#' Plot interactive waveform graph
#'
#' @description Function for plotting time series of EEG signal colour-coded by epoch, channel or subject (depending on selected level) in an interactive `plotly` graph. When using the function for plotting the average, there is an option to add a confidence band using the `CI` argument. The output in \link[plotly]{plotly} format enables to easily edit the image layout.
#'
#' @param data A data frame, tibble or a database table with input data containing a `time` column and columns corresponding to the selected `level` and `amplitude` parameter (see Details).
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values. Default is \code{"signal"}.
#' @param subject A subject(s) chosen to plot.
#' @param channel A channel(s) to plot.
#' @param FS The sampling frequency. Default value is 250 Hz.
#' @param t0 Index of the zero time point, i.e. point, where 0 ms should be marked (most often time of the stimulus or time of the response).
#' @param col_palette Optionally, a colour palette for plotting lines. If missing, the rainbow palette is used. The expected length is the same (or higher) as the number of unique levels (e.g. number of epochs for `level = "epoch"`).
#' @param level A character specifying the level of the time curves. The possible values are \code{"epoch"} (default option), \code{"sensor"} and \code{"subject"}. See details for more information.
#' @param avg A logical value indicating, if the average black curve should be plotted. Default is `TRUE`.
#' @param CI A logical value indicating, if the confidence ribbon should be plotted. Default is `FALSE`. See Details for more information.
#' @param use_latex A logical value indicating whether to use LaTeX formatting for the y-axis title. The default is `TRUE`.
#'
#' @details
#' The input data frame or database table must contain at least some of the following columns (according to the `level` parameter - for \code{"sensor"} level no epoch column is needed etc.):
#' subject - a column with subject IDs,
#' sensor - a column with sensor labels,
#' time - a column with time point numbers,
#' epoch - a column with epoch numbers,
#' signal (or other name specified in `amplitude` parameter) - a column with measured EEG signal values.
#' Note: The average signals must be pre-aggregated before plotting at higher grouping levels, for example `sensor` level assumes a mean sensor signal in the `amplitude` column (the input data for individual epochs together with sensor level setting will result in a mess output).
#'
#' Plotting confidence ribbon:
#' To plot the confidence bands around the average lines (`CI = TRUE`), the input data must include the `ci_up` and `ci_low` columns (as in the output tibble from \link{compute_mean} function).
#'
#' @return A \code{plotly} object showing an interactive time series of the signal according to the chosen level.
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
#' interactive_waveforms(epochdata, amplitude = "signal", subject = 1, channel = "E65",
#' t0 = 10, level = "epoch")
#' # 2) Plot sensor level waveforms with confidence bands for subject 1 and electrodes "E65" and "E182"
#' # a) preparing data
#' sendata <- compute_mean(epochdata, amplitude = "signal", subject = 1, channel = c("E65", "E182"),
#'  group = "time", level = "epoch")
#' # b) plot the waveforms without the average
#' interactive_waveforms(sendata, amplitude = "average", subject = 1, t0 = 10,
#' level = "sensor", avg = FALSE, CI = TRUE)
interactive_waveforms <- function(data,
                                  amplitude = "signal",
                                  subject = NULL,
                                  channel = NULL,
                                  FS = 250,
                                  t0 = NULL,
                                  col_palette,
                                  level = "epoch",
                                  avg = TRUE,
                                  CI = FALSE,
                                  use_latex = TRUE) {

  if (!amplitude %in% colnames(data)) {
    stop(paste0("There is no column '", amplitude, "' in the input data."))
  }

  if (CI && (!all(c("ci_low", "ci_up") %in% colnames(data)))) {
    stop("To plot confidence bands, the data must include 'ci_low' and 'ci_up' columns.")
  }

  group_arg <- switch(
    level,
    "epoch" = sym("epoch"),
    "sensor" = sym("sensor"),
    "subject" = sym("subject"),
    stop("Invalid level argument.")
  )

  group_name <- rlang::as_string(group_arg)

  if (!group_name %in% colnames(data)) {
    stop(paste0("There is no column '", level, "' required for chosen level."))
  }

  newdata <- pick_data(data, subject_rg = {{ subject }}, sensor_rg = {{ channel }})

  if (avg == TRUE) {
    avgdata <- newdata |>
      group_by(.data$time) |>
      summarise(avg = mean(.data[[amplitude]], na.rm = TRUE), .groups = "drop")
  }


  if (!is.factor(newdata[[group_name]])) { # convert to factor
    newdata[[group_name]] <- factor(newdata[[group_name]])
  }

  group_levels <- unique(newdata[[group_name]])

  if (missing(col_palette)) {
    n <- length(group_levels)
    col_palette <- rainbow(n)
  }


  base_colors <- setNames(col_palette, group_levels)
  ribbon_colors <- setNames(alpha(col_palette, 0.4), group_levels)

  if (is.null(t0)) {
    t0 <- min(newdata$time)
    warning("The argument t0 was not specified. The minimal value of time was chosen as 0 ms time point.")
  }

  k <- 1000 / FS
  k0 <- t0 * k


  p <- plot_ly()

  for (grp in group_levels) {
    data_grp <- newdata %>% filter(.data[[group_name]] == grp)

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
#' Plot a time course of the average EEG signal amplitude with pointwise confidence intervals (CIs), colour-coded by condition. If the `condition` column is missing, all observations are treated as a single condition.
#'
#'
#' @param data A data frame, tibble or a database table with input data to plot. It should be an output from \code{\link{compute_mean}} function or an object with the same structure, containing columns: \code{time} with labels of time points and \code{average, ci_low, ci_up} with values of average signal and lower and upper CI bounds.
#' @param FS The sampling frequency. Default value is 250 Hz.
#' @param t0 Index of the zero time point, i.e. point, where 0 ms should be marked (most often time of the stimulus or time of the response).
#' @param transp A numeric value between 0 and 1 controlling the transparency of the confidence ribbon (corresponding to \code{alpha} parameter in \link[ggplot2]{geom_ribbon} function).
#' @param y_limits A numeric vector of length two, specifying the minimum and maximum y-axis limits. Defaults to `NULL`for plot limits determined according to input data.
#' @param label_0ms Character string for the annotation label at the 0ms mark. Default is `"stimulus"`.
#' @param label_offset A numeric vector of length two to offset the stimulus label. The first value indicates a horizontal shift, the second a vertical one. Default is `c(0,0)` for no shift.
#' @param legend_title Character string specifying the legend title shown in the plot. Default is `"Condition"`. If the condition column is missing, all observations are treated as a single condition and no legend is plotted.
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
#' # excluding outlier epochs 14 and 15 and for the both subjects treated as conditions
#'
#' # a) preparing data
#' # a1) extract required data
#' edata <- epochdata |>
#' dplyr::filter(sensor == "E65" & epoch %in% 1:13)
#' # a2) baseline correction
#' data_base <- baseline_correction(edata, baseline_range = 1:10)
#' # a3) average computing
#' data_mean <- compute_mean(data_base, amplitude = "signal_base", channel = "E65",
#'  type = "point")
#'
#' # b) filter subject 2 and plot the average line with default settings
#' # (the whole dataset treated as one condition, no legend plotted)
#' data_mean2 <- data_mean |>
#' dplyr::filter(subject == 2)
#' plot_time_mean(data = data_mean2, t0 = 10)
#'
#' # c) add condition column and plot time course by condition (subject)
#' data_mean$condition <- data_mean$subject
#' plot_time_mean(data = data_mean, t0 = 10, legend_title = "Subject")
plot_time_mean <- function(data,
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

  if (!"condition" %in% names(data)) {
    data$condition <- factor("all")
  } else {
    data$condition <- factor(data$condition)
  }

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

