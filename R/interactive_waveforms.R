#' Plot interactive waveform graph
#'
#' @description Function for plotting time series of EEG signal colour-coded by epoch, channel or subject (according to chosen level) in interactive plotly graph. The output in plotly format enables to easily edit the image layout.
#'
#' @param data A data frame, tibble or a database table with input data containing a time column and columns corresponding to the selected `level` and `value` parameter.
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values. Default is \code{"signal"}.
#' @param subject A subject(s) chosen to plot.
#' @param channel A channel(s) to plot.
#' @param FS The sampling frequency. Default value is 250 Hz.
#' @param t0 Index of the zero time point, i.e. point, where 0 ms should be marked (most often time of the stimulus or time of the response).
#' @param col_palette Optionally, a colour palette for plotting lines. If missing, the rainbow palette is used.
#' @param level A character specifying the level of the time curves. The possible values are \code{"epoch"}, \code{"sensor"} and \code{"subject"}. See details for more information.
#' @param avg A logical value indicating, if the average black curve should be plotted. Default is `TRUE`.
#'
#' @details
#' The input data frame or database table must contain at least some of the following columns (according to the `level` parameter - for \code{"sensor"} level no epoch column is needed etc.):
#' subject - a column with subject IDs,
#' sensor - a column with sensor labels,
#' time - a column with time point numbers,
#' epoch - a column with epoch numbers,
#' signal (or other name specified in `amplitude` parameter) - a column with measured EEG signal values.
#' Note: The higher level assumes the average signal for the given level in input data, for example `sensor` level assumes a mean sensor signal in the `amplitude` column. The input data for individual epochs together with sensor level setting will result in a mess output.
#'
#' @return A plotly graph.
#'
#' @import ggplot2
#' @rawNamespace import(plotly, except = last_plot)
#' @importFrom grDevices rainbow
#' @importFrom rlang .data
#' @importFrom stats as.formula
#'
#' @export
#'
#' @examples
#' # 1) Plot epoch waveforms with average curve for subject 1 and electrode "E65"
#' # with 250 sampling frequency rate (default) and 10 as zero time point
#' interactive_waveforms(epochdata, amplitude = "signal", subject = 1, channel = "E65",
#' t0 = 10, level = "epoch")
#' # 2) Plot sensor level waveforms for subject 1 and electrodes "E65" and "E182"
#' # a) preparing data
#' sendata <- compute_mean(epochdata, amplitude = "signal", subject = 1, channel = c("E65", "E182"),
#'  group = "time", level = "epoch")
#' # b) plot the waveforms without the average
#' interactive_waveforms(sendata, amplitude = "average", subject = 1, t0 = 10,
#' level = "sensor", avg = FALSE)
interactive_waveforms <- function(data, amplitude = "signal", subject = NULL, channel = NULL,
                                  FS = 250, t0 = NULL, col_palette, level = c("epoch", "sensor", "subject"),
                                  avg = TRUE) {

  amp_value <- {{ amplitude }}
  amp_name <- rlang::as_string(amp_value)

  if (!amp_name %in% names(data)) {
    stop(paste0("There is no column '", amp_name, "' in the input data."))
  }


  group_arg <- switch(
    level,
    "epoch" = sym("epoch"),
    "sensor" = sym("sensor"),
    "subject" = sym("subject"),
    stop("Invalid level argument.")
  )

  group_name <- rlang::as_string(group_arg)

  if (!group_name %in% names(data)) {
    stop(paste0("There is no column '", level, "' required for chosen level."))
  }

  newdata <- pick_data(data, subject_rg = {{ subject }}, sensor_rg = {{ channel }})

  newdata <- newdata |>
    dplyr::select("time", !!amp_value, !!group_arg)

  if (avg == TRUE) {
    avgdata <- newdata |>
      group_by(.data$time) |>
      summarise(avg = mean(.data[[amp_name]], na.rm = TRUE), .groups = "drop")
  }


  if (!is.factor(newdata[[group_name]])) { # convert to factor
    newdata[[group_name]] <- factor(newdata[[group_name]])
  }

  if (missing(col_palette)) {
    n <- length(levels(newdata[[group_name]]))
    col_palette <- rainbow(n)
  }

  if (is.null(t0)) {
    t0 <- min(newdata$time)
    warning("The argument t0 was not specified. The minimal value of time was chosen as 0 ms time point.")
  }

  k <- 1000 / FS
  k0 <- t0 * k

  curv_epoch <- newdata |>
    group_by(!!group_arg) |>
    plot_ly() |>
    add_trace(x = ~time * k - k0, y = as.formula(paste0("~.data$", amp_name)), color = as.formula(paste0("~", group_name)), colors = col_palette,
            type = "scatter",  mode = "lines",
            name = ~.data[[group_name]])
  if (avg == TRUE) {
    curv_epoch <- curv_epoch |>
      add_trace(data = avgdata, x = ~time * k - k0, y = ~.data$avg, type = 'scatter', mode = 'lines',
                line = list(color = 'black'),
                name = 'Average')
  }

  curv_epoch |>
    plotly::layout(xaxis = list(title = "Time (ms)"),
           yaxis = list(title = TeX("\\mu V")),
           showlegend = FALSE
           ) |>
    config(mathjax = 'cdn')

}

