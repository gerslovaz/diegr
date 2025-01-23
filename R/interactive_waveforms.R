#' Plot interactive waveform graph
#'
#' @description Function for plotting time series of EEG signal color-coded by epoch in interactive plotly graph. The output in plotly format enables to easily edit the image layout.
#'
#' @param data A data frame, tibble or a database table with input data, must contain at least following columns: subject, sensor, time, signal, epoch.
#' @param subject A subject chosen to plot.
#' @param channel A channel to plot.
#' @param FS The sampling frequency. Default value is 250 Hz.
#' @param t0 Index of the zero time point, i.e. point, where 0 ms should be marked (most often time of the stimulus or time of the response).
#' @param col.palette Optionally, color palette for plotting lines. If missing, the rainbow palette is used.
#'
#' @details
#' The input data frame or database table must contain at least following columns:
#' subject - a column with subject IDs,
#' sensor - a column with sensor labels,
#' time - a column with time point numbers,
#' signal - a column with measured EEG signal values,
#' epoch - a column with epoch numbers.
#'
#' @return A plotly graph.
#'
#' @import ggplot2
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @export
#'
#' @examples
#' # Plot waveforms for subject 1 and electrode "E65" with 250 sampling frequency rate
#' # and 1 as zero time point
#' data("epochdata")
#' interactive_waveforms(epochdata, subject = 1, channel = "E65", t0 = 1)
#'
interactive_waveforms <- function(data, subject, channel, FS = 250, t0 = NULL, col.palette) {

  if (missing(subject)) {
    stop("Argument 'subject' is missing, with no default.")
  }

  if (missing(channel)) {
    stop("Argument 'channel' is missing, with no default.")
  }

  required_cols <- c("time", "signal", "epoch", "sensor", "subject")
  missing_cols <- setdiff(required_cols, colnames(data))

  if (length(missing_cols) > 0) {
    stop(paste("The following required data columns are missing:",
               paste(missing_cols, collapse = ", ")))
  }


  data <- data %>%
    dplyr::filter(subject == {{ subject }} & (sensor == {{ channel }}))  %>%
    dplyr::select(time, signal, epoch)
  data <- data %>%
    group_by(time) %>%
    mutate(average = mean(signal, na.rm = TRUE))
  data <- dplyr::collect(data)
  data$epoch <- factor(data$epoch)

  label <- rlang::englue("Subject { subject }, channel { channel }")

  if (missing(col.palette)) {
    n <- length(levels(data$epoch))
    col.palette <- rainbow(n)
  }

  if (is.null(t0)) {
    t0 <- min(data$time)
    warning("The argument t0 was not specified. The minimal value of time was chosen as 0 ms time point.")
  }

  k <- 1000 / FS
  k0 <- t0 * k

  curv_epoch <- data %>%
    group_by(epoch) %>%
    plot_ly(x = ~time * k - k0, y = ~signal, color = ~epoch, colors = col.palette,
            type = "scatter",  mode = "lines")
  curv_epoch %>%
    add_trace(x = ~time * k - k0, y = ~average, type = 'scatter', mode = 'lines',
              line = list(color = 'black'),
              name = 'Average') %>%
    layout(xaxis = list(title = "Time (ms)"),
           yaxis = list(title = TeX("\\mu V")),
           showlegend = F,
           title = label) %>%
    config(mathjax = 'cdn')

}
