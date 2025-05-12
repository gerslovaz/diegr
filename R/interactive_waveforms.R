#' Plot interactive waveform graph
#'
#' @description Function for plotting time series of EEG signal colour-coded by epoch in interactive plotly graph. The output in plotly format enables to easily edit the image layout.
#'
#' @param data A data frame, tibble or a database table with input data, must contain at least following columns: subject, sensor, time, signal, epoch.
#' @param subject A subject chosen to plot.
#' @param channel A channel to plot.
#' @param FS The sampling frequency. Default value is 250 Hz.
#' @param t0 Index of the zero time point, i.e. point, where 0 ms should be marked (most often time of the stimulus or time of the response).
#' @param col_palette Optionally, a colour palette for plotting lines. If missing, the rainbow palette is used.
#' @param base_int Optionally, an interval for baseline correction.
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
#' @importFrom grDevices rainbow
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' # Plot waveforms for subject 1 and electrode "E65" with 250 sampling frequency rate (default)
#' # and 10 as zero time point
#' interactive_waveforms(epochdata, subject = 1, channel = "E65", t0 = 10)
#'
interactive_waveforms <- function(data, subject, channel, FS = 250, t0 = NULL, col_palette,
                                  base_int = NULL) {

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


  data <- data |>
    dplyr::filter(.data$subject == {{ subject }} & (.data$sensor == {{ channel }}))  |>
    dplyr::select("time", "signal", "epoch", "subject", "sensor")
  if (!is.null(base_int)) {
    newdata <- baseline_correction(data, base_int = { base_int }, type = "absolute")
  } else {
    newdata <- collect(data)
    newdata <- newdata |>
      dplyr::mutate(signal_base = .data$signal)
  }


  newdata <- newdata |>
    dplyr::select("time", "signal_base", "epoch") |>
    group_by(.data$time) |>
    mutate(average = mean(.data$signal_base, na.rm = TRUE))
  #data <- dplyr::collect(data)
  newdata$epoch <- factor(newdata$epoch)

  label <- rlang::englue("Subject { subject }, channel { channel }")

  if (missing(col_palette)) {
    n <- length(levels(newdata$epoch))
    col_palette <- rainbow(n)
  }

  if (is.null(t0)) {
    t0 <- min(newdata$time)
    warning("The argument t0 was not specified. The minimal value of time was chosen as 0 ms time point.")
  }

  k <- 1000 / FS
  k0 <- t0 * k

  curv_epoch <- newdata |>
    group_by(.data$epoch) |>
    plot_ly(x = ~time * k - k0, y = ~signal_base, color = ~epoch, colors = col_palette,
            type = "scatter",  mode = "lines")
  curv_epoch |>
    add_trace(x = ~time * k - k0, y = ~average, type = 'scatter', mode = 'lines',
              line = list(color = 'black'),
              name = 'Average') |>
    layout(xaxis = list(title = "Time (ms)"),
           yaxis = list(title = TeX("\\mu V")),
           showlegend = F,
           title = label) |>
    config(mathjax = 'cdn')

}
