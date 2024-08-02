#' Plot interactive waveform graph
#'
#' @description Function for plotting time series of EEG signal color-coded by epoch in interactive plotly graph.
#'
#' @param data A data frame with input data, must contain at least following columns: subject, electrode, time, signal, epoch.
#' @param subject A subject chosen to plot.
#' @param sensor An electrode to plot.
#' @param FS The sampling frequency. Default value is 250.
#' @param t0 Index of the zero time point, i.e. point, where 0 ms should be marked (e.g. time of stimulus or time of response).
#' @param col.palette Optionally, color palette for plotting lines. If missing, the rainbow palette is used.
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
#' # and 251 zero time point
#' data(epochdata)
#' interactive_waveforms(epochdata, subject = 1, sensor = "E65", t0 = 251)
interactive_waveforms <- function(data, subject, sensor, FS = 250, t0 = 251, col.palette) {

  if (missing(col.palette)) {
    n <- length(levels(data$epoch))
    col.palette <- rainbow(n)
  }

  data <- data %>%
    dplyr::filter(subject == {{ subject }} & (electrode == {{ sensor }}))  %>%
    dplyr::select(time, signal, epoch)
  data <- data %>%
    group_by(time) %>%
    mutate(average = mean(signal, na.rm = TRUE))

  label <- rlang::englue("Subject {{ subject }}, electrode { sensor }")

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
           showlegend = F, title = label) %>%
    config(mathjax = 'cdn')

}
