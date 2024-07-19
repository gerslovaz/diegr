#' Plot interactive boxplots of trials
#'
#' @description
#' Function for plotting interactive boxplots for selected subject and electrode
#'
#' @param data A data frame or a database table with EEG dataset. Required columns: subject, electrode, epoch, time, signal.
#' @param subject An integer or character ID of choosen subject to plot.
#' @param channel An integer or character ID of channel to plot.
#' @param time_lim A character vector with time-range to plot.
#' @param ... Additional arguments affecting the plot.
#'
#' @return A plotly object.
#'
#' @import plotly
#' @import dplyr
#' @import ggplot2
#' @examples
#' # Interactive boxplots of signal from channel E3 for subject 1 (health control) in choosen time points
#' data("epochdata")
#' boxplot_epoch(epochdata, subject = 1, channel = "E3", time_lim = c(260:270))
#'
#' @export
boxplot_epoch <- function(data,
                         subject,
                         channel,
                         time_lim,
                         ...) {
  db_sub <- data |>
    dplyr::filter(subject == {{ subject }} & (electrode == {{ channel }}) & time %in% time_lim)  |>
    dplyr::select(time, signal, epoch)
  db_df <- as.data.frame(db_sub)
  db_df$epoch <- factor(db_df$epoch)

  fig <- plot_ly(db_df, x = ~time, y = ~signal) |>
    add_boxplot(hovertext = paste("Trial :", db_df$epoch))
  fig2 <- fig |> layout(xaxis = list(title = "Time point"),
                         yaxis = list(title = TeX("\\mu V"))) |>
    config(mathjax = 'cdn')
  fig2
}
