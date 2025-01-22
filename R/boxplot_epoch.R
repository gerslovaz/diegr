#' Plot interactive boxplots of epochs
#'
#' @description
#' Function for plotting interactive boxplots of EEG amplitude in individual epochs for selected subject and channel within the chosen time interval. The interactive plotly output enables to easily determine the epoch number from which outliers come and also allows to easily edit the image layout.
#'
#' @param data A data frame or a database table with EEG dataset. Required columns: subject, sensor, epoch, time, signal.
#' @param subject An integer or character ID of selected subject to plot.
#' @param channel An integer or character ID of channel to plot.
#' @param time_lim A character vector with time range to plot. If not defined, the first ten time points are plotted.
#'
#' @return A plotly object.
#'
#' @import dplyr
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @examples
#' # Interactive boxplots of signal from channel E3 for subject 1 (health control)
#' # in chosen time points
#' data("epochdata")
#' boxplot_epoch(epochdata, subject = 1, channel = "E3", time_lim = c(10:20))
#'
#' @export
boxplot_epoch <- function(data,
                         subject,
                         channel,
                         time_lim = NULL) {

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

  if (is.null(time_lim)) {
    min.t <- min(dplyr::pull(data, time))
    time_lim <- c(min.t:(min.t + 9))
    warning("The argument 'time_lim' was not defined, the first ten time points from data are plotted.")
  }

  # check time range
  #  min_lim <- min(time_lim)
  #  max_lim <- max(time_lim)
  #
  # if (!any(dplyr::pull(data, time) >= min_lim & dplyr::pull(data, time) <= max_lim)) {
  #   stop("There are no time values in the input data within the range defined by 'time_lim'.")
  # }

  db_sub <- data |>
    dplyr::filter(subject == {{ subject }} & (sensor == {{ channel }}) & time %in% time_lim)  |>
    dplyr::select(time, signal, epoch)
  db_df <- collect(db_sub) #as.data.frame(db_sub)
  #db_df$epoch <- factor(db_df$epoch)

  fig <- plot_ly(db_df, x = ~time, y = ~signal) |>
    add_boxplot(hovertext = paste("Epoch :", db_df$epoch))
  fig2 <- fig |> layout(xaxis = list(title = "Time point"),
                         yaxis = list(title = TeX("\\mu V")),
                        title = paste("Subject ", {{ subject }}, ", channel ", {{ channel }}, sep = "")) |>
    config(mathjax = 'cdn')
  fig2
}



