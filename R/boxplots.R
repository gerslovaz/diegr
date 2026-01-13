#' Plot interactive boxplots of EEG amplitude on epoch level
#'
#' @description
#' Function for plotting interactive boxplots of EEG amplitude in individual epochs for selected subject and channel within the chosen time interval. The interactive `plotly` output enables to easily determine the epoch number from which outliers come and also allows to easily edit the image layout.
#'
#' @param data A data frame or a database table with EEG dataset. Required columns: `sensor`, `epoch`, `time` and the column with EEG amplitude named as in `amplitude` parameter.
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values. Default is \code{"signal"}.
#' @param subject An integer or character ID of selected subject to plot. If missing, a single subject input data are expected.
#' @param channel An integer or character ID of channel to plot.
#' @param time_lim A numeric vector with time range to plot.
#' @param use_latex A logical value indicating whether to use LaTeX formatting for the y-axis title. The default is `TRUE`.
#'
#' @details
#' The input data frame or database table must contain at least following columns:
#' `sensor` - a column with sensor labels,
#' `epoch` - a column with epoch numbers,
#' `time` - a column with time point numbers,
#' and a column with measured EEG signal values (or their averages) called as in `amplitude`.
#' If the `subject` parameter is specified, the `subject` column with subject IDs is also required.
#'
#' @seealso \code{\link{boxplot_subject}}
#'
#' @return A `plotly` object with boxplots of EEG amplitude for individual epochs.
#'
#' @import dplyr
#' @importFrom rlang .data
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @examples
#' # Interactive boxplots of signal from channel E34 for subject 1 (health control)
#' # in chosen time points
#' boxplot_epoch(epochdata, amplitude = "signal", subject = 1, channel = "E34",
#'  time_lim = c(10:20))
#'
#' @export
boxplot_epoch <- function(data,
                          amplitude = "signal",
                          subject = NULL,
                          channel,
                          time_lim,
                          use_latex = TRUE) {

  stop_if_missing_cols(data, required_cols = c("time", "epoch", "sensor", amplitude))

  if (!is.numeric(time_lim)) {
    stop("'time_lim' must be a numeric vector of time points.")
  }

  db_sub <- pick_data(data, subject_rg = {{ subject }}, sensor_rg = {{ channel }}, time_rg = {{ time_lim }})
  db_sub <- db_sub |>
    dplyr::select("time", "epoch", {{ amplitude }})
  db_df <- collect(db_sub)

  if ("subject" %in% colnames(db_df)) {
    label <- rlang::englue("Epoch boxplots of EEG amplitude for channel { channel }")
  } else {
    label <- rlang::englue("Epoch boxplots of EEG amplitude for subject { subject }, channel { channel }")
  }

  fig <- plot_ly(db_df, x = ~time, y = as.formula(paste0("~.data$", amplitude))) |>
    add_boxplot(hovertext = paste("Epoch :", db_df$epoch))

  if (use_latex) {
    y_label <- TeX("\\mu V")
    mathjax_config <- 'cdn'
  } else {
    y_label <- "Microvolts (uV)"
    mathjax_config <- NULL
  }

  fig |>
  layout(
    xaxis = list(title = "Time point"),
    yaxis = list(title = y_label),
    title = label
  ) |>
  config(mathjax = mathjax_config)

}



#' Plot interactive boxplots of EEG amplitude across subjects
#'
#' @description
#' Function for plotting interactive boxplots of EEG amplitude across subjects for a selected single epoch and channel, within a specified time interval. The interactive plotly output enables to easily determine the subjects with outlier amplitude and also allows to easily edit the image layout.
#'
#' @param data A data frame or a database table with EEG dataset. Required columns: `subject`, `sensor`, `time` and the column with EEG amplitude named as in `amplitude` parameter.
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values. Default is \code{"signal"}.
#' @param epoch An integer or character ID of epoch to plot. If missing, a single epoch input data are expected.
#' @param subject A vector with IDs of subjects to plot. If missing, boxplots are drawn for all avaliable subjects in `data`.
#' @param channel An integer or character ID of channel to plot.
#' @param time_lim A numeric vector with time range to plot.
#' @param use_latex A logical value indicating whether to use LaTeX formatting for the y-axis title. The default is `TRUE`.
#'
#' @details
#' The input data frame or database table must contain at least following columns:
#' `subject` - a column with subject IDs,
#' `sensor` - a column with sensor labels,
#' `time` - a column with time point numbers,
#' and a column with measured EEG signal values (or their averages) called as in `amplitude`.
#' If the `epoch` parameter is specified, the `epoch` column with epoch number or ID is also required.
#'
#' @seealso \code{\link{boxplot_epoch}}
#'
#' @return A `plotly` object with boxplots of EEG amplitude across subjects.
#'
#' @import dplyr
#' @importFrom rlang .data
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @examples
#' # Interactive boxplots of signal from channel E34 in epoch 1
#' # for both subjects in chosen time points
#' ## Note: it has no statistical sense to make boxplot from only 2 observations, but
#' ## larger example dataset is not possible due to size limit of the package
#' boxplot_subject(epochdata, amplitude = "signal",
#' epoch = 1, channel = "E34", time_lim = c(10:20))
#'
#' @export
boxplot_subject <- function(data,
                          amplitude = "signal",
                          epoch = NULL,
                          subject = NULL,
                          channel,
                          time_lim,
                          use_latex = TRUE) {

  stop_if_missing_cols(data, required_cols = c("time", "sensor", "subject", amplitude))

  if (!is.numeric(time_lim)) {
    stop("'time_lim' must be a numeric vector of time points.")
  }

  db_sub <- pick_data(data, subject_rg = {{ subject }}, sensor_rg = {{ channel }}, time_rg = {{ time_lim }}, epoch_rg = {{ epoch }})
  db_sub <- db_sub |>
    dplyr::select("time", "subject", {{ amplitude }})
  db_df <- collect(db_sub)

  label <- rlang::englue("Boxplots across subjects for channel { channel }")

  fig <- plot_ly(db_df, x = ~time, y = as.formula(paste0("~.data$", amplitude))) |>
   add_boxplot(hovertext = paste("Subject :", db_df$subject))

  if (use_latex) {
    y_label <- TeX("\\mu V")
    mathjax_config <- 'cdn'
  } else {
    y_label <- "Microvolts (uV)"
    mathjax_config <- NULL
  }

  fig |>
    layout(
      xaxis = list(title = "Time point"),
      yaxis = list(title = y_label),
      title = label
    ) |>
    config(mathjax = mathjax_config)
}



#' Plot interactive boxplots of response times
#'
#' @description
#' Function for plotting interactive boxplots of response time in individual epochs for selected subjects. If the `condition` column is present in the input data, the boxplots are color-coded by condition. The interactive plotly output enables to easily determine the epoch number from which outliers come and also allows to easily edit the image layout.
#'
#' @param data A data frame or a database table containing response time data. Required columns: `subject`, `epoch`, `RT` (value of response time in ms). An optional `condition` column may be used to
#'   colour-code the boxplots.
#' @param subject A vector with IDs of subjects to plot. If missing, boxplots are drawn for all available subjects in `data`.
#'
#' @return A `plotly` object with boxplots of response times.
#'
#' @import dplyr
#' @importFrom rlang .data
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @export
#'
#' @examples
#' # Display interactive boxplots for both example subjects
#' boxplot_rt(rtdata)
#'
#' # Interactive boxplots per subject divided by condition
#' # a) add condition column to data (just for example)
#' data_cond <- rtdata
#' data_cond$condition <- c(rep("a", 7), rep("b", 7), rep("a", 8), rep("b",7))
#' # b) plot boxplots (colour-coded by condition)
#' boxplot_rt(data_cond)

boxplot_rt <- function(data,
                       subject = NULL) {
  stop_if_missing_cols(data, required_cols = c("RT", "epoch", "subject"))

  if (!is.null(subject)) {
    data <- pick_data(data, subject_rg = {{ subject }})
  }

  select_cols <- c("subject", "epoch", "RT")
  has_condition <- "condition" %in% colnames(data)

  if (has_condition) {
    select_cols <- c(select_cols, "condition")
  }

  data <- data |>
    dplyr::select(dplyr::all_of(select_cols))
  data <- collect(data)

  if (has_condition) {
    fig <- plot_ly(
      data,
      x = ~subject,
      y = ~RT,
      color = ~condition
    ) |>
      add_boxplot(
        hovertext = ~ paste("Epoch:", epoch)
      ) |>
      layout(
        boxmode = "group",
        yaxis = list(title = "Response time (ms)"),
        xaxis = list(title = "Subject"),
        title = "Response time per subject and condition"
      )
  } else {
    fig <- plot_ly(data, x = ~subject, y = ~RT) |>
      add_boxplot(hovertext = paste("Epoch :", data$epoch)) |>
      layout(
        yaxis = list(title = "Response time (ms)"),
        xaxis = list(title = "Subject"),
        title = "Response time per subject"
      )
  }

  fig
}
