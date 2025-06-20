#' Plot interactive boxplots of epochs
#'
#' @description
#' Function for plotting interactive boxplots of EEG amplitude in individual epochs for selected subject and channel within the chosen time interval. The interactive plotly output enables to easily determine the epoch number from which outliers come and also allows to easily edit the image layout.
#'
#' @param data A data frame or a database table with EEG dataset. Required columns: subject, sensor, epoch, time and the column with EEG amplitude named as in \code{amplitude} parameter.
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values. Default is \code{"signal"}.
#' @param subject An integer or character ID of selected subject to plot.
#' @param channel An integer or character ID of channel to plot.
#' @param time_lim A character vector with time range to plot. If not defined, the first ten time points are plotted.
#'
#' @details
#' The input data frame or database table must contain at least following columns:
#' subject - a column with subject IDs,
#' sensor - a column with sensor labels,
#' epoch - a column with epoch numbers,
#' time - a column with time point numbers,
#' and a column with measured EEG signal values (or their averages) called as in \code{amplitude}.
#'
#'
#' @return A plotly object.
#'
#' @import dplyr
#' @importFrom rlang .data
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @examples
#' # Interactive boxplots of signal from channel E34 for subject 1 (health control)
#' # in chosen time points
#' boxplot_epoch(epochdata, amplitude = "signal", subject = 1, channel = "E34", time_lim = c(10:20))
#'
#' @export
boxplot_epoch <- function(data,
                          amplitude = "signal",
                          subject,
                          channel,
                          time_lim = NULL) {

  amp_value <- {{ amplitude }}
  amp_name <- rlang::as_string(amp_value)

  if (!amp_name %in% names(data)) {
    stop(paste0("There is no column '", amp_name, "' in the input data."))
  }

  if (missing(subject)) {
    stop("Argument 'subject' is missing, with no default.")
  }

  if (missing(channel)) {
    stop("Argument 'channel' is missing, with no default.")
  }

  required_cols <- c("time", "epoch", "sensor", "subject")
  missing_cols <- setdiff(required_cols, colnames(data))

  if (length(missing_cols) > 0) {
    stop(paste("The following required data columns are missing:",
               paste(missing_cols, collapse = ", ")))
  }

  if (is.null(time_lim)) {
    min_t <- min(dplyr::pull(data, .data$time))
    time_lim <- c(min_t:(min_t + 9))
    warning("The argument 'time_lim' was not defined, the first ten time points from data are plotted.")
  } ## zkusit jak moc to zpomaluje pri vstupu primo DB

  db_sub <- pick_data(data, subject_rg = {{ subject }}, sensor_rg = {{ channel }}, time_rg = {{ time_lim }})
  db_sub <- db_sub |>
    dplyr::select("time", "epoch", amp_name)
  db_df <- collect(db_sub)

  label <- rlang::englue("Subject { subject }, channel { channel }")

  fig <- plot_ly(db_df, x = ~time, y = as.formula(paste0("~.data$", amp_name))) |>
    add_boxplot(hovertext = paste("Epoch :", db_df$epoch))
  fig2 <- fig |> layout(xaxis = list(title = "Time point"),
                        yaxis = list(title = TeX("\\mu V")),
                        title = label) |>
    config(mathjax = 'cdn')
  fig2
}



#' Plot interactive boxplots of subjects
#'
#' @description
#' Function for plotting interactive boxplots of EEG amplitude for individual subjects for selected channel within the chosen time interval. The interactive plotly output enables to easily determine the subjects with outlier amplitude and also allows to easily edit the image layout.
#'
#' @param data A data frame or a database table with EEG dataset. Required columns: subject, sensor, time and the column with EEG amplitude named as in \code{amplitude} parameter.
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values. Default is \code{"signal"}.
#' @param subject A vector with IDs of subjects to plot.
#' @param channel An integer or character ID of channel to plot.
#' @param time_lim A character vector with time range to plot. If not defined, the first ten time points are plotted.
#'
#' @details
#' The input data frame or database table must contain at least following columns:
#' subject - a column with subject IDs,
#' sensor - a column with sensor labels,
#' time - a column with time point numbers,
#' and a column with measured EEG signal values (or their averages) called as in \code{amplitude}.
#'
#'
#' @return A plotly object.
#'
#' @import dplyr
#' @importFrom rlang .data
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @examples
#' # Interactive boxplots of signal from channel E34 for both subjects
#' # in chosen time points
#' ## Note: it has no statistical sense to make boxplot from only 2 observations, but
#' ## larger example dataset is not possible due to size limit of the package
#' boxplot_subject(epochdata, amplitude = "signal", channel = "E34", time_lim = c(10:20))
#'
#' @export
boxplot_subject <- function(data,
                          amplitude = "signal",
                          subject = NULL,
                          channel,
                          time_lim = NULL) {

  amp_value <- {{ amplitude }}
  amp_name <- rlang::as_string(amp_value)

  if (!amp_name %in% names(data)) {
    stop(paste0("There is no column '", amp_name, "' in the input data."))
  }

  if (missing(channel)) {
    stop("Argument 'channel' is missing, with no default.")
  }

  required_cols <- c("time", "sensor", "subject")
  missing_cols <- setdiff(required_cols, colnames(data))

  if (length(missing_cols) > 0) {
    stop(paste("The following required data columns are missing:",
               paste(missing_cols, collapse = ", ")))
  }

  if (is.null(time_lim)) {
    min_t <- min(dplyr::pull(data, .data$time))
    time_lim <- c(min_t:(min_t + 9))
    warning("The argument 'time_lim' was not defined, the first ten time points from data are plotted.")
  }

  db_sub <- pick_data(data, subject_rg = {{ subject }}, sensor_rg = {{ channel }}, time_rg = {{ time_lim }})
  db_sub <- db_sub |>
    dplyr::select("time", "subject", amp_name)
  db_df <- collect(db_sub)

  label <- rlang::englue("Channel { channel }")

  fig <- plot_ly(db_df, x = ~time, y = as.formula(paste0("~.data$", amp_name))) |>
    add_boxplot(hovertext = paste("Subject :", db_df$subject))
  fig2 <- fig |> layout(xaxis = list(title = "Time point"),
                        yaxis = list(title = TeX("\\mu V")),
                        title = label) |>
    config(mathjax = 'cdn')
  fig2
}





#' Plot interactive boxplots of response times
#'
#' @description
#' Function for plotting interactive boxplots of response time in individual epochs for selected subjects. The interactive plotly output enables to easily determine the epoch number from which outliers come and also allows to easily edit the image layout.
#'
#' @param data A data frame or a database table with response times dataset. Required columns: subject, epoch, RT (value of response time in ms).
#' @param subject A character vector with subjects to plot. If missing, boxplots are drawn for all avaliable subjects in \code{data}.
#'
#' @return A plotly object.
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

boxplot_rt <- function(data, subject = NULL){

  required_cols <- c("RT", "epoch", "subject")
  missing_cols <- setdiff(required_cols, colnames(data))

  if (length(missing_cols) > 0) {
    stop(paste("The following required data columns are missing:",
               paste(missing_cols, collapse = ", ")))
  }

  if (!is.null(subject)){
    data <- pick_data(data, subject_rg = {{ subject }})
  }

  data <- data |>
    dplyr::select("subject", "epoch", "RT")
  data <- collect(data)

  fig <- plot_ly(data, x = ~subject, y = ~RT) |>
    add_boxplot(hovertext = paste("Epoch :", data$epoch)) |>
    layout(yaxis = list(title = "Response time (ms)"),
           xaxis = list(title = "Subject"))
  fig
}
