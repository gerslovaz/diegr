#' Plot interactive boxplots of epochs
#'
#' @description
#' Function for plotting interactive boxplots of EEG amplitude in individual epochs for selected subject and channel within the chosen time interval. The interactive `plotly` output enables to easily determine the epoch number from which outliers come and also allows to easily edit the image layout.
#'
#' @param data A data frame or a database table with EEG dataset. Required columns: subject, sensor, epoch, time and the column with EEG amplitude named as in \code{amplitude} parameter.
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values. Default is \code{"signal"}.
#' @param subject An integer or character ID of selected subject to plot.
#' @param channel An integer or character ID of channel to plot.
#' @param time_lim A numeric vector with time range to plot.
#' @param use_latex A logical value indicating whether to use LaTeX formatting for the y-axis title. The default is `TRUE`.
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
                          subject,
                          channel,
                          time_lim,
                          use_latex = TRUE) {

  amp_value <- {{ amplitude }}
  amp_name <- rlang::as_string(amp_value)

  if (!amp_name %in% colnames(data)) {
    stop(paste0("There is no column '", amp_name, "' in the input data."))
  }

  if (!is.numeric(time_lim)) {
    stop("'time_lim' must be a numeric vector of time points.")
  }

  required_cols <- c("time", "epoch", "sensor", "subject")
  missing_cols <- setdiff(required_cols, colnames(data))

  if (length(missing_cols) > 0) {
    stop(paste("The following required data columns are missing:",
               paste(missing_cols, collapse = ", ")))
  }


  db_sub <- pick_data(data, subject_rg = {{ subject }}, sensor_rg = {{ channel }}, time_rg = {{ time_lim }})
  db_sub <- db_sub |>
    dplyr::select("time", "epoch", {{ amplitude }})
  db_df <- collect(db_sub)

  label <- rlang::englue("Subject { subject }, channel { channel }")

  fig <- plot_ly(db_df, x = ~time, y = as.formula(paste0("~.data$", amp_name))) |>
    add_boxplot(hovertext = paste("Epoch :", db_df$epoch))

  if (use_latex) {
    y_label <- TeX("\\mu V")
    mathjax_config <- 'cdn'
  } else {
    y_label <- "Microvolts (uV)"
    mathjax_config <- NULL
  }

  fig2 <- fig |> layout(xaxis = list(title = "Time point"),
                        yaxis = list(title = y_label),
                        title = label) |>
    config(mathjax = mathjax_config)

  return(fig2)
}



#' Plot interactive boxplots of subjects
#'
#' @description
#' Function for plotting interactive boxplots of EEG amplitude for individual subjects for selected channel within the chosen time interval. The interactive plotly output enables to easily determine the subjects with outlier amplitude and also allows to easily edit the image layout.
#'
#' @param data A data frame or a database table with EEG dataset. Required columns: subject, sensor, time and the column with EEG amplitude named as in \code{amplitude} parameter.
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values. Default is \code{"signal"}.
#' @param subject A vector with IDs of subjects to plot. If missing, boxplots are drawn for all avaliable subjects in \code{data}.
#' @param channel An integer or character ID of channel to plot.
#' @param time_lim A numeric vector with time range to plot.
#' @param use_latex A logical value indicating whether to use LaTeX formatting for the y-axis title. The default is `TRUE`.
#'
#' @details
#' The input data frame or database table must contain at least following columns:
#' subject - a column with subject IDs,
#' sensor - a column with sensor labels,
#' time - a column with time point numbers,
#' and a column with measured EEG signal values (or their averages) called as in \code{amplitude}.
#'
#'
#' @return A `plotly` object with boxplots of EEG amplitude for subjects.
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
#' epoch1 <- epochdata|> dplyr::filter(epoch == 1)
#' boxplot_subject(epoch1, amplitude = "signal", channel = "E34", time_lim = c(10:20))
#'
#' @export
boxplot_subject <- function(data,
                          amplitude = "signal",
                          subject = NULL,
                          channel,
                          time_lim,
                          use_latex = TRUE) {

  amp_value <- {{ amplitude }}
  amp_name <- rlang::as_string(amp_value)

  if (!amp_name %in% colnames(data)) {
    stop(paste0("There is no column '", amp_name, "' in the input data."))
  }

  if (!is.numeric(time_lim)) {
    stop("'time_lim' must be a numeric vector of time points.")
  }

  required_cols <- c("time", "sensor", "subject")
  missing_cols <- setdiff(required_cols, colnames(data))

  if (length(missing_cols) > 0) {
    stop(paste("The following required data columns are missing:",
               paste(missing_cols, collapse = ", ")))
  }

  db_sub <- pick_data(data, subject_rg = {{ subject }}, sensor_rg = {{ channel }}, time_rg = {{ time_lim }})
  db_sub <- db_sub |>
    dplyr::select("time", "subject", {{ amplitude }})
  db_df <- collect(db_sub)

  label <- rlang::englue("Boxplots across subjects for channel { channel }")

  fig <- plot_ly(db_df, x = ~time, y = as.formula(paste0("~.data$", amp_name))) |>
    add_boxplot(hovertext = paste("Subject :", db_df$subject))

  if (use_latex) {
    y_label <- TeX("\\mu V")
    mathjax_config <- 'cdn'
  } else {
    y_label <- "Microvolts (uV)"
    mathjax_config <- NULL
  }

  fig2 <- fig |> layout(xaxis = list(title = "Time point"),
                        yaxis = list(title = y_label),
                        title = label) |>
    config(mathjax = mathjax_config)

  return(fig2)
}





#' Plot interactive boxplots of response times
#'
#' @description
#' Function for plotting interactive boxplots of response time in individual epochs for selected subjects. The interactive plotly output enables to easily determine the epoch number from which outliers come and also allows to easily edit the image layout.
#'
#' @param data A data frame or a database table with response times dataset. Required columns: subject, epoch, RT (value of response time in ms).
#' @param subject A vector with IDs of subjects to plot. If missing, boxplots are drawn for all available subjects in \code{data}.
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

boxplot_rt <- function(data, subject = NULL){

  required_cols <- c("RT", "epoch", "subject")
  missing_cols <- setdiff(required_cols, colnames(data))

  if (length(missing_cols) > 0) {
    stop(paste("The following required data columns are missing:",
               paste(missing_cols, collapse = ", ")))
  }

  if (!is.null(subject)) {
    data <- pick_data(data, subject_rg = {{ subject }})
  }

  data <- data |>
    dplyr::select("subject", "epoch", "RT")
  data <- collect(data)

  fig <- plot_ly(data, x = ~subject, y = ~RT) |>
    add_boxplot(hovertext = paste("Epoch :", data$epoch)) |>
    layout(yaxis = list(title = "Response time (ms)"),
           xaxis = list(title = "Subject"),
           title = "Response time per subject")
  fig
}
