#' Plot boxplots of EEG amplitude on epoch level
#'
#' @description
#' Function for plotting interactive or static boxplots of EEG amplitude in individual epochs within the chosen time interval. The function assumes data from a single subject and a single sensor.
#' Both the static `ggplot` output and the interactive `plotly` output can be easily customized and edited, while the interactive `plotly` output additionally allows easy identification of the epochs corresponding to outliers.
#'
#' @param data A data frame or a database table with EEG dataset. Required columns: `epoch`, `time` and the column with EEG amplitude named as in `amplitude` parameter.
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values. Default is `"signal"`.
#' @param epoch A vector with numbers of epochs to plot. If missing, boxplots are drawn for all avaliable epochs in `data`.
#' @param time_lim A numeric vector with time range to plot.
#' @param title_label A character string specifying the title of the plot. Defaults to `NULL` for plot without title.
#' @param use_latex A logical value indicating whether to use LaTeX formatting for the y-axis title. The default is `TRUE`.
#' @param interactivity Logical. Determines whether to render an interactive `plotly` graph (default) or a static `ggplot` graph.

#'
#' @details
#' The input data frame or database table must contain at least following columns:
#' `epoch` - a column with epoch numbers,
#' `time` - a column with time point numbers,
#' and a column with measured EEG signal values (or their averages) called as in `amplitude`.
#'
#'
#' @seealso \code{\link{boxplot_subject}}
#'
#' @return A `plotly` or `ggplot2` object with boxplots of EEG amplitude for individual epochs.
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang .data
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @examples
#' # Interactive boxplots of signal from channel E34 for subject 1 (health control)
#' # in time points 10:20
#' exampledata <- epochdata |>
#' pick_data(subject_rg = 1, sensor_rg = "E34")
#' boxplot_epoch(exampledata, amplitude = "signal", time_lim = c(10:20),
#' title_label = "Subject 1, channel E34")
#'
#' # static version of the previous graph
#' boxplot_epoch(exampledata, amplitude = "signal", time_lim = c(10:20),
#' title_label = "Subject 1, channel E34", interactivity = FALSE)
#'
#' @export
boxplot_epoch <- function(data,
                          amplitude = "signal",
                          epoch = NULL,
                          time_lim,
                          title_label = NULL,
                          use_latex = TRUE,
                          interactivity = TRUE) {

  stop_if_missing_cols(data, required_cols = c("time", "epoch", amplitude))

  if (!is.numeric(time_lim)) {
    stop("'time_lim' must be a numeric vector of time points.")
  }

  db_sub <- pick_data(data, epoch_rg = {{ epoch }}, time_rg = {{ time_lim }})
  db_sub <- db_sub |>
    dplyr::select("time", "epoch", {{ amplitude }})
  db_df <- collect(db_sub)

  if (!is.null(title_label)) {
    label <- title_label
  } else {
    label = ""
  }

  if (interactivity) {
    fig <- plot_ly(db_df, x = ~time, y = as.formula(paste0("~.data$", amplitude))) |>
      add_boxplot(hovertext = paste("Epoch :", db_df$epoch))

    if (use_latex) {
      y_label <- TeX("\\mu V")
      mathjax_config <- 'cdn'
    } else {
      y_label <- "Microvolts (uV)"
      mathjax_config <- NULL
    }

    fig <- fig |>
      layout(
        xaxis = list(title = "Time point"),
        yaxis = list(title = y_label),
        title = label
      ) |>
      config(mathjax = mathjax_config)

    return(fig)
  }

  # static graph
  ggplot2::ggplot(
    db_df,
    ggplot2::aes(
      x = .data$time,
      y = .data[[amplitude]],
      group = .data$time
    )
  ) +
    ggplot2::geom_boxplot() +
    ggplot2::stat_summary(
      fun = mean, colour = "darkred", geom = "point",
      shape = 18, size = 2, show.legend = FALSE
    ) +
    ggplot2::labs(
      title = label,
      x = "Time point",
      y = expression(mu * "V")
    ) +
    ggplot2::theme_minimal()

}



#' Plot boxplots of EEG amplitude across subjects
#'
#' @description
#' Function for plotting interactive or static boxplots of EEG amplitude across subjects for a single epoch and channel, within a specified time interval. The function assumes data from a single epoch and a single sensor.
#' Both the static `ggplot` output and the interactive `plotly` output can be easily customized and edited, while the interactive `plotly` output additionally allows easy identification of the subjects corresponding to outliers.
#'
#' @param data A data frame or a database table with EEG dataset. Required columns: `subject`, `sensor`, `time` and the column with EEG amplitude named as in `amplitude` parameter.
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values. Default is \code{"signal"}.
#' @param subject A vector with IDs of subjects to plot. If missing, boxplots are drawn for all avaliable subjects in `data`.
#' @param time_lim A numeric vector with time range to plot.
#' @param title_label A character string specifying the title of the plot. Defaults to `NULL` for plot without title.
#' @param use_latex A logical value indicating whether to use LaTeX formatting for the y-axis title. The default is `TRUE`.
#' @param interactivity Logical. Determines whether to render an interactive `plotly` graph (default) or a static `ggplot` graph.
#'
#' @details
#' The input data frame or database table must contain at least following columns:
#' `subject` - a column with subject IDs,
#' `time` - a column with time point numbers,
#' and a column with measured EEG signal values (or their averages) called as in `amplitude`.
#'
#' Note: The function assumes that subject IDs are unique across the entire dataset. Using the same subject IDs in multiple groups may result in incorrect or misleading visualizations.
#'
#' @seealso \code{\link{boxplot_epoch}}
#'
#' @return A `plotly` or `ggplot2` object with boxplots of EEG amplitude across subjects.
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang .data
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @examples
#' # Interactive boxplots of signal from channel E34 in epoch 1
#' # for both subjects in chosen time points
#' ## Note: it has no statistical sense to make boxplot from only 2 observations, but
#' ## larger example dataset is not possible due to size limit of the package
#' exampledata <- epochdata |>
#' pick_data(sensor_rg = "E34", epoch_rg = 1)
#' boxplot_subject(exampledata, amplitude = "signal", time_lim = c(10:20),
#' title_label = "Sensor E34, epoch 1")
#'
#' # Static version of the previous graph
#' boxplot_subject(exampledata, amplitude = "signal", time_lim = c(10:20),
#' title_label = "Sensor E34, epoch 1", interactivity = FALSE)
#'
#' @export
boxplot_subject <- function(data,
                          amplitude = "signal",
                          subject = NULL,
                          time_lim,
                          title_label = NULL,
                          use_latex = TRUE,
                          interactivity = TRUE) {

  stop_if_missing_cols(data, required_cols = c("time", "subject", amplitude))

  if (!is.numeric(time_lim)) {
    stop("'time_lim' must be a numeric vector of time points.")
  }

  db_sub <- pick_data(data, subject_rg = {{ subject }}, time_rg = {{ time_lim }})
  db_sub <- db_sub |>
    dplyr::select("time", "subject", {{ amplitude }})
  db_df <- collect(db_sub)

  if (!is.null(title_label)) {
    label <- title_label
  } else {
    label = NULL
  }

  if (interactivity) {
    if (use_latex) {
      y_label <- TeX("\\mu V")
      mathjax_config <- 'cdn'
    } else {
      y_label <- "Microvolts (uV)"
      mathjax_config <- NULL
    }

    fig <- plot_ly(db_df, x = ~time, y = as.formula(paste0("~.data$", amplitude))) |>
      add_boxplot(hovertext = paste("Subject :", db_df$subject))

    fig <- fig |>
      layout(
        xaxis = list(title = "Time point"),
        yaxis = list(title = y_label),
        title = label
      ) |>
      config(mathjax = mathjax_config)

    return(fig)
  }

  # static graph
  ggplot2::ggplot(
    db_df,
    ggplot2::aes(
      x = .data$time,
      y = .data[[amplitude]],
      group = .data$time
      )
  ) +
    ggplot2::geom_boxplot() +
    ggplot2::stat_summary(
      fun = mean, colour = "darkred", geom = "point",
      shape = 18, size = 2, show.legend = FALSE
    ) +
    ggplot2::labs(
      title = label,
      x = "Time point",
      y = expression(mu * "V")
    ) +
    ggplot2::theme_minimal()

}



#' Plot boxplots of response times
#'
#' @description
#' Function for plotting interactive or static boxplots of response time in individual epochs for selected subjects. Both the static `ggplot` output and the interactive `plotly` output can be easily customized and edited, while the interactive `plotly` output additionally allows easy identification of the epoch numbers corresponding to outliers.
#'
#' If the `condition` column is present in the input data, the boxplots are color-coded by condition.
#'
#' @param data A data frame or a database table containing response time data. Required columns: `subject`, `epoch`, `RT` (value of response time in ms). An optional `condition` column may be used to
#'   colour-code the boxplots.
#' @param subject A vector with IDs of subjects to plot. If missing, boxplots are drawn for all available subjects in `data`.
#' @param interactivity Logical. Determines whether to render an interactive `plotly` graph (default) or a static `ggplot` graph.
#'
#' @return A `plotly` or `ggplot2` object with boxplots of response times.
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang .data
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @export
#'
#' @examples
#' # Display interactive boxplots for both example subjects
#' boxplot_rt(rtdata)
#'
#' # Display static boxplots for both example subjects
#' boxplot_rt(rtdata, interactivity = FALSE)
#'
#' # Interactive boxplots per subject divided by condition
#' # a) add condition column to data (just for example)
#' data_cond <- rtdata
#' data_cond$condition <- c(rep("a", 7), rep("b", 7), rep("a", 8), rep("b",7))
#' # b) plot boxplots (colour-coded by condition)
#' boxplot_rt(data_cond)

boxplot_rt <- function(data,
                       subject = NULL,
                       interactivity = TRUE) {
  stop_if_missing_cols(data, required_cols = c("RT", "epoch", "subject"))

  if (!is.null(subject)) {
    data <- pick_data(data, subject_rg = {{ subject }})
  }

  select_cols <- c("subject", "epoch", "RT")
  has_condition <- "condition" %in% colnames(data)

  if (has_condition) {
    select_cols <- c(select_cols, "condition")
    graph_title <- "Response time per subject and condition"
  } else {
    graph_title <- "Response time per subject"
  }

  data <- data |>
    dplyr::select(dplyr::all_of(select_cols))
  data <- collect(data)

  if (interactivity) {
    args <- list(
      data = data,
      x = ~subject,
      y = ~RT
    )

    if (has_condition) {
      args$color <- ~ factor(.data$condition)
    }

    fig <- do.call(plotly::plot_ly, args) |>
      plotly::add_boxplot(
        hovertext = ~ paste("Epoch:", epoch)
      ) |>
      plotly::layout(
        boxmode = if (has_condition) "group" else "overlay",
        title = graph_title,
        xaxis = list(title = "Subject"),
        yaxis = list(title = "Response time (ms)")
      )

    return(fig)
  }

  # static graph
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$subject,
      y = .data$RT,
      fill = if (has_condition) factor(.data$condition) else NULL,
      group = if (has_condition) interaction(.data$subject, .data$condition) else .data$subject
    )
  ) +
    ggplot2::geom_boxplot(position = position_dodge(width = 0.8)) +
    ggplot2::stat_summary(
      fun = mean, colour = "darkred", geom = "point",
      shape = 18, size = 2, show.legend = FALSE,
      position = position_dodge(width = 0.8)
    ) +
    ggplot2::labs(
      title = graph_title,
      x = "Subject",
      y = "Response time (ms)",
      fill = if (has_condition) "Condition" else NULL
    ) +
    ggplot2::theme_minimal()
}
