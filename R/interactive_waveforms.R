#' Plot interactive waveform graph
#'
#' @description Function for plotting time series of EEG signal colour-coded by epoch, channel or subject (according to chosen level) in interactive `plotly` graph. When using the function for plotting the average, there is an option to add a confidence band using the `CI` argument. The output in \link[plotly]{plotly} format enables to easily edit the image layout.
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
#' @param CI A logical value indicating, if the confidence ribbon should be plotted. Default is `FALSE`. See Details for more information.
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
#' Plotting confidence ribbon:
#' To plot the confidence bands around the average lines (`CI = TRUE`), the input data must include the `ci_up` and `ci_low` columns (as in the output tibble from \link{compute_mean} function).
#'
#' @return A \code{plotly} object showing an interactive time series of the signal according to the chosen level.
#'
#' @import ggplot2
#' @rawNamespace import(plotly, except = last_plot)
#' @importFrom grDevices rainbow
#' @importFrom rlang .data
#' @importFrom stats as.formula setNames
#'
#' @export
#'
#' @examples
#' # 1) Plot epoch waveforms with average curve for subject 1 and electrode "E65"
#' # with 250 sampling frequency rate (default) and 10 as zero time point
#' interactive_waveforms(epochdata, amplitude = "signal", subject = 1, channel = "E65",
#' t0 = 10, level = "epoch")
#' # 2) Plot sensor level waveforms with confidence bands for subject 1 and electrodes "E65" and "E182"
#' # a) preparing data
#' sendata <- compute_mean(epochdata, amplitude = "signal", subject = 1, channel = c("E65", "E182"),
#'  group = "time", level = "epoch")
#' # b) plot the waveforms without the average
#' interactive_waveforms(sendata, amplitude = "average", subject = 1, t0 = 10,
#' level = "sensor", avg = FALSE, CI = TRUE)
interactive_waveforms <- function(data, amplitude = "signal", subject = NULL, channel = NULL,
                                  FS = 250, t0 = NULL, col_palette, level = c("epoch", "sensor", "subject"),
                                  avg = TRUE, CI = FALSE) {

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

  if (avg == TRUE) {
    avgdata <- newdata |>
      group_by(.data$time) |>
      summarise(avg = mean(.data[[amp_name]], na.rm = TRUE), .groups = "drop")
  }


  if (!is.factor(newdata[[group_name]])) { # convert to factor
    newdata[[group_name]] <- factor(newdata[[group_name]])
  }

  group_levels <- unique(newdata[[group_name]])

  if (missing(col_palette)) {
    n <- length(group_levels)
    col_palette <- rainbow(n)
  }


  base_colors <- setNames(col_palette, group_levels)
  ribbon_colors <- setNames(alpha(col_palette, 0.4), group_levels)

  if (is.null(t0)) {
    t0 <- min(newdata$time)
    warning("The argument t0 was not specified. The minimal value of time was chosen as 0 ms time point.")
  }

  k <- 1000 / FS
  k0 <- t0 * k


  p <- plot_ly()

  for (grp in group_levels) {
    data_grp <- newdata %>% filter(.data[[group_name]] == grp)

    if (CI == TRUE) { # add ribbons
      p <- p |>
        add_ribbons(
          data = data_grp,
          x = ~time * k - k0,
          ymin = ~ci_low,
          ymax = ~ci_up,
          fillcolor = ribbon_colors[[grp]],
          line = list(color = 'transparent'),
          showlegend = FALSE,
          name = paste0(grp, " CI")
        )
    }

    p <- p |> # add traces
      add_trace(
        data = data_grp,
        x = ~time * k - k0,
        y = as.formula(paste0("~.data$", amp_name)),
        type = "scatter",
        mode = "lines",
        line = list(color = base_colors[[grp]]),
        name = grp
      )
  }

  p |> plotly::layout(xaxis = list(title = "Time (ms)"),
            yaxis = list(title = TeX("\\mu V")),
            showlegend = FALSE
            ) |>
     config(mathjax = 'cdn')

}




