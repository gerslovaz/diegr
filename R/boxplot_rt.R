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
    data <- pick_data(data, subject.rg = {{ subject }})
  }

  data <- data |>
    dplyr::select(subject, epoch, RT)
  data <- collect(data)

  fig <- plot_ly(data, x = ~subject, y = ~RT) |>
    add_boxplot(hovertext = paste("Epoch :", data$epoch)) |>
    layout(yaxis = list(title = "Response time (ms)"),
           xaxis = list(title = "Subject"))
  fig
}
