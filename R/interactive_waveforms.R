interactive_waveforms <- function(data, subject, sensor, FS = 250, t0 = 251, col.palette) {
  ## pridat argument na vyber barevne palety - nebo zjisit, jestli to jde pak predelat pridanim
  ## jako v gglotu

  if(missing(col.palette)) {
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
