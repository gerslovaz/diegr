interactive_waveforms <- function(data, subject, sensor) {
  data <- data %>%
    dplyr::filter(subject == {{ subject }} & (electrode == {{ sensor }}))  %>%
    dplyr::select(time, signal, epoch)
  data <- data %>%
    group_by(time) %>%
    mutate(average = mean(signal, na.rm = TRUE))

  label <- rlang::englue("Subject {{ subject }}, electrode { sensor }")

  curv_epoch <- data %>%
    group_by(epoch) %>%
    plot_ly(x = ~time * 4 - 1004, y = ~signal, color = ~epoch, colors = rainbow(50),
            type = "scatter",  mode = "lines")
  curv_epoch %>%
    add_trace(x = ~time * 4 - 1004, y = ~average, type = 'scatter', mode = 'lines',
              line = list(color = 'black'),
              name = 'Average') %>%
    layout(xaxis = list(title = "Time (ms)"),
           yaxis = list(title = TeX("\\mu V")),
           showlegend = F, title = label) %>%
    config(mathjax = 'cdn')

}
