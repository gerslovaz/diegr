interactive_waveforms <- function() {

}



controls01 <- controls %>%
  dplyr::filter(subject == 1 & (electrode == "E65"))  %>%
  dplyr::select(time, signal, epoch)

controls01df <- as.data.frame(controls01)
controls01df <- controls01df %>%
  group_by(time) %>%
  mutate(average = mean(signal, na.rm = TRUE))
controls01df$epoch <- factor(controls01df$epoch)
curv_epoch <- controls01df %>%
  group_by(epoch) %>%
  plot_ly(x = ~time * 4 - 1004, y = ~signal, color = ~epoch,
          type = "scatter",  mode = "lines")
curv_epoch %>%
  add_trace(x = ~time * 4 - 1004, y = ~average, type = 'scatter', mode = 'lines',
            line = list(color='red'),
            name = 'Average') %>%
  layout(xaxis = list(title = "Time (ms)"),
         yaxis = list(title = TeX("\\mu V")),
         showlegend = F, title='Subject HC01, electrode E65') %>%
  config(mathjax = 'cdn')
