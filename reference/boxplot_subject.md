# Plot interactive boxplots of EEG amplitude across subjects

Function for plotting interactive boxplots of EEG amplitude across
subjects for a single epoch and channel, within a specified time
interval. The function assumes data from a single epoch and a single
sensor. The interactive plotly output enables to easily determine the
subjects with outlier amplitude and also allows to easily edit the image
layout.

## Usage

``` r
boxplot_subject(
  data,
  amplitude = "signal",
  subject = NULL,
  time_lim,
  title_label = NULL,
  use_latex = TRUE
)
```

## Arguments

- data:

  A data frame or a database table with EEG dataset. Required columns:
  `subject`, `sensor`, `time` and the column with EEG amplitude named as
  in `amplitude` parameter.

- amplitude:

  A character specifying the name of the column from input data with an
  EEG amplitude values. Default is `"signal"`.

- subject:

  A vector with IDs of subjects to plot. If missing, boxplots are drawn
  for all avaliable subjects in `data`.

- time_lim:

  A numeric vector with time range to plot.

- title_label:

  A character string specifying the title of the plot. Defaults to
  `NULL` for plot without title.

- use_latex:

  A logical value indicating whether to use LaTeX formatting for the
  y-axis title. The default is `TRUE`.

## Value

A `plotly` object with boxplots of EEG amplitude across subjects.

## Details

The input data frame or database table must contain at least following
columns: `subject` - a column with subject IDs, `time` - a column with
time point numbers, and a column with measured EEG signal values (or
their averages) called as in `amplitude`.

Note: The function assumes that subject IDs are unique across the entire
dataset. Using the same subject IDs in multiple groups may result in
incorrect or misleading visualizations.

## See also

[`boxplot_epoch`](https://gerslovaz.github.io/diegr/reference/boxplot_epoch.md)

## Examples

``` r
# Interactive boxplots of signal from channel E34 in epoch 1
# for both subjects in chosen time points
## Note: it has no statistical sense to make boxplot from only 2 observations, but
## larger example dataset is not possible due to size limit of the package
epochdata |>
pick_data(sensor_rg = "E34", epoch_rg = 1) |>
boxplot_subject(amplitude = "signal", time_lim = c(10:20),
title_label = "Sensor E34, epoch 1")

{"x":{"visdat":{"2a0213ad9465":["function () ","plotlyVisDat"]},"cur_data":"2a0213ad9465","attrs":{"2a0213ad9465":{"x":{},"y":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"box","hovertext":["Subject : 1","Subject : 1","Subject : 1","Subject : 1","Subject : 1","Subject : 1","Subject : 1","Subject : 1","Subject : 1","Subject : 1","Subject : 1","Subject : 2","Subject : 2","Subject : 2","Subject : 2","Subject : 2","Subject : 2","Subject : 2","Subject : 2","Subject : 2","Subject : 2","Subject : 2"],"inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"title":"Time point"},"yaxis":{"domain":[0,1],"automargin":true,"title":"$\\mu V$"},"title":"Sensor E34, epoch 1","hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"fillcolor":"rgba(31,119,180,0.5)","x":[10,11,12,13,14,15,16,17,18,19,20,10,11,12,13,14,15,16,17,18,19,20],"y":[-43.905323028564453,-36.785026550292969,-31.523902893066406,-28.109518051147461,-29.982431411743164,-34.491954803466797,-41.322681427001953,-52.870857238769531,-50.807582855224609,-28.422760009765625,-17.27446174621582,-9.9935894012451172,1.0134574174880981,1.0834642648696899,-2.2529289722442627,2.6003866195678711,0.2714323103427887,-1.4999648332595825,1.6285418272018433,8.1910562515258789,-0.98435550928115845,-12.244734764099121],"type":"box","hovertext":["Subject : 1","Subject : 1","Subject : 1","Subject : 1","Subject : 1","Subject : 1","Subject : 1","Subject : 1","Subject : 1","Subject : 1","Subject : 1","Subject : 2","Subject : 2","Subject : 2","Subject : 2","Subject : 2","Subject : 2","Subject : 2","Subject : 2","Subject : 2","Subject : 2","Subject : 2"],"marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
