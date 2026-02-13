# Plot interactive boxplots of response times

Function for plotting interactive boxplots of response time in
individual epochs for selected subjects. If the `condition` column is
present in the input data, the boxplots are color-coded by condition.
The interactive plotly output enables to easily determine the epoch
number from which outliers come and also allows to easily edit the image
layout.

## Usage

``` r
boxplot_rt(data, subject = NULL)
```

## Arguments

- data:

  A data frame or a database table containing response time data.
  Required columns: `subject`, `epoch`, `RT` (value of response time in
  ms). An optional `condition` column may be used to colour-code the
  boxplots.

- subject:

  A vector with IDs of subjects to plot. If missing, boxplots are drawn
  for all available subjects in `data`.

## Value

A `plotly` object with boxplots of response times.

## Examples

``` r
# Display interactive boxplots for both example subjects
boxplot_rt(rtdata)

{"x":{"visdat":{"270342826425":["function () ","plotlyVisDat"]},"cur_data":"270342826425","attrs":{"270342826425":{"x":{},"y":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"box","hovertext":["Epoch : 1","Epoch : 2","Epoch : 3","Epoch : 4","Epoch : 5","Epoch : 6","Epoch : 7","Epoch : 8","Epoch : 9","Epoch : 10","Epoch : 11","Epoch : 12","Epoch : 13","Epoch : 14","Epoch : 1","Epoch : 2","Epoch : 3","Epoch : 4","Epoch : 5","Epoch : 6","Epoch : 7","Epoch : 8","Epoch : 9","Epoch : 10","Epoch : 11","Epoch : 12","Epoch : 13","Epoch : 14","Epoch : 15"],"inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"yaxis":{"domain":[0,1],"automargin":true,"title":"Response time (ms)"},"xaxis":{"domain":[0,1],"automargin":true,"title":"Subject","type":"category","categoryorder":"array","categoryarray":["1","2"]},"title":"Response time per subject","hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"fillcolor":"rgba(31,119,180,0.5)","x":["1","1","1","1","1","1","1","1","1","1","1","1","1","1","2","2","2","2","2","2","2","2","2","2","2","2","2","2","2"],"y":[270,194,206,222,198,194,210,194,170,182,206,186,174,166,354,290,294,374,258,358,366,282,426,342,358,378,314,334,358],"type":"box","hovertext":["Epoch : 1","Epoch : 2","Epoch : 3","Epoch : 4","Epoch : 5","Epoch : 6","Epoch : 7","Epoch : 8","Epoch : 9","Epoch : 10","Epoch : 11","Epoch : 12","Epoch : 13","Epoch : 14","Epoch : 1","Epoch : 2","Epoch : 3","Epoch : 4","Epoch : 5","Epoch : 6","Epoch : 7","Epoch : 8","Epoch : 9","Epoch : 10","Epoch : 11","Epoch : 12","Epoch : 13","Epoch : 14","Epoch : 15"],"marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
# Interactive boxplots per subject divided by condition
# a) add condition column to data (just for example)
data_cond <- rtdata
data_cond$condition <- c(rep("a", 7), rep("b", 7), rep("a", 8), rep("b",7))
# b) plot boxplots (colour-coded by condition)
boxplot_rt(data_cond)

{"x":{"visdat":{"27032014f557":["function () ","plotlyVisDat"]},"cur_data":"27032014f557","attrs":{"27032014f557":{"x":{},"y":{},"color":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"box","hovertext":{},"inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"boxmode":"group","yaxis":{"domain":[0,1],"automargin":true,"title":"Response time (ms)"},"xaxis":{"domain":[0,1],"automargin":true,"title":"Subject","type":"category","categoryorder":"array","categoryarray":["1","2"]},"title":"Response time per subject and condition","hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"fillcolor":"rgba(102,194,165,0.5)","x":["1","1","1","1","1","1","1","2","2","2","2","2","2","2","2"],"y":[270,194,206,222,198,194,210,354,290,294,374,258,358,366,282],"type":"box","hovertext":["Epoch: 1","Epoch: 2","Epoch: 3","Epoch: 4","Epoch: 5","Epoch: 6","Epoch: 7","Epoch: 1","Epoch: 2","Epoch: 3","Epoch: 4","Epoch: 5","Epoch: 6","Epoch: 7","Epoch: 8"],"name":"a","marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"line":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"fillcolor":"rgba(141,160,203,0.5)","x":["1","1","1","1","1","1","1","2","2","2","2","2","2","2"],"y":[194,170,182,206,186,174,166,426,342,358,378,314,334,358],"type":"box","hovertext":["Epoch: 8","Epoch: 9","Epoch: 10","Epoch: 11","Epoch: 12","Epoch: 13","Epoch: 14","Epoch: 9","Epoch: 10","Epoch: 11","Epoch: 12","Epoch: 13","Epoch: 14","Epoch: 15"],"name":"b","marker":{"color":"rgba(141,160,203,1)","line":{"color":"rgba(141,160,203,1)"}},"line":{"color":"rgba(141,160,203,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
