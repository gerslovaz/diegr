# Plot interactive boxplots of EEG amplitude on epoch level

Function for plotting interactive boxplots of EEG amplitude in
individual epochs within the chosen time interval. The function assumes
data from a single subject and a single sensor. The interactive `plotly`
output enables to easily determine the epoch number from which outliers
come and also allows to easily edit the image layout.

## Usage

``` r
boxplot_epoch(
  data,
  amplitude = "signal",
  epoch = NULL,
  time_lim,
  title_label = NULL,
  use_latex = TRUE
)
```

## Arguments

- data:

  A data frame or a database table with EEG dataset. Required columns:
  `epoch`, `time` and the column with EEG amplitude named as in
  `amplitude` parameter.

- amplitude:

  A character specifying the name of the column from input data with an
  EEG amplitude values. Default is `"signal"`.

- epoch:

  A vector with numbers of epochs to plot. If missing, boxplots are
  drawn for all avaliable epochs in `data`.

- time_lim:

  A numeric vector with time range to plot.

- title_label:

  A character string specifying the title of the plot. Defaults to
  `NULL` for plot without title.

- use_latex:

  A logical value indicating whether to use LaTeX formatting for the
  y-axis title. The default is `TRUE`.

## Value

A `plotly` object with boxplots of EEG amplitude for individual epochs.

## Details

The input data frame or database table must contain at least following
columns: `epoch` - a column with epoch numbers, `time` - a column with
time point numbers, and a column with measured EEG signal values (or
their averages) called as in `amplitude`.

## See also

[`boxplot_subject`](https://gerslovaz.github.io/diegr/reference/boxplot_subject.md)

## Examples

``` r
# Interactive boxplots of signal from channel E34 for subject 1 (health control)
# in time points 10:20
epochdata |>
pick_data(subject_rg = 1, sensor_rg = "E34") |>
boxplot_epoch(amplitude = "signal", time_lim = c(10:20),
title_label = "Subject 1, channel E34")

{"x":{"visdat":{"29694d80ccf5":["function () ","plotlyVisDat"]},"cur_data":"29694d80ccf5","attrs":{"29694d80ccf5":{"x":{},"y":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"box","hovertext":["Epoch : 1","Epoch : 1","Epoch : 1","Epoch : 1","Epoch : 1","Epoch : 1","Epoch : 1","Epoch : 1","Epoch : 1","Epoch : 1","Epoch : 1","Epoch : 2","Epoch : 2","Epoch : 2","Epoch : 2","Epoch : 2","Epoch : 2","Epoch : 2","Epoch : 2","Epoch : 2","Epoch : 2","Epoch : 2","Epoch : 3","Epoch : 3","Epoch : 3","Epoch : 3","Epoch : 3","Epoch : 3","Epoch : 3","Epoch : 3","Epoch : 3","Epoch : 3","Epoch : 3","Epoch : 4","Epoch : 4","Epoch : 4","Epoch : 4","Epoch : 4","Epoch : 4","Epoch : 4","Epoch : 4","Epoch : 4","Epoch : 4","Epoch : 4","Epoch : 5","Epoch : 5","Epoch : 5","Epoch : 5","Epoch : 5","Epoch : 5","Epoch : 5","Epoch : 5","Epoch : 5","Epoch : 5","Epoch : 5","Epoch : 6","Epoch : 6","Epoch : 6","Epoch : 6","Epoch : 6","Epoch : 6","Epoch : 6","Epoch : 6","Epoch : 6","Epoch : 6","Epoch : 6","Epoch : 7","Epoch : 7","Epoch : 7","Epoch : 7","Epoch : 7","Epoch : 7","Epoch : 7","Epoch : 7","Epoch : 7","Epoch : 7","Epoch : 7","Epoch : 8","Epoch : 8","Epoch : 8","Epoch : 8","Epoch : 8","Epoch : 8","Epoch : 8","Epoch : 8","Epoch : 8","Epoch : 8","Epoch : 8","Epoch : 9","Epoch : 9","Epoch : 9","Epoch : 9","Epoch : 9","Epoch : 9","Epoch : 9","Epoch : 9","Epoch : 9","Epoch : 9","Epoch : 9","Epoch : 10","Epoch : 10","Epoch : 10","Epoch : 10","Epoch : 10","Epoch : 10","Epoch : 10","Epoch : 10","Epoch : 10","Epoch : 10","Epoch : 10","Epoch : 11","Epoch : 11","Epoch : 11","Epoch : 11","Epoch : 11","Epoch : 11","Epoch : 11","Epoch : 11","Epoch : 11","Epoch : 11","Epoch : 11","Epoch : 12","Epoch : 12","Epoch : 12","Epoch : 12","Epoch : 12","Epoch : 12","Epoch : 12","Epoch : 12","Epoch : 12","Epoch : 12","Epoch : 12","Epoch : 13","Epoch : 13","Epoch : 13","Epoch : 13","Epoch : 13","Epoch : 13","Epoch : 13","Epoch : 13","Epoch : 13","Epoch : 13","Epoch : 13","Epoch : 14","Epoch : 14","Epoch : 14","Epoch : 14","Epoch : 14","Epoch : 14","Epoch : 14","Epoch : 14","Epoch : 14","Epoch : 14","Epoch : 14"],"inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"title":"Time point"},"yaxis":{"domain":[0,1],"automargin":true,"title":"$\\mu V$"},"title":"Subject 1, channel E34","hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"fillcolor":"rgba(31,119,180,0.5)","x":[10,11,12,13,14,15,16,17,18,19,20,10,11,12,13,14,15,16,17,18,19,20,10,11,12,13,14,15,16,17,18,19,20,10,11,12,13,14,15,16,17,18,19,20,10,11,12,13,14,15,16,17,18,19,20,10,11,12,13,14,15,16,17,18,19,20,10,11,12,13,14,15,16,17,18,19,20,10,11,12,13,14,15,16,17,18,19,20,10,11,12,13,14,15,16,17,18,19,20,10,11,12,13,14,15,16,17,18,19,20,10,11,12,13,14,15,16,17,18,19,20,10,11,12,13,14,15,16,17,18,19,20,10,11,12,13,14,15,16,17,18,19,20,10,11,12,13,14,15,16,17,18,19,20],"y":[-43.905323028564453,-36.785026550292969,-31.523902893066406,-28.109518051147461,-29.982431411743164,-34.491954803466797,-41.322681427001953,-52.870857238769531,-50.807582855224609,-28.422760009765625,-17.27446174621582,-14.138941764831543,-14.733395576477051,-9.6068248748779297,-9.6599225997924805,-11.581780433654785,-13.714192390441895,-17.112525939941406,-23.138612747192383,-7.5622334480285645,-5.2162351608276367,-6.8843412399291992,-12.325247764587402,-12.868644714355469,-6.9835896492004395,-9.8436708450317383,-5.6302680969238281,-0.93420702219009399,-6.0381193161010742,-24.173931121826172,-13.199359893798828,-4.2748441696166992,-16.947486877441406,-38.6953125,-29.613872528076172,-24.35528564453125,-31.318019866943359,-26.75810432434082,-23.480194091796875,-26.039487838745117,-24.426090240478516,-33.176338195800781,-41.952011108398438,-30.893589019775391,13.956737518310547,17.625396728515625,7.5035223960876465,0.72521591186523438,-3.1802992820739746,-0.53345370292663574,-0.80198925733566284,6.1910557746887207,15.383658409118652,10.455982208251953,-5.5118212699890137,0.85229086875915527,-0.7971498966217041,2.8530476093292236,-3.3241369724273682,-8.8038444519042969,-6.6609649658203125,-2.0853073596954346,1.4522066116333008,2.3966901302337646,3.5075995922088623,-0.38455358147621155,-1.128725528717041,-9.129094123840332,-16.794601440429688,-15.758330345153809,-14.903970718383789,-26.184560775756836,-29.443429946899414,-20.068275451660156,-7.8588275909423828,1.3875503540039062,-6.5238499641418457,-26.785842895507812,-27.585433959960938,-16.262416839599609,-12.322946548461914,-12.874764442443848,-22.400554656982422,-19.924057006835938,-11.22941780090332,-19.624122619628906,-22.913843154907227,-25.137720108032227,0.30110016465187073,-2.2920103073120117,1.0696816444396973,0.23176893591880798,-2.2400367259979248,8.5870513916015625,12.164501190185547,4.2018179893493652,1.4046486616134644,5.8682107925415039,-0.94738936424255371,-24.097875595092773,-26.362556457519531,-35.343185424804688,-32.683319091796875,-21.179233551025391,-15.223004341125488,-14.407870292663574,-15.24314022064209,-19.323890686035156,-26.012121200561523,-38.790164947509766,4.0593328475952148,3.3774256706237793,2.4022586345672607,-9.3209686279296875,-13.948081016540527,-17.428079605102539,-8.6278581619262695,-5.7572731971740723,-5.4775791168212891,-0.43980655074119568,-0.94631308317184448,-2.138502836227417,1.9502149820327759,-3.166562557220459,-10.557832717895508,-11.751588821411133,-11.100879669189453,-10.675412178039551,-18.674882888793945,-19.377756118774414,-7.628605842590332,-2.5207555294036865,0.65415316820144653,19.880067825317383,36.803520202636719,32.531696319580078,24.477428436279297,18.788896560668945,12.229158401489258,13.612020492553711,14.762633323669434,14.776350021362305,9.8632001876831055,-527.04180908203125,-526.5350341796875,-524.755126953125,-514.60089111328125,-516.84942626953125,-528.60430908203125,-521.3115234375,-520.3096923828125,-527.114990234375,-535.426025390625,-541.60113525390625],"type":"box","hovertext":["Epoch : 1","Epoch : 1","Epoch : 1","Epoch : 1","Epoch : 1","Epoch : 1","Epoch : 1","Epoch : 1","Epoch : 1","Epoch : 1","Epoch : 1","Epoch : 2","Epoch : 2","Epoch : 2","Epoch : 2","Epoch : 2","Epoch : 2","Epoch : 2","Epoch : 2","Epoch : 2","Epoch : 2","Epoch : 2","Epoch : 3","Epoch : 3","Epoch : 3","Epoch : 3","Epoch : 3","Epoch : 3","Epoch : 3","Epoch : 3","Epoch : 3","Epoch : 3","Epoch : 3","Epoch : 4","Epoch : 4","Epoch : 4","Epoch : 4","Epoch : 4","Epoch : 4","Epoch : 4","Epoch : 4","Epoch : 4","Epoch : 4","Epoch : 4","Epoch : 5","Epoch : 5","Epoch : 5","Epoch : 5","Epoch : 5","Epoch : 5","Epoch : 5","Epoch : 5","Epoch : 5","Epoch : 5","Epoch : 5","Epoch : 6","Epoch : 6","Epoch : 6","Epoch : 6","Epoch : 6","Epoch : 6","Epoch : 6","Epoch : 6","Epoch : 6","Epoch : 6","Epoch : 6","Epoch : 7","Epoch : 7","Epoch : 7","Epoch : 7","Epoch : 7","Epoch : 7","Epoch : 7","Epoch : 7","Epoch : 7","Epoch : 7","Epoch : 7","Epoch : 8","Epoch : 8","Epoch : 8","Epoch : 8","Epoch : 8","Epoch : 8","Epoch : 8","Epoch : 8","Epoch : 8","Epoch : 8","Epoch : 8","Epoch : 9","Epoch : 9","Epoch : 9","Epoch : 9","Epoch : 9","Epoch : 9","Epoch : 9","Epoch : 9","Epoch : 9","Epoch : 9","Epoch : 9","Epoch : 10","Epoch : 10","Epoch : 10","Epoch : 10","Epoch : 10","Epoch : 10","Epoch : 10","Epoch : 10","Epoch : 10","Epoch : 10","Epoch : 10","Epoch : 11","Epoch : 11","Epoch : 11","Epoch : 11","Epoch : 11","Epoch : 11","Epoch : 11","Epoch : 11","Epoch : 11","Epoch : 11","Epoch : 11","Epoch : 12","Epoch : 12","Epoch : 12","Epoch : 12","Epoch : 12","Epoch : 12","Epoch : 12","Epoch : 12","Epoch : 12","Epoch : 12","Epoch : 12","Epoch : 13","Epoch : 13","Epoch : 13","Epoch : 13","Epoch : 13","Epoch : 13","Epoch : 13","Epoch : 13","Epoch : 13","Epoch : 13","Epoch : 13","Epoch : 14","Epoch : 14","Epoch : 14","Epoch : 14","Epoch : 14","Epoch : 14","Epoch : 14","Epoch : 14","Epoch : 14","Epoch : 14","Epoch : 14"],"marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
