# Calculate mean in temporal or spatial domain

Calculate a pointwise or a jackknife (leave-one-out) average signal
across temporal or spatial domain together with standard error and
pointwise confidence interval (CI) bounds. Pointwise averages can be
computed in two ways: standard (un-weighted) by default, or weighted
using the values in the column specified by `weights_col`.

The function computes an average at group, subject, sensor/time point,
condition or epoch level (according to the `level` parameter). For the
option `level = "epoch"` the epochs are averaged etc. Function assumes
pre-prepared data according to the chosen level.

## Usage

``` r
compute_mean(
  data,
  amplitude = "signal_base",
  domain = c("time", "space"),
  level = c("epoch", "condition", "sensor", "subject", "group"),
  type = c("point", "jack"),
  weights_col = NULL,
  R = NULL,
  alpha = 0.95
)
```

## Arguments

- data:

  A data frame, tibble or a database table with input data, required
  columns: `time` or `sensor` (according to the selected domain), the
  column with the EEG amplitude specified in the argument `amplitude`
  and columns corresponding to the selected `level`.

- amplitude:

  A character specifying the name of the column from input data with an
  EEG amplitude values. Default is `"signal_base"` for computing average
  from baseline corrected signal.

- domain:

  A character specifying the domain over which the average is computed.
  One of `"time"` or `"space"`. Option `"time"` computes a time-resolved
  average at each time point, whereas `"space"` computes a
  space-resolved average at each sensor.

- level:

  A character specifying the level of average calculation. The possible
  values are `"epoch"`,`"condition"`, `"sensor"`, `"subject"` and
  `"group"`. See Details for more information.

- type:

  A character specifying the method of calculating the average,
  `"point"` for pointwise average and `"jack"` for jackknife
  leave-one-out average.

- weights_col:

  A character specifying the name of the column containing observation
  weights. If `NULL`, un-weighted standard pointwise average is
  computed.

- R:

  The number of replications used in bootstrap interval calculation.
  Required only for computing pointwise mean. Default value is 1000.

- alpha:

  A number indicating confidence interval level. The default value is
  0.95 for 95% confidence intervals.

## Value

A tibble with resulting average and CI bounds according to the chosen
`level`, `domain` and `alpha` arguments. The statistics are saved in
columns

- `average` for computed average amplitude value,

- `n` for number of observations used in average computing,

- `se` for standard error of the mean,

- `ci_low` for lower bound of the confidence interval and

- `ci_up` for upper bound of the confidence interval.

## Details

The function supports averaging at different hierarchical levels of the
data (using `level` argument):

- `"epoch"`: averaging across epochs. Returns the average curve (time
  domain) or sensor array (space domain) for each combination of other
  grouping variables.

- `"condition"`: averages across experimental conditions.

- `"sensor"`: averages across sensors (space domain) or time points
  (time domain).

- `"subject"`: averages across subjects.

- `"group"`: averages across groups of subjects (highest aggregation
  level). The function assumes input adapted to the desired level of
  averaging (i.e. for `epoch` level the `epoch` column must be present
  in `data` etc.). For all levels higher than epochs, the averages of
  the lower level are assumed as the input data.

Weighted vs un-weighted average (`type = "point"`):

- If `weights_col` is `NULL`, each observation is treated equally (with
  weight = 1), producing a standard un-weighted mean, standard errors
  (SE), and CI.

- If `weight_cols` is provided, a weighted average is computed using the
  values in the specified column as weights. SE and CI are computed
  based on the weighted variance.

Computing standard error of the mean:

- For `type = "point"`, the standard error is computed as sample
  standard deviation divided by square root of the sample size for
  standard mean or its weighted alternative (if `weights_col` is
  specified).

- For `type = "jack"`, the standard error is jackknife standard error of
  the mean (for the exact formula see Efron and Tibshirani 1994).

Computing point confidence intervals: For each average value, the upper
and lower bounds of the point confidence interval are also available.

- Setting `type = "point"` and `R`: the bounds are computed using
  percentile method from bootstrapping with `R` replicates (can be slow
  for large amounts of data).

- Setting `type = "point"` without specifying `R`: the bounds are
  computed using standard error of the mean and approximation by the
  Student distribution.

- Setting `type = "jack"`: the bounds are computed using jackknife
  standard error of the mean and approximation by the Student
  t-distribution. Note: used method assumes equal variance and symmetric
  distribution, which may be problematic for very small samples.

Note: If there are `NA`'s in `amplitude` or `weights_col` columns,
corresponding rows are ignored in the average calculation and function
prints a warning message.

## References

Efron B., Tibshirani RJ. *An Introduction to the Bootstrap.* Chapman &
Hall/CRC; 1994.

## Examples

``` r
# Average (pointwise) raw signal for subject 1 and electrode E1
# without outlier epoch 14
avg_data <- epochdata |>
pick_data(subject_rg = 1, epoch_rg = 1:13, sensor_rg = "E1") |>
compute_mean(amplitude = "signal", level = "epoch", domain = "time")
str(avg_data)
#> 'data.frame':    50 obs. of  8 variables:
#>  $ subject: Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ sensor : chr  "E1" "E1" "E1" "E1" ...
#>  $ time   : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ average: num  8.59 9.99 9.41 11.68 9.75 ...
#>  $ n      : int  13 13 13 13 13 13 13 13 13 13 ...
#>  $ se     : num  3.06 2.69 3.24 3.19 3.73 ...
#>  $ ci_low : num  1.92 4.13 2.35 4.73 1.61 ...
#>  $ ci_up  : num  15.3 15.8 16.5 18.6 17.9 ...
# \donttest{
# plot the result using interactive plot with pointwise CI
avg_data |>
pick_data(subject = 1) |>
interactive_waveforms(amplitude = "average", t0 = 10,
level = "sensor", avg = FALSE, CI = TRUE)

{"x":{"visdat":{"2cef24271e29":["function () ","plotlyVisDat"],"2cef41dbec8a":["function () ","data"],"2cef497f81c9":["function () ","data"]},"cur_data":"2cef497f81c9","attrs":{"2cef41dbec8a":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"ymin":{},"ymax":{},"type":"scatter","mode":"lines","hoveron":"points","fill":"toself","fillcolor":"#FF000066","line":{"color":"transparent"},"showlegend":false,"name":"E1 CI","inherit":true},"2cef497f81c9":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines","line":{"color":"#FF0000"},"name":"E1","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"title":"Time (ms)"},"yaxis":{"domain":[0,1],"automargin":true,"title":"$\\mu V$"},"showlegend":false,"hovermode":"closest"},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"fillcolor":"#FF000066","x":[-36,-32,-28,-24,-20,-16,-12,-8,-4,0,4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100,104,108,112,116,120,124,128,132,136,140,144,148,152,156,160,160,160,156,152,148,144,140,136,132,128,124,120,116,112,108,104,100,96,92,88,84,80,76,72,68,64,60,56,52,48,44,40,36,32,28,24,20,16,12,8,4,0,-4,-8,-12,-16,-20,-24,-28,-32,-36],"type":"scatter","mode":"lines","hoveron":"points","fill":"toself","line":{"color":"transparent"},"showlegend":false,"name":"E1 CI","y":[1.9151481854895946,4.1327609507790637,2.3456931081540615,4.7275114935180751,1.6075625421587372,1.9178721041983913,1.6866479781594563,1.0218066378410082,1.4279473429145852,1.3954184744393263,1.9544335006266795,0.47552287183866238,0.79387678813637574,1.9869677385064346,2.5815285862902968,2.0904266320262836,1.9523392779160487,1.5957569149393134,-1.2941402361867649,-1.5441199891265676,1.097004383150038,1.9990584258880348,-0.64937419931719287,0.086371541693701204,1.4997909758502646,-0.007815695390658739,-0.53282888247832716,-1.2714140052586664,-2.1996318105034227,-1.6428667868769242,-1.1884568231970603,-1.9933007874171933,0.036450274820523099,-0.62014086074287977,-3.1396565205118367,-0.94765108235273576,0.41969723581157048,-0.31194008445937893,-1.4468890969238091,-1.4571726073666715,-0.44963744162386821,-0.68134793432749152,0.38330882224150287,-1.4013663097211539,-0.096933057235587761,3.3977508337550093,0.3270884102318794,-0.99630114744637055,-0.1449325029145152,0.14355500829917567,0.14355500829917567,14.990122556835313,15.809886871799261,14.959784150915985,15.826951504559521,11.561662401249123,9.3896019867024236,12.838958559371374,15.240160182022741,14.883496701945525,14.598552347318684,12.781262923112099,12.639938014254039,15.390849389885169,11.289021151168633,10.166817241982557,12.528422158203041,13.492503180982606,14.920705503037329,12.496959302173615,14.439775739302366,13.905036111311645,13.389968624796918,14.363989711227083,14.446173962633919,15.399927198551577,17.458610447383943,16.145302102445516,14.465927810338968,17.312763195309888,17.540994766319194,15.209879149095016,13.440276964803626,14.593984623181838,14.351390512991589,15.527694386750245,15.651534799746504,14.448686094736127,12.272623343764117,13.251998786105062,14.31620575845494,16.438595904621856,17.050693561878788,17.445393457995927,15.845352361788759,15.618538012033966,17.882854131354819,18.634582358598436,16.469053675175829,15.84221423168084,15.264058405896467],"marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[-36,-32,-28,-24,-20,-16,-12,-8,-4,0,4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100,104,108,112,116,120,124,128,132,136,140,144,148,152,156,160],"y":[8.5896032956930313,9.9874875912299519,9.4073733916649456,11.681046926058256,9.7452083367567788,8.7682050581161786,8.7660001699741077,9.2336000479184666,9.2393204523966865,8.917007189530592,8.1353196295408097,6.8637608289718628,6.5332500659502468,8.2178269166212807,9.1165316930184002,8.8090605093882637,8.1518648954538193,8.0948707690605755,6.0730683643084307,6.8328795799842243,9.3189995747346153,9.6559108105989608,6.9082768055108881,8.1158368220696087,9.4792007116171035,7.6960557515804586,6.956672540077796,6.5462878529842081,5.5951684071467476,6.1310846622173605,6.6256594580526533,5.251829257378211,7.4785778889289265,6.4361811601198635,4.694382818845602,4.6095830798149109,5.8543591934901018,7.5394546527128954,5.5965244586651144,5.6620451578727131,7.0744574528474073,7.1010743838090162,7.8117345021321221,5.7187961248251105,4.6463344647334175,7.4797066175020657,8.0770199573957004,6.981741501734807,7.8324771844423733,7.5668387825672445],"type":"scatter","mode":"lines","line":{"color":"#FF0000"},"name":"E1","marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}# }

# Jackknife average signal for subject 1 in all electrodes in time point 11 with baseline correction
# on interval 1:10 (again without outlier epoch 14)
# a) prepare corrected data
basedata <- pick_data(epochdata, subject_rg = 1) |>
baseline_correction(baseline_range = 1:10, type = "absolute")
# b) filter time point 11 (without epoch 14) and compute the average
avg_data <- pick_data(basedata, time_rg = 11, epoch_rg = 1:13) |>
compute_mean(amplitude = "signal_base", level = "epoch", domain = "space", type = "jack")
str(avg_data)
#> 'data.frame':    204 obs. of  7 variables:
#>  $ subject: Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ time   : int  11 11 11 11 11 11 11 11 11 11 ...
#>  $ sensor : chr  "E1" "E2" "E3" "E4" ...
#>  $ average: num  -1.298 -0.975 -0.547 -0.722 -0.468 ...
#>  $ se     : num  1.522 0.895 1.07 1.616 0.895 ...
#>  $ ci_low : num  -4.61 -2.92 -2.88 -4.24 -2.42 ...
#>  $ ci_up  : num  2.017 0.975 1.785 2.799 1.482 ...
# c) plot the result with topo_plot()
topo_plot(data = avg_data, amplitude = "average")


# Space average on subject level (average for all included subjects in time point 11)
# a) compute mean from all epochs for each subject
mean_epoch <- epochdata |>
pick_data(time_rg = 11, epoch_rg = 1:13) |>
compute_mean(amplitude = "signal", level = "epoch", domain = "space", type = "point")
# b) compute mean on subject level
mean_subjects <- compute_mean(mean_epoch, amplitude = "average", level = "subject",
domain = "space", type = "point")
head(mean_subjects)
#>   sensor   average n        se    ci_low     ci_up
#> 1     E1  4.434246 2 3.7010733 -42.59235 51.460841
#> 2     E2 -2.238430 2 2.5388228 -34.49723 30.020372
#> 3     E3 -2.996385 2 2.2566554 -31.66991 25.677141
#> 4     E4 -8.130263 2 6.7581756 -94.00103 77.740500
#> 5     E5 -3.230486 2 2.6041078 -36.31881 29.857841
#> 6     E6 -2.121224 2 0.7030299 -11.05407  6.811618
# c) compute weighted mean with number of observations as weights
weighted_mean_subjects <- compute_mean(mean_epoch, amplitude = "average", level = "subject",
domain = "space", type = "point", weights_col = "n")
head(weighted_mean_subjects)
#>   sensor   average n        se     ci_low     ci_up
#> 1     E1  4.434246 2 0.7402147   2.909746  5.958747
#> 2     E2 -2.238430 2 0.5077646  -3.284191 -1.192669
#> 3     E3 -2.996385 2 0.4513311  -3.925919 -2.066852
#> 4     E4 -8.130263 2 1.3516351 -10.914007 -5.346518
#> 5     E5 -3.230486 2 0.5208216  -4.303138 -2.157834
#> 6     E6 -2.121224 2 0.1406060  -2.410808 -1.831641
```
