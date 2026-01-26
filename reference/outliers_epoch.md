# Select outlier epochs

Function identifies epochs with outlier values in a numeric EEG
amplitude variable in chosen time points. Outliers are detected
separately at each time point within the groups present in the data. The
function then summarizes how many times each epoch was marked as an
outlier across all time points.

Epochs are marked as outliers based on one of the following criteria:
interquartile range criterion, percentile approach or Hampel filter
method.

## Usage

``` r
outliers_epoch(
  data,
  amplitude = "signal",
  time = NULL,
  method = c("iqr", "percentile", "hampel"),
  k_iqr = 1.5,
  k_mad = 3,
  p = 0.975,
  print_tab = TRUE
)
```

## Arguments

- data:

  A data frame, tibble or a database table with input data, required
  columns: `time`, `epoch` and the column with EEG amplitude specified
  by `amplitude` parameter. Optional columns: `group`, `subject`,
  `sensor`, `condition`.

- amplitude:

  A character specifying the name of the column from input data with an
  EEG amplitude values. Default is `"signal"`.

- time:

  A vector with time range for outliers detection. If not defined, the
  outliers are searched for all time points in the dataset.

- method:

  A character denoting the method used for outlier detection. The
  options are: `"iqr"` for interquartile range (IQR) criterion (default
  value), `"percentile"` for percentile method and `"hampel"` for Hampel
  filter method. See details for further information about methods.

- k_iqr:

  A positive numeric value denoting the scaling factor used in the IQR
  criterion. Default value is `k_iqr = 1.5`.

- k_mad:

  A positive numeric value denoting the scaling factor used in the
  Hampel filter method. Default value is `k_mad = 3`.

- p:

  A probability value from `[0,1]` interval determining percentile to
  the percentile method (according to `probs` argument in
  [`quantile()`](https://rdrr.io/r/stats/quantile.html) function). The
  default value is set to 0.975 for the interval formed by the 2.5 and
  97.5 percentiles.

- print_tab:

  Logical. Indicates, whether result table should be printed in console.
  Default is `TRUE`.

## Value

A list with following components:

- epoch_table:

  A data frame with epoch ID and the number of time points in which the
  epoch was evaluated as outlier. (Only epochs with occurrence of
  outliers in at least one time point are included.)

- outliers_data:

  A data frame with subset of data corresponding to the outliers found.
  (The full record for each flagged point from `epoch_table`.)

With the setting `print_tab = TRUE`, the `epoch_table` is also printed
to the console.

## Details

The input data frame or database table must contain at least following
columns: `epoch` - a column with epoch numbers/labels, `time` - a column
with time point numbers, signal (or other name specified in `amplitude`
parameter) - a column with measured EEG signal values.

The outlier detection method is chosen through `method` argument. The
possibilities are

- `iqr`: The interquartile range criterion, values outside the interval
  `[lower quartile - k_iqr * IQR, upper quartile + k_iqr * IQR]` are
  considered as outliers. IQR denotes interquartile range and `k_iqr`
  the scaling factor.

- `percentile`: The percentile method, values outside the interval
  defined by the chosen percentiles are considered as outliers. Note:
  chosing small `p`leads to marking too many (or all) values.

- `hampel`: The Hampel filter method, values outside the interval
  `[median - k_mad * MAD, median + k_mad * MAD]` are considered as
  outliers. MAD denotes median absolute deviation and `k_mad` the
  scaling factor. Each of the above methods operates independently per
  time point, not globally across time.

Note: For large datasets, the calculation can be time-consuming. It is
recommended to pre-filter or subset the data before using this function
to reduce computation time.

## Examples

``` r
# 1. Outlier epoch detection for subject 2, electrode E45 for the whole time range with IQR method
epochdata |>
pick_data(subject_rg = 2, sensor_rg = "E45") |>
outliers_epoch(amplitude = "signal")
#> # A tibble: 8 × 4
#>   subject sensor epoch count
#>   <fct>   <chr>  <fct> <int>
#> 1 2       E45    7         5
#> 2 2       E45    8         6
#> 3 2       E45    9         1
#> 4 2       E45    10        1
#> 5 2       E45    11        2
#> 6 2       E45    13        1
#> 7 2       E45    14       50
#> 8 2       E45    15       50
## From the result table we see that epochs 14 and 15 were marked as outliers in 50 cases out of 50

# 2. Outlier epoch detection for both subjects, electrode E45 for the whole time range
# using percentile method with 1 and 99 percentiles
outdata <- epochdata |>
pick_data(sensor_rg = "E45") |>
outliers_epoch(amplitude = "signal", method = "percentile", p = 0.99)
#> # A tibble: 4 × 4
#>   subject sensor epoch count
#>   <fct>   <chr>  <fct> <int>
#> 1 1       E45    13       50
#> 2 1       E45    14       50
#> 3 2       E45    14       50
#> 4 2       E45    15       50
# see head of outliers data
head(outdata$outliers_data)
#> # A tibble: 6 × 5
#>    time signal epoch sensor subject
#>   <int>  <dbl> <fct> <chr>  <fct>  
#> 1     1  -22.6 13    E45    1      
#> 2     2  -25.0 13    E45    1      
#> 3     3  -24.0 13    E45    1      
#> 4     4  -23.1 13    E45    1      
#> 5     5  -21.1 13    E45    1      
#> 6     6  -19.3 13    E45    1      
```
