# Compute summary statistics of reaction times

Calculates basic descriptive statistics of reaction time (RT).
Statistics are computed separately for each combination of grouping
variables present in the data (e.g., group, subject, condition).

Computed statistics include: the number of epochs, minimum, maximum,
median, mean, and standard deviation of RT.

## Usage

``` r
summary_stats_rt(data)
```

## Arguments

- data:

  A data frame or a database table with reaction times dataset. Required
  columns are `epoch` and `RT` (value of reaction time in ms). Optional
  columns: `group`, `subject`, `condition` for computing summary
  statistics per group/subject/condition.

## Value

A tibble with summary statistics of reaction times consisting of the
following columns:

- group:

  Group identifier (only if present in the input data).

- subject:

  Subject identifier (only if present in the input data).

- condition:

  Experimental condition (only if present in the input data).

- n_epoch:

  Number of epochs.

- min_rt:

  Minimum reaction time.

- max_rt:

  Maximum reaction time.

- median_rt:

  Median reaction time.

- avg_rt:

  Mean reaction time.

- sd_rt:

  Standard deviation of reaction time.

## Examples

``` r
# 1. Summary statistics for rtdata
# two different subjects, no group or conditions - results are computed per subject
summary_stats_rt(rtdata)
#> # A tibble: 2 × 7
#>   subject n_epoch min_rt max_rt median_rt avg_rt sd_rt
#>   <fct>     <int>  <dbl>  <dbl>     <dbl>  <dbl> <dbl>
#> 1 1            14    166    270       194   198   26.1
#> 2 2            15    258    426       354   339.  44.1

# 2. Summary statistics for data with conditions
# a) create example data
data_cond <- rtdata
data_cond$condition <- c(rep("a", 7), rep("b", 7), rep("a", 8), rep("b",7))
# b) compute statistics per subject and condition
summary_stats_rt(data_cond)
#> # A tibble: 4 × 8
#>   subject condition n_epoch min_rt max_rt median_rt avg_rt sd_rt
#>   <fct>   <chr>       <int>  <dbl>  <dbl>     <dbl>  <dbl> <dbl>
#> 1 1       a               7    194    270       206   213.  26.9
#> 2 1       b               7    166    206       182   183.  14.1
#> 3 2       a               8    258    374       324   322   45.5
#> 4 2       b               7    314    426       358   359.  36.0
# c) compute statistics per conditions regardless of subjects
# exclude "subject" column from computing
summary_stats_rt(data_cond[,-1])
#> # A tibble: 2 × 7
#>   condition n_epoch min_rt max_rt median_rt avg_rt sd_rt
#>   <chr>       <int>  <dbl>  <dbl>     <dbl>  <dbl> <dbl>
#> 1 a              15    194    374       270   271.  67.0
#> 2 b              14    166    426       260   271.  95.0
```
