# Subsets EEG data by group, subject, sensor, time, experimental condition or epoch

Filters an input dataset by optional constraints on group, subject,
sensor, time, condition and epoch. Filters are combined with logical
AND, and exact value matching (`%in%`) is used.

## Usage

``` r
pick_data(
  data,
  group_rg = NULL,
  subject_rg = NULL,
  sensor_rg = NULL,
  condition_rg = NULL,
  epoch_rg = NULL,
  time_rg = NULL
)
```

## Arguments

- data:

  A data frame, tibble or database table with input data. Required
  columns depend on the further parameters: setting `subject_rg`
  requires `subject` column etc.

- group_rg:

  Optional vector of group identifiers to keep (character or numeric,
  matching `data$group`). If `NULL` (default), no filtering is applied
  based on group.

- subject_rg:

  Optional vector of subject identifiers to keep (character or numeric,
  matching `data$subject`). If `NULL` (default), no filtering is applied
  based on subject.

- sensor_rg:

  Optional vector of sensor identifiers to keep (character or numeric,
  matching `data$sensor`). If `NULL` (default), no filtering is applied
  based on sensor.

- condition_rg:

  Optional vector of experimental condition identifiers to keep
  (character or numeric, matching `data$condition`). If `NULL`
  (default), no filtering is applied based on condition.

- epoch_rg:

  Optional vector of epoch identifiers to keep (character or numeric,
  matching `data$epoch`). If `NULL` (default), no filtering is applied
  based on epoch.

- time_rg:

  Optional vector of time points to keep (numeric, matching
  `data$time`). If `NULL` (default), no filtering is applied based on
  time.

## Value

An object of the same class as `data` with rows filtered by the provided
criteria; columns are unchanged. If all filters are `NULL`, the input is
returned unmodified. If no rows match, the function ends with error
message.

## Details

All filters are combined conjunctively (AND). Matching uses membership
(`%in%`) with case-sensitive comparison for character columns. On
database backends, very long \*\_rg vectors may not translate
efficiently; consider pre-filtering or semi-joins.

## See also

[`compute_mean`](https://gerslovaz.github.io/diegr/reference/compute_mean.md),
[`baseline_correction`](https://gerslovaz.github.io/diegr/reference/baseline_correction.md),
[`pick_region`](https://gerslovaz.github.io/diegr/reference/pick_region.md)

## Examples

``` r
# Filtering epochs 1:5 and time points 1:10 for all subjects and sensor "E45"
data_subset <- pick_data(epochdata, sensor_rg = "E45",
 time_rg = 1:10, epoch_rg = 1:5)
head(data_subset)
#>   time    signal epoch sensor subject
#> 1    1 -5.554384     1    E45       1
#> 2    2 -5.370255     1    E45       1
#> 3    3 -5.098571     1    E45       1
#> 4    4 -6.179277     1    E45       1
#> 5    5 -6.868986     1    E45       1
#> 6    6 -6.813554     1    E45       1

# \donttest{
# Setting parameters outside the input data range (there is no subject 6 in epochdata)
# results in an error message
try(
pick_data(epochdata, subject_rg = 6,
 time_rg = 1:10, epoch_rg = 1:5)
 )
#> Error in pick_data(epochdata, subject_rg = 6, time_rg = 1:10, epoch_rg = 1:5) : 
#>   The subset of original data is empty.
# }
```
