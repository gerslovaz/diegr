# Check data structure and print inferred hierarchy

A diagnostic helper function to run before starting the `diegr`
analysis. It infers the experimental hierarchy based on the standardized
column names strictly required by the package and prints a readable
summary. Supports data frames, tibbles, and database tables.

## Usage

``` r
check_structure(data, value_col = "signal")
```

## Arguments

- data:

  A data frame, tibble, or database table containing the EEG data in
  long format.

- value_col:

  Character string specifying the signal amplitude column name (default
  `"signal"`).

## Value

The original data object invisibly, allowing it to be used in pipes.

## Details

The `diegr` package does not strictly require all structural columns for
every function (e.g., `group`, `condition`, and `epoch` may be
optional). However, if they are present, they must follow the exact
naming convention (`group`, `subject`, `sensor`, `epoch`, `condition`,
`time`). Only the signal amplitude column can be custom-named via the
`value_col` argument. This function helps verify which columns were
correctly recognized.

## Examples

``` r
# Checking the structure of epochdata
check_structure(epochdata)
#> ------------------------------------------------------- 
#>  diegr: Inferred Data Structure
#> ------------------------------------------------------- 
#>  |-- Groups:      Not found (optional)
#>  |-- Subjects:    2 found
#>  |-- Conditions:  Not found (optional)
#>  |-- Epochs:      15 found
#>  |-- Sensors:     204 found
#>  |-- Timepoints:  50  [Indices:  1  to  50 ]
#>  |-- Signal:      Present ('signal' column)
#> ------------------------------------------------------- 
```
