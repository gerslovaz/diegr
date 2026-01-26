# Animate EEG average topographic map with confidence bounds

An animation of the average signal time course as a topographic map
along with the lower and upper bounds of the confidence interval. In the
output, three facets are plotted per frame: CI lower, average, CI upper.

## Usage

``` r
animate_topo_mean(
  data,
  t_lim,
  FS = 250,
  t0 = 1,
  mesh,
  coords = NULL,
  template = NULL,
  col_range = NULL,
  col_scale = NULL,
  show_legend = TRUE,
  contour = FALSE,
  output_path = NULL,
  ...
)
```

## Arguments

- data:

  A data frame, tibble or a database table with input data to plot. It
  should be an output from
  [`compute_mean`](https://gerslovaz.github.io/diegr/reference/compute_mean.md)
  function or an object with the same structure. Required columns:
  `sensor` - sensor labels, `time` - numbers of time points, `average` -
  average signal values, `ci_low` and `ci_up` - lower and upper CI
  bounds.

- t_lim:

  Limits of time points (i.e., the length of the timeline displayed
  below the animation).

- FS:

  The sampling frequency. Default value is 250 Hz.

- t0:

  Index of the zero time point, i.e. point, where 0 ms should be marked
  (most often time of the stimulus or time of the response).

- mesh:

  A `"mesh"` object (or a named list with the same structure) containing
  at least `D2` element with x and y coordinates of a point mesh used
  for computing IM model. If not defined, the point mesh with default
  settings from
  [`point_mesh`](https://gerslovaz.github.io/diegr/reference/point_mesh.md)
  function is used.

- coords:

  Sensor coordinates as a tibble or data frame with named `x`, `y`
  columns of sensor coordinates and `sensor` column with sensor names.
  If not defined, the HCGSN256 template is used.

- template:

  The kind of sensor template montage used. Currently the only available
  option is `"HCGSN256"` denoting the 256-channel HydroCel Geodesic
  Sensor Net v.1.0, which is also a default setting.

- col_range:

  A vector with minimum and maximum value of the amplitude used in the
  colour palette for plotting. If not defined, the range of the input
  signal is used.

- col_scale:

  Optionally, a colour scale to be utilised for plotting. If not
  defined, it is computed from `col_range`.

- show_legend:

  Logical. Indicates, whether legend should be displayed below the
  graph. Default value is `TRUE`.

- contour:

  Logical. Indicates, whether contours should be plotted in the graph.
  Default value is `FALSE`.

- output_path:

  File path where the animation will be saved using `gifski` renderer
  (optional). If not defined, the animation is plotted in the RStudio
  Viewer.

- ...:

  Additional parameters for animation according to
  [gganimate::animate](https://gganimate.com/reference/animate.html).

## Value

If `output_path` is `NULL`, the function prints the animation to the
RStudio Viewer. If `output_path` is specified, the animation is saved to
the given file path and not displayed. The `gifski` and `magick`
packages are required for animation export.

## Details

Note: When specifying the `coords` and `template` at the same time, the
`template` parameter takes precedence and the `coords` parameter is
ignored.

## See also

[`animate_topo`](https://gerslovaz.github.io/diegr/reference/animate_topo.md),
[`compute_mean`](https://gerslovaz.github.io/diegr/reference/compute_mean.md),
[`baseline_correction`](https://gerslovaz.github.io/diegr/reference/baseline_correction.md),
static version:
[`plot_topo_mean`](https://gerslovaz.github.io/diegr/reference/plot_topo_mean.md)

## Examples

``` r
# \donttest{
# This example may take a few seconds to render.
# Run only if you want to generate the full animation.

# a) prepare data: compute the mean from baseline corrected signal for subject 2,
# first 10 points and only 13 epochs (epochs 14 and 15 are outliers)
edata <- pick_data(epochdata, subject_rg = 2, epoch_rg = 1:13, time_rg = 1:10)
data_base <- baseline_correction(edata, baseline_range = 1:10) # baseline correction
data_mean <- compute_mean(data_base, amplitude = "signal_base",
 type = "jack", domain = "space") # compute mean
# b) render the animation
# (t0 = 10 because the time of the stimulus in epochdata is in time point 10)
animate_topo_mean(data_mean, t_lim = c(1,50), t0 = 10)
#> `nframes` and `fps` adjusted to match transition
#> `nframes` and `fps` adjusted to match transition
#> # A tibble: 10 × 7
#>    format width height colorspace matte filesize density
#>    <chr>  <int>  <int> <chr>      <lgl>    <int> <chr>  
#>  1 GIF      480    360 sRGB       TRUE         0 72x72  
#>  2 GIF      480    360 sRGB       FALSE        0 72x72  
#>  3 GIF      480    360 sRGB       FALSE        0 72x72  
#>  4 GIF      480    360 sRGB       FALSE        0 72x72  
#>  5 GIF      480    360 sRGB       FALSE        0 72x72  
#>  6 GIF      480    360 sRGB       FALSE        0 72x72  
#>  7 GIF      480    360 sRGB       FALSE        0 72x72  
#>  8 GIF      480    360 sRGB       FALSE        0 72x72  
#>  9 GIF      480    360 sRGB       FALSE        0 72x72  
#> 10 GIF      480    360 sRGB       FALSE        0 72x72  
# }
```
