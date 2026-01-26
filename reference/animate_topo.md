# Topographic map animation in time

Display a topographic animation of the change in amplitude over time.
The function enables direct rendering in Rstudio Viewer or saving the
animation in gif format to the chosen location.

## Usage

``` r
animate_topo(
  data,
  amplitude,
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

  An input data frame or tibble with at least this required columns:
  `time` - the number of time point,`sensor` - the sensor label and the
  column with the EEG amplitude to plot specified in the argument
  `amplitude`.

- amplitude:

  A character specifying the name of the column from input data with EEG
  amplitude values.

- t_lim:

  A numeric vector of length 2 with limits of time points (i.e., the
  length of the timeline displayed below the animation).

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
  colour palette for plotting. If not defined, the range of interpolated
  signal is used.

- col_scale:

  Optionally, a colour scale to be utilised for plotting. If not
  defined, it is computed from `col_range`.

- show_legend:

  Logical. Indicates, whether legend should be displayed beside the
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
the given file path and not displayed.

## Details

For more details about required mesh structure see
[`point_mesh`](https://gerslovaz.github.io/diegr/reference/point_mesh.md)
function. If the input `mesh` structure does not match this format, an
error or incorrect function behavior may occur.

The time part of input data is assumed to be in numbers of time points,
conversion to ms takes place inside the function for drawing the
timeline labels. Due to the flexibility of the function (e.g. to mark
and animate only a short section from the entire time course or to
compare different data in the same time interval), it allows to enter
and plot user-defined time ranges. If some values of the time are
outside the `t_lim` range, the function writes a warning message - in
that case the animation is still rendered, but the timeline will not
match reality.

Note: When specifying the `coords` and `template` at the same time, the
`template` parameter takes precedence and the `coords` parameter is
ignored.

## See also

Static version:
[`topo_plot`](https://gerslovaz.github.io/diegr/reference/topo_plot.md),
animated 3D scalp map:
[`animate_scalp`](https://gerslovaz.github.io/diegr/reference/animate_scalp.md)

## Examples

``` r
# \donttest{
# This example may take a few seconds to render.
# Run only if you want to generate the full animation.
# Prepare a data structure:
s1e05 <- pick_data(epochdata, subject_rg = 1, epoch_rg = 5, time_rg = 10:20)
# Plot animation
# t0 = 10 indicates the time point of stimulus in epochdata,
# t_lim is the whole range of epochdata, we animate only a short period
animate_topo(s1e05, amplitude = "signal", t_lim = c(1,50), t0 = 10)
#> `nframes` and `fps` adjusted to match transition
# }
```
