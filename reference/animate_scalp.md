# 3D scalp plot animation in time

Display a topographic 3D scalp animation of the change in amplitude over
time. The function enables direct rendering in Rstudio Viewer or saving
the animation in MP4 format or individual frames in PNG format to the
chosen location.

## Usage

``` r
animate_scalp(
  data,
  amplitude,
  mesh,
  tri,
  coords = NULL,
  template = NULL,
  col_range = NULL,
  col_scale = NULL,
  sec = 0.3,
  frames_dir = NULL,
  output_path = NULL,
  framerate = 3,
  cleanup = TRUE
)
```

## Arguments

- data:

  An input data frame or tibble with at least this required columns:
  `time` - the number of time point, `sensor` - the sensor label and the
  column with the EEG amplitude to plot specified in the argument
  `amplitude`.

- amplitude:

  A character string naming the column with EEG amplitude values.

- mesh:

  An object of class `"mesh"` (or a named list with the same structure)
  used for computing IM model. If not defined, the polygon point mesh
  with default settings from
  [`point_mesh`](https://gerslovaz.github.io/diegr/reference/point_mesh.md)
  function is used. See
  [`scalp_plot`](https://gerslovaz.github.io/diegr/reference/scalp_plot.md)
  for details about the structure.

- tri:

  A matrix with indices of the triangles. If missing, the triangulation
  is computed using
  [`make_triangulation`](https://gerslovaz.github.io/diegr/reference/make_triangulation.md)
  function from `D2` element of the mesh.

- coords:

  Sensor coordinates as a tibble or data frame with named `x`, `y` and
  `z` columns of sensor coordinates and `sensor` column with sensor
  names. If not defined, the HCGSN256 template is used.

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

- sec:

  The time interval used between individual animation frames, in seconds
  (default: 0.3).

- frames_dir:

  Directory where the individual frames will be saved. If NULL, the
  video is only displayed in viewer and the frames are not saved.

- output_path:

  Optional path to the output mp4 video file (".mp4" extension is
  required for correct rendering). If NULL, no video is created.

- framerate:

  Number of frames per second for the output mp4 video (default: 3).

- cleanup:

  Logical. Indicates, if all the PNG files should be deleted after
  encoding video. Default value is `TRUE`.

## Value

The output depends on the provided arguments:

- If `frames_dir` is specified, individual animation frames (PNG) are
  saved to that directory.

- If also `output_path` is specified, a video (MP4) is created and saved
  using the `av` package.

- Otherwise, the animation is displayed in an interactive rgl window.

## Details

Setting the parameter `tri` requires defining a `mesh` parameter. The
parameter `mesh` should optimally be a `"mesh"` object (output from
[`point_mesh`](https://gerslovaz.github.io/diegr/reference/point_mesh.md)
function) or a list with the same structure (see
[`point_mesh`](https://gerslovaz.github.io/diegr/reference/point_mesh.md)
for more information). In that case, setting the argument `tri` is
optional, and if it is absent, a triangulation based on the `D2` element
of the mesh is calculated and used in the plot. If the input `mesh`
contains only 3D coordinates of a point mesh in `D3` element, the use of
previously created triangulation (through `tri` argument) is required.

Notes: For exporting the video, setting `frames_dir` together with
`output_path` is required.

When specifying the `coords` and `template` at the same time, the
`template` parameter takes precedence and the `coords` parameter is
ignored.

## See also

Static version:
[`scalp_plot`](https://gerslovaz.github.io/diegr/reference/scalp_plot.md),
animated 2D topo map:
[`animate_topo`](https://gerslovaz.github.io/diegr/reference/animate_topo.md)

## Examples

``` r
# \donttest{
# This example may take a few seconds to render.
# Run only if you want to generate the full animation.
# Note: The example opens a rgl 3D viewer.
# Prepare a data structure:
s1e05 <- pick_data(epochdata, subject_rg = 1, epoch_rg = 5, time_rg = 10:20)
# Plot animation with default mesh and triangulation:
animate_scalp(s1e05, amplitude = "signal")
# }
```
