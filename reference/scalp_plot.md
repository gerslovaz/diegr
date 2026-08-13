# Plot scalp map of EEG signal

Plot a scalp polygon map of the EEG signal amplitude using topographic
colour scale. The thin-plate spline interpolation model \\\text{IM:}\\
\mathbb{R}^3 \rightarrow \mathbb{R}\\ is used for signal interpolation
between the sensor locations. The
[`shape3d`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
function is used for plotting.

The function assumes that the input data have already been filtered to
the desired subset (e.g., group, subject, time point).

## Usage

``` r
scalp_plot(
  data,
  amplitude,
  mesh,
  tri = NULL,
  coords = NULL,
  template = NULL,
  col_range = NULL,
  col_scale = NULL,
  view
)
```

## Arguments

- data:

  A data frame, tibble or a database table with input data to plot with
  at least two columns: `sensor` with sensor labels and the column with
  the EEG amplitude specified in the argument `amplitude`.

- amplitude:

  A character specifying the name of the column from input data with EEG
  amplitude values.

- mesh:

  A `"mesh"` object (or a named list with the same structure) containing
  at least a `D3` element with x, y and z coordinates of a point mesh
  used for computing the IM model, and a `template` element specifying
  the sensor montage. If not defined, the polygon point mesh with
  default settings from
  [`point_mesh`](https://gerslovaz.github.io/diegr/reference/point_mesh.md)
  function is used. See details for more information about the
  structure.

- tri:

  A three column matrix with indices of the vertices of the triangles.
  Each row represents one triangle, defined by three vertex indices. If
  missing, the triangulation is computed using
  [`make_triangulation`](https://gerslovaz.github.io/diegr/reference/make_triangulation.md)
  function from `D2` element of the mesh.

- coords:

  Sensor coordinates as a tibble or data frame with named `x`, `y`, `z`
  and `sensor` columns. The `sensor` labels must match the labels in
  sensor column in `data`. If not defined, the template specified in
  `mesh$template` (or the default `"HCGSN256"`) is used.

- template:

  The kind of sensor template montage used. Available options are
  `"HCGSN256"`, `"biosemi128"`, `"biosemi256"`, and `"system1005"`.
  Default setting is `"HCGSN256"`.

- col_range:

  A vector with minimum and maximum value of the amplitude used in the
  colour palette for plotting. If not defined, the range of interpolated
  signal is used.

- col_scale:

  Optionally, a colour scale to use for plotting. If not defined, it is
  computed from `col_range`.

- view:

  A character for creating a temporary rotated scene (according to
  neurological terminology). Possible values are:
  `"superior", "anterior", "posterior", "left", "right"`. If missing,
  the default view according to user settings is displayed. Note: Input
  coordinates corresponding to the positions in the HCGSN template are
  required to obtain an appropriate view.

## Value

Called for its side effect of drawing a 3D scalp map in an interactive
`rgl` window. It returns an object of class `"rglLowLevel"` (an integer
vector) containing the IDs of the drawn objects.

Additionally, the returned object carries a `"diegr_metadata"` attribute
with metadata such as details about the mesh used for plotting.

## Details

The parameter `mesh` should optimally be a `"mesh"` object (output from
[`point_mesh`](https://gerslovaz.github.io/diegr/reference/point_mesh.md)
function) or a list with the same structure: `D2` data frame with `x`
and `y` columns, `D3` data frame with `x`, `y` and `z` columns and
`template` element specifying the used template. See
[`point_mesh`](https://gerslovaz.github.io/diegr/reference/point_mesh.md)
for more information. In that case, setting the argument `tri` is
optional, and if it is absent, a triangulation based on the `D2` element
of the mesh is calculated and used in the plot. If the input `mesh`
contains only 3D coordinates of a point mesh in `D3` element, the use of
previously created triangulation (through `tri` argument) is necessary.
To compare results between 2D topographical plot and 3D scalp plot use
the same mesh in both cases.

Be careful when choosing the argument `col_range`. If the amplitude in
input data contains values outside the chosen range, this will cause
"holes" in the resulting plot. To compare results for different subjects
or conditions, set the same values of `col_range` and `col_scale`
arguments in all cases. The default used scale is based on topographical
colours with zero value always at the border of blue and green shades.

Notes: This function focuses on visualization and does not perform any
data subsetting. Users are expected to filter the data beforehand using
standard dplyr verbs or
[`pick_data`](https://gerslovaz.github.io/diegr/reference/pick_data.md)
function.

If a `mesh` object is provided, its internal template name
(`mesh$template`) overrides the `template` argument to ensure spatial
consistency. When custom `coords` are provided, they are used for
plotting the sensor locations. The `template` parameter (or
`mesh$template`) is then used only for generating the background `mesh`
if it is not provided.

For correct rendering of a plot, the function requires an openGL-capable
device. Displaying the rotated scalp map using the `view` argument
requires previous call `open3d()`.

## See also

[`point_mesh`](https://gerslovaz.github.io/diegr/reference/point_mesh.md),
[`make_triangulation`](https://gerslovaz.github.io/diegr/reference/make_triangulation.md),
[`create_scale`](https://gerslovaz.github.io/diegr/reference/create_scale.md),
animated version:
[`animate_scalp`](https://gerslovaz.github.io/diegr/reference/animate_scalp.md)

## Examples

``` r
# \donttest{
## Note: The example opens a rgl 3D viewer.
# Plot average scalp map of signal for subject 2 from the time point 10 (the time of the stimulus)
# the outliers (epoch 14 and 15) are extracted before computing

# a) preparing data
edata <- pick_data(epochdata, subject_rg = 2, epoch_rg = 1:13, time_rg = 1:10)
# a2) baseline correction (needed for suitable topographic map)
data_base <- baseline_correction(edata, baseline_range = 1:9)
# a3) average computing
data_mean <- data_base |>
dplyr::filter(time == 10) |>
compute_mean(amplitude = "signal_base", type = "point", domain = "space")

# b) plotting the scalp polygon map
scalp_plot(data_mean, amplitude = "average", col_range = c(-30, 15))
# }
```
