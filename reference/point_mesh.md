# Create regular mesh of points

Function creates an object of class `"mesh"`, which is a list of data
frames with coordinates of a regular (in the sense of the equidistant
distance between mesh nodes) mesh of points on the space defined by
sensor coordinates. Circular or polygonal shape of the result mesh is
available. For the equivalence between 2D and 3D mesh and the
possibility to compare models in different dimensions, the thin-plate
spline interpolation model \\\mathbb{R}^2 \rightarrow \mathbb{R}^3\\ is
used for creating 3D mesh.

## Usage

``` r
point_mesh(
  dimension = c(2, 3),
  n = 10000,
  r,
  template = NULL,
  sensor_select = NULL,
  own_coordinates = NULL,
  type = "polygon"
)
```

## Arguments

- dimension:

  A number (or a vector) indicating a dimension of the mesh: `2` for two
  dimensional, `3` for three dimensional mesh and `c(2,3)` for both of
  them in one output (default setting).

- n:

  Optionally, the required number of mesh points. Default setting is
  `n = 10 000`.

- r:

  Optionally, desired radius of a circular mesh. If not defined, it is
  computed from the convex hull of sensor locations, based on maximum
  Euclidean distance from centroid.

- template:

  A character denoting sensor template montage used. Currently the only
  available option is `"HCGSN256"` denoting the 256-channel HydroCel
  Geodesic Sensor Net v.1.0.

- sensor_select:

  Optionally, a vector with sensor labels to select from the template.
  If not defined, all sensors from the template montage are used to
  create a mesh.

- own_coordinates:

  Optionally, a list with own sensor coordinates for mesh building. See
  Details for more information.

- type:

  A character indicating the shape of the mesh with 2 possible values:
  `"circle"` for circular mesh, `"polygon"` for irregular polygon shape
  with boundaries defined by sensor locations (default).

## Value

Returns a list of class `"mesh"` containing some (or all) of the
following components:

- D2:

  A data frame with `x` and `y` coordinates of the created two
  dimensional point mesh.

- D3:

  A data frame with `x`, `y` and `z` coordinates of the created three
  dimensional point mesh.

- template:

  A character indicating the template of the sensor coordinates used for
  mesh computing.

- r:

  A radius of the circle used for mesh creating.

## Details

If neither `template` nor `own_coordinates` is specified, `"HCGSN256"`
template is used to create the mesh.

In the case of using Geodesic Sensor Net (`template = 'HCGSN256'`), the
(0,0) point of the resulting 2D mesh corresponds to a reference
electrode located at the vertex.

The number `n` for controlling the mesh density is only an approximate
value. The final number of mesh nodes depends on the exact shape of the
polygon (created as a convex hull of the sensor locations), and is only
close to, not exactly equal to, the number `n`.

The `own_coordinates` enables computing a mesh from user's own sensor
locations. The input must be a list containing following elements:

- `D2` a tibble or data frame with sensor coordinates in named `x` and
  `y` columns,

- `D3` a tibble or data frame with sensor coordinates in named `x`, `y`
  and `z` columns.

To build the appropriate meshes in both dimensions, it is necessary to
have the input of 3D sensor locations and their corresponding projection
onto a plane; the function itself does not perform this projection. It
is also necessary to keep the same sensor locations order in `D2` and
`D3` part of the coordinates.

Note: When specifying the `own_coordinates` and `template` at the same
time, the `template` parameter takes precedence and the
`own_coordinates` parameter is ignored.

## References

EGI Geodesic Sensor Net Technical Manual (2024)

## Examples

``` r
# Computing circle 2D mesh with starting number 4000 points for HCGSN256 template
# using all electrodes
M <- point_mesh(dimension = 2, n = 4000, template = "HCGSN256", type = "circle")

# Computing polygon 3D mesh with starting number 2000 points and own coordinates
## Note: the coordinates are the same as for HCGSN256 template, it is
## just a mod example of using the own_coordinates parameter
M <- point_mesh(dimension = 3, n = 2000, own_coordinates = HCGSN256)
#> Warning: The X_P matrix is ill-conditioned (kappa = 1.76e+12 ) and the results from solve() could be inaccurate.

# Computing coordinates of a polygon mesh in 2D and 3D in one step (starting number 3000 points),
# using 204 electrodes selected for epochdata
# a) create vector with selected sensor labels
sensors <- unique(epochdata$sensor)
# b) create a mesh for selected sensors using sensor_select parameter
M <- point_mesh(n = 3000, template = "HCGSN256", sensor_select = sensors)
```
