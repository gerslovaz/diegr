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

  A character denoting the sensor template montage used. Available
  options are `"HCGSN256"` (256-channel HydroCel Geodesic Sensor Net
  v.1.0), `"biosemi128"` (128-channel BioSemi), `"biosemi256"`
  (256-channel BioSemi), and `"system1005"` (the 10-05 system).

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

Additionally, the returned object carries a `"diegr_metadata"` attribute
with metadata such as the actual number of generated mesh points or the
template used.

## Details

If neither `template` nor `own_coordinates` is specified, the
`"HCGSN256"` template is used by default to create the mesh.

For the provided built-in templates, the (0,0) point of the resulting 2D
mesh generally corresponds to the vertex of the head model (e.g., the Cz
electrode).

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
have the input of 3D sensor locations and their corresponding 2D layout;
the function itself does not perform a projection onto a plane. It is
also necessary to keep the same sensor locations order in `D2` and `D3`
part of the coordinates.

Note: When specifying the `own_coordinates` and `template` at the same
time, the `template` parameter takes precedence and the
`own_coordinates` parameter is ignored.

Note on Matrix Conditioning: The interpolation matrix used in thin-plate
spline interpolation can be ill-conditioned depending on the spatial
configuration of input sensor locations. If this occurs, the function
automatically uses a Moore-Penrose pseudoinverse
([`MASS::ginv()`](https://rdrr.io/pkg/MASS/man/ginv.html)) instead of
the standard matrix inverse to improve numerical stability and issues a
warning.

## References

EGI Geodesic Sensor Net Technical Manual (2024) Oostenveld, R., &
Praamstra, P. (2001). The five percent electrode system for
high-resolution EEG and ERP measurements. *Clinical Neurophysiology*,
112(4), 713-719.
[doi:10.1016/s1388-2457(00)00527-7](https://doi.org/10.1016/s1388-2457%2800%2900527-7)

## Examples

``` r
# Computing circle 2D mesh with starting number 4000 points for HCGSN256 template
# using all electrodes
M <- point_mesh(dimension = 2, n = 4000, template = "HCGSN256", type = "circle")
# Inspect the attached metadata
attr(M, "diegr_metadata")
#> $step
#> [1] "point_mesh"
#> 
#> $timestamp
#> [1] "2026-08-13 11:38:28 UTC"
#> 
#> $package_version
#> [1] "0.2.0"
#> 
#> $mesh_parameters
#> $mesh_parameters$dimension
#> [1] 2
#> 
#> $mesh_parameters$start_n
#> [1] 4000
#> 
#> $mesh_parameters$density
#> [1] 6077
#> 
#> $mesh_parameters$radius
#> [1] 483
#> 
#> $mesh_parameters$template
#> [1] "HCGSN256"
#> 
#> $mesh_parameters$sensor_subset
#> [1] "all"
#> 
#> $mesh_parameters$shape_type
#> [1] "circle"
#> 
#> 

# Computing polygon 3D mesh with starting number 2000 points for BioSemi 128 template
M_bio <- point_mesh(dimension = 3, n = 2000, template = "biosemi128")

# Computing polygon 3D mesh with starting number 2000 points and own coordinates
## Note: the coordinates are the same as for HCGSN256 template, it is
## just a mod example of using the own_coordinates parameter
M <- point_mesh(dimension = 3, n = 2000, own_coordinates = HCGSN256)
#> Warning: The X_P matrix is ill-conditioned (kappa = 1.76e+12 ). Falling back to MASS::ginv() to prevent 3D geometry corruption.

# Computing coordinates of a polygon mesh in 2D and 3D in one step (starting number 3000 points),
# using 204 electrodes selected for epochdata
# a) create vector with selected sensor labels
sensors <- unique(epochdata$sensor)
# b) create a mesh for selected sensors using sensor_select parameter
M <- point_mesh(n = 3000, template = "HCGSN256", sensor_select = sensors)
```
