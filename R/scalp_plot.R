#' Plot scalp map of EEG signal
#'
#' @description
#' Plot a scalp polygon map of the EEG signal amplitude using topographic colour scale. The thin-plate spline interpolation model \eqn{\text{IM:}\; \mathbb{R}^3 \rightarrow \mathbb{R}} is used for signal interpolation between the sensor locations. The \code{\link[rgl]{shape3d}} function is used for plotting.
#'
#' The function assumes that the input data have already been filtered to the desired subset (e.g., group, subject, time point).
#'
#' @param data A data frame, tibble or a database table with input data to plot with at least two columns: `sensor` with sensor labels and the column with the EEG amplitude specified in the argument `amplitude`.
#' @param amplitude A character specifying the name of the column from input data with EEG amplitude values.
#' @param mesh An object of class `"mesh"` (or a named list with the same structure) used for computing IM model. If not defined, the polygon point mesh with default settings from \code{\link{point_mesh}} function is used. See details for more information about the structure.
#' @param tri A three column matrix with indices of the vertices of the triangles. Each row represents one triangle, defined by three vertex indices. If missing, the triangulation is computed using \code{\link{make_triangulation}} function from `D2` element of the mesh.
#' @param coords Sensor coordinates as a tibble or data frame with named `x`, `y`, `z` and `sensor` columns. The `sensor` labels must match the labels in sensor column in `data`. If not defined, the HCGSN256 template is used.
#' @param template The kind of sensor template montage used. Currently the only available option is `"HCGSN256"` denoting the 256-channel HydroCel Geodesic Sensor Net v.1.0, which is also a default setting.
#' @param col_range A vector with minimum and maximum value of the amplitude used in the colour palette for plotting. If not defined, the range of interpolated signal is used.
#' @param col_scale Optionally, a colour scale to use for plotting. If not defined, it is computed from `col_range`.
#' @param view A character for creating a temporary rotated scene (according to neurological terminology). Possible values are: \code{"superior", "anterior", "posterior", "left", "right"}. If missing, the default view according to user settings is displayed. Note: Input coordinates corresponding to the positions in the HCGSN template are required to obtain an appropriate view.
#'
#' @details
#' The parameter `mesh` should optimally be a `"mesh"` object (output from \code{\link{point_mesh}} function) or a list with the same structure:
#' `D2`	data frame with `x` and `y` columns and `D3` data frame with `x`, `y` and `z` columns. See \code{\link{point_mesh}} for more information.
#' In that case, setting the argument `tri` is optional, and if it is absent, a triangulation based on the `D2` element of the mesh is calculated and used in the plot.
#' If the input `mesh` contains only 3D coordinates of a point mesh in `D3` element, the use of previously created triangulation (through `tri` argument) is necessary.
#' To compare results between 2D topographical plot and 3D scalp plot use the same mesh in both cases.
#'
#' Be careful when choosing the argument `col_range`. If the amplitude in input data contains values outside the chosen range, this will cause "holes" in the resulting plot.
#' To compare results for different subjects or conditions, set the same values of `col_range` and `col_scale` arguments in all cases.
#' The default used scale is based on topographical colours with zero value always at the border of blue and green shades.
#'
#' Notes:
#' This function focuses on visualization and does not perform any data subsetting. Users are expected to filter the data beforehand using standard dplyr verbs or \code{\link{pick_data}} function.
#'
#' For correct rendering of a plot, the function requires an openGL-capable device.
#' Displaying the rotated scalp map using the `view` argument requires previous call `open3d()`.
#' When specifying the `coords` and `template` at the same time, the `template` parameter takes precedence and the `coords` parameter is ignored.
#'
#' @return A 3D scalp map rendered via `rgl::shade3d()` in an interactive window.
#' @export
#'
#' @seealso \code{\link{point_mesh}}, \code{\link{make_triangulation}}, \code{\link{create_scale}}, animated version: \code{\link{animate_scalp}}
#'
#' @import rgl
#' @importFrom rlang .data
#'
#' @examples
#' \donttest{
#' ## Note: The example opens a rgl 3D viewer.
#' # Plot average scalp map of signal for subject 2 from the time point 10 (the time of the stimulus)
#' # the outliers (epoch 14 and 15) are extracted before computing
#'
#' # a) preparing data
#' edata <- pick_data(epochdata, subject_rg = 2, epoch_rg = 1:13, time_rg = 1:10)
#' # a2) baseline correction (needed for suitable topographic map)
#' data_base <- baseline_correction(edata, baseline_range = 1:10)
#' # a3) average computing
#' data_mean <- compute_mean(data_base, amplitude = "signal_base", subject = 2, time = 10,
#'  type = "point", ex_epoch = c(14,15))

#'
#' # b) plotting the scalp polygon map
#' scalp_plot(data_mean, amplitude = "average", col_range = c(-30, 15))
#' }
scalp_plot <- function(data,
                       amplitude,
                       mesh,
                       tri = NULL,
                       coords = NULL,
                       template = NULL,
                       col_range = NULL,
                       col_scale = NULL,
                       view) {

  stop_if_missing_cols(data, required_cols = c(amplitude, "sensor"))

  if (any(is.na(data[[amplitude]]))) {
    stop("There are NA's in amplitude column.")
  }

  if (!is.null(template) && !is.null(coords)) {
    warning("Both 'template' and 'coords' were specified. Using 'template' and ignoring 'coords'.")
  }

  if (inherits(data, "tbl_sql") || inherits(data, "tbl_dbi")) {
    data <- dplyr::collect(data) # collect data from DB table
  }

  if (is.null(template) && is.null(coords)) {
    # use HCGSN256 template
    template <- "HCGSN256"
  }

  sensor_select <- unique(data$sensor)

  if (!is.null(template)) {
    coords_full <- switch(template,
                          "HCGSN256" = diegr::HCGSN256$D3,
                          stop("Unknown template.")
    )
    sensor_index <- which(coords_full$sensor %in% sensor_select)
    coords <- coords_full[sensor_index,]
  }

  stop_if_missing_cols(coords, required_cols = c("x", "y", "z", "sensor"))

  if (missing(mesh)) {
    if (!is.null(tri)) {
      stop("The argument 'mesh' must be provided when argument 'tri' is specified.")
    }
    mesh <- point_mesh(dimension = c(2,3), template = template,
                       sensor_select = sensor_select, type = "polygon")
  }

  if (control_D3(mesh)) {
    mesh3 <- mesh$D3
  }

  if (is.null(tri)) {
    if (control_D2(mesh)) {
      mesh2 <- mesh$D2
    }
    tri <- make_triangulation(mesh2)
  }

  coords_xyz <- coords |>
    dplyr::select("x", "y", "z")

  if (!all(unique(coords$sensor) %in% data$sensor)) {
    stop("Mismatch between sensors in data and coords.")
  }

  sensor_order <- as.factor(coords$sensor) # reorder data according to sensor
  data_order <- data |>
    mutate(sensor = factor(.data$sensor, levels = sensor_order)) |>
    arrange(.data$sensor)


  y_hat <- IM(coords_xyz, data_order[[amplitude]], mesh3)$Y_hat
  ycp_IM <- y_hat[1:nrow(mesh3)]

  if (is.null(col_range)) {
    col_range <- range(ycp_IM)
  }
  if (is.null(col_scale)) {
    col_scale <- create_scale(col_range)
  }


  y_cut <- cut(ycp_IM, breaks = col_scale$breaks, include.lowest = TRUE)
  y_col <- col_scale$colors[y_cut]


  if (missing(view)) {
    rgl::shade3d(rgl::mesh3d(x = mesh3$x, y = mesh3$y, z = mesh3$z, triangles = t(tri)),
                 col = y_col, lit = FALSE)
  } else {
    U0 <- diag(4)
    Upost <- rgl::rotate3d(U0, pi/2, -1,0,0)

    U <- switch(view,
                "superior" = U0,
                "anterior" = rgl::rotate3d(Upost, pi, 0,0,1),
                "posterior" = Upost,
                "left" = rgl::rotate3d(Upost, pi/2, 0,0,1),
                "right" = rgl::rotate3d(Upost, -pi/2, 0,0,1),
                stop("Invalid view argument."))

    if (rgl::rgl.cur() == 0) {
      stop("No active rgl window found. Please run open3d() before calling 'scalp_plot' with 'view' setting.")
    }

    current <- rgl::currentSubscene3d()
    newscene <- rgl::newSubscene3d(copyShapes = TRUE)
    rgl::useSubscene3d(newscene)

    rgl::par3d(userMatrix = U) # rotate view

    rgl::shade3d(rgl::mesh3d(x = mesh3$x, y = mesh3$y, z = mesh3$z, triangles = t(tri)),
                 col = y_col, lit = FALSE)

    rgl::useSubscene3d(current) # original subscene
  }

 }
