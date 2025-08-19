#' Plot scalp map of EEG signal
#'
#' @description
#' Plot a scalp polygon map of the EEG signal amplitude using topographic colour scale. The thin-plate spline interpolation model \eqn{\text{IM:}\; \mathbb{R}^3 \rightarrow \mathbb{R}} is used for signal interpolation between the sensor locations. The \code{\link[rgl]{shape3d}} function is used for plotting.
#'
#' @param data A data frame, tibble or a database table with input data to plot with at least two columns: \code{sensor} with sensor labels and the column with the EEG amplitude specified in the argument \code{amplitude}.
#' @param amplitude A character specifying the name of the column from input data with EEG amplitude values.
#' @param mesh An object of class \code{"mesh"} (or a named list with the same structure) used for computing IM model. If not defined, the polygon point mesh with default settings from \code{\link{point_mesh}} function is used. See details for more information about the structure.
#' @param tri A three column matrix with indices of the vertices of the triangles. Each row represents one triangle, defined by three vertex indices. If missing, the triangulation is computed using \code{\link{make_triangulation}} function from \code{D2} element of the mesh.
#' @param coords Sensor coordinates as a tibble or data frame with named \code{x}, \code{y}, \code{z} and \code{sensor} columns. The \code{sensor} labels must match the labels in sensor column in \code{data}. If not defined, the HCGSN256 template is used.
#' @param template The kind of sensor template montage used. Currently the only available option is \code{"HCGSN256"} denoting the 256-channel HydroCel Geodesic Sensor Net v.1.0, which is also a default setting.
#' @param col_range A vector with minimum and maximum value of the amplitude used in the colour palette for plotting. If not defined, the range of interpolated signal is used.
#' @param col_scale Optionally, a colour scale to use for plotting. If not defined, it is computed from \code{col_range}.
#' @param view A character for creating a temporary rotated scene (according to neurological terminology). Possible values are: \code{"superior", "anterior", "posterior", "left", "right"}. If missing, the default view according to user settings is displayed. Note: Input coordinates corresponding to the positions in the HCGSN template are required to obtain an appropriate view.
#'
#' @details
#' The parameter \code{mesh} should optimally be a \code{"mesh"} object (output from \code{\link{point_mesh}} function) or a list with the same structure:
#' \code{D2}	data frame with \code{x} and \code{y} columns and \code{D3} data frame with \code{x}, \code{y} and \code{z} columns. See \code{\link{point_mesh}} for more information.
#' In that case, setting the argument \code{tri} is optional, and if it is absent, a triangulation based on the \code{D2} element of the mesh is calculated and used in the plot.
#' If the input \code{mesh} contains only 3D coordinates of a point mesh in \code{D3} element, the use of previously created triangulation (through \code{tri} argument) is necessary.
#' To compare results between 2D topographical plot and 3D scalp plot use the same mesh in both cases.
#'
#' Be careful when choosing the argument \code{col_range}. If the amplitude in input data contains values outside the chosen range, this will cause "holes" in the resulting plot.
#' To compare results for different subjects or conditions, set the same values of \code{col_range} and \code{col_scale} arguments in all cases.
#' The default used scale is based on topographical colours with zero value always at the border of blue and green shades.
#'
#' Notes:
#' For correct rendering of a plot, the function requires an openGL-capable device.
#' Displaying the rotated scalp map using the \code{view} argument requires previous call \code{open3d()}.
#' When specifying the `coords` and `template` at the same time, the `template` parameter takes precedence and the `coords` parameter is ignored.
#'
#' @return A 3D scalp map rendered via `rgl::shade3d()` in an interactive window.
#' @export
#'
#' @seealso \code{\link{animate_scalp}}, \code{\link{point_mesh}}, \code{\link{make_triangulation}}, \code{\link{create_scale}}
#'
#' @import rgl
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' ## Note: The example opens a rgl 3D viewer.
#' # Plot average scalp map of signal for subject 2 from the time point 10 (the time of the stimulus)
#' # the outliers (epoch 14 and 15) are extracted before computing
#'
#' # a) preparing data
#' edata <- epochdata |>
#' dplyr::filter(subject == 2 & time %in% 1:10 & epoch %in% 1:13)
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

  if (!amplitude %in% colnames(data)) {
    stop(paste0("There is no column '", amplitude, "' in the input data."))
  }

  if (!is.null(template) && !is.null(coords)) {
    warning("Both 'template' and 'coords' were specified. Using 'template' and ignoring 'coords'.")
  }

  if (is.null(template) && is.null(coords)) {
    # use HCGSN256 template
    template <- "HCGSN256"
  }

  if (!is.null(template)) {
    coords <- switch(template,
                     "HCGSN256" = diegr::HCGSN256$D3,
                     stop("Unknown template."))
  }

  required_cols <- c("x", "y", "z", "sensor")
  missing_cols <- setdiff(required_cols, colnames(coords))

  if (length(missing_cols) > 0) {
    stop(paste("The following required columns in 'coords' are missing:",
               paste(missing_cols, collapse = ", ")))
  }


  if (missing(mesh)) {
    if (!is.null(tri)) {
      stop("The argument 'mesh' must be provided when argument 'tri' is specified.")
    }
    mesh <- point_mesh(dimension = c(2,3), template = template, type = "polygon")
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

  if (inherits(data, "tbl_sql") || inherits(data, "tbl_dbi")) {
    data <- dplyr::collect(data) # collect data for DB table
  }

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
    #padding <- 0.05 * diff(range(ycp_IM))
    col_range <- range(ycp_IM) #+ c(-1, 1) * padding
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
