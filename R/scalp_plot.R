#' Plot scalp map of EEG signal
#'
#' @description
#' Plot a scalp polygon map of the EEG signal amplitude using topographic colour scale. The thin-plate spline interpolation model \eqn{\text{IM:}\; \mathbb{R}^3 \rightarrow \mathbb{R}} is used for signal interpolation between the sensor locations. The \code{\link[rgl]{shape3d}} function is used for plotting.
#'
#' @param data A data frame, tibble or a database table with input data to plot with at least two columns: \code{sensor} with sensor labels and the column with the EEG amplitude specified in the argument \code{amplitude}.
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values.
#' @param mesh An object of class \code{"mesh"} used for computing IM model. If not defined, the polygon point mesh with default settings from \code{\link{point_mesh}} function is used. Can also be a data frame or a matrix with x, y and z coordinates of a point mesh. See details for more information about the structure.
#' @param tri A matrix with indices of the triangles. If missing, the triangulation is computed using \code{\link{make_triangulation}} function from \code{D2} element of the input mesh object (or a list).
#' @param coords Sensor coordinates as a tibble or data frame with named \code{x}, \code{y}, \code{z} and \cite{sensor} columns. The \code{sensor} labels must match the labels in sensor column in \code{data}. If not defined, the HCGSN256 template is used.
#' @param template The kind of sensor template montage used. Currently the only available option is \code{"HCGSN256"} denoting the 256-channel HydroCel Geodesic Sensor Net v.1.0, which is also a default setting.
#' @param col_range A vector with minimum and maximum value of the amplitude used in the colour palette for plotting. If not defined, the range of the input signal is used.
#' @param col_scale Optionally, a colour scale to use for plotting. If not defined, it is computed from \code{col_range}.
#' @param view A character denoting the view of the plot (according to neurological terminology). Possible values are: \code{"superior", "anterior", "posterior", "left", "right"}. If missing, the default view according to user settings is displayed.
#'
#' @details
#' The parameter \code{mesh} should optimally be a \code{"mesh"} object (output from \code{\link{point_mesh}} function) or a list with the same structure (see \code{\link{point_mesh}} for more information). In that case, setting the argument \code{tri} is optional, and if it is absent, a triangulation based on the \code{D2} element of the mesh is calculated and used in the plot.
#' If the input \code{mesh} is a data frame or a matrix with only 3D coordinates of a point mesh, the use of previously created triangulation (through \code{tri} argument) is necessary.
#' To compare results between 2D topographical plot and 3D scalp plot use the same mesh in both cases.
#'
#' Be careful when choosing the argument \code{col_range}. If the amplitude in input data contains values outside the chosen range, this will cause "holes" in the resulting plot.
#' To compare results for different subjects or conditions, set the same values of \code{col_range} and \code{col_scale} arguments in all cases.
#' The default used scale is based on topographical colours with zero value always at the border of blue and green shades.

#'
#' @return A rgl plot of scalp EEG signal.
#' @export
#'
#' @import rgl
#' @importFrom rlang .data
#'
#' @examples
#' # Plot average scalp map of signal for subject 2 from the time point 10 (the time of the stimulus)
#' # the outliers (epoch 14 and 15) are extracted before computing
#'
#' # a) preparing data
#' edata <- epochdata |>
#' dplyr::filter(subject == 2 & time %in% 1:10 & epoch %in% 1:13)
#' # a2) baseline correction (needed for suitable topographic map)
#' data_base <- baseline_correction(edata, base_int = 1:10)
#' # a3) average computing
#' data_mean <- compute_mean(data_base, amplitude = "signal_base", subject = 2, time = 10,
#'  type = "point", ex_epoch = c(14,15))

#'
#' # b) plotting the scalp polygon map
#' scalp_plot(data_mean, amplitude = "average", col_range = c(-30, 15))

scalp_plot <- function(data, amplitude, mesh, tri,
                      coords = NULL, template = NULL,
                      col_range = NULL, col_scale = NULL,
                      view = "posterior") {

  amp_value <- {{ amplitude }}
  amp_name <- rlang::as_string(amp_value)

  if (!amp_name %in% names(data)) {
    stop(paste0("There is no column '", amp_name, "' in the input data."))
  }

  if (!is.null(template)) {
    coords <- switch(template,
                     "HCGSN256" = diegr::HCGSN256$D3,
                     stop("Unknown template."))
  }

  if (is.null(template) && is.null(coords)) {
    coords <- diegr::HCGSN256$D3
  }

  required_cols <- c("x", "y", "z", "sensor")
  missing_cols <- setdiff(required_cols, colnames(coords))

  if (length(missing_cols) > 0) {
    stop(paste("The following required columns in 'coords' are missing:",
               paste(missing_cols, collapse = ", ")))
  }


  if (missing(mesh)) {
    mesh <- point_mesh(dim = c(2,3), template = {{ template }}, type = "polygon")
  }

  if (is.list(mesh) && !is.data.frame(mesh)) {
    if (all(c("D3", "D2") %in% names(mesh))) {
      mesh3 <- mesh$D3
      mesh2 <- mesh$D2
    } else if (!("D2" %in% names(mesh)) && !missing(tri)) {
      mesh3 <- mesh$D3
    } else {
      stop("Elements D2 and D3 are required in input 'mesh' list if argument 'tri' is not defined.")
    }
  } else {
    if (!missing(tri)) {
      mesh3 <- mesh
    } else{
      stop("The 'mesh' input with only 3D coordinates of the mesh needs a 'tri' argument setting.")
    }

  }


  if (dim(mesh3)[2] < 3) {
    stop("The input 3D mesh must contain at least three columns.")
  }
  if (!all(c("x", "y", "z") %in% colnames(mesh3))) {
    mesh3 <- mesh3[,1:3]
    colnames(mesh3) <- c("x","y","z")
    warning("The input 3D mesh does not contain the columns x, y and z. The first three column were used instead.")
  }

  if (missing(tri)) {
    tri <- make_triangulation(mesh2)
  }
  if (is.null(col_range)) {
    col_range <- range(data[[amp_name]])
  }
  if (is.null(col_scale)) {
    col_scale <- create_scale(col_range)
  }

  coords_xyz <- coords |>
    dplyr::select("x", "y", "z")

  sensor_order <- as.factor(coords$sensor) # reorder data according to sensor
  data_order <- data |>
    mutate(sensor = factor(.data$sensor, levels = sensor_order)) |>
    arrange(.data$sensor)


  y_hat <- IM(coords_xyz, data_order[[amp_name]], mesh3)$Y_hat
  ycp_IM <- y_hat[1:length(mesh3[,1])]


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

    rotate_view(U, plot_function = rgl::shade3d(rgl::mesh3d(x = mesh3$x, y = mesh3$y, z = mesh3$z, triangles = t(tri)),
                                                col = y_col, lit = FALSE))
  }

 }


rotate_view <- function(U, plot_function) {

  current <- rgl::currentSubscene3d()
  newscene <- rgl::newSubscene3d(copyShapes = TRUE)
  rgl::useSubscene3d(newscene)

  rgl::par3d(userMatrix = U) # rotate view

  plot_function

  rgl::useSubscene3d(current) # original subscene
}

req_cols <- function(obj, required_cols) {
  # control required columns
  is.atomic(names(obj)) && all(required_cols %in% names(obj))
}

control_mesh <- function(mesh, tri = NULL) {
  # control the structure of the mesh

  if (is.null(tri)) {

    if (inherits(mesh, "mesh")) {
      #mesh2 <- mesh$D2
      #mesh3 <- mesh$D3
    } else if (is.list(mesh) && all(c("D2", "D3") %in% names(mesh))) {
      if (!req_cols(mesh$D2, c("x", "y"))) {
        stop("Columns 'x', 'y' are required in 'D2' for missing 'tri' argument.")
      } else {
        #mesh2 <- mesh$D2
      }
      if (!req_cols(mesh$D3, c("x", "y", "z"))) {
        stop("Columns 'x', 'y', 'z' are required in 'D3' part of a mesh.")
      } else {
        #mesh3 <- mesh$D3
      }
    } else {
      stop("Elements D2 and D3 are required in input 'mesh' list if argument 'tri' is not defined.")
    }

  } else {

    if (inherits(mesh, "mesh")) {
      #mesh3 <- mesh$D3
    } else if (is.list(mesh) && "D3" %in% names(mesh)) {
      if (!req_cols(mesh$D3, c("x", "y", "z"))) {
        stop("Columns 'x', 'y', 'z' are required in 'D3' part of a mesh.")
      }
      #mesh3 <- mesh$D3

    } else if (req_cols(mesh, c("x", "y", "z"))) {
      #mesh3 <- mesh

    } else {
      stop("Invalid 'mesh' input.")
    }
  }

}
