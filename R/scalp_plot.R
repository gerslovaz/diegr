#' Plot scalp map of EEG signal
#'
#' @description
#' Plot a scalp polygon map of the EEG signal amplitude using topographic colour scale. The thin-plate spline interpolation model \eqn{\text{IM:}\; \mathbb{R}^3 \rightarrow \mathbb{R}} is used for signal interpolation between the sensor locations. The \code{\link[rgl]{shape3d}} function is used for plotting.
#'
#' @param signal A vector with signal to plot.
#' @param mesh An object of class \code{"mesh"} used for computing IM model. If not defined, the polygon point mesh with default settings from \code{\link{point_mesh}} function is used. Can also be a data frame or a matrix with x, y and z coordinates of a point mesh. See details for more information about the structure.
#' @param tri A matrix with indices of the triangles. If missing, the triangulation is computed using \code{\link{make_triangulation}} function from \code{D2} element of the input mesh object (or a list).
#' @param coords Sensor coordinates as a tibble or data frame with named \code{x} and \code{y} columns. If not defined, the HCGSN256 template is used.
#' @param col.range A vector with minimum and maximum value of the amplitude used in the colour palette for plotting. If not defined, the range of the input signal is used.
#' @param col.scale Optionally, a colour scale to be utilised for plotting. If not defined, it is computed from \code{col.range}.
#'
#' @details
#' The parameter \code{mesh} should optimally be a \code{"mesh"} object (output from \code{\link{point_mesh}} function) or a list with the same structure (see \code{\link{point_mesh}} for more information). In that case, setting the argument \code{tri} is optional, and if it is absent, a triangulation based on the \code{D2} element of the mesh is calculated and used in the plot.
#' If the input \code{mesh} is a data frame or a matrix with only 3D coordinates of a point mesh, the use of previously created triangulation (through \code{tri} argument) is necessary.
#' To compare results between 2D topographical plot and 3D scalp plot use the same mesh in both cases.
#'
#' Be careful when choosing the argument \code{col.range}. If the input \code{signal} contains values outside the chosen range, this will cause "holes" in the resulting plot.
#' To compare results for different subjects or conditions, set the same values of \code{col.range} and \code{col.scale} arguments in all cases.
#' The default used scale is based on topographical colours with zero value always at the border of blue and green shades.

#'
#' @return A rgl plot of scalp EEG signal.
#' @export
#'
#' @import rgl
#'
#' @examples
#' # Plot average scalp map of signal for subject 2 from the time point 10 (the time of the stimulus)
#' # the outliers (epoch 14 and 15) are extracted before computing average
#'
#' # a) preparing data
#' s1 <- epochdata |>
#' dplyr::filter(time == 10 & subject == 2 & !epoch %in% c(14,15)) |>
#' dplyr::select(signal, sensor, epoch) |>
#' dplyr::group_by(sensor) |>
#' dplyr::mutate(average = mean(signal, na.rm = TRUE))
#' s1 <- s1$average[1:204]
#'
#' # b) plotting the scalp polygon map
#' scalp_plot(signal = s1, col.range = c(-30, 15))

scalp_plot <- function(signal, mesh, tri,
                      coords = NULL, col.range = NULL, col.scale = NULL) {

  if (missing(mesh)) {
    mesh <- point_mesh(dim = c(2,3), template = "HCGSN256", type = "polygon")
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
  if (is.null(col.range)) {
    col.range <- range(signal)
  }
  if (is.null(col.scale)) {
    col.scale <- create_scale(col.range)
  }
  if (is.null(coords)) {
    coords <- diegr::HCGSN256$D3
  }

  y.hat <- IM(coords, signal, mesh3)$Y.hat
  ycp.IM <- y.hat[1:length(mesh3[,1])]

  y.cut <- cut(ycp.IM, breaks = col.scale$breaks, include.lowest = TRUE)
  y.col <- col.scale$colors[y.cut]

  shade3d(mesh3d(x = mesh3$x, y = mesh3$y, z = mesh3$z, triangles = t(tri)),
          col = y.col, lit = FALSE)
 }


