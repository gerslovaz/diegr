#' Plot point mesh
#'
#' @description
#' Function for plotting a mesh of points (not only) created by \code{\link{point_mesh}} function. The output is two dimensional \code{ggplot} of point mesh or three dimensional \code{rgl} plot depending on the input dimension.
#'
#' @param mesh A data frame or tibble with cartesian coordinates of point mesh to plot. It could be \code{D2} or \code{D3} element of output from \code{\link{point_mesh}} function or any data frame (or tibble) with named x and y (x, y and z, respectively) columns. See Details for more information.
#' @param sensors A logical value indicating whether the sensor locations should also be plotted (default value is \code{TRUE}).
#' @param names A logical value indicating whether the sensor names should also be plotted (default value is \code{FALSE}).
#' @param names.vec A vector with sensor names. The argument is required when using \code{own.coordinates} together with setting \code{names = TRUE}, otherwise is optional.
#' @param col The colour of mesh points (default colour is gray).
#' @param cex The \code{cex} argument for points of the mesh.
#' @param col.sensors The colour of sensor locations points (default colour is green).
#' @param own.coordinates A data frame or tibble with coordinates of the sensor locations. If the value is \code{NULL} and \code{sensors} is set to \code{TRUE}, the HCGSN256 template is used.
#'
#' @details Please follow the instructions below when entering \code{own.coordinates}:
#'
#' The output plot is designed with frontal part of the brain above and occipital part of the brain bottom. The orientation of \code{own.coordinates} should be consistent with this. In other case the results could be distorted.
#'
#' For displaying 3D rgl plot, the \code{own.coordinates} must contain the x, y and z coordinates of the sensors, otherwise the function does not work correctly.
#'
#' The order of elements in \code{names.vec} must be consistent with elements of \code{own.coordinates}.
#'
#' When both \code{names.vec} and \code{own.coordinates} are provided, it is essential that the length of \code{names.vec} matches the number of rows in \code{own.coordinates}, otherwise the names are not plotted (despite the setting \code{names = TRUE}).
#'
#' @return A plot.
#'
#' @import rgl
#'
#' @export
#'
#' @examples
#' # 2D polygon point mesh with plotted sensors and default settings
#' par(mar = c(0,0,0,0))
#' M <- point_mesh(n = 4000, template = "HCGSN256")
#' plot_point_mesh(M$D2)
#'
#' # Plotting 3D polygon point mesh with default settings
#' plot_point_mesh(M$D3)
#'
#' # Plotting 2D circle point mesh with sensors as orange points
#' par(mar = c(0,0,0,0))
#' M <- point_mesh(dim = 2, n = 4000, template = "HCGSN256", type = "circle")
#' plot_point_mesh(M$D2, col.sensors = "orange")
#'
#' # Plotting the same mesh with marking only midline electrodes
#' midline <- HCGSN256$D2[c(8, 15, 21, 26, 78, 86, 95, 111, 117, 127, 136, 204),]
#' names.vec <- HCGSN256$sensor[c(8, 15, 21, 26, 78, 86, 95, 111, 117, 127, 136, 204)]
#' par(mar = c(0,0,0,0))
#' plot_point_mesh(M$D2, names = TRUE, names.vec = names.vec, own.coordinates = midline)
#'
plot_point_mesh <- function(mesh, sensors = TRUE, names = FALSE, names.vec = NULL,
                            col = "gray", cex = 0.4, col.sensors = "green",
                            own.coordinates = NULL ) {

  if (!(is.logical(sensors))) {
    stop("Argument 'sensors' has to be logical.")
  }
  if (!(is.logical(names))) {
    stop("Argument 'names' has to be logical.")
  }

  if (names == TRUE && !is.null(own.coordinates) && is.null(names.vec)) {
    stop("With using own.coordinates please define the names.vec or set names to FALSE.")
  }

  if (names == TRUE && is.null(names.vec)) {
    names.vec <- HCGSN256$sensor
  }


  if (all(c("x", "y", "z") %in% colnames(mesh))) {
    rgl::points3d(mesh$x, mesh$y, mesh$z, col = {{ col }}, cex = {{ cex }})

    if (is.null(own.coordinates)) {
      own.coordinates <- HCGSN256$D3
    }

    if (sensors == TRUE) {

      rgl::points3d(own.coordinates, col = {{ col.sensors}}, size = 5)
    }

    if (names == TRUE) {

      if (length(names.vec) != length(own.coordinates$x)) {
        warning("The length of 'names.vec' must be the same as the number of coordinates rows. Names are not plotted.")
      } else { rgl::text3d(own.coordinates, texts = names.vec, cex = 0.7) }
    }

  }

  else if (all(c("x", "y") %in% colnames(mesh))) {

    if (is.null(own.coordinates)) {
      own.coordinates <- HCGSN256$D2
    }

    M <- max(max(mesh$y, na.rm = TRUE), max(own.coordinates$y))
    x0 <- mean(mesh$x, na.rm = TRUE)

    g <- ggplot(mesh, aes(x = x, y = y)) +
      geom_point(col = { col }, cex = { cex }) +
      coord_fixed(ratio = 1) +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()
      ) +
      annotate("segment", x = x0, y = M + 0.07 * abs(M), xend = x0 - 0.08 * M, yend = M + 0.01 * abs(M), col = "gray40") +
      annotate("segment", x = x0, y = M + 0.07 * abs(M), xend = x0 + 0.08 * M, yend = M + 0.01 * abs(M), col = "gray40")

    if (sensors == TRUE) {
       g <- g +
        annotate("point", x = own.coordinates$x, y = own.coordinates$y, col = { col.sensors })
    }

    if (names == TRUE) {
      if (length(names.vec) != length(own.coordinates$x)) {
        warning("The length of 'names.vec' must be the same as the number of coordinates rows. Names are not plotted.")
      } else {
        g <- g + geom_text(data = own.coordinates, aes(label = names.vec), size = 2, vjust = -0.9) }
          }

    g

  }
  else
  stop("The mesh input does not have x and y column.")

}

