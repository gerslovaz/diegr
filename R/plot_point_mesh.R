#' Plot point mesh
#'
#' @description
#' Function for plotting a mesh of points (not only) created by point_mesh() function. The output is two dimensional plot of point mesh or three dimensional rgl plot depending on the input dimension.
#'
#' @param mesh A data.frame or tibble with cartesian coordinates of point mesh to plot. It could be D2 or D3 output from point_mesh() function or any data.frame (or tibble) with named x and y (x, y and z, respectively) columns. See Details for more information.
#' @param sensors A logical value indicating whether the sensor locations should also be plotted (default value is TRUE).
#' @param names A logical value indicating whether the sensor names should also be plotted (default value is FALSE).
#' @param names.vec A vector with sensor names. The argument is required only when using own.coordinates and setting names = TRUE.
#' @param col.sensors The color of sensor locations points (default color is green).
#' @param cex.sensors The cex argument for sensor location points.
#' @param pch.sensors The pch argument for sensor location points.
#' @param own.coordinates A data.frame or tibble with coordinates of the sensor locations. If NULL, the HCGSN256 template is used.
#' @param col The color of mesh points (default color is gray).
#' @param pch The symbol used for points in the mesh.
#' @param cex The cex argument for points of the mesh.
#' @param axes A logical value indicating, if axes should be plotted (default value is FALSE).
#' @param xlab The label of x axis (default is no label).
#' @param ylab The label of y axis (default is no label).
#' @param ... Further graphical parameters, same as in \link{plot} function.
#'
#' @details The output plot is designed with frontal part of the brain above and occipital part of the brain bottom. The orientation of own.coordinates should be consistent with this, in other case the results could be distorted.
#'
#' @return A plot.
#'
#' @import rgl
#'
#' @export
#'
#' @examples
#' data("HCGSN256")
#' # 2D polygon point mesh with plotted sensors and default settings
#' par(mar = c(0,0,0,0))
#' M <- point_mesh(dim = c(2,3), n = 4000, template = 'HCGSN256', type = 'polygon')
#' plot_point_mesh(M$D2, sensors = TRUE)
#' dev.off()
#'
#' # Plotting 3D polygon point mesh with default settings
#' open3d()
#' plot_point_mesh(M$D3, sensors = TRUE)
#' close3d()
#'
#' # Plotting 2D circle point mesh with sensors as orange points
#' par(mar = c(0,0,0,0))
#' M <- point_mesh(dim = 2, n = 4000, template = 'HCGSN256', type = 'circle')
#' plot_point_mesh(M$D2, sensors = TRUE, col.sensors = "orange")
#' dev.off()
#'
#'
plot_point_mesh <- function(mesh, sensors = TRUE, names = FALSE, names.vec = NULL,
                            col.sensors = "green", cex.sensors = 0.7,
                            pch.sensors = 16, own.coordinates = NULL,
                            col = "gray", pch = 20, cex = 0.4,
                            axes = FALSE, xlab = "", ylab = "", ...){

  if (!(is.logical(sensors))) {
    stop("Argument 'sensors' has to be logical.")
  }
  if (!(is.logical(names))) {
    stop("Argument 'names' has to be logical.")
  }

  if (names == TRUE && !is.null(own.coordinates) && is.null(names.vec)) {
    stop("With using own.coordinates please define the names.vec or set names to FALSE.")
  }

  if (names == TRUE && is.null(own.coordinates)) {
    names.vec <- HCGSN256$number
  }


  if (all(c("x", "y", "z") %in% colnames(mesh))) {
    points3d(mesh$x, mesh$y, mesh$z, col = {{ col }}, pch = {{ pch }}, cex = {{ cex }})

    if (sensors == TRUE) {
      if (is.null(own.coordinates)) {
        own.coordinates <- HCGSN256$D3
      }
      points3d(own.coordinates, col = {{ col.sensors}}, cex = {{ cex.sensors}}, pch = {{ pch.sensors}})

      if (names == TRUE) {
        text3d(own.coordinates, texts = names.vec, cex = 0.9)
      }
    }
  }

  else if (all(c("x", "y") %in% colnames(mesh))) {
    M <- max(mesh$y, na.rm = TRUE)
    M <- 1.05 * M
    m <- min(mesh$y, na.rm = TRUE)
    m <- m - 0.1 * abs(m)
    plot(mesh$x, mesh$y, asp = 1, col = {{ col }}, pch = {{ pch }}, cex = {{ cex }},
         axes = {{ axes }}, xlab = {{ xlab }}, ylab = {{ ylab }},
         ylim = c(m, 1.1 * M), ...)
    segments(0,1.1 * M, -0.1 * M, M, col = "gray60")
    segments(0,1.1 * M, 0.1 * M, M, col = "gray60")

    if (sensors == TRUE) {
      if (is.null(own.coordinates)) {
        own.coordinates <- HCGSN256$D2
      }
      points(own.coordinates, col = {{ col.sensors}}, cex = {{ cex.sensors}}, pch = {{ pch.sensors}})
      if (names == TRUE) {
        text(own.coordinates, labels = names.vec, cex = 0.9)
      }
    }

  }
  else
  stop("The mesh input does not have x and y column.")

}

