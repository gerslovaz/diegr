#' Plot point mesh
#'
#' #' @description
#' Function for plotting a mesh of points  (not only) created by point_mesh() function.
#'
#' @param mesh A coordinate data frame of point mesh to plot.
#' @param sensors A logical value indicating whether the sensor locations should also be plotted.
#' @param names A logical value indicating whether the sensor names should also be plotted.
#' @param col.sensors The color of sensor locations points.
#' @param cex.sensors The cex argument for sensor location points.
#' @param pch.sensors The pch argument for sensor location points.
#' @param own.coordinates A list with coordinates of the sensor locations. If NULL, the HCGSN256 template is used.
#' @param col The color of mesh points. Default is gray.
#' @param pch The symbol used for points in the mesh.
#' @param cex The cex argument for points of the mesh.
#' @param axes A logical value indicating, if axes should be plotted. Default value is FALSE.
#' @param xlab The label of x axis (default is no label).
#' @param ylab The label of y axis (default is no label).
#' @param ... Further graphical parameters, same as in \link{plot} function.
#'
#' @return A plot.
#'
#' @import rgl
#'
#' @export
#'
#' @examples
#' data(HCGSN256)
#' # 2D polygon point mesh with plotted sensors and default settings
#' par(mar = c(0,0,0,0))
#' M <- point_mesh(dim = 2, n = 4000, type = 'polygon')
#' plot_point_mesh(M, sensors = TRUE)
#' dev.off()
#'
#' # Plotting 2D circle point mesh with sensors as orange points
#' par(mar = c(0,0,0,0))
#' M <- point_mesh(dim = 2, n = 4000, type = 'circle')
#' plot_point_mesh(M, sensors = TRUE, col.sensors = "orange")
#' dev.off()
#'
plot_point_mesh <- function(mesh, sensors = TRUE, names = FALSE,
                            col.sensors = "green", cex.sensors = 0.7,
                            pch.sensors = 16, own.coordinates = NULL,
                            col = "gray", pch = 20, cex = 0.4,
                            axes = FALSE, xlab = "", ylab = "", ...){

  M <- max(mesh[,2], na.rm = TRUE)
  M <- 1.05 * M
  m <- min(mesh[,2], na.rm = TRUE)
  m <- m - 0.1 * abs(m)

  if (ncol(mesh) == 2) {
    plot(mesh, asp = 1, col = {{ col }}, pch = {{ pch }}, cex = {{ cex }},
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
        text(own.coordinates, labels = HCGSN256$number, cex = 0.9)
      }
    }

  }

  else if (ncol(mesh) == 3) {
    points3d(mesh, col = {{ col }}, pch = {{ pch }}, cex = {{ cex }})

    if (sensors == TRUE) {
      if (is.null(own.coordinates)) {
        own.coordinates <- HCGSN256$D3
      }
      points3d(own.coordinates, col = {{ col.sensors}}, cex = {{ cex.sensors}}, pch = {{ pch.sensors}})

      if (names == TRUE) {
        text3d(own.coordinates, texts = HCGSN256$number, cex = 0.9)
      }
    }
  }
  else
  stop("The mesh input must have 2 or 3 columns.")

}

