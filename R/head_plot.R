#' Plot scalp map of EEG signal
#'
#' @description
#' A short description...
#'
#'
#' @param mesh A data frame with coordinates of 3D point mesh.
#' @param tri A matrix with indices of the triangles.
#' @param mesh2d A data frame with coordinates of 2D point mesh (needed for triangulation computing, if argument tri is missing).
#' @param signal A vector with signal to plot.
#' @param col.scale A color scale which should be used for plotting.
#'
#' @return A rgl plot of scalp EEG signal.
#' @export
#'
#' @import rgl
#'
#' @examples
#' # Tady potrebujeme doplnit priklad
head_plot <- function(mesh, tri, mesh2d = NULL, signal = NULL, col.scale) {
  if (!all(c("x", "y", "z") %in% colnames(mesh))) {
    stop("The input mesh must contain the columns x, y and z.")
  }

  ## zapotrebi poradne osetrit chybejici site
  ## a take chybejici barevnou skalu

  if (missing(tri)) {
    tri <- make_triangulation(mesh2d)
  }

  y.col <- cut_signal(signal = signal, mesh = mesh2d, col.scale = col.scale)

  open3d()
  shade3d(mesh3d(x = mesh$x, y = mesh$y, z = mesh$z, triangles = t(tri)),
          col = y.col, lit = FALSE)
 }



cut_signal <- function(signal, mesh, coords, col.scale) {
  if (missing(coords)) {
    coords <- HCGSN256$D2
  }

  if (ncol(mesh) > 2) {
    mesh <- mesh[,1:2]
  }

  beta.hat <- IM(coords,signal)
  X.Pcp <- XP_IM(coords, mesh)
  y.Pcp <- X.Pcp %*% beta.hat
  ycp.IM2 <- y.Pcp[1:length(mesh[,1])]

  y.cut <- cut(ycp.IM2, breaks = col.scale$breaks, include.lowest = TRUE)
  y.color <- col.scale$colors[y.cut]
  return(y.color)
}

## Bude zapotrebi udelat vice univerzalne - zamyslet se nad pripady, kde potrebuji obe site - mozna udelat neco jako mesh.objekt, kde bude vsechno v jednom.
