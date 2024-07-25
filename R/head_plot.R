head_plot <- function(mesh, tri, mesh2d = NULL, signal = NULL, col.scale) {
  if (!all(c("x", "y", "z") %in% colnames(mesh))) {
    stop("The input mesh must contain the columns x, y and z.")
  }

  ## zapotrebi poradne osetrit chybejici site

  if (missing(tri)) {
    tri <- make_triangulation(mesh2d) # zatim dobre nefunguje pri chybejicim zadani tri
  }

  y.col <- cut_signal(signal = signal, mesh = mesh2d, col.scale = col.scale)

  open3d()
  shade3d(mesh3d(x = mesh$x, y = mesh$y, z = mesh$z, triangles = t(tri)),
          col = y.col, lit = FALSE)
## jeste neco nesedi s barvama...
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

