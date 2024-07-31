#' Create regular mesh of points
#'
#' @description
#' Function creates a regular (in the sense ...) mesh of points on the space defined by sensor coordinates.
#'
#'
#' @param dim A number indicating a dimension of the mesh: 2 for two dimensional and 3 for three dimensional mesh.
#' @param n Optionally, the required number of mesh points. Default setting is n = 10 000. (doplnit, ze cislo neni presne)
#' @param r Optionally, desired radius of a circular mesh. If not defined, it is selected according to the sensor locations. (doplnit, kde je nulovy bod)
#' @param template The kind of sensor template montage used. Default setting 'HCGSN256' denotes the 256-channel HydroCel Geodesic Sensor Net v.1.0.
#' @param type A character indicating the shape of the mesh with 2 possible values: 'circle' for circular mesh, 'polygon' for irregular polygon shape with boundaries defined by used sensor locations.
#'
#' @return A data frame with number of columns according to selected dimension of coordinates of mesh points.
#'
#' @import sf
#'
#' @export
#'
#' @examples
#' # Computing circle 2D mesh with starting number 4000 points for HCGSN256 template
#' M <- point_mesh(dim = 2, n = 4000, type = 'circle')
#' # Computing polygon 3D mesh with starting number 2000 points for HCGSN256 template
#' M <- point_mesh(dim = 3, n = 2000, type = 'polygon')
point_mesh <- function(dim, n = 10000, r, template = 'HCGSN256', type = 'circle') {
  if (template == 'HCGSN256') {
    coordinates <- HCGSN256
  }
  if (missing(r)) {
    r <- ceiling(max(abs(range(coordinates$D2))))
  }

  N <- round(sqrt(2*n))
  x.vec <- rep(seq(-r, r, length.out = N), N)
  y.vec <- rep(seq(-r, r, length.out = N), each = N)
  mesh.circle <- cbind(x.vec, y.vec)

  eu.vec <- edist0(x.vec, y.vec)
  out.vec <- which(eu.vec > r)
  mesh.circle[out.vec,] <- NA
  mesh.circle <- data.frame(x = mesh.circle[,1], y = mesh.circle[,2])

  if (dim == 2) {
    mesh.out <- mesh.circle
  }

  if (type == 'polygon') {
    mesh.polygon <- make_polygon(coordinates$D2 ,mesh.circle)
    mesh.out <- data.frame(x = mesh.polygon[,1], y = mesh.polygon[,2])

    if (dim == 3) {
      mesh.out <- recompute_3d(coordinates$D2, coordinates$D3, mesh.polygon)
    }

  }

  if (dim == 3 && type == 'circle') {
   mesh.out <- recompute_3d(coordinates$D2, coordinates$D3, mesh.circle)
  }

  return(mesh.out)

}


edist0 <- function(x1, x2){
  ## compute euclidean distance of a point (x1,x2) from (0,0)
  distvec <- sqrt(x1^2 + x2^2)
  return(distvec)
}

spline_matrix <- function(X, Xcp = X) {
  ## computing S matrix to using in spline methods for d = 2 or d = 3
  X <- as.matrix(X)
  k <- dim(X)[1]
  kcp <- dim(Xcp)[1]
  d <- dim(X)[2]
  X1 <- t(matrix(rep(t(Xcp), k), nrow = d))
  X2 <- matrix(rep(X, each = kcp), ncol = d)
  diff.sq <- (X1 - X2)^2
  argument <- rowSums(diff.sq)
  if (d == 2) {
    S <- 1 / (8 * pi) * (argument) * log(sqrt(argument))
  } else if (d == 3) S <- -1 / (8 * pi) * sqrt(argument)
  S[argument == 0] <- 0
  S <- matrix(S, kcp, k, byrow = F)
  return(S)
}

XP_IM <- function(X, Xcp) {
  ## computing X_p for interpolation spline method
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  if (missing(Xcp)) {
    Xcp <- X
  }
  if (!is.matrix(Xcp)) {
    Xcp <- as.matrix(Xcp)
  }
  k <- dim(X)[1]
  kcp <- dim(Xcp)[1]
  d <- dim(X)[2]
  X.P <- matrix(0, kcp + 1 + d, k + 1 + d)
  X.P[1:kcp, 1:k] <- spline_matrix(X, Xcp)
  X.P[1:kcp, k + 1] <- 1
  X.P[kcp + 1, 1:k] <- 1
  X.P[1:kcp, (k + 2):(k + d + 1)] <- Xcp
  X.P[(kcp+2):(kcp+d+1), 1:k] <- t(X)
  return(X.P)
}


recompute_3d <- function(X2D, X3D, mesh) {
  ## recomputing 3D net from 2D coordinates
  X3D <- as.matrix(X3D)
  mesh <- na.omit(mesh)
  k.m <- dim(mesh)[1]
  X.P <- XP_IM(X2D)
  Y.P <- rbind(X3D, matrix(0, 3, 3))
  beta.hat <- solve(X.P) %*% Y.P
  X.Pcp <- XP_IM(X2D, mesh)
  Y.Pcp <- X.Pcp %*% beta.hat
  Y <- data.frame(x = Y.Pcp[1:k.m, 1], y = Y.Pcp[1:k.m, 2], z = Y.Pcp[1:k.m, 3])
  return(Y)
}

make_polygon <- function(locations, mesh) {
  roi <- st_as_sf(locations, coords = c("x", "y"))
  roi.mp <- st_combine(roi)
  roi.poly <- st_convex_hull(roi.mp) # Define convex hull

  mesh <- na.omit(mesh)
  mesh.sf <- st_as_sf(mesh, coords = c("x", "y"))
  mesh.inside <- st_intersection(mesh.sf, roi.poly)
  mesh.inside <- st_coordinates(mesh.inside)

  return(mesh.inside)

}


