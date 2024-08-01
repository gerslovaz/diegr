#' Create regular mesh of points
#'
#' @description
#' Function creates a data.frame (or list of data.frames) with coordinates of a regular (in the sense of the equidistant distance between mesh nodes) mesh of points on the space defined by sensor coordinates.
#'
#'
#' @param dim A number (or a vector) indicating a dimension of the mesh: 2 for two dimensional, 3 for three dimensional mesh and c(2,3) for both of them in one output (default setting).
#' @param n Optionally, the required number of mesh points. Default setting is n = 10 000.
#' @param r Optionally, desired radius of a circular mesh. If not defined, it is selected according to the sensor locations.
#' @param template The kind of sensor template montage used. Default setting 'HCGSN256' denotes the 256-channel HydroCel Geodesic Sensor Net v.1.0.
#' @param own.coordinates A list with own sensor coordinates for mesh building, if no pre-defined template is to be used. See Details for more information.
#' @param type A character indicating the shape of the mesh with 2 possible values: 'circle' for circular mesh, 'polygon' for irregular polygon shape with boundaries defined by sensor locations.
#'
#' @details
#' In the case of using Geodesic Sensor Net (\code{template = 'HCGSN256'}), the (0,0) point of the resulting mesh corresponds to a reference electrode located at the vertex.
#'
#' The number \code{n} for controlling the mesh density is only approximate value. The final number of mesh nodes depends on the exact shape of the polygon (created as a convex hull of the sensor locations), and is only close to, not exactly equal to, the number \code{n}.
#'
#' The \code{own.coordinates} enables computing a mesh from your own sensor locations. The input must be a list containing following elements:
#' \itemize{
#' \item \code{D2} a tibble or data.frame with sensor coordinates in named x and y columns,
#' \item \code{D3} a tibble or data.frame with sensor coordinates in named x, y and z columns.
#' }
#'
#'
#' @return Returns an object of class \code{"mesh"}. It is a data frame with mesh coordinates - number of columns corresponds to the selected dimension or a list containing the following components:
#'
#' \item{D2}{A data frame with x and y coordinates of the created point mesh.}
#' \item{D3}{A data frame with x, y and z coordinates of the created point mesh.}
#' \item{template}{A character indicating the template of the sensor coordinates used for mesh computing.}
#'
#' @references EGI Geodesic Sensor Net Technical Manual (2024)
#'
#' @import sf
#'
#' @export
#'
#' @examples
#' # Computing circle 2D mesh with starting number 4000 points for HCGSN256 template
#' M <- point_mesh(dim = 2, n = 4000, type = 'circle')
#'
#' # Computing polygon 3D mesh with starting number 2000 points for HCGSN256 template
#' M <- point_mesh(dim = 3, n = 2000, type = 'polygon')
#'
#' # Computing coordinates of a polygon mesh in 2D and 3D in one step:
#' M <- point_mesh(dim = c(2,3), n = 2000, type = 'polygon')
point_mesh <- function(dim = c(2,3), n = 10000, r, template = 'HCGSN256', own.coordinates = NULL, type = 'circle') {
  if (template == 'HCGSN256') {
    coordinates <- HCGSN256
  }
  if (missing(template) && !is.null(own.coordinates)) {
    if (!"D2" %in% names(own.coordinates)) {
      stop("There must be an element named D2 in the own.coordinates list.")
    }
    coordinates <- {{ own.coordinates }}
  }

  if (missing(r)) {
    r <- ceiling(max(abs(range(coordinates$D2))))
  }

  N <- round(sqrt(2*n))
  x.vec <- rep(seq(-r, r, length.out = N), N)
  y.vec <- rep(seq(-r, r, length.out = N), each = N)
  mesh.circle <- cbind(x.vec, y.vec)

  eu.vec <- edist0(x.vec, y.vec)
  index <- which(eu.vec <= r)
  mesh.circle <- mesh.circle[index,]
  mesh.circle <- data.frame(x = mesh.circle[,1], y = mesh.circle[,2])

  if (identical(dim, 2)) {
    switch(type,
      "circle" = {
        mesh.out <- mesh.circle
      },
      "polygon" = {
        mesh.polygon <- make_polygon(coordinates$D2, mesh.circle)
        mesh.out <- data.frame(x = mesh.polygon[,1], y = mesh.polygon[,2])
      },
      stop("Invalid type argument")
    )
  } else if (identical(dim, 3)) {
    switch(type,
           "circle" = {
             mesh.out <- recompute_3d(coordinates$D2, coordinates$D3, mesh.circle)
           },
           "polygon" = {
             mesh.polygon <- make_polygon(coordinates$D2, mesh.circle)
             mesh.out <- recompute_3d(coordinates$D2, coordinates$D3, mesh.polygon)
           },
           stop("Invalid type argument")
    )
  } else if (identical(dim, c(2, 3)) || identical(dim, c(3, 2))) {
    switch(type,
           "circle" = {
             mesh.out <- list(D2 = data.frame(x = mesh.circle[,1], y = mesh.circle[,2]),
                              D3 = recompute_3d(coordinates$D2, coordinates$D3, mesh.circle))
           },
           "polygon" = {
             mesh.polygon <- make_polygon(coordinates$D2, mesh.circle)
             mesh.out <- list(D2 = data.frame(x = mesh.polygon[,1], y = mesh.polygon[,2]),
                              D3 = recompute_3d(coordinates$D2, coordinates$D3, mesh.polygon))
           },
           stop("Invalid type argument")
    )
  } else {
    stop("Invalid dim argument")
  }

  class(mesh.out) <- c("mesh", class(mesh.out))
  mesh.out$template <- {{ template }}
  return(mesh.out)

}


edist0 <- function(x1, x2){
  ## compute euclidean distance of a point (x1,x2) from (0,0)
  distvec <- sqrt(x1^2 + x2^2)
  return(distvec)
}

spline_matrix <- function(X, Xcp = X) {
  ## compute S matrix to using in spline methods for d = 2 or d = 3
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  if (!is.matrix(Xcp)) {
    Xcp <- as.matrix(Xcp)
  }

  if (ncol(X) != ncol(Xcp)) {
    stop("The matrices X and Xcp for spline matrix computing must have the same number of columns")
  }

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
  ## compute X_p for interpolation spline method
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  if (missing(Xcp)) {
    Xcp <- X
  }
  if (!is.matrix(Xcp)) {
    Xcp <- as.matrix(Xcp)
  }

  if (ncol(X) != ncol(Xcp)) {
    stop("The matrices X and Xcp for X.P computing must have the same number of columns")
  }

  k <- dim(X)[1]
  kcp <- dim(Xcp)[1]
  d <- dim(X)[2]
  S <- spline_matrix(X, Xcp)
  kcp.block <- cbind(1, Xcp)
  k.block <- rbind(1, t(X))
  zero.block <- matrix(0, d + 1, d + 1)
  X.P <- cbind(rbind(S, k.block), rbind(kcp.block, zero.block))

  return(X.P)
}


recompute_3d <- function(X2D, X3D, mesh) {
  ## recompute 3D net from 2D coordinates
  if (!is.matrix(X3D)) {
    X3D <- as.matrix(X3D)
  }

  if (any(is.na(mesh))) {
    mesh <- na.omit(mesh)
  }

  k.cp <- dim(mesh)[1]
  X.P <- XP_IM(X2D, X2D)
  Y.P <- rbind(X3D, matrix(0, 3, 3))
  beta.hat <- solve(X.P) %*% Y.P
  X.Pcp <- XP_IM(X2D, mesh)
  Y.Pcp <- X.Pcp %*% beta.hat
  Y <- data.frame(x = Y.Pcp[1:k.cp, 1], y = Y.Pcp[1:k.cp, 2], z = Y.Pcp[1:k.cp, 3])
  return(Y)
}

make_polygon <- function(locations, mesh) {
  # create polygon mesh as convex hull of locations
  if (is.list(mesh) && "D2" %in% names(mesh)) {
    mesh <- mesh$D2
  }

  if (dim(mesh)[2] > 2) {
    mesh <- mesh[,1:2]
    warning("The number of input mesh columns > 2. The first two columns were used for furher computing. You should be careful with the result.")
  }

  if (!is.data.frame(mesh)) {
    mesh <- data.frame(x = mesh[,1], y = mesh[,2])
    warning("The input mesh was not data.frame. The data.frame from first two columns of the input mesh was used for further calculation.")
  }

  if (any(is.na(mesh))) {
    mesh <- na.omit(mesh)
  }

  roi <- st_as_sf(locations, coords = c("x", "y"))
  roi.mp <- st_combine(roi)
  roi.poly <- st_convex_hull(roi.mp) # Define convex hull

  mesh.sf <- st_as_sf(mesh, coords = c("x", "y"))
  mesh.inside <- st_intersection(mesh.sf, roi.poly)
  mesh.inside <- st_coordinates(mesh.inside)

  return(mesh.inside)

}


