#' Create regular mesh of points
#'
#' @description
#' Function creates an object of class \code{"mesh"}, which is a list of data frames with coordinates of a regular (in the sense of the equidistant distance between mesh nodes) mesh of points on the space defined by sensor coordinates.
#'
#'
#' @param dim A number (or a vector) indicating a dimension of the mesh: 2 for two dimensional, 3 for three dimensional mesh and \code{c(2,3)} for both of them in one output (default setting).
#' @param n Optionally, the required number of mesh points. Default setting is \code{n = 10 000}.
#' @param r Optionally, desired radius of a circular mesh. If not defined, it is selected according to the sensor locations.
#' @param template The kind of sensor template montage used. Default setting \code{"HCGSN256"} denotes the 256-channel HydroCel Geodesic Sensor Net v.1.0 (currently the only available option).
#' @param own.coordinates Optionally, a list with own sensor coordinates for mesh building, if no pre-defined template is to be used. See Details for more information.
#' @param type A character indicating the shape of the mesh with 2 possible values: \code{"circle"} for circular mesh (default), \code{"polygon"} for irregular polygon shape with boundaries defined by sensor locations.
#'
#' @details
#' In the case of using Geodesic Sensor Net (\code{template = 'HCGSN256'}), the (0,0) point of the resulting 2D mesh corresponds to a reference electrode located at the vertex.
#'
#' The number \code{n} for controlling the mesh density is only an approximate value. The final number of mesh nodes depends on the exact shape of the polygon (created as a convex hull of the sensor locations), and is only close to, not exactly equal to, the number \code{n}.
#'
#' The \code{own.coordinates} enables computing a mesh from user's own sensor locations. The input must be a list containing following elements:
#' \itemize{
#' \item \code{D2} a tibble or data frame with sensor coordinates in named x and y columns,
#' \item \code{D3} a tibble or data frame with sensor coordinates in named x, y and z columns.
#' }
#'
#'
#' @return Returns an object of class \code{"mesh"}. It is a list containing some (or all) of the following components:
#'
#' \item{D2}{A data frame with x and y coordinates of the created two dimensional point mesh.}
#' \item{D3}{A data frame with x, y and z coordinates of the created three dimensional point mesh.}
#' \item{template}{A character indicating the template of the sensor coordinates used for mesh computing.}
#' \item{r}{A radius of the circle used for mesh creating.}
#'
#' @references EGI Geodesic Sensor Net Technical Manual (2024)
#'
#' @import sf
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#' data("HCGSN256")
#' # Computing circle 2D mesh with starting number 4000 points for HCGSN256 template
#' M <- point_mesh(dim = 2, n = 4000, template = "HCGSN256", type = 'circle')
#'
#' # Computing polygon 3D mesh with starting number 2000 points for HCGSN256 template
#' M <- point_mesh(dim = 3, n = 2000, template = "HCGSN256", type = 'polygon')
#'
#' # Computing coordinates of a polygon mesh in 2D and 3D in one step:
#' M <- point_mesh(dim = c(2,3), n = 2000, template = "HCGSN256", type = 'polygon')
point_mesh <- function(dim = c(2,3), n = 10000, r, template = NULL, own.coordinates = NULL, type = 'circle') {

  if (is.null(template) && !is.null(own.coordinates)) {
    if (!"D2" %in% names(own.coordinates)) {
      stop("There must be an element named D2 in the own.coordinates list.")
    }
    if (any(dim == 3) && !"D3" %in% names(own.coordinates)) {
      stop("There must be an element named D3 in the own.coordinates list for computing 3D mesh.")
    }
    coordinates <- {{ own.coordinates }}
  }


  if (!is.null(template)) {
    coordinates <- switch(template,
                          "HCGSN256" = HCGSN256)
    if (is.null(coordinates)) {
      stop("Unknown template.")
    }
  }

  coords <- coordinates$D2
  conv_hull <- chull(coords$x, coords$y)
  ch_x <- coords$x[conv_hull]
  ch_y <- coords$y[conv_hull]

  x0 <- mean(ch_x)
  y0 <- mean(ch_y)

  if (missing(r)) {
    Edist <- sqrt((ch_x - x0)^2 + (ch_y - y0)^2)
    r <- ceiling(max(Edist))
  }

  N <- round(sqrt(2*n))
  x.vec <- rep(seq(x0 - r, x0 + r, length.out = N), N)
  y.vec <- rep(seq(y0 - r, y0 + r, length.out = N), each = N)
  mesh.circle <- cbind(x.vec, y.vec)

  eu.vec <- sqrt((x.vec - x0)^2 + (y.vec - y0)^2)
  index <- which(eu.vec <= r)
  mesh.circle <- mesh.circle[index,]
  mesh.circle <- data.frame(x = mesh.circle[,1], y = mesh.circle[,2])

  coords.ch <- coords[conv_hull,]
  coords.ch <- rbind(coords.ch, coords.ch[1,])

  if (identical(dim, 2)) {

    switch(type,
           "circle" = {
             mesh.out <- list(D2 = mesh.circle)
           },
           "polygon" = {
             inside <- sp::point.in.polygon(mesh.circle$x, mesh.circle$y, coords.ch$x, coords.ch$y)
             mesh.polygon <- mesh.circle[inside > 0,]
             mesh.out <- list(D2 = data.frame(x = mesh.polygon[,1], y = mesh.polygon[,2]))
           },
           stop("Invalid type argument")
    )
  } else if (identical(dim, 3)) {
    switch(type,
           "circle" = {
             mesh.out <- list(D3 = recompute_3d(coordinates$D2, coordinates$D3, mesh.circle))
           },
           "polygon" = {
             inside <- sp::point.in.polygon(mesh.circle$x, mesh.circle$y, coords.ch$x, coords.ch$y)
             mesh.polygon <- mesh.circle[inside > 0,]
             mesh.out <- list(D3 = recompute_3d(coordinates$D2, coordinates$D3, mesh.polygon))
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
             inside <- sp::point.in.polygon(mesh.circle$x, mesh.circle$y, coords.ch$x, coords.ch$y)
             mesh.polygon <- mesh.circle[inside > 0,]
             mesh.out <- list(D2 = data.frame(x = mesh.polygon[,1], y = mesh.polygon[,2]),
                              D3 = recompute_3d(coordinates$D2, coordinates$D3, mesh.polygon))
           },
           stop("Invalid type argument")
    )
  } else {
    stop("Invalid dim argument")
  }

  mesh.out$template <- {{ template }}
  mesh.out$r <- r
  class(mesh.out) <- c("mesh", class(mesh.out))

  return(mesh.out)

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
  if (!is.matrix(X2D)) {
    X2D <- as.matrix(X2D)
  }

  if (!is.matrix(X3D)) {
    X3D <- as.matrix(X3D)
  }

  if (is.list(mesh) && "D2" %in% names(mesh)) {
    mesh <- mesh$D2
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

