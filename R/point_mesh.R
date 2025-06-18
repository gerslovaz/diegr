#' Create regular mesh of points
#'
#' @description
#' Function creates an object of class \code{"mesh"}, which is a list of data frames with coordinates of a regular (in the sense of the equidistant distance between mesh nodes) mesh of points on the space defined by sensor coordinates. Circular or polygonal shape of the result mesh is available.
#' For the equivalence between 2D and 3D mesh and the possibility to compare models in different dimensions, the thin-plate spline interpolation model \eqn{\mathbb{R}^2 \rightarrow \mathbb{R}^3} is used for creating 3D mesh.
#'
#'
#' @param dim A number (or a vector) indicating a dimension of the mesh: \code{2} for two dimensional, \code{3} for three dimensional mesh and \code{c(2,3)} for both of them in one output (default setting).
#' @param n Optionally, the required number of mesh points. Default setting is \code{n = 10 000}.
#' @param r Optionally, desired radius of a circular mesh. If not defined, it is selected according to the sensor locations.
#' @param template The kind of sensor template montage used. Currently the only available option is \code{"HCGSN256"} denoting the 256-channel HydroCel Geodesic Sensor Net v.1.0.
#' @param own_coordinates Optionally, a list with own sensor coordinates for mesh building, if no pre-defined template is to be used. See Details for more information.
#' @param type A character indicating the shape of the mesh with 2 possible values: \code{"circle"} for circular mesh, \code{"polygon"} for irregular polygon shape with boundaries defined by sensor locations (default).
#'
#' @details
#' In the case of using Geodesic Sensor Net (\code{template = 'HCGSN256'}), the (0,0) point of the resulting 2D mesh corresponds to a reference electrode located at the vertex.
#'
#' The number \code{n} for controlling the mesh density is only an approximate value. The final number of mesh nodes depends on the exact shape of the polygon (created as a convex hull of the sensor locations), and is only close to, not exactly equal to, the number \code{n}.
#'
#' The \code{own_coordinates} enables computing a mesh from user's own sensor locations. The input must be a list containing following elements:
#' \itemize{
#' \item \code{D2} a tibble or data frame with sensor coordinates in named \code{x} and \code{y} columns,
#' \item \code{D3} a tibble or data frame with sensor coordinates in named \code{x}, \code{y} and \code{z} columns.
#' }
#' To build the appropriate meshes in both dimensions, it is necessary to have the input of 3D sensor locations and their corresponding projection onto a plane obtained in another way; the function itself does not perform this projection.
#'
#' @return Returns an object of class \code{"mesh"}. It is a list containing some (or all) of the following components:
#'
#' \item{D2}{A data frame with \code{x} and \code{y} coordinates of the created two dimensional point mesh.}
#' \item{D3}{A data frame with \code{x}, \code{y} and \code{z} coordinates of the created three dimensional point mesh.}
#' \item{template}{A character indicating the template of the sensor coordinates used for mesh computing.}
#' \item{r}{A radius of the circle used for mesh creating.}
#'
#' @references EGI Geodesic Sensor Net Technical Manual (2024)
#'
#' @import sp
#' @importFrom stats na.omit
#' @importFrom grDevices chull
#'
#' @export
#'
#' @examples
#' # Computing circle 2D mesh with starting number 4000 points for HCGSN256 template
#' M <- point_mesh(dim = 2, n = 4000, template = "HCGSN256", type = "circle")
#'
#' # Computing polygon 3D mesh with starting number 2000 points for HCGSN256 template
#' M <- point_mesh(dim = 3, n = 2000, template = "HCGSN256")
#'
#' # Computing coordinates of a polygon mesh in 2D and 3D in one step (starting number 3000 points):
#' M <- point_mesh(n = 3000, template = "HCGSN256")
point_mesh <- function(dim = c(2,3), n = 10000, r, template = NULL, own_coordinates = NULL, type = "polygon") {

  if (is.null(template) && !is.null(own_coordinates)) {
    if (!"D2" %in% names(own_coordinates)) {
      stop("There must be an element named D2 in the own_coordinates list.")
    }
    if (any(dim == 3) && !"D3" %in% names(own_coordinates)) {
      stop("There must be an element named D3 in the own_coordinates list for computing 3D mesh.")
    }
    coordinates <- {{ own_coordinates }}
  } else if (is.null(template) && is.null(own_coordinates)) {
    coordinates <- diegr::HCGSN256

   }


  if (!is.null(template)) {
    coordinates <- switch(template,
                          "HCGSN256" = diegr::HCGSN256)
    if (is.null(coordinates)) {
      stop("Unknown template.")
    }
  }

  coords <- coordinates$D2
  coords_xy <- coords |>
    dplyr::select("x", "y")
  conv_hull <- chull(coords$x, coords$y)
  ch_x <- coords$x[conv_hull]
  ch_y <- coords$y[conv_hull]

  x0 <- mean(ch_x)
  y0 <- mean(ch_y)

  if (missing(r)) {
    Edist <- sqrt((ch_x - x0)^2 + (ch_y - y0)^2)
    r <- max(Edist)
    if (r > 100) {
      r <- ceiling(r)
    }

  }

  N <- round(sqrt(2*n))
  x_vec <- rep(seq(x0 - r, x0 + r, length.out = N), N)
  y_vec <- rep(seq(y0 - r, y0 + r, length.out = N), each = N)
  mesh_circle <- cbind(x_vec, y_vec)

  eu_vec <- sqrt((x_vec - x0)^2 + (y_vec - y0)^2)
  index <- which(eu_vec <= r)
  mesh_circle <- mesh_circle[index,]
  mesh_circle <- data.frame(x = mesh_circle[,1], y = mesh_circle[,2])

  coords_ch <- coords_xy[conv_hull,]
  coords_ch <- rbind(coords_ch, coords_ch[1,])

  if (identical(dim, 2)) {

    switch(type,
           "circle" = {
             mesh_out <- list(D2 = mesh_circle)
           },
           "polygon" = {
             inside <- sp::point.in.polygon(mesh_circle$x, mesh_circle$y, coords_ch$x, coords_ch$y)
             mesh_polygon <- mesh_circle[inside > 0,]
             mesh_out <- list(D2 = data.frame(x = mesh_polygon[,1], y = mesh_polygon[,2]))
           },
           stop("Invalid type argument")
    )
  } else if (identical(dim, 3)) {
    coords_xyz <- coordinates$D3 |>
      dplyr::select("x", "y", "z")
    switch(type,
           "circle" = {
             mesh_out <- list(D3 = recompute_3d(coords_xy, coords_xyz, mesh_circle))
           },
           "polygon" = {
             inside <- sp::point.in.polygon(mesh_circle$x, mesh_circle$y, coords_ch$x, coords_ch$y)
             mesh_polygon <- mesh_circle[inside > 0,]
             mesh_out <- list(D3 = recompute_3d(coords_xy, coords_xyz, mesh_polygon))
           },
           stop("Invalid type argument")
    )
  } else if (identical(dim, c(2, 3)) || identical(dim, c(3, 2))) {
    coords_xyz <- coordinates$D3 |>
      dplyr::select("x", "y", "z")
    switch(type,
           "circle" = {
             mesh_out <- list(D2 = data.frame(x = mesh_circle[,1], y = mesh_circle[,2]),
                              D3 = recompute_3d(coords_xy, coords_xyz, mesh_circle))
           },
           "polygon" = {
             inside <- sp::point.in.polygon(mesh_circle$x, mesh_circle$y, coords_ch$x, coords_ch$y)
             mesh_polygon <- mesh_circle[inside > 0,]
             mesh_out <- list(D2 = data.frame(x = mesh_polygon[,1], y = mesh_polygon[,2]),
                              D3 = recompute_3d(coords_xy, coords_xyz, mesh_polygon))
           },
           stop("Invalid type argument")
    )
  } else {
    stop("Invalid dim argument")
  }

  mesh_out$template <- {{ template }}
  mesh_out$r <- r
  class(mesh_out) <- c("mesh", class(mesh_out))

  return(mesh_out)

}


spline_matrix <- function(X, Xcp = X) {
  ## compute S matrix to using in spline methods for d in c(1,2,3)
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

  diff_sq <- (X1 - X2)^2
  argument <- rowSums(diff_sq)
  if (d == 2) {
    S <- 1 / (8 * pi) * (argument) * log(sqrt(argument))
    S[argument == 0] <- 0
  } else if (d == 3) {
    S <- -1 / (8 * pi) * sqrt(argument)
    S[argument == 0] <- 0
  } else if (d == 1) {
    S <- 1/12 * abs(X1 - X2)^3
  } else {
    stop("X input in spline_matrix must be dim 1, 2 or 3")
  }

  S <- matrix(S, kcp, k, byrow = FALSE)
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

  X_P <- matrix(0, nrow = kcp + d + 1, ncol = k + d + 1)
  X_P[1:kcp, 1:k] <- spline_matrix(X, Xcp)
  X_P[1:kcp, (k + 1):(k + d + 1)] <- cbind(1, Xcp)
  X_P[(kcp + 1):(kcp + d + 1), 1:k] <- rbind(1, t(X))

  return(X_P)
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

  kcp <- dim(mesh)[1]
  Y_Pcp <- IM(X2D, X3D, mesh)$Y_hat
  Y <- data.frame(x = Y_Pcp[1:kcp, 1], y = Y_Pcp[1:kcp, 2], z = Y_Pcp[1:kcp, 3])
  return(Y)
}

