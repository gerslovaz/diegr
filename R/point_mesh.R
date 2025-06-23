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



#' Plot point mesh
#'
#' @description
#' Function for plotting a mesh of points (not only) created by \code{\link{point_mesh}} function. The output is two dimensional \code{ggplot} of point mesh or three dimensional \code{rgl} plot depending on the input dimension.
#'
#' @param mesh A data frame or tibble with cartesian coordinates of point mesh to plot. It could be \code{D2} or \code{D3} element of output from \code{\link{point_mesh}} function or any data frame (or tibble) with named x and y (x, y and z, respectively) columns. See Details for more information.
#' @param sensors A logical value indicating whether the sensor locations should also be plotted (default value is \code{TRUE}).
#' @param names A logical value indicating whether the sensor names should also be plotted (default value is \code{FALSE}).
#' @param names_vec A vector with sensor names. The argument is required when using \code{own_coordinates} together with setting \code{names = TRUE}, otherwise is optional.
#' @param col The colour of mesh points (default colour is gray).
#' @param cex The \code{cex} argument for points of the mesh.
#' @param col_sensors The colour of sensor locations points (default colour is green).
#' @param own_coordinates A data frame or tibble with coordinates of the sensor locations. If the value is \code{NULL} and \code{sensors} is set to \code{TRUE}, the HCGSN256 template is used.
#'
#' @details Please follow the instructions below when entering \code{own_coordinates}:
#'
#' The output plot is designed with frontal part of the brain above and occipital part of the brain bottom. The orientation of \code{own_coordinates} should be consistent with this. In other case the results could be distorted.
#'
#' For displaying 3D rgl plot, the \code{own_coordinates} must contain the x, y and z coordinates of the sensors, otherwise the function does not work correctly.
#'
#' The order of elements in \code{names_vec} must be consistent with elements of \code{own_coordinates}.
#'
#' When both \code{names_vec} and \code{own_coordinates} are provided, it is essential that the length of \code{names_vec} matches the number of rows in \code{own_coordinates}, otherwise the names are not plotted (despite the setting \code{names = TRUE}).
#'
#' @return A plot.
#'
#' @import rgl
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' # 2D polygon point mesh with plotted sensors and default settings
#' par(mar = c(0,0,0,0))
#' M <- point_mesh(n = 4000, template = "HCGSN256")
#' plot_point_mesh(M$D2)
#'
#' \dontrun{
#' ## Note: the example opens a rgl 3D viewer
#' # Plotting 3D polygon point mesh with default settings
#' plot_point_mesh(M$D3)
#' }
#'
#' # Plotting 2D circle point mesh with sensors as orange points
#' par(mar = c(0,0,0,0))
#' M <- point_mesh(dim = 2, n = 4000, template = "HCGSN256", type = "circle")
#' plot_point_mesh(M$D2, col_sensors = "orange")
#'
#' # Plotting the same mesh with marking only midline electrodes
#' midline <- HCGSN256$D2[c(8, 15, 21, 26, 78, 86, 95, 111, 117, 127, 136, 204),]
#' names_vec <- HCGSN256$sensor[c(8, 15, 21, 26, 78, 86, 95, 111, 117, 127, 136, 204)]
#' par(mar = c(0,0,0,0))
#' plot_point_mesh(M$D2, names = TRUE, names_vec = names_vec, own_coordinates = midline)
#'
plot_point_mesh <- function(mesh, sensors = TRUE, names = FALSE, names_vec = NULL,
                            col = "gray", cex = 0.4, col_sensors = "green",
                            own_coordinates = NULL ) {

  if (!(is.logical(sensors))) {
    stop("Argument 'sensors' has to be logical.")
  }
  if (!(is.logical(names))) {
    stop("Argument 'names' has to be logical.")
  }

  if (names == TRUE && !is.null(own_coordinates) && is.null(names_vec)) {
    stop("With using own_coordinates please define the names_vec or set names to FALSE.")
  }

  if (names == TRUE && is.null(names_vec)) {
    names_vec <- diegr::HCGSN256$sensor
  }


  if (all(c("x", "y", "z") %in% colnames(mesh))) {
    rgl::points3d(mesh$x, mesh$y, mesh$z, col = {{ col }}, cex = {{ cex }})

    if (is.null(own_coordinates)) {
      own_coordinates <- diegr::HCGSN256$D3
    }

    if (sensors == TRUE) {

      rgl::points3d(own_coordinates, col = {{ col_sensors}}, size = 5)
    }

    if (names == TRUE) {

      if (length(names_vec) != length(own_coordinates$x)) {
        warning("The length of 'names_vec' must be the same as the number of coordinates rows. Names are not plotted.")
      } else { rgl::text3d(own_coordinates, texts = names_vec, cex = 0.7) }
    }

  }

  else if (all(c("x", "y") %in% colnames(mesh))) {

    if (is.null(own_coordinates)) {
      own_coordinates <- diegr::HCGSN256$D2
    }

    M <- max(max(mesh[["y"]], na.rm = TRUE), max(own_coordinates[["y"]]))
    x0 <- mean(mesh[["x"]], na.rm = TRUE)

    g <- ggplot(mesh, aes(x = .data$x, y = .data$y)) +
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
        annotate("point", x = own_coordinates$x, y = own_coordinates$y, col = { col_sensors })
    }

    if (names == TRUE) {
      if (length(names_vec) != length(own_coordinates$x)) {
        warning("The length of 'names_vec' must be the same as the number of coordinates rows. Names are not plotted.")
      } else {
        g <- g + geom_text(data = own_coordinates, aes(label = names_vec), size = 2, vjust = -0.9) }
    }

    g

  }
  else
    stop("The mesh input does not have x and y column.")

}


#' Make triangulation of 2D point mesh
#'
#' @description
#' Function for creating Delaunay type-I triangulation (see Schumaker 2007) with consistent oriented edges adapted for a regular point mesh created by \code{\link{point_mesh}} function.
#' See Details for more information.
#'
#'
#' @param mesh A data frame or tibble with named columns: \code{x}, \code{y} (required) and \code{index} (optionally). It should optimally be a \code{D2} element of a \code{"mesh"} object or a list with the same structure.
#'
#' @details
#' The type-I Delaunay triangulation is a triangulation obtained by drawing in the north-east diagonals in all subrectangles of the triangulated area.
#' Due to the regularity of the input mesh (in the sense of distances between mesh points), a simplified procedure is used: The triangulation is created within the individual strips and then bound together.
#' The order of the vertices is chosen to maintain a consistent orientation of the triangles (for more details see Schneider 2003).
#'
#'
#' @return A three column matrix with indices of the vertices of the triangles.
#'
#' @references Lai M-J, Schumaker LL. \emph{Spline functions on triangulations.} Cambridge University Press; 2007.
#'
#' Schneider PJ, Eberly DH. \emph{Geometric Tools for Computer Graphics.} The Morgan Kaufmann Series in Computer Graphics. San Francisco: Morgan Kaufmann, 2003.
#'
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#'
#' # a) Create small mesh for triangulation example
#' M <- point_mesh(n = 500, template = "HCGSN256")
#'
#' # b) Make triangulation on this mesh
#' TRI <- make_triangulation(M$D2)
#' head(TRI)
#' \dontrun{
#' ## Note: this code opens a rgl 3D viewer
#' # c) Plot the result triangulation as 3D wire model using rgl
#'  rgl::wire3d(mesh3d(M$D3$x, M$D3$y, M$D3$z, triangles = t(TRI)))
#'  }
make_triangulation <- function(mesh) {

  required_cols <- c("x", "y")
  missing_cols <- setdiff(required_cols, colnames(mesh))

  if (length(missing_cols) > 0) {
    stop(paste("The following required columns in mesh are missing:",
               paste(missing_cols, collapse = ", ")))
  }

  if (!"index" %in% colnames(mesh) ) {
    mesh$index <- 1:length(mesh$x)
  }

  seqy <- unique(mesh$y)
  ky <- length(seqy)

  TRIMAT <- c()
  for (i in (1:(ky - 1))) {

    line1 <- mesh[mesh$y == seqy[i],]
    line2 <- mesh[mesh$y == seqy[i + 1],]

    seqx <- sort(unique(c(line1$x, line2$x)))
    k <- length(seqx)
    x1 <- unique(line1$x)
    x2 <- unique(line2$x)

    row1 <- rep(NA, length.out = k)
    row1[which(seqx %in% x1)] <- line1$index

    row2 <- rep(NA, length.out = k)
    row2[which(seqx %in% x2)] <- line2$index

    col1 <- rep(row1[1:(k - 1)], each = 2)
    col2 <- c(rbind(row2[2:k], row1[2:k]))
    col3 <- c(row2[1], rep(row2[2:(k - 1)], each = 2), row2[k])

    TRI <- cbind(col1, col2, col3)
    TRIMAT <- rbind(TRIMAT, na.omit(TRI))
  }
  colnames(TRIMAT) <- NULL
  return(TRIMAT)
}



