#' Make triangulation of 2D point mesh
#'
#' @description
#' Function for creating Delaunay type-I triangulation (see Schumaker 2007) with consistent oriented edges adapted for a regular point mesh created by \code{\link{point_mesh}} function.
#' See Details for more information.
#'
#'
#' @param mesh A data frame or tibble with named columns: \code{x}, \code{y} (required) and \code{index} (optionally).
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
#'
#' # c) Plot the result triangulation as 3D wire model using rgl
#' library(rgl)
#' wire3d(mesh3d(M$D3$x, M$D3$y, M$D3$z, triangles = t(TRI)))
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
  k.y <- length(seqy)

  TRIMAT <- c()
  for (i in (1:(k.y - 1))) {

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
