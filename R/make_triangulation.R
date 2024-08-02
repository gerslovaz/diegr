#' Make triangulation of 2D point mesh
#'
#' @description
#' Popsat triangulaci, prip. dolu pridat odkazy na literaturu...
#'
#'
#' @param mesh A data frame with named columns: x, y (required) and index (optionally).
#'
#' @return A three column matrix with indices of the vertices of the triangles.
#'
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#'
#' # Create small mesh for triangulation example
#' data(HCGSN256)
#' M <- point_mesh(dim = 2, n = 500, type = "polygon")
#' # Make triangulation on this mesh
#' TRI <- make_triangulation(M)
#' head(TRI)
make_triangulation <- function(mesh) {

  if (is.null(mesh$index)) {
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

    col1 <- c(row1[1], rep(row1[2:(k - 1)], each = 2), row1[k])
    col2odd <- row1[2:k]
    col2even <- row2[2:k]
    col2 <- c(rbind(col2odd, col2even))
    col3 <- rep(row2[1:(k - 1)], each = 2)

    TRI <- cbind(col1, col2, col3)
    TRIMAT <- rbind(TRIMAT, na.omit(TRI))
    colnames(TRIMAT) <- NULL
  }
  return(TRIMAT)
}

# prejmenovat sloupce vystupni matice, uvazit jeji tvar - do shape3d musi jit transponovana - podivat se, jak je to jinde, a prip. ji rovnou transponovat do vystupu
