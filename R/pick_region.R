#' Choose region of interest
#'
#' @param coords A data frame or matrix with two columns of sensor coordinates. If not defined, HCGSN256 template is used.
#' @param hemisphere A character vector denoting hemisphere to choose. Possible values: "left", "right" or c("left", "right") for both. If not defined, both hemispheres are chosen.
#' @param region A character vector denoting region to choose. Possible values: "frontal", "central", "parietal", "occipital", "temporal" or any combination of them. If not defined, all regions are chosen.
#' @param ROI A character vector with labels of regions. The order and length must be the same as in \code{coords}. If not defined, the predefined vector is used.
#'
#' @return A subset of coords appropriate to the chosen region/hemisphere.
#' @export
#'
#' @examples
#'
#' # Choosing regions from HCGSN256 template
#' # 1) temporal region in left hemisphere
#' pick_region(hemisphere = "left", region = "temporal")
#' # 2) frontal and central region (both hemispheres)
#' pick_region(region = c("frontal", "central"))
#'
pick_region <- function(coords = NULL, hemisphere = NULL, region = NULL, ROI = NULL) {
  if (missing(coords)) {
    coords <- HCGSN256
  }

  if (missing(ROI)) {
    ROI <- ROIs
  }

  idx.reg <- grep(paste(region, collapse = "|"), ROI)
  new.coords <- coords$D2[idx.reg,]

  if (missing(region)) {
    new.coords <- coords$D2
  }

  x <- new.coords$x

  midline <- which(x == 0)
  idx.l <- c()
  idx.r <- c()

  if (any(hemisphere == "left")) {
    idx.l <- which(x < 0)
  }
  if (any(hemisphere == "right")) {
    idx.r <- which(x > 0)
  }
  idx <- c(idx.l, idx.r, midline)

  if (missing(hemisphere)) {
    idx <- 1:length(x)
  }

  new.coords <- new.coords[idx,]

  return(new.coords)
}

ROIs <- c(rep('frontal', 6), rep('central', 3), rep('frontal', 6), rep('central', 2),
          rep('frontal', 6), 'central', rep('frontal',5), 'central', rep('frontal', 9), rep('central', 5),
          rep('frontal', 3), rep('central', 5), rep('frontal', 2), rep('central', 5), rep('temporal',3),
          rep('central', 3), rep('temporal',3), rep('central', 2), rep('temporal',2), rep('central',6),
          rep('temporal',2), rep('parietal',6), rep('temporal',2), rep('parietal',7), 'temporal',
          rep('parietal',7), rep('occipital', 7), 'parietal', rep('occipital', 7), rep('parietal',3),
          rep('central',2), rep('occipital', 6), rep('parietal',3), rep('central',2), rep('occipital', 5),
          rep('parietal',3), rep('central',2), rep('occipital', 4), rep('parietal',3), rep('central',2), rep('occipital', 3),
          rep('parietal',3), rep('central',2), rep('occipital', 2), rep('parietal',2), rep('temporal',2),
          rep('central',6), 'occipital', 'parietal', rep('temporal', 4), rep('central',5), rep('temporal', 4), rep('central',4),
          rep('temporal', 2), rep('central', 4), 'temporal', rep('frontal', 4), 'central')

make_rect_polygon <- function(mesh) {
  x <- unique(mesh[,1])
  y <- unique(mesh[,2])
  nx <- length(x)
  ny <- length(y)

  x.seq <- rep(x, ny)
  y.seq <- rep(y, each = nx)
  rectangle <- cbind(x.seq, y.seq)

  return(rectangle)
}
