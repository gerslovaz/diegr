#' Choose region of interest
#'
#' @param coords A data frame or matrix with two columns of sensor coordinates. If not defined, HCGSN256 template is used.
#' @param hemisphere A character vector denoting hemisphere to choose. Possible values: "left", "right", "midline" or any combination of them. If not defined, both hemispheres with midline are chosen.
#' @param region A character vector denoting region to choose. Possible values: "frontal", "central", "parietal", "occipital", "temporal" or any combination of them. If not defined, all regions are chosen.
#' @param ROI A character vector with labels of regions. The order and length must be the same as in \code{coords}. If not defined, the predefined vector (according to HCGSN256 template determined by an expert from Central European Institute of Technology, Masaryk University, Brno, Czech Republic) is used.
#'
#' @return A subset of \code{coords} appropriate to the chosen region/hemisphere.
#' @export
#'
#' @examples
#'
#' # Choosing regions from HCGSN256 template
#' data("HCGSN256")
#' # 1) temporal region in left hemisphere
#' pick_region(hemisphere = "left", region = "temporal")
#' # 2) frontal and central region
#' pick_region(region = c("frontal", "central"))
#'
pick_region <- function(coords = NULL, hemisphere = c("left", "right", "midline"), region = c("frontal", "central", "parietal", "occipital", "temporal"), ROI = NULL) {

  if (is.null(coords)) {
    coords <- HCGSN256$D2
  }

  if (is.null(ROI)) {
    ROI <- HCGSN256$ROI
  }


  idx.reg <- grep(paste(region, collapse = "|"), ROI)
  new.coords <- coords[idx.reg,]

  x <- new.coords$x

  idx.l <- c()
  idx.r <- c()
  idx.m <- c()

  if (any(hemisphere == "left")) {
    idx.l <- which(x < 0)
  }
  if (any(hemisphere == "right")) {
    idx.r <- which(x > 0)
  }
  if (any(hemisphere == "midline")) {
    idx.m <- which(x == 0)
  }
  idx <- c(idx.l, idx.m, idx.r)

  new.coords <- new.coords[idx,]

  return(new.coords)
}

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
