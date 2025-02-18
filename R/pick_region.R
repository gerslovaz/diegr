#' Choose region of interest
#'
#' @description
#' The function extracts the selected regions or hemisphere (or a combination of both) from the specified sensor coordinates.
#'
#' @param coords A data frame, matrix or named tibble with two columns of sensor coordinates. If not defined, HCGSN256 template is used. See details for more information about coordinate requirements.
#' @param hemisphere A character vector denoting hemisphere to choose. Possible values: \code{"left"}, \code{"right"}, \code{"midline"} or any combination of them. If not defined, both hemispheres with midline are chosen.
#' @param region A character vector denoting region to choose. Possible values: \code{"frontal"}, \code{"central"}, \code{"parietal"}, \code{"occipital"}, \code{"temporal"} or any combination of them. If not defined, all regions are chosen.
#' @param ROI A character vector with labels of regions. The order and length must be the same as in \code{coords}. If not defined, the predefined vector (according to HCGSN256 template determined by an expert from Central European Institute of Technology, Masaryk University, Brno, Czech Republic) is used.
#'
#' @details
#' If the \code{coords} input is data frame or matrix with no named columns, the first column is considered as x coordinate.
#' For the correct selection of the hemisphere with own coordinates, it is necessary that the 2D layout is oriented with the nose up and that the midline electrodes have a zero x-coordinate.
#'
#' Remark: The option \code{hemisphere = "left"} (respectively \code{hemisphere = "right"}) means only the left hemisphere without the midline. If you want to include midline as well, use \code{hemisphere = c("left", "midline")} (respectively \code{hemisphere = c("right", "midline")}).
#'
#' @return A subset of \code{coords} appropriate to the chosen region/hemisphere.
#' @export
#'
#' @examples
#'
#' # Choosing regions from HCGSN256 template
#' # a) temporal region in left hemisphere
#' pick_region(hemisphere = "left", region = "temporal")
#' # b) frontal and central region
#' region_fc <- pick_region(region = c("frontal", "central"))
#' head(region_fc)
#' # c) left hemisphere including midline
#' hemi_lm <- pick_region(hemisphere = c("left", "midline"))
#' head(hemi_lm)
#' # plot the result in c)
#' plot(hemi_lm$x, hemi_lm$y, pch = 16, asp = 1)
pick_region <- function(coords = NULL, hemisphere = c("left", "right", "midline"), region = c("frontal", "central", "parietal", "occipital", "temporal"), ROI = NULL) {

  if (!is.null(coords) && is.null(ROI)) {
    warning("ROIs are not defined for own coordinates. The results should be carefully reviewed.")
  }

  if (is.null(coords)) {
    coords <- cbind(HCGSN256$D2, HCGSN256$sensor)
    colnames(coords) <- c("x", "y", "sensor")
   }

  if (is.null(ROI)) {
    ROI <- HCGSN256$ROI
  }

  idx.reg <- grep(paste(region, collapse = "|"), ROI)
  new.coords <- coords[idx.reg,]

  if (!"x" %in% colnames(new.coords) && is.matrix(new.coords) ) {
    x <- new.coords[,1]
  } else {
    x <- new.coords$x
  }

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
  idx <- sort(idx)

  new.coords <- new.coords[idx,]

  return(new.coords)
}

pick_data <- function(data, subject.rg = NULL, sensor.rg = NULL,
                      time.rg = NULL, epoch.rg = NULL) {
  ## function for creating subset of data according chosen parameters

  conditions <- list()

  if (!is.null(subject.rg)) {
    conditions <- append(conditions, expr(subject %in% {{ subject.rg }}))
  }
  if (!is.null(sensor.rg)) {
    conditions <- append(conditions, expr(sensor %in% {{ sensor.rg }}))
  }

  if (!is.null(time.rg)) {
    conditions <- append(conditions, expr(time %in% {{ time.rg }}))
  }

  if (!is.null(epoch.rg)) {
    conditions <- append(conditions, expr(epoch %in% {{ epoch.rg }}))
  }

  data |>
    dplyr::filter(!!!conditions)

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
