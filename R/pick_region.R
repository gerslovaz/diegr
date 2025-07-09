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
#' @importFrom rlang .data
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
    coords <- diegr::HCGSN256$D2
   }

  if (is.null(ROI)) {
    ROI <- diegr::HCGSN256$ROI
  }

  idxreg <- grep(paste(region, collapse = "|"), ROI)
  new_coords <- coords[idxreg,]

  if (!"x" %in% colnames(new_coords) && is.matrix(new_coords) ) {
    x <- new_coords[,1]
  } else {
    x <- new_coords$x
  }

  idxl <- c()
  idxr <- c()
  idxm <- c()

  if (any(hemisphere == "left")) {
    idxl <- which(x < 0)
  }
  if (any(hemisphere == "right")) {
    idxr <- which(x > 0)
  }
  if (any(hemisphere == "midline")) {
    idxm <- which(x == 0)
  }
  idx <- c(idxl, idxm, idxr)
  idx <- sort(idx)

  new_coords <- new_coords[idx,]

  return(new_coords)
}

