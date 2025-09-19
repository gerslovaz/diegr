#' Choose region of interest
#'
#' @description
#' The function extracts the selected regions or hemisphere (or a combination of both) from the specified sensor coordinates.
#'
#' @param coords A data frame, matrix or named tibble with numeric columns of "x" and "y" sensor coordinates. If not defined, HCGSN256 template is used. See details for more information about coordinate requirements.
#' @param hemisphere A character vector denoting hemisphere to choose. Possible values: \code{"left"}, \code{"right"}, \code{"midline"} or any combination of them. If not defined, both hemispheres with midline are chosen.
#' @param region A character vector denoting region to choose. Possible values: \code{"frontal"}, \code{"central"}, \code{"parietal"}, \code{"occipital"}, \code{"temporal"}, \code{"face"} or any combination of them. If not defined, all regions are chosen.
#' @param ROI A character or factor vector with labels of regions, aligned row-wise with \code{coords}. If not defined, the predefined vector (according to HCGSN256 template determined by an expert from Central European Institute of Technology, Masaryk University, Brno, Czech Republic) is used.
#' @param tol A numeric value indicating tolerance for midline selection. (Values of x fulfilling abs(x) < tol are denoted as midline.) Default value is 1e-6.
#'
#' @details
#' If the \code{coords} input is data frame or matrix with no named columns, the first column is considered as "x" coordinate and second as "y" coordinate.
#' For the correct selection of the hemisphere with own coordinates, it is necessary that the 2D layout is oriented with the nose up and that the midline electrodes should have a zero x-coordinate (or approximately zero within tolerance). Otherwise, the results will not match reality.
#'
#' Notes:
#' The option \code{hemisphere = "left"} (respectively \code{hemisphere = "right"}) means only the left hemisphere without the midline. If you want to include midline as well, use \code{hemisphere = c("left", "midline")} (respectively \code{hemisphere = c("right", "midline")}).
#'
#' The matching of region/hemisphere is exact and the function will stop with an the function stops with an error if no coordinates match the requested `region` and `hemisphere` combination.
#'
#' @return A tibble or data frame subset of `coords` filtered by the selected `region` and `hemisphere` criteria.
#' @importFrom rlang .data
#' @export
#'
#' @seealso \code{\link{point_mesh}}
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
pick_region <- function(coords = NULL,
                        hemisphere = c("left", "right", "midline"),
                        region = c("frontal", "central", "parietal", "occipital", "temporal", "face"),
                        ROI = NULL,
                        tol = 1e-6) {

  if (!is.null(coords) && is.null(ROI)) {
    warning("ROIs are not defined for own coordinates. The results should be carefully reviewed.")
  }

  allowed_hemis <- c("left", "right", "midline")
  allowed_regions <- c("frontal", "central", "parietal", "occipital", "temporal", "face")

  bad_h <- setdiff(hemisphere, allowed_hemis)
  if (length(bad_h) > 0) {
    stop("Unknown hemisphere: ", paste(bad_h, collapse = ", "))
  }
  bad_r <- setdiff(region, allowed_regions)
  if (length(bad_r) > 0) {
    stop("Unknown region: ", paste(bad_r, collapse = ", "))
  }

  # check of coords and ROI

  if (is.null(coords)) {
    coords <- diegr::HCGSN256$D2
   }
  if (is.null(ROI)) {
    ROI <- diegr::HCGSN256$ROI
  }

  if (!is.data.frame(coords) && !is.matrix(coords)) {
    stop("`coords` must be a data frame or matrix.")
  }
  if (is.matrix(coords)) {
    coords <- as.data.frame(coords)
  }
  if (ncol(coords) < 2) {
    stop("`coords` must have at least two columns.")
  }
  if (!all(c("x","y") %in% names(coords))) {
    names(coords)[1:2] <- c("x","y")
  }
  if (!is.numeric(coords$x) || !is.numeric(coords$y)) {
    stop("`coords$x` and `coords$y` must be numeric.")
  }

  if (length(ROI) != nrow(coords)) {
    stop("Length of ROI does not match number of rows in coords.")
  }


  idxreg <- which(ROI %in% region)

  if (length(idxreg) == 0) {
    stop("No coordinates match the requested region(s): ",
         paste(region, collapse = ", "))
  }

  new_coords <- coords[idxreg, , drop = FALSE]
  x <- new_coords$x


  idxl <- c()
  idxr <- c()
  idxm <- c()

  if (any(hemisphere == "left")) {
    idxl <- which(x < -tol)
  }
  if (any(hemisphere == "right")) {
    idxr <- which(x > tol)
  }
  if (any(hemisphere == "midline")) {
    idxm <- which(abs(x) <= tol)
  }
  idx <- sort(c(idxl, idxm, idxr))

  if (length(idx) == 0) {
    stop("No coordinates remain after hemisphere filtering. Region(s): ",
         paste(region, collapse = ", "),"; Hemisphere(s): ",
         paste(hemisphere, collapse = ", "))
  }

  new_coords <- new_coords[idx, , drop = FALSE]

  return(new_coords)
}

