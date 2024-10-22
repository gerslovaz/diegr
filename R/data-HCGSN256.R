#' @title Coordinates of 256-channel HCGSN sensors
#'
#' @description A file containing the Cartesian coordinates of high-density EEG sensor positions in 3D space on the scalp surface and their projection into 2D space.
#' The coordinates belong to 256-channel HydroCel Geodesic Sensor Net average template montage.
#'
#' @docType data
#'
#' @usage data("HCGSN256")
#'
#' @format A list with following elements:
#' \describe{
#'   \item{sensor}{Sensor label (according to EGI GSN Technical Manual)}
#'   \item{D2}{A tibble with 2 columns containing x and y coordinates of sensors in 2D}
#'   \item{D3}{A tibble with 3 columns containing x, y and z coordinates of sensors in 3D. See 'Details' for more information.}
#'   \item{ROI}{Factor containing the name of the region to which the corresponding sensor belongs.
#'   The levels are: "central", "frontal", "occipital", "parietal", "temporal".
#'   The regions were determined by an expert from Central European Institute of Technology, Masaryk University, Brno, Czech Republic.}
#' }
#'
#' @details
#' The axis orientation in the 3D case is as follows: the x axis runs left (negative) to right (positive); the y axis runs posterior (negative) to anterior (positive); and the z axis runs inferior (negative) to superior (positive).
#' The reference electrode (Cz) is fixed at point (0, 0, Z), where Z is the positive height of Cz. The nasion is fixed at (0, Y, Z). Since both the nasion and Cz are always fixed at x = 0, they are assumed to be in the same y plane. The origin is the center of the head, defined as the center of a sphere fit to fiducial points above the plane made up of the Left Preauricular Point (LPA), the Right Preauricular Point (RPA), and the nasion.
#'
#' The 2D coordinates were obtained as a stereographic projection of the sensor positions in 3D space.
#'
#' @keywords dataset
#'
#' @references EGI Geodesic Sensor Net Technical Manual (2024), www.egi.com
#'
#' @source Central European Institute of Technology, Masaryk University, Brno, Czech Republic.
#'
#' @examples
#' data("HCGSN256")
#'
#' # A simple plot of sensor coordinates as points in 2D
#' plot(HCGSN256$D2, pch = 16, asp = 1)
#'
"HCGSN256"
