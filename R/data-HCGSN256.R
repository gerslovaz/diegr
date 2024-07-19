#' @title Coordinates of 256-channel HCGSN sensors
#'
#' @description A file containing coordinates ....
#' The coordinates belong to 256-channel HydroCel Geodesic Sensor Net template montage (averaging ... montages)
#'
#' @docType data
#'
#' @usage HCGSN256
#'
#' @format A list with following elements:
#' \describe{
#'   \item{number}{Number of sensor (according to EGI GSN Technical Manual)}
#'   \item{D2}{A tibble with 2 columns containing x and y coordinates of sensors in 2D}
#'   \item{D3}{A tibble with 3 columns containing x, y and z coordinates of sensors in 3D}
#' }
#'
#' @keywords dataset
#'
#' @references EGI Geodesic Sensor Net Technical Manual (2024)
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
