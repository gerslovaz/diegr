#' @title Example HD-EEG epoched data
#'
#' @description This dataset is a short slice of a HD-EEG dataset from visual task experiment ...
#' Example dataset contains amplitude values measured on 204 channels in 50 time points
#' The sampling frequency was 250 Hz, so the time interval between two points is 4 ms long (with 0 indicating the time of the stimulus).
#' The experiment was supported by Czech Health Research Council AZV NU21-04-00445.
#'
#' @docType data
#'
#' @usage data(epochdata)
#'
#' @format The data frame contains five columns:
#' \describe{
#'   \item{time}{Number of time point}
#'   \item{signal}{HD-EEG signal amplitude (in microvolts)}
#'   \item{epoch}{Trial number}
#'   \item{electrode}{Channel label}
#'   \item{subject}{Subject ID, 1 - representative health control subject, 2 - representative patient subject}
#' }
#'
#' @keywords dataset
#'
#' @source Central European Institute of Technology, Masaryk University, Brno, Czech Republic.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' data(epochdata)
#' head(epochdata)
#'

"epochdata"
