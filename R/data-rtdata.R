#' @title Example response time data
#'
#' @description This dataset is a short slice of a HD-EEG dataset from a study investigating the impact of deep brain stimulation on patients with advanced Parkinson's disease.
#' The data contains response times (time between stimulus presentation and pressing the button) from the experiment involving a simple visual motor task.
#' The study was carried out by Central European Institute of Technology in Brno and was supported by Czech Health Research Council AZV NU21-04-00445.
#'
#' Example dataset contains response time values in individual experiment epochs for 2 representative subjects (one from patient, one from health control group). The epochs and subjects correspond to the sample dataset \link[diegr]{epochdata}.
#'
#' @docType data
#'
#' @usage data("rtdata")
#'
#' @format The data frame contains three columns:
#' \describe{
#'   \item{subject}{Factor variable with subject ID, 1 - representative health control subject, 2 - representative patient subject}
#'   \item{epoch}{Factor variable with epoch number (there are 14 epochs for subject one, 15 epochs for subject two)}
#'   \item{RT}{Response time in ms.}
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
#' # Data preview
#' head(rtdata)
#'

"rtdata"
