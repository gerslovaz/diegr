#' @title Example response time data
#'
#' @description This dataset is a short slice of a high-density (HD-EEG) dataset from a study investigating the impact of deep brain stimulation on patients with advanced Parkinson's disease (Madetko-Alster, 2025).
#' The data contains response times (time between stimulus presentation and pressing the button) from the experiment involving a simple visual-motor task.
#' The study was carried out by Central European Institute of Technology in Brno and was supported by Czech Health Research Council AZV NU21-04-00445.
#'
#' Example dataset contains response time values in individual experiment epochs for 2 representative subjects (one patient and one healthy control subject).
#'
#' @docType data
#'
#' @usage data("rtdata")
#'
#' @format The data frame consists of 29 rows (14 for subject 1, 15 for subject 2) and three columns:
#' \describe{
#'   \item{subject}{Factor variable with subject ID, 1 - representative healthy control subject, 2 - representative patient subject.}
#'   \item{epoch}{Factor variable with epoch number (14 epochs for subject 1, 15 epochs for subject 2).}
#'   \item{RT}{Response time in milliseconds.}
#' }
#'
#' @details
#' The epochs and subjects correspond to the sample dataset \link[diegr]{epochdata}.
#'
#'
#' @keywords dataset
#'
#' @references Madetko-Alster N., Alster P., Lamoš M., Šmahovská L., Boušek T., Rektor I. and Bočková M. The role of the somatosensory cortex in self-paced movement impairment in Parkinson’s disease. Clinical Neurophysiology. 2025, vol. 171, 11-17.
#'
#' @source Central European Institute of Technology, Masaryk University, Brno, Czech Republic.
#'
#'
#' @examples
#' # Data preview
#' head(rtdata)
#'

"rtdata"
