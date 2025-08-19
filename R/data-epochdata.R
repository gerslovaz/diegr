#' @title Example high-density (HD-EEG) epoched data
#'
#' @description This dataset is a short slice of a HD-EEG dataset from a study investigating the impact of deep brain stimulation on patients with advanced Parkinson's disease.
#' During the experiment subjects performed a simple visual motor task (pressing the response button in case of target visual stimulus presentation). The data was measured by 256-channel HydroCel Geodesic Sensor Net and sampling frequency is 250 Hz.
#' The study was carried out by Central European Institute of Technology in Brno and was supported by Czech Health Research Council AZV NU21-04-00445.
#'
#' Example dataset contains amplitude values measured on chosen 204 channels in 50 time points (with the stimulus in the time point 10) for 2 representative subjects (one patient and one healthy control subject). From the total number of 50 epochs for each subject, 14 (or 15) epochs were selected for the sample dataset. This data is intended for testing EEG preprocessing and visualization methods.
#'
#' @docType data
#'
#' @usage data("epochdata")
#'
#' @format The data frame consist of 295 800 rows (50 time points x 204 sensors x 29 epochs) and five columns:
#' \describe{
#'   \item{time}{Number of time point. Time point 10 corresponds to stimulus onset (0 ms) and the interval between two time points corresponds to the time period 4 ms.}
#'   \item{signal}{HD-EEG signal amplitude, in microvolts.}
#'   \item{epoch}{Factor variable with epoch number, 14 epochs for subject 1, 15 epochs for subject 2.}
#'   \item{sensor}{Sensor label, according to labeling used in the EGI Geodesic Sensor Net Technical Manual.}
#'   \item{subject}{Factor variable with subject ID, 1 - representative healthy control subject, 2 - representative patient subject.}
#' }
#'
#' @keywords dataset
#'
#' @references EGI Geodesic Sensor Net Technical Manual (2024) <https://www.egi.com/knowledge-center>
#'
#' @source Central European Institute of Technology, Masaryk University, Brno, Czech Republic.
#'
#'
#' @examples
#' # Data preview
#' head(epochdata)
#'

"epochdata"
