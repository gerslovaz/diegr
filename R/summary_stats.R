#' Compute summary statistics of reaction times
#'
#' @description
#' Calculates basic descriptive statistics of reaction time (RT).
#' Statistics are computed separately for each combination of grouping variables present in the data (e.g., group, subject, condition).
#'
#' Computed statistics include: the number of epochs, minimum, maximum, median, mean, and standard deviation of RT.
#'
#'
#' @param data A data frame or a database table with reaction times dataset. Required columns are `epoch` and `RT` (value of reaction time in ms).
#' Optional columns: `group`, `subject`, `condition` for computing summary statistics per group/subject/condition.
#'
#' @returns A tibble with summary statistics of reaction times consisting of the following columns:
#' \describe{
#'   \item{group}{Group identifier (only if present in the input data).}
#'   \item{subject}{Subject identifier (only if present in the input data).}
#'   \item{condition}{Experimental condition (only if present in the input data).}
#'   \item{n_epoch}{Number of epochs.}
#'   \item{min_rt}{Minimum reaction time.}
#'   \item{max_rt}{Maximum reaction time.}
#'   \item{median_rt}{Median reaction time.}
#'   \item{avg_rt}{Mean reaction time.}
#'   \item{sd_rt}{Standard deviation of reaction time.}
#' }
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' # 1. Summary statistics for rtdata
#' # two different subjects, no group or conditions - results are computed per subject
#' summary_stats_rt(rtdata)
#'
#' # 2. Summary statistics for data with conditions
#' # a) create example data
#' data_cond <- rtdata
#' data_cond$condition <- c(rep("a", 7), rep("b", 7), rep("a", 8), rep("b",7))
#' # b) compute statistics per subject and condition
#' summary_stats_rt(data_cond)
#' # c) compute statistics per conditions regardless of subjects
#' # exclude "subject" column from computing
#' summary_stats_rt(data_cond[,-1])
summary_stats_rt <- function(data) {

  if (nrow(data |> head(1) |> collect()) == 0) {
    stop("Input data is empty.")
  }

  stop_if_missing_cols(data, required_cols = c("epoch", "RT"))

  group_vars <- intersect(c("group", "subject", "condition"), names(data))
  check_grouping_vars(data, vars = group_vars, action = "warn")

  results <- data |>
    dplyr::group_by(dplyr::across(all_of(group_vars))) |>
    dplyr::summarize(
      n_epoch = n(),
      min_rt = min(.data$RT),
      max_rt = max(.data$RT),
      median_rt = median(.data$RT),
      avg_rt = mean(.data$RT),
      sd_rt = sd(.data$RT),
      .groups = "drop"
    )

  return(results)
}


#' Check data structure and print inferred hierarchy
#'
#' @description
#' A diagnostic helper function to run before starting the `diegr` analysis.
#' It infers the experimental hierarchy based on the standardized column names
#' strictly required by the package and prints a readable summary. Supports
#' data frames, tibbles, and database tables.
#'
#' @param data A data frame, tibble, or database table containing the EEG data in long format.
#' @param value_col Character string specifying the signal amplitude column name (default `"signal"`).
#'
#' @details
#' The `diegr` package does not strictly require all structural columns for every function
#' (e.g., `group`, `condition`, and `epoch` may be optional). However, if they are present,
#' they must follow the exact naming convention (`group`, `subject`, `sensor`, `epoch`,
#' `condition`, `time`). Only the signal amplitude column can be custom-named via the
#' `value_col` argument. This function helps verify which columns were correctly recognized.
#'
#' @return The original data object invisibly, allowing it to be used in pipes.
#' @importFrom dplyr pull distinct summarize
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' # Checking the structure of epochdata
#' check_structure(epochdata)
check_structure <- function(data,
                            value_col = "signal") {

  if (nrow(data |> head(1) |> collect()) == 0) {
    stop("Input data is empty.")
  }

  if (!inherits(data, c("data.frame", "tbl"))) {
    stop("Input `data` must be a data frame, tibble, or database table.")
  }

  col_names <- colnames(data)

  cat(strrep("-", 55), "\n")
  cat(" diegr: Inferred Data Structure\n")
  cat(strrep("-", 55), "\n")

  get_unique <- function(df, col) {
    df |> dplyr::distinct(!!rlang::sym(col)) |> dplyr::pull()
  }

  # group
  if ("group" %in% col_names) {
    grps <- get_unique(data, "group")
    cat(" |-- Groups:     ", length(grps), " (", paste(grps, collapse = ", "), ")\n")
  } else {
    cat(" |-- Groups:      Not found (optional)\n")
  }

  # subject
  if ("subject" %in% col_names) {
    n_subj <- length(get_unique(data, "subject"))
    cat(" |-- Subjects:   ", n_subj, "found\n")
  } else {
    cat(" |-- Subjects:    Not found\n")
  }

  # condition
  if ("condition" %in% col_names) {
    conds <- get_unique(data, "condition")
    cat(" |-- Conditions: ", length(conds), " (", paste(conds, collapse = ", "), ")\n")
  } else {
    cat(" |-- Conditions:  Not found (optional)\n")
  }

  # epoch
  if ("epoch" %in% col_names) {
    n_epoch <- length(get_unique(data, "epoch"))
    cat(" |-- Epochs:     ", n_epoch, "found\n")
  } else {
    cat(" |-- Epochs:      Not found (optional)\n")
  }

  # sensor
  if ("sensor" %in% col_names) {
    n_sens <- length(get_unique(data, "sensor"))
    cat(" |-- Sensors:    ", n_sens, "found\n")
  } else {
    cat(" |-- Sensors:     Not found\n")
  }

  # time
  if ("time" %in% col_names) {
     t_range <- data |>
      dplyr::summarize(min_t = min(!!rlang::sym("time"), na.rm = TRUE),
                       max_t = max(!!rlang::sym("time"), na.rm = TRUE)) |>
      dplyr::collect()

    n_time <- length(get_unique(data, "time"))

    cat(" |-- Timepoints: ", n_time,
        " [Indices: ", t_range$min_t[1], " to ", t_range$max_t[1], "]\n")
  } else {
    cat(" |-- Timepoints:  Not found\n")
  }

  # amplitude (signal or other name)
  if (value_col %in% col_names) {
    cat(" |-- Signal:      Present ('", value_col, "' column)\n", sep = "")
  } else {
    cat(" |-- Signal:      Not found (check your value_col argument)\n")
  }
  cat(strrep("-", 55), "\n")

  invisible(data)
}
