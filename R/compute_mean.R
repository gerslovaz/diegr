#' Calculate mean in temporal or spatial domain
#'
#' @description
#' Function calculates a pointwise or a jackknife (leave-one-out) average signal for chosen subject (or more subjects) in temporal or spatial domain. The function computes an average at subject, sensor/time point or epoch level (according to the `level` parameter).
#'
#'
#' @param data A data frame, tibble or a database table with input data, required columns: subject, sensor, epoch (only for epoch level), time and signal (for raw data) or signal_base (for baseline corrected data).
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values. Default is \code{"signal_base"} for computing average from baseline corrected signal.
#' @param subject A vector of subject indices to be included in the average calculation.
#' @param channel A character vector specifying the channels to be averaged.  If NULL, all channels present in input data are included.
#' @param group A character specifying the grouping factor. Default is \code{"time"} for calculation of the average at individual time points, other possibility is \code{"space"} for calculation of the average at individual space points (sensors).
#' @param level A character specifying the level of average calculation. The possible values are \code{"epoch"}, \code{"sensor"} and \code{"subject"}. See details for more information.
#' @param ex_epoch Optional. A vector of epoch indices to be excluded from the average calculation.
#' @param time A numeric vector specifying the time points used for computing the average signal. If NULL, the whole time interval is included.
#' @param type A character specifying the method of calculating the average, \code{"point"} for pointwise arithmetic average (default) and \code{"jack"} for jackknife leave-one-out average.
#'
#' @details
#' The `level` parameter enables to choose the level at which the average is calculated:
#' - \code{"epoch"} means averaging on epoch level, the result is the average curve (from all included epochs) for individual sensors and subjects in the \code{group = "time"} case or the average area (from all included epochs) for individual time points and subjects in the \code{group = "space"} case.
#' - \code{"sensor"} means averaging on sensor or time point level, the result is the average curve from all included sensors for individual subjects in the \code{group = "time"} case or the average area from all time points for individual subjects in the \code{group = "space"} case.
#' - \code{"subject"} means averaging on subject level, the result is the average curve or area from all included subjects.
#' The function assumes input adapted to the desired level of averaging (i.e. for epoch level the epoch column must be present etc.).
#'
#'
#' @return A tibble with result average according to the chosen `level` and `group` arguments.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' # Average (pointwise) raw signal for subject 1 and electrode E1
#' # without outlier epoch 14
#' avg <- compute_mean(epochdata, amplitude = "signal", subject = 1, channel = "E1", level = "epoch",
#'  ex_epoch = 14)
#' str(avg)
#' # plot the result
#' plot(avg$average, type = "l")
#'
#' # Average signal for subject 1 in all electrodes in time point 11 with baseline correction
#' # on interval 1:10 (again without outlier epoch 14)
#' # a) prepare corrected data
#' data01 <- epochdata |> dplyr::filter(.data$subject == 1)
#' basedata <- baseline_correction(data01, base_int = 1:10, type = "absolute")
#' # b) compute the average in time point 11
#' avg <- compute_mean(basedata, amplitude = "signal_base", time = 11, level = "epoch",
#'  group = "space", ex_epoch = 14)
#' str(avg)
#' # c) plot the result with topo_plot()
#' topo_plot(data = avg, amplitude = "average")
compute_mean <- function(data, amplitude = "signal_base", subject = NULL, channel = NULL, group = "time", level = "epoch",
                         ex_epoch = NULL, time = NULL, type = "point"){

  amp_value <- {{ amplitude }}
  amp_name <- rlang::as_string(amp_value)

  if (!amp_name %in% names(data)) {
    stop(paste0("There is no column '", amp_name, "' in the input data."))
  }

  if (!is.null(ex_epoch)) {
    data <- exclude_epoch(data, ex_epoch = {{ ex_epoch }})
  }

  newdata <- pick_data(data, subject_rg = {{ subject }}, sensor_rg = {{ channel }}, time_rg = {{ time }}) # subset data

  #if (!raw && !"signal_base" %in% colnames(newdata)) {
  #  stop("There is no column 'signal_base' in corrected input data.")
  #}

  # if (raw) {
  #
  #   if (!is.null(base_int)) { # baseline correction
  #     newdata <- baseline_correction(newdata, base_int = { base_int }, type = "absolute")
  #     newdata <- pick_data(newdata, time_rg = {{ time }})
  #   } else {
  #     newdata <- pick_data(newdata, time_rg = {{ time }})
  #   }
  #
  # }

  if (type == "point") { # pointwise average
    output_df <- pointwise_mean(newdata, amp_name = amp_name, group = { group }, level = { level })
  }

  if (type == "jack") { # jackknife average
    output_df <- jackknife_mean(newdata, amp_name = amp_name, group = { group }, level = { level })
  }


  return(output_df)

}


#######################


exclude_epoch <- function(data, ex_epoch){
  # exclude chosen epoch(s)
  newdata <- data |>
    dplyr::filter(!.data$epoch %in% {{ ex_epoch }})

  return(newdata)
}


########### TO-DO: otestovat to pro group = space


########## POZOR NA GRUPOVANI - u sensoru mam asi na vystupu potom lexikograficke serazeni, coz je ale neco, co nechci
########## musim tomu nejak predejit, nebo upravit vystup, kazdopadne ale musim osetrit to, aby bylo spravne poradi pro spravne pojmenovani!!!!!!!!!!!!!!!!!!
#### je to jeste horsi, nekdy to je E1, E2, E3, ... a nekdy E1, E10, E100, ... tohle chce idealne sjednotit
#### ale hlavne je dulezite, aby to pak slo spravne do topoplotu!!! - viz reseni v shiny






## zamyslet se nad tim, kdyz to budu chtit pro vice elektrod spolu vs. kazdou zvlast... dava smysl chtit to nekdy spolu? napr. pro prumer regionu? nebo to budeme pouzivat jen na filtraci "preproces" a tak to budeme chtit po elektrodach?


# mm1 <- compute_mean(data_hc, subject = 1, channel = "E15")
# mm1b <- compute_mean(data_hc, subject = 1, channel = "E15", base.int = c(125:250))
# plot(mm1$average, type = "l", ylim = c(-50,50), col = "blue")
# lines(mm1$average + mm1$sd, lty = 2, col = "royalblue")
# lines(mm1$average - mm1$sd, lty = 2, col = "royalblue")
# lines(mm1b$average, col = "red")
# lines(mm1b$average + mm1b$sd, lty = 2, col = "coral")
# lines(mm1b$average - mm1b$sd, lty = 2, col = "coral")
# abline(v = 250, lty = 3, col = "gray60")
# abline(h = 0, lty = 3, col = "gray40")
# legend(0, 50, legend = c("raw", "baseline corected"), col = c("blue", "red"), lty = 1, cex = 0.8)
# title("Subject HC01, sensor E15")
#
#

## vymyslet funkci na zobrazeni prumeru - samotny plus nejake porovnani
## muzu chtit ruzne el., ruzne skupiny/subjekty, raw vs. baseline apod.
## vymyslet, pro co vse chci samostatnou funkci a co necham na uzivateli, at si to kresli sam



pointwise_mean <- function(data, amp_name, group = c("time", "space"),
                           level = c("epoch", "sensor", "subject")) {
  # compute pointwise mean across different levels

  #signal_value <- if (raw) sym("signal") else sym("signal_base") # signal vs. signal_base

  level <- match.arg(level)

  if (group == "time") {
    group_vars <- switch(
      level,
      "epoch"  = syms(c("subject", "sensor", "time")),
      "sensor"  = syms(c("subject", "time")),
      "subject" = syms(c("time")),
      stop("Invalid 'level' argument.")
    ) # different levels of group_by for time mean
  } else if (group == "space") {
    group_vars <- switch(
      level,
      "epoch"  = syms(c("subject", "time", "sensor")),
      "sensor"  = syms(c("subject", "sensor")),
      "subject" = syms(c("sensor")),
      stop("Invalid 'level' argument.")
      ) # different levels of group_by for space mean
  } else {
    stop("Invalid 'group' argument.")
  }

  avg_data <- data |>
    group_by(!!!group_vars) |>
    summarise(average = mean(.data[[amp_name]], na.rm = TRUE), .groups = "drop") |> #!!signal_value
    collect()
  return(avg_data)
}

leave_one_mean <- function(x, id) {

    vec_ids <- unique(id)
    if (length(vec_ids) <= 1) {
      stop("There is no jackknife mean for less then 2 elements.")
    }

    means <- vapply(vec_ids, function(i) {
      mean(x[id != i], na.rm = TRUE)
    }, numeric(1))

    mean(means, na.rm = TRUE)

}


jackknife_mean <- function(data, amp_name, group = c("time", "space"),
                                   level = c("epoch", "sensor", "subject")) {
  # compute jackknife mean across different levels

  #signal_value <- if (raw) sym("signal") else sym("signal_base")
  level <- match.arg(level)

  if (group == "time") {
    group_vars <- switch(
      level,
      "epoch"  = syms(c("subject", "sensor", "time")),
      "sensor" = syms(c("subject", "time")),
      "subject" = syms(c("time")),
      stop("Invalid 'level' argument.")
    )
    id_sym <- switch(
      level,
      "epoch"  = sym("epoch"),
      "sensor" = sym("sensor"),
      "subject" = sym("subject")
    )
  } else if (group == "space") {
    group_vars <- switch(
      level,
      "epoch"  = syms(c("subject", "time", "sensor")),
      "sensor" = syms(c("subject", "sensor")),
      "subject" = syms(c("sensor")),
      stop("Invalid 'level' argument.")
    )
    id_sym <- switch(
      level,
      "epoch"  = sym("epoch"),
      "sensor" = sym("time"),
      "subject" = sym("subject")
    )
  } else {
    stop("Invalid 'group' argument.")
  }

  avg_data <- data |>
    group_by(!!!group_vars) |>
    summarise(average = leave_one_mean(.data[[amp_name]], id = !!id_sym), .groups = "drop") |>
    collect()

  return(avg_data)
}


