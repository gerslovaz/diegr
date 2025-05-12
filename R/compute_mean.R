#' Calculate mean in temporal or spatial domain
#'
#' @description
#' Function calculates the average signal for chosen subject (or average of more subjects) in temporal or spatial domain. The function computes a single overall average, considering all provided subjects and channels (or time points). If multiple subjects are provided, the result is the average across all specified subjects. Similarly, if multiple channels/time points are provided, the result is the average of all specified channels/time points.
#'
#'
#' @param data A data frame, tibble or a database table with input data, required columns: subject, sensor, epoch, time and signal (for raw data) or signal_base (for baseline corrected data).
#' @param subject A vector of subject indices to be included in the average calculation. If multiple values are provided, the overall mean across all subjects will be calculated.
#' @param channel A character vector specifying the channels to be averaged.  If NULL, all channels present in input data are included.
#' @param group A character specifying the grouping factor. Default is \code{"time"} for calculation of the average at individual time points, other possibility is \code{"space"} for calculation of the average at individual space points (sensors).
#' @param ex_epoch Optional. A vector of epoch indices to be excluded from the average calculation.
#' @param time A numeric vector specifying the time points used for computing the average signal. If NULL, the whole time interval is included.
#' @param base_int Numeric character vector including time points to use as a baseline. See \code{\link{baseline_correction}} for details.
#' @param raw A logical value indicating whether the input data are in raw form (\code{raw = TRUE}) or baseline corrected (\code{raw = FALSE}). Default is \code{TRUE}.
#' @param type A character specifying the method of calculating the average, \code{"point"} for pointwise arithmetic average (default) and \code{"jack"} for jacknife average.
#'
#' @return A list with result average and standard deviation for each time point (\code{group = "time"}) or sensor (\code{group = "space"}).
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' # Average signal for subject 1 and electrode E1 with baseline correction on interval 1:10
#' # without outlier epoch 14
#' avg <- compute_mean(epochdata, subject = 1, channel = "E1", base_int = 1:10, ex_epoch = 14)
#' avg$average
#' # plot the result
#' plot(avg$average, type = "l")
#'
#' # Average signal for subject 1 in all electrodes in time point 11 with baseline correction
#' # on interval 1:10 (again without outlier epoch 14)
#' avg <- compute_mean(epochdata, subject = 1, time = 11, group = "space",
#' base_int = 1:10, ex_epoch = 14)
#' str(avg)
#' # plot the result with topo_plot()
#' topo_plot(avg$average)
compute_mean <- function(data, subject = NULL, channel = NULL, group = "time", ex_epoch = NULL,
                         time = NULL, base_int = NULL, raw = TRUE, type = "point"){

  if (!is.null(ex_epoch)) {
    data <- exclude_epoch(data, ex_epoch = {{ ex_epoch }})
  }

  # if (missing(channel)) { ## tady to potrebuju jeste nejak osetrit
  #   if (missing(region)) {
  #     region <- c("frontal", "central", "parietal", "occipital", "temporal")
  #   }
  #   if (missing(hemisphere)) {
  #     hemisphere <- c("left", "right", "midline")
  #   }
  #   region_id <- diegr::pick_region(hemisphere = hemisphere, region = region)
  #   channel_id <- region_id$sensor
  # } else {
  #   channel_id <- channel
  # }  ## napsat do popisku, ze pri soucasne volbe channel, region a hemisphere se uprednostni channel a zbytek se ignoruje



  newdata <- pick_data(data, subject_rg = {{ subject }}, sensor_rg = {{ channel }})

  if (raw == FALSE) {
    if ("signal_base" %in% colnames(newdata)) {
      newdata <- pick_data(newdata, time_rg = {{ time }})
      #newdata <- newdata |>
      #  dplyr::select("subject", "sensor", "epoch", "time", "signal_base")
      newdata <- collect(newdata)
    } else {
      stop("There is no column 'signal_base' in corrected input data.")
    }
  } else {
    if (!is.null(base_int)) {
      newdata <- baseline_correction(newdata, base_int = { base_int }, type = "absolute")
      newdata <- pick_data(newdata, time_rg = {{ time }})
    } else {
      newdata <- pick_data(newdata, time_rg = {{ time }})
      #newdata <- newdata |>
      #  dplyr::select("subject", "sensor", "epoch", "time", "signal")
      newdata <- collect(newdata)
      newdata <- newdata |>
        dplyr::mutate(signal_base = .data$signal)
    }
  }

  #level_e <- levels(newdata$epoch)
  #n_e <- length(level_e)  # number of epochs

  if (type == "point") {

    group_arg <- switch(group,
                        "time" = "time",
                        "space" = "sensor",
                        stop("Invalid group argument."))

    newdata <- newdata |>
      dplyr::group_by(!!sym(group_arg)) |>
      dplyr::summarise(average = mean(.data$signal_base, na.rm = TRUE), sd = sd(.data$signal_base, na.rm = TRUE))

    output_list <- switch(group,
                          "time" = { list(time = newdata$time, average = newdata$average) },
                          "space" = { list(sensor = newdata$sensor, average = newdata$average) }
    )

  }

  if (group == "time") {

    if (type == "jack") {
      # pouze jeden sensor!!!!
      ## takto je jack jen pro jeden subjekt, kdyz jich budu mit vice s ruznym poctem epoch, nebude to fungovat
      n_t <- length(unique(newdata$time)) # number of time points

      if (!is.factor(newdata$epoch)) {
        newdata$epoch <- as.factor(newdata$epoch)
      }

      level_e <- levels(newdata$epoch)
      n_e <- length(level_e)  # number of epochs
      leave_one_out_means <- matrix(NA, ncol = n_t, nrow = n_e)

      for (i in 1:n_e) {
        mean_i <- newdata |>
          exclude_epoch(ex_epoch = level_e[i]) |>
          dplyr::group_by(.data$subject, .data$time) |>
          dplyr::summarise(average = mean(.data$signal_base, na.rm = TRUE))
        leave_one_out_means[i, ] <- mean_i$average
      }

      loomean <- colMeans(leave_one_out_means)
      diff_m <- sweep(leave_one_out_means, 2, colMeans(leave_one_out_means), "-")
      loo_var <- (n_e - 1) / n_e * colSums(diff_m^2)

      output_list <- list(time = newdata$time, average = loomean, sd = sqrt(loo_var))
     }
    # else {
    #   newdata <- newdata |>
    #     dplyr::group_by(.data$subject, .data$time) |>
    #     dplyr::summarise(average = mean(.data$signal_base, na.rm = TRUE), sd = sd(.data$signal_base, na.rm = TRUE))
    #
    #   output_list <- list(time = newdata$time, average = newdata$average, sd = newdata$sd)
    # }

  } else if (group == "space") {
    if (type == "jack") {
      n_s <- length(unique(newdata$sensor)) # number of sensors

      if (!is.factor(newdata$epoch)) {
        newdata$epoch <- as.factor(newdata$epoch)
      }
      level_e <- levels(newdata$epoch)
      n_e <- length(level_e)  # number of epochs
      leave_one_out_means <- matrix(NA, ncol = n_s, nrow = n_e)

      for (i in 1:n_e) {
        mean_i <- newdata |>
          exclude_epoch(ex_epoch = level_e[i]) |>
          dplyr::group_by(.data$sensor) |>
          dplyr::summarise(average = mean(.data$signal_base, na.rm = TRUE))
        leave_one_out_means[i, ] <- mean_i$average
      }
      loomean <- colMeans(leave_one_out_means)
      output_list <- list(sensor = mean_i$sensor, average = loomean)

    }
    # else{
    #   newdata <- newdata |>
    #     dplyr::mutate(sensor = factor(.data$sensor, levels = unique(.data$sensor))) |>
    #     dplyr::group_by(.data$sensor) |>
    #     dplyr::summarise(average = mean(.data$signal_base, na.rm = TRUE), sd = sd(.data$signal_base, na.rm = TRUE))
    #
    #   output_list <- list(sensor = newdata$sensor, average = newdata$average, sd = newdata$sd)
    # }

  } else {
    stop("Allowed 'group' argument values are only 'time' or 'space'.")
  }

  ## pokud vyberu napr. vice elektrod a chci porovnani mezi sebou, musim najit zpusob, jak to automaticky vytahovat z dat, coz nebude uplne jednoduche...
  ## teoreticky spise proste spocitat prumer pro vybrane elektrody nez to ukladat oboje? ovsem musi se to udelat komplexne i pro pripady prumeru subjektu, kdy ma kazdy jiny pocet epoch... melo by to jit nejak postupne...

  return(output_list)

}


#######################


exclude_epoch <- function(data, ex_epoch){
  newdata <- data |>
    dplyr::filter(!.data$epoch %in% {{ ex_epoch }})

  return(newdata)
}

jack_epoch <- function(data, group) {
  ## jackknife average on epoch level for one subject and one sensor/time point

  if (!is.factor(data$epoch)) {
    data$epoch <- as.factor(data$epoch)
  }

  level_e <- levels(data$epoch)
  n_e <- length(level_e)

  group_arg <- switch(group,
                      "time" = "time",
                      "space" = "sensor",
                      stop("Invalid group argument."))

  k <- switch(group,
              "time" = length(unique(data$time)),
              "space" = length(unique(data$sensor)))

  leave_one_out_means <- matrix(NA, ncol = k, nrow = n_e)

  for (i in 1:n_e) {
    mean_i <- data |>
      exclude_epoch(ex_epoch = level_e[i]) |>
      dplyr::group_by(!!sym(group_arg)) |>
      dplyr::summarise(average = mean(.data$signal_base, na.rm = TRUE))
    leave_one_out_means[i, ] <- mean_i$average
  }

  loomean <- colMeans(leave_one_out_means)

  output_list <- switch(group,
                        "time" = { list(time = mean_i$time, average = loomean) },
                        "space" = { list(sensor = mean_i$sensor, average = loomean) }
  )

  return(output_list)

}


jack_sensor <- function(data, group) {
  ## jackknife average on sensor/time point level for one subject

  #n_s <- length(unique(data$sensor))
  #sen_names <- unique(data$sensor)
  #n_t <- length(unique(data$time))

  group_arg <- switch(group,
                      "time" = "time",
                      "space" = "sensor",
                      stop("Invalid group argument."))

  filter_arg <- switch(group,
                       "time" = "sensor",
                       "space" = "time",
                       stop("Invalid group argument."))

  names_vec <- switch(group,
                      "time" = unique(data$sensor),
                      "space" = unique(data$time))

  N <- length(names_vec)
  k <- switch(group,
              "time" = length(unique(data$time)),
              "space" = length(unique(data$sensor)))

  leave_one_out_means <- matrix(NA, ncol = k, nrow = N)

  for (i in 1:N) {
    mean_i <- data |>
      dplyr::filter(!!sym(filter_arg) != names_vec[i]) |> # leave one sensor
      dplyr::group_by(!!sym(group_arg)) |>
      dplyr::summarise(average = mean(.data$signal_base, na.rm = TRUE), .groups = "drop")
    leave_one_out_means[i, ] <- mean_i$average
  }

  loomean <- colMeans(leave_one_out_means)

  output_list <- switch(group,
                        "time" = { list(time = mean_i$time, average = loomean) },
                        "space" = { list(sensor = mean_i$sensor, average = loomean) }
  )

  return(output_list)

}


jack_subject <- function(data, group = "time") {
  ## jackknife average on subject level

  if (group == "time") {
    N <- length(unique(data$time))
  } else if (group == "space") {
    N <- length(unique(data$sensor))
  } else {
    stop("Invalid 'group' argument.")
  }

  group_arg <- switch(group,
                     "time" = "time",
                     "space" = "sensor",
                     stop("Invalid group argument."))

  n_s <- length(unique(data$subject))

  if (n_s < 2) {
    stop("There must be at least 2 different subjects for computing jacknife average on subject level.")
  }

  leave_one_out_means <- matrix(NA, ncol = N, nrow = n_s)

  for (i in 1:n_s) {
    mean_i <- data |>
      dplyr::filter(subject != unique(data$subject)[i]) |> # leave one subject
      dplyr::group_by(!!sym(group_arg)) |>
      dplyr::summarise(average = mean(.data$signal_base, na.rm = TRUE), .groups = "drop")
    leave_one_out_means[i, ] <- mean_i$average
  }

  loomean <- colMeans(leave_one_out_means)
  output_list <- switch(group,
                  "time" = { list(time = mean_i$time, average = loomean) },
                  "space" = { list(sensor = mean_i$sensor, average = loomean) }
  )

  return(output_list)

}
########### TO-DO: otestovat to pro group = space
########### prepsat take dalsi urovne do obecne formy, pokud to pujde
########### mozna by to slo spojit do jedne funkce, kde by se rozhodovalo podle arg. level?
########## UPRAVIT to se signal_base!!!!

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
