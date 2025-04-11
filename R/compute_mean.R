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
#' @param ex.epoch Optional. A vector of epoch indices to be excluded from the average calculation.
#' @param time A numeric vector specifying the time points used for computing the average signal. If NULL, the whole time interval is included.
#' @param base.int Numeric character vector including time points to use as a baseline. See \code{\link{baseline_correction}} for details.
#' @param raw A logical value indicating whether the input data are in raw form (\code{raw = TRUE}) or baseline corrected (\code{raw = FALSE}). Default is \code{TRUE}.
#' @param type A character specifying the method of calculating the average, \code{"point"} for pointwise arithmetic average (default) and \code{"jack"} for jacknife average.
#'
#' @return A list with result average and standard deviation.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' # Average signal for subject 1 and electrode E1 with baseline correction on interval 1:10
#' avg <- compute_mean(epochdata, subject = 1, channel = "E1", base.int = 1:10)
#' avg$average
compute_mean <- function(data, subject = NULL, channel = NULL, group = "time", ex.epoch = NULL,
                         time = NULL, base.int = NULL, raw = TRUE, type = "point"){

  if (!is.null(ex.epoch)) {
    data <- exclude_epoch(data, ex.epoch = {{ ex.epoch }})
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



  newdata <- pick_data(data, subject.rg = {{ subject }}, sensor.rg = {{ channel }}, time.rg = {{ time }})
  #newdata <- data |>
  #  dplyr::filter(subject == {{ subject }} & (sensor %in% channel_id) & (time %in% time_lim))  |>
  #  dplyr::select(time, signal, epoch, sensor)


  if (raw == FALSE) {
    if ("signal_base" %in% colnames(newdata)) {
      newdata <- newdata |>
        dplyr::select("subject", "sensor", "epoch", "time", "signal_base")
      newdata <- collect(newdata)
    } else {
      stop("There is no column 'signal_base' in corrected input data.")
    }
  } else {
    if (!is.null(base.int)) {
      newdata <- baseline_correction(newdata, base.int = { base.int }, type = "absolute")
    } else {
      newdata <- newdata |>
        dplyr::select("subject", "sensor", "epoch", "time", "signal")
      newdata <- collect(newdata)
      newdata <- newdata |>
        dplyr::mutate(signal_base = .data$signal)
    }
  }

  if (group == "time") {

    if (type == "jack") {
      ## takto je jack jen pro jeden subjekt, kdyz jich budu mit vice s ruznym poctem epoch, nebude to fungovat
      level_e <- levels(newdata$epoch)
      n_e <- length(level_e) # number of epochs (curves)
      n_t <- length(unique(newdata$time)) # number of time points
      leave_one_out_means <- matrix(NA, ncol = n_t, nrow = n_e)

      for (i in 1:n_e) {
        mean_i <- newdata |>
          exclude_epoch(ex.epoch = level_e[i]) |>
          dplyr::group_by(.data$subject, .data$time) |>
          dplyr::summarise(average = mean(.data$signal_base, na.rm = TRUE))
        leave_one_out_means[i, ] <- mean_i$average
      }

      loomean <- colMeans(leave_one_out_means)
      diff_m <- sweep(leave_one_out_means, 2, colMeans(leave_one_out_means), "-")
      loo_var <- (n_e - 1) / n_e * colSums(diff_m^2)

      avg_vec <- loomean
      sd_vec <- sqrt(loo_var)
    } else {
      newdata <- newdata |>
        dplyr::group_by(.data$subject, .data$time) |>
        dplyr::summarise(average = mean(.data$signal_base, na.rm = TRUE), sd = sd(.data$signal_base, na.rm = TRUE))

      avg_vec <- newdata$average
      sd_vec <- newdata$sd
    }

  } else if (group == "space") {
    if (type == "jack") {
      stop("jack for space is not available yet")
    } else{
      newdata <- newdata |>
        dplyr::mutate(sensor = factor(.data$sensor, levels = unique(.data$sensor))) |>
        dplyr::group_by(.data$sensor) |>
        dplyr::summarise(average = mean(.data$signal, na.rm = TRUE))

      avg_vec <- newdata$average
      sd_vec <- NA
    }

  } else {
    stop("Allowed 'group' argument values are only 'time' or 'space'.")
  }

  ## pokud vyberu napr. vice elektrod a chci porovnani mezi sebou, musim najit zpusob, jak to automaticky vytahovat z dat, coz nebude uplne jednoduche...
  ## teoreticky spise proste spocitat prumer pro vybrane elektrody nez to ukladat oboje? ovsem musi se to udelat komplexne i pro pripady prumeru subjektu, kdy ma kazdy jiny pocet epoch... melo by to jit nejak postupne...

  return(list(average = avg_vec, sd = sd_vec))

}


#######################


exclude_epoch <- function(data, ex.epoch){
  newdata <- data |>
    dplyr::filter(!.data$epoch %in% {{ ex.epoch }})

  return(newdata)
}


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
