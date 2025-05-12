#' Topographic map animation in time
#'
#' @description Display a topographic animation of the change in amplitude over time. The function enables direct rendering in Rstudio Viewer or saving the animation in gif format to the chosen location.
#'
#' @param data An input data frame or tibble with required columns: \code{time} - the number of time point, \code{sensor} - the sensor label, \code{signal} - EEG signal value to plot.
#' @param t_lim Limits of time points (i.e., the length of the timeline displayed below the animation).
#' @param FS The sampling frequency. Default value is 250 Hz.
#' @param mesh A \code{"mesh"} object, data frame or matrix with x and y coordinates of a point mesh used for computing IM model. If not defined, the point mesh with default settings from \code{\link{point_mesh}} function is used.
#' @param coords Sensor coordinates as a tibble or data frame with named \code{x} and \code{y} columns of sensor coordinates and \code{sensor} column with sensor names. If not defined, the HCGSN256 template is used.
#' @param col_range A vector with minimum and maximum value of the amplitude used in the colour palette for plotting. If not defined, the range of input signal is used.
#' @param col_scale Optionally, a colour scale to be utilised for plotting. If not defined, it is computed from \code{col_range}.
#' @param legend Logical. Indicates, whether legend should be displayed beside the graph. Default value is \code{TRUE}.
#' @param contour Logical. Indicates, whether contours should be plotted in the graph. Default value is \code{FALSE}.
#' @param output_path File path where the animation will be saved (optional). If not defined, the animation is plotted in the RStudio Viewer.
#'
#' @details
#' The time part of input data is assumed to be in numbers of time points, conversion to ms takes place inside the function for drawing the timeline labels.
#' Due to the flexibility of the function (e.g. to mark and animate only a short section from the entire time course or to compare different data in the same time interval), it allows the user to enter and plot an arbitrary timeline values. If some values of the time are outside the \code{t_lim} range, the function writes a warning message - in that case the animation is still rendered, but the timeline will not match reality.
#'
#' @returns
#' If `output_path` is `NULL`, the function prints the animation to the RStudio Viewer.
#' If `output_path` is specified, the animation is saved to the given file path and not displayed.
#'
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @import gganimate
#' @import gifski
#' @importFrom grDevices hsv
#' @importFrom scales rescale
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # This example may take a few seconds to render.
#' # Run only if you want to generate the full animation.
#' # Preparing a data structure:
#' s1e05 <- epochdata |> dplyr::filter(subject == 1 & epoch == 5 & time %in% c(10:20))
#' # Plot animation:
#' animate_topo(s1e05, t_lim = c(0,50))
#' }
animate_topo <- function(data, t_lim, FS = 250, mesh, coords = NULL, col_range = NULL, col_scale = NULL,
                         legend = TRUE, contour = FALSE,
                         output_path = NULL){

  if (!(is.logical(contour))) {
    stop("Argument 'contour' has to be logical.")
  }

  if (!(is.logical(legend))) {
    stop("Argument 'legend' has to be logical.")
  }

  if (is.null(coords)) {
    # use HCGSN256 template
    coords <- diegr::HCGSN256$D2 |>
      mutate(sensor = diegr::HCGSN256$sensor)
  }

  if (missing(mesh)) {
    mesh <- point_mesh(dim = 2, template = "HCGSN256")
  }

  if (inherits(mesh, "mesh")) {
    mesh_mat <- mesh$D2
  } else {
    mesh_mat <- mesh[,1:2] ## hm, tady to jeste je trochu problem, kdyby nekdo mel v mesh osy x a y jinde nez na prvnich dvou mistech - chci to vybirat takto nebo podle jmen? dobre pak popsat v helpu
  }

  newdata <- prepare_anim_structure(data, coords, mesh_mat)

  M <- max(mesh_mat[,2], na.rm = TRUE)
  Mm <- min(mesh_mat[,2], na.rm = TRUE)
  y_l <- Mm - 0.1 * abs(Mm)
  x_range <- range(mesh_mat[,1])
  x0 <- mean(mesh_mat[,1], na.rm = TRUE)

  if (any(newdata$time <  range(t_lim)[1] | newdata$time > range(t_lim)[2])) {
    warning("Some values of 'time' are outside the 't_lim' range.")
  }

  k0 <- 1000 / FS
  k <- range(t_lim)[2] - range(t_lim)[1]
  time_positions <- seq(x_range[1], x_range[2], length.out = k)
  time_range <- sort(unique(newdata$time))
  timeline <- tibble(time = time_range, x = time_positions[time_range], y = y_l)


  if (is.null(col_scale)) {

    if (is.null(col_range)) {
      if (all(is.na(newdata$y_IM))) {
        stop("No valid values in y_IM to create color scale.")
      }
      col_range <- c(1.1 * range(newdata$y_IM))
    }

    col_scale <- create_scale(col_range)
  }

  g <- ggplot(newdata, aes(x = .data$x, y = .data$y)) +
    geom_raster(aes(fill = .data$y_IM)) +
    scale_fill_gradientn(
      colors = col_scale$colors,
      breaks = col_scale$breaks,
      limits = range(col_scale$breaks),
      labels = round(col_scale$breaks, 2),
      values = scales::rescale(col_scale$breaks)
    ) +
    coord_fixed(ratio = 1) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = "none"
    ) +
    annotate("segment", x = x0, y = M + 0.07 * abs(M), xend = x0 - 0.08 * M, yend = M + 0.01 * abs(M), col = "gray40") +
    annotate("segment", x = x0, y = M + 0.07 * abs(M), xend = x0 + 0.08 * M, yend = M + 0.01 * abs(M), col = "gray40") +
    geom_point(data = coords, aes(x = .data$x, y = .data$y), color = "black", cex = 0.7) +

    annotate("segment", x = x_range[1], y = y_l, xend = x_range[2], yend = y_l, col = "black")  + # time line
    annotate(geom = "text", x = x_range[1], y = y_l + 0.05 * abs(M), label = paste0(range(t_lim)[1] * k0)) +
    annotate(geom = "text", x = x_range[2], y = y_l + 0.05 * abs(M), label = paste0(range(t_lim)[2] * k0)) +
    geom_point(data = timeline, aes(x = .data$x, y = .data$y, group = .data$time),
               color = "red", size = 3, inherit.aes = FALSE) + # red moving point
    geom_text(data = timeline, aes(x = .data$x, y = .data$y, label = paste0("t = ", .data$time * k0, " ms"), group = .data$time),
              vjust = 1.5, color = "red", inherit.aes = FALSE)  # label for the point


  if (legend == TRUE) {
    g <- g  +
      labs(fill = expression(paste("Amplitude (", mu, "V)"))) +
      guides(fill = guide_colorbar(barwidth = 0.7, barheight = 20)) +
      theme(
        legend.position = "right",
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8)
      )
  }

  if (contour == TRUE) {
    g <- g + geom_contour(aes(z = .data$y_IM), color = "gray", breaks = col_scale$breaks)
  }


  g <- g + gganimate::transition_manual(.data$time) # animation

  anim <- gganimate::animate(g, renderer = gganimate::gifski_renderer())

  if (!is.null(output_path)) { # save animation
    gganimate::anim_save(output_path, animation = anim)
    message("Animation saved into: ", output_path)
  } else {
    print(anim)
  }

}


prepare_anim_structure <- function(data, coords, mesh_mat) {
  ## create structure for animate_topo from database

  if (inherits(data, "tbl_sql")) {
    data <- dplyr::collect(data)
  }

  # add x and y coordinates of sensors to the tibble with time and signal data
  tib_signal <- data |>
    left_join(coords, by = "sensor")


  # splitting the tibble and computing IM model on splitted tibble
  tib_split <- split(tib_signal, tib_signal$time)

  tib_IM <- purrr::map_dfr(names(tib_split), function(t) {
    df_t <- tib_split[[t]]
    interpolated_values <- IM(df_t[,c("x", "y")], df_t$signal, mesh_mat)$Y_hat

    tibble(
      time = as.numeric(t),
      x = mesh_mat[,1], y = mesh_mat[,2],
      y_IM = interpolated_values[1:dim(mesh_mat)[1]]
    )
  })

  return(tib_IM)
}



