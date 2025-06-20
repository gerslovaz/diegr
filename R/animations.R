#' Topographic map animation in time
#'
#' @description Display a topographic animation of the change in amplitude over time. The function enables direct rendering in Rstudio Viewer or saving the animation in gif format to the chosen location.
#'
#' @param data An input data frame or tibble with at least this required columns: \code{time} - the number of time point, \code{sensor} - the sensor label and the column with the EEG amplitude to plot specified in the argument \code{amplitude}.
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values.
#' @param t_lim Limits of time points (i.e., the length of the timeline displayed below the animation).
#' @param FS The sampling frequency. Default value is 250 Hz.
#' @param mesh A \code{"mesh"} object, data frame or matrix with x and y coordinates of a point mesh used for computing IM model. If not defined, the point mesh with default settings from \code{\link{point_mesh}} function is used.
#' @param coords Sensor coordinates as a tibble or data frame with named \code{x}, \code{y} columns of sensor coordinates and \code{sensor} column with sensor names. If not defined, the HCGSN256 template is used.
#' @param template The kind of sensor template montage used. Currently the only available option is \code{"HCGSN256"} denoting the 256-channel HydroCel Geodesic Sensor Net v.1.0, which is also a default setting.
#' @param col_range A vector with minimum and maximum value of the amplitude used in the colour palette for plotting. If not defined, the range of input signal is used.
#' @param col_scale Optionally, a colour scale to be utilised for plotting. If not defined, it is computed from \code{col_range}.
#' @param legend Logical. Indicates, whether legend should be displayed beside the graph. Default value is \code{TRUE}.
#' @param contour Logical. Indicates, whether contours should be plotted in the graph. Default value is \code{FALSE}.
#' @param output_path File path where the animation will be saved using `gifski` renderer (optional). If not defined, the animation is plotted in the RStudio Viewer.
#' @param ... Additional parameters for animation according to [gganimate::animate].
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
#' @importFrom grDevices hsv
#' @importFrom scales rescale
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # This example may take a few seconds to render.
#' # Run only if you want to generate the full animation.
#' # Prepare a data structure:
#' s1e05 <- epochdata |> dplyr::filter(subject == 1 & epoch == 5 & time %in% c(10:20))
#' # Plot animation:
#' animate_topo(s1e05, amplitude = "signal", t_lim = c(0,50))
#' }
#'
animate_topo <- function(data, amplitude, t_lim, FS = 250, mesh, coords = NULL, template = NULL,
                         col_range = NULL, col_scale = NULL,
                         legend = TRUE, contour = FALSE,
                         output_path = NULL, ...){

  amp_value <- {{ amplitude }}
  amp_name <- rlang::as_string(amp_value)

  if (!amp_name %in% names(data)) {
    stop(paste0("There is no column '", amp_name, "' in the input data."))
  }

  if (!(is.logical(contour))) {
    stop("Argument 'contour' has to be logical.")
  }

  if (!(is.logical(legend))) {
    stop("Argument 'legend' has to be logical.")
  }

  if (!is.null(template)) {
    coords <- switch(template,
                     "HCGSN256" = diegr::HCGSN256$D2,
                     stop("Unknown template.")
                     )
  }

  if (is.null(template) && is.null(coords)) {
    # use HCGSN256 template
    coords <- diegr::HCGSN256$D2
  }

  required_cols <- c("x", "y", "sensor")
  missing_cols <- setdiff(required_cols, colnames(coords))

  if (length(missing_cols) > 0) {
    stop(paste("The following required columns in 'coords' are missing:",
               paste(missing_cols, collapse = ", ")))
  }

  if (missing(mesh)) {
    mesh <- point_mesh(dim = 2, template = { template })
  }

  if ("D2" %in% names(mesh)) { # control mesh structure
    mesh_mat <- mesh$D2
  } else if (req_cols(mesh, c("x", "y"))) {
    mesh_mat <- mesh[, c("x", "y")]
  } else {
    if (is.null(dim(mesh))) {
      stop("At least two columns are required in `mesh` input parameter.")
    } else if (ncol(mesh) < 2) {
      stop("At least two columns are required in `mesh` input parameter.")
    }

    mesh_mat <- data.frame("x" = mesh[,1], "y" = mesh[,2])
  }

  newdata <- prepare_anim_structure(data, amp_name = amp_name, coords, mesh_mat)

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

  g <- ggplot(newdata, aes(x = .data$mesh_coord$x, y = .data$mesh_coord$y)) +
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

  if (!is.null(output_path)) {
    if (!requireNamespace("gifski", quietly = TRUE)) {
      stop("To export animation, the 'gifski' package is required.")
    }
    anim <- gganimate::animate(g, renderer = gganimate::gifski_renderer(), ...)

    gganimate::anim_save(output_path, animation = anim)
    message("Animation saved into: ", output_path)
  } else {
  print(gganimate::animate(g, ...))
}

}


prepare_anim_structure <- function(data, amp_name, coords, mesh_mat) {
  ## create structure for animate_topo from database
  ## need to change also for 3D case

  if (inherits(data, "tbl_sql")) {
    data <- dplyr::collect(data)
  }


  if (all(c("x", "y", "z") %in% names(coords))) {
    coords_df <- data.frame(x = coords[["x"]], y = coords[["y"]], z = coords[["z"]])
  } else {
    coords_df <- data.frame(x = coords[["x"]], y = coords[["y"]])
  }

  # add coordinates of sensors to the tibble with time and signal data
  tib_signal <- data |>
    left_join(coords, by = "sensor") #coords_sen


  # splitting the tibble and computing IM model on splitted tibble
  tib_split <- split(tib_signal, tib_signal$time)

  tib_IM <- purrr::map_dfr(names(tib_split), function(t) {
    df_t <- tib_split[[t]]
    interpolated_values <- IM(coords_df, df_t[[amp_name]], mesh_mat)$Y_hat

    tibble(
      time = as.numeric(t),
      mesh_coord = mesh_mat,
      y_IM = interpolated_values[1:dim(mesh_mat)[1]]
    )
  })

  return(tib_IM)
}


#' 3D scalp plot animation in time
#'
#' @param data An input data frame or tibble with at least this required columns: \code{time} - the number of time point, \code{sensor} - the sensor label and the column with the EEG amplitude to plot specified in the argument \code{amplitude}.
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values.
#' @param mesh An object of class \code{"mesh"} used for computing IM model. If not defined, the polygon point mesh with default settings from \code{\link{point_mesh}} function is used. Can also be a data frame or a matrix with x, y and z coordinates of a point mesh. See \code{\link{scalp_plot}} for details about the structure.
#' @param tri A matrix with indices of the triangles. If missing, the triangulation is computed using \code{\link{make_triangulation}} function from \code{D2} element of the input mesh object (or a list).
#' @param coords Sensor coordinates as a tibble or data frame with named \code{x}, \code{y} and \code{z} columns of sensor coordinates and \code{sensor} column with sensor names. If not defined, the HCGSN256 template is used.
#' @param template The kind of sensor template montage used. Currently the only available option is \code{"HCGSN256"} denoting the 256-channel HydroCel Geodesic Sensor Net v.1.0, which is also a default setting.
#' @param col_range A vector with minimum and maximum value of the amplitude used in the colour palette for plotting. If not defined, the range of input signal is used.
#' @param col_scale Optionally, a colour scale to be utilised for plotting. If not defined, it is computed from \code{col_range}.
#' @param sec The time interval used between individual animation frames, in seconds (default: 0.3).
#' @param frames_dir Directory where the individual frames will be saved. If NULL, the video is only displayed in viewer and the frames are not saved.
#' @param output_path Optional path to the output mp4 video file (".mp4" extension is required for correct rendering). If NULL, no video is created.
#' @param framerate Number of frames per second for the output mp4 video (default: 3).
#'
#' @details
#' Setting the parameter `tri` requires defining a `mesh` parameter.
#' The parameter \code{mesh} should optimally be a \code{"mesh"} object (output from \code{\link{point_mesh}} function) or a list with the same structure (see \code{\link{point_mesh}} for more information). In that case, setting the argument \code{tri} is optional, and if it is absent, a triangulation based on the \code{D2} element of the mesh is calculated and used in the plot.
#' If the input \code{mesh} is a data frame or a matrix with only 3D coordinates of a point mesh, the use of previously created triangulation (through \code{tri} argument) is necessary.
#'
#' Note: For exporting the video, setting `frames_dir` together with `output_path` is required.
#'
#' @returns
#' The output depends on the provided arguments:
#' - If `frames_dir` is specified, individual animation frames (PNG) are saved to that directory.
#' - If also `output_path` is specified, a video (MP4) is created and saved using the `av` package.
#' - Otherwise, the animation is displayed in an interactive rgl window.
#'
#' @export
#'
#' @importFrom rlang .data
#' @import rgl
#'
#'
#' @examples
#' \dontrun{
#' # This example may take a few seconds to render.
#' # Run only if you want to generate the full animation.
#' # Prepare a data structure:
#' s1e05 <- epochdata |> dplyr::filter(subject == 1 & epoch == 5 & time %in% c(10:20))
#' # Plot animation with default mesh and triangulation:
#' animate_scalp(s1e05, amplitude = "signal")
#' }
animate_scalp <- function(data, amplitude, mesh, tri = NULL, coords = NULL, template = NULL,
                          col_range = NULL, col_scale = NULL,
                          sec = 0.3, frames_dir = NULL, output_path = NULL, framerate = 3) {

  amp_value <- {{ amplitude }}
  amp_name <- rlang::as_string(amp_value)

  if (!amp_name %in% names(data)) {
    stop(paste0("There is no column '", amp_name, "' in the input data."))
  }

  if (!is.null(template)) {
    coords <- switch(template,
                     "HCGSN256" = diegr::HCGSN256$D3,
                     stop("Unknown template.")
    )
  }

  if (is.null(template) && is.null(coords)) {
    # use HCGSN256 template
    coords <- diegr::HCGSN256$D3
  }

  required_cols <- c("x", "y", "z", "sensor")
  missing_cols <- setdiff(required_cols, colnames(coords))

  if (length(missing_cols) > 0) {
    stop(paste("The following required columns in 'coords' are missing:",
               paste(missing_cols, collapse = ", ")))
  }


  if (missing(mesh)) {
    if (!is.null(tri)) {
      stop("The argument 'mesh' must be provided when argument 'tri' is specified.")
    }
     mesh <- point_mesh(dim = c(2,3), template = { template })
  }

  control_mesh(mesh, tri = { tri }) # control the input mesh structure

  if ("D3" %in% names(mesh)) {
    mesh3 <- mesh$D3
  } else {
    mesh3 <- mesh
  }

  if (is.null(tri)) {
    tri <- make_triangulation(mesh$D2)
  }

  newdata <- prepare_anim_structure(data, amp_name = amp_name, coords, mesh3)

  if (is.null(col_scale)) {

    if (is.null(col_range)) {
      if (all(is.na(newdata$y_IM))) {
        stop("No valid values in y_IM to create color scale.")
      }
      col_range <- c(1.1 * range(newdata$y_IM))
    }

    col_scale <- create_scale(col_range)
  }


  mesh0 <- rgl::mesh3d(x = mesh3$x, y = mesh3$y, z = mesh3$z, triangles = t(tri))

  plot_3d_time <- function(time_point) {
    time_data <- newdata |>
      filter(.data$time == time_point) # filter actual time point

    rgl::clear3d()
    y_cut <- cut(time_data$y_IM, breaks = col_scale$breaks, include.lowest = TRUE)
    y_col <- col_scale$colors[y_cut]

    rgl::shade3d(mesh0, col = y_col, lit = FALSE)

  }

  if (is.null(frames_dir) && !is.null(output_path)) {
    warning("The 'frames_dir' setting is required for video export.")
  }

  if (!is.null(frames_dir)) { # export frames (and mp4 video)
    export_video(frames_dir = frames_dir, plot_function = plot_3d_time,
                 time_points = unique(newdata$time),
                 output_path = output_path, framerate = framerate)
  } else {

    for (t in unique(newdata$time)) {  # animation
      plot_3d_time(t)
      Sys.sleep({ sec })
    }
  }

}

export_video <- function(frames_dir, plot_function, time_points,
                         output_path, framerate){

    if (!dir.exists(frames_dir)) { # create a dir if not exists
      dir.create(frames_dir, recursive = TRUE)
    }

    frame_index <- 1

    for (t in time_points) {
      plot_function(t)

      snapshot_filename <- sprintf("%s/frame_%03d.png", frames_dir, frame_index)
      rgl::rgl.snapshot(snapshot_filename)

      frame_index <- frame_index + 1
    }

    message("Frames saved to: ", normalizePath(frames_dir))

    if (!is.null(output_path)) { # create video in mp4
      if (!requireNamespace("av", quietly = TRUE)) {
        stop("To export video, the 'av' package is required.")
      }

      img_list <- list.files(frames_dir, pattern = "frame_\\d+\\.png", full.names = TRUE)
      av::av_encode_video(img_list, output = output_path, framerate = framerate)
      message("Video created at: ", normalizePath(output_path))
    }
  }


