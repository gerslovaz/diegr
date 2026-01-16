#' Topographic map animation in time
#'
#' @description Display a topographic animation of the change in amplitude over time. The function enables direct rendering in Rstudio Viewer or saving the animation in gif format to the chosen location.
#'
#' @param data An input data frame or tibble with at least this required columns: `time` - the number of time point,`sensor` - the sensor label and the column with the EEG amplitude to plot specified in the argument `amplitude`.
#' @param amplitude A character specifying the name of the column from input data with EEG amplitude values.
#' @param t_lim A numeric vector of length 2 with limits of time points (i.e., the length of the timeline displayed below the animation).
#' @param FS The sampling frequency. Default value is 250 Hz.
#' @param t0 Index of the zero time point, i.e. point, where 0 ms should be marked (most often time of the stimulus or time of the response).
#' @param mesh A `"mesh"` object (or a named list with the same structure) containing at least `D2` element with x and y coordinates of a point mesh used for computing IM model. If not defined, the point mesh with default settings from \code{\link{point_mesh}} function is used.
#' @param coords Sensor coordinates as a tibble or data frame with named `x`, `y` columns of sensor coordinates and `sensor` column with sensor names. If not defined, the HCGSN256 template is used.
#' @param template The kind of sensor template montage used. Currently the only available option is `"HCGSN256"` denoting the 256-channel HydroCel Geodesic Sensor Net v.1.0, which is also a default setting.
#' @param col_range A vector with minimum and maximum value of the amplitude used in the colour palette for plotting. If not defined, the range of interpolated signal is used.
#' @param col_scale Optionally, a colour scale to be utilised for plotting. If not defined, it is computed from `col_range`.
#' @param show_legend Logical. Indicates, whether legend should be displayed beside the graph. Default value is `TRUE`.
#' @param contour Logical. Indicates, whether contours should be plotted in the graph. Default value is `FALSE`.
#' @param output_path File path where the animation will be saved using `gifski` renderer (optional). If not defined, the animation is plotted in the RStudio Viewer.
#' @param ... Additional parameters for animation according to [gganimate::animate].
#'
#' @details
#' For more details about required mesh structure see \code{\link{point_mesh}} function. If the input `mesh` structure does not match this format, an error or incorrect function behavior may occur.
#'
#' The time part of input data is assumed to be in numbers of time points, conversion to ms takes place inside the function for drawing the timeline labels.
#' Due to the flexibility of the function (e.g. to mark and animate only a short section from the entire time course or to compare different data in the same time interval), it allows to enter and plot user-defined time ranges. If some values of the time are outside the `t_lim` range, the function writes a warning message - in that case the animation is still rendered, but the timeline will not match reality.
#'
#' Note: When specifying the `coords` and `template` at the same time, the `template` parameter takes precedence and the `coords` parameter is ignored.
#'
#' @return
#' If `output_path` is `NULL`, the function prints the animation to the RStudio Viewer.
#' If `output_path` is specified, the animation is saved to the given file path and not displayed.
#'
#' @export
#'
#' @seealso Static version: \code{\link{topo_plot}}, animated 3D scalp map: \code{\link{animate_scalp}}
#'
#' @import ggplot2
#' @import dplyr
#' @import gganimate
#' @importFrom grDevices hsv
#' @importFrom scales rescale
#' @importFrom rlang .data
#'
#' @examples
#' \donttest{
#' # This example may take a few seconds to render.
#' # Run only if you want to generate the full animation.
#' # Prepare a data structure:
#' s1e05 <- pick_data(epochdata, subject_rg = 1, epoch_rg = 5, time_rg = 10:20)
#' # Plot animation
#' # t0 = 10 indicates the time point of stimulus in epochdata,
#' # t_lim is the whole range of epochdata, we animate only a short period
#' animate_topo(s1e05, amplitude = "signal", t_lim = c(1,50), t0 = 10)
#' }
#'
animate_topo <- function(data,
                         amplitude,
                         t_lim,
                         FS = 250,
                         t0 = 1,
                         mesh,
                         coords = NULL,
                         template = NULL,
                         col_range = NULL,
                         col_scale = NULL,
                         show_legend = TRUE,
                         contour = FALSE,
                         output_path = NULL,
                         ...){

  stop_if_missing_cols(data, required_cols = c(amplitude, "time", "sensor"))

  if (any(is.na(data[[amplitude]]))) {
    stop("There are NA's in amplitude column.")
  }

  if (!(is.logical(contour))) {
    stop("Argument 'contour' has to be logical.")
  }

  if (!(is.logical(show_legend))) {
    stop("Argument 'show_legend' has to be logical.")
  }

  if (!is.numeric(t_lim) || length(t_lim) != 2) {
    stop("'t_lim' must be a numeric vector of length 2.")
  }

  if (!is.numeric(FS) || FS <= 0) {
    stop("'FS' must be a positive number.")
  }

  if (!is.numeric(t0) || length(t0) != 1) {
    stop("'t0' must be a numeric value.")
  }

  if (!is.null(template) && !is.null(coords)) {
    warning("Both 'template' and 'coords' were specified. Using 'template' and ignoring 'coords'.")
  }

  if (is.null(template) && is.null(coords)) {
    # use HCGSN256 template
    template <- "HCGSN256"
  }

  sensor_select <- unique(data$sensor)

  if (!is.null(template)) {
    coords_full <- switch(template,
                          "HCGSN256" = diegr::HCGSN256$D2,
                          stop("Unknown template.")
    )
    sensor_index <- which(coords_full$sensor %in% sensor_select)
    coords <- coords_full[sensor_index,]
  }

  stop_if_missing_cols(coords, required_cols = c("x", "y", "sensor"))

  if (missing(mesh)) {
    mesh <- point_mesh(dimension = 2, template = template,
                       sensor_select = sensor_select)
  }

  if (control_D2(mesh)) {
    mesh_mat <- mesh$D2
  }

  newdata <- prepare_anim_structure(data, amp_name = amplitude, coords, mesh_mat)

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
  time_positions <- seq(x_range[1], x_range[2], length.out = k + 1)
  time_range <- sort(unique(newdata$time))
  timeline <- tibble(time = time_range, x = time_positions[time_range], y = y_l)

  if (is.null(col_scale)) {

    if (is.null(col_range)) {
      if (all(is.na(newdata$amplitude_IM))) {
        stop("No valid values in amplitude_IM to create color scale.")
      }
      col_range <- range(newdata$amplitude_IM)
    }

    col_scale <- create_scale(col_range)
  }

  g <- ggplot(newdata, aes(x = .data$mesh_coord$x, y = .data$mesh_coord$y)) +
    geom_raster(aes(fill = .data$amplitude_IM)) +
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
      axis.title = element_blank()
    ) +
    annotate("segment", x = x0, y = M + 0.07 * abs(M), xend = x0 - 0.08 * M, yend = M + 0.01 * abs(M), col = "gray40") +
    annotate("segment", x = x0, y = M + 0.07 * abs(M), xend = x0 + 0.08 * M, yend = M + 0.01 * abs(M), col = "gray40") +
    geom_point(data = coords, aes(x = .data$x, y = .data$y), color = "black", cex = 0.7) +

    annotate("segment", x = x_range[1], y = y_l, xend = x_range[2], yend = y_l, col = "black")  + # time line
    annotate(geom = "text", x = x_range[1], y = y_l + 0.05 * abs(M), label = paste0((range(t_lim)[1] - t0) * k0)) +
    annotate(geom = "text", x = x_range[2], y = y_l + 0.05 * abs(M), label = paste0((range(t_lim)[2] - t0) * k0, " ms")) +
    geom_point(data = timeline, aes(x = .data$x, y = .data$y, group = .data$time),
               color = "red", size = 3, inherit.aes = FALSE) + # red moving point
    geom_text(data = timeline, aes(x = .data$x, y = .data$y, label = paste0("t = ", (.data$time - t0) * k0, " ms"), group = .data$time),
              vjust = 1.5, color = "red", inherit.aes = FALSE)  # label for the point


  if (show_legend == TRUE) {
    g <- g  +
      labs(fill = expression(paste("Amplitude (", mu, "V)"))) +
      guides(fill = guide_colorbar(barwidth = 0.7, barheight = 20)) +
      theme(
        legend.position = "right",
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8)
      )
  } else {
    g <- g +
      theme(
        legend.position = "none"
      )
  }

  if (contour == TRUE) {
    g <- g + geom_contour(aes(z = .data$amplitude_IM), color = "gray", breaks = col_scale$breaks)
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


#' Prepare animation structure from EEG data
#'
#' @description
#' Internal helper function to prepare time-split spatial interpolation results for topographic animation.
#' It joins sensor coordinates to EEG signal data, performs interpolation for each time point,
#' and returns a long-format tibble suitable for animation.
#'
#' @param data A data frame, tibble or database table containing EEG signal data, including time, sensor, and amplitude.
#' @param amp_name A character string specifying the name of the amplitude column in `data`.
#' @param coords A data frame with sensor coordinate information. Must include columns `x`, `y` (and `z` for 3D animation) and `sensor`.
#' @param mesh_mat A matrix of 2D or 3D coordinates over which the EEG signal will be interpolated.
#'
#' @return
#' A tibble with interpolated signal values over the mesh, including columns:
#' \itemize{
#'   \item{time}{Time point corresponding to the EEG frame.}
#'   \item{mesh_coord}{Matrix of mesh coordinates (repeated across time).}
#'   \item{amplitude_IM}{Interpolated amplitude values at mesh points.}
#' }
#'
#' @import dplyr
#' @importFrom purrr map_dfr
#'
#' @noRd
prepare_anim_structure <- function(data,
                                   amp_name,
                                   coords,
                                   mesh_mat) {

  if (inherits(data, "tbl_sql") || inherits(data, "tbl_dbi")) {
    data <- dplyr::collect(data)
  }

  if (all(c("x", "y", "z") %in% names(coords))) {
    coords_df <- data.frame(x = coords[["x"]], y = coords[["y"]], z = coords[["z"]])
  } else {
    coords_df <- data.frame(x = coords[["x"]], y = coords[["y"]])
  }

  if (!all(unique(coords$sensor) %in% data$sensor)) {
    stop("Mismatch between sensors in data and coords.")
  }

  sensor_order <- as.factor(coords$sensor) # reorder data according to sensor
  data_order <- data |>
    mutate(sensor = factor(.data$sensor, levels = sensor_order)) |>
    arrange(.data$sensor)

  # add coordinates of sensors to the tibble with time and signal data
  tib_signal <- data_order |>
    left_join(coords, by = "sensor")

  col_name <- enquo(amp_name)

  # split the tibble and computing IM model on splitted tibble
  tib_split <- split(tib_signal, tib_signal$time)

  tib_IM <- purrr::map_dfr(names(tib_split), function(t) {
    df_t <- tib_split[[t]]
    y_values <- df_t |>
      pull(!!col_name)
    interpolated_values <- IM(coords_df, y_values , mesh_mat)$Y_hat

    tibble(
      time = as.numeric(df_t$time[1]),
      mesh_coord = mesh_mat,
      amplitude_IM = interpolated_values[1:dim(mesh_mat)[1]]
    )
  })

  return(tib_IM)
}


#' 3D scalp plot animation in time
#'
#' @description Display a topographic 3D scalp animation of the change in amplitude over time. The function enables direct rendering in Rstudio Viewer or saving the animation in MP4 format or individual frames in PNG format to the chosen location.
#'
#' @param data An input data frame or tibble with at least this required columns: `time` - the number of time point, `sensor` - the sensor label and the column with the EEG amplitude to plot specified in the argument `amplitude`.
#' @param amplitude A character string naming the column with EEG amplitude values.
#' @param mesh An object of class `"mesh"` (or a named list with the same structure) used for computing IM model. If not defined, the polygon point mesh with default settings from \code{\link{point_mesh}} function is used. See \code{\link{scalp_plot}} for details about the structure.
#' @param tri A matrix with indices of the triangles. If missing, the triangulation is computed using \code{\link{make_triangulation}} function from `D2` element of the mesh.
#' @param coords Sensor coordinates as a tibble or data frame with named `x`, `y` and `z` columns of sensor coordinates and `sensor` column with sensor names. If not defined, the HCGSN256 template is used.
#' @param template The kind of sensor template montage used. Currently the only available option is `"HCGSN256"` denoting the 256-channel HydroCel Geodesic Sensor Net v.1.0, which is also a default setting.
#' @param col_range A vector with minimum and maximum value of the amplitude used in the colour palette for plotting. If not defined, the range of interpolated signal is used.
#' @param col_scale Optionally, a colour scale to be utilised for plotting. If not defined, it is computed from `col_range`.
#' @param sec The time interval used between individual animation frames, in seconds (default: 0.3).
#' @param frames_dir Directory where the individual frames will be saved. If NULL, the video is only displayed in viewer and the frames are not saved.
#' @param output_path Optional path to the output mp4 video file (".mp4" extension is required for correct rendering). If NULL, no video is created.
#' @param framerate Number of frames per second for the output mp4 video (default: 3).
#' @param cleanup Logical. Indicates, if all the PNG files should be deleted after encoding video. Default value is `TRUE`.
#'
#' @details
#' Setting the parameter `tri` requires defining a `mesh` parameter.
#' The parameter `mesh` should optimally be a `"mesh"` object (output from \code{\link{point_mesh}} function) or a list with the same structure (see \code{\link{point_mesh}} for more information). In that case, setting the argument `tri` is optional, and if it is absent, a triangulation based on the `D2` element of the mesh is calculated and used in the plot.
#' If the input `mesh` contains only 3D coordinates of a point mesh in `D3` element, the use of previously created triangulation (through `tri` argument) is required.
#'
#' Notes:
#' For exporting the video, setting `frames_dir` together with `output_path` is required.
#'
#' When specifying the `coords` and `template` at the same time, the `template` parameter takes precedence and the `coords` parameter is ignored.
#'
#' @return
#' The output depends on the provided arguments:
#' - If `frames_dir` is specified, individual animation frames (PNG) are saved to that directory.
#' - If also `output_path` is specified, a video (MP4) is created and saved using the `av` package.
#' - Otherwise, the animation is displayed in an interactive rgl window.
#'
#' @seealso Static version: \code{\link{scalp_plot}}, animated 2D topo map: \code{\link{animate_topo}}
#'
#' @export
#'
#' @importFrom rlang .data
#' @import rgl
#'
#'
#' @examples
#' \donttest{
#' # This example may take a few seconds to render.
#' # Run only if you want to generate the full animation.
#' # Note: The example opens a rgl 3D viewer.
#' # Prepare a data structure:
#' s1e05 <- pick_data(epochdata, subject_rg = 1, epoch_rg = 5, time_rg = 10:20)
#' # Plot animation with default mesh and triangulation:
#' animate_scalp(s1e05, amplitude = "signal")
#' }
animate_scalp <- function(data,
                          amplitude,
                          mesh,
                          tri,
                          coords = NULL,
                          template = NULL,
                          col_range = NULL,
                          col_scale = NULL,
                          sec = 0.3,
                          frames_dir = NULL,
                          output_path = NULL,
                          framerate = 3,
                          cleanup = TRUE) {

  stop_if_missing_cols(data, required_cols = c(amplitude, "time", "sensor"))

  if (any(is.na(data[[amplitude]]))) {
    stop("There are NA's in amplitude column.")
  }

  if (!is.numeric(sec) || sec <= 0) {
    stop("'sec' must be a positive number.")
  }

  if (!is.numeric(framerate) || framerate <= 0) {
    stop("'framerate' must be a positive number.")
  }

  if (!is.null(output_path) && !endsWith(output_path, ".mp4")) {
    stop("The output path does not end with '.mp4'.")
  }

  if (!is.null(template) && !is.null(coords)) {
    warning("Both 'template' and 'coords' were specified. Using 'template' and ignoring 'coords'.")
  }

  if (is.null(template) && is.null(coords)) {
    # use HCGSN256 template
    template <- "HCGSN256"
  }

  sensor_select <- unique(data$sensor)

  if (!is.null(template)) {
    coords_full <- switch(template,
                          "HCGSN256" = diegr::HCGSN256$D3,
                          stop("Unknown template.")
    )
    sensor_index <- which(coords_full$sensor %in% sensor_select)
    coords <- coords_full[sensor_index,]
  }

  stop_if_missing_cols(coords, required_cols = c("x", "y", "z", "sensor"))

  if (missing(mesh)) {
    if (!missing(tri)) {
      stop("The argument 'mesh' must be provided when argument 'tri' is specified.")
    }
     mesh <- point_mesh(dimension = c(2,3), template = template,
                        sensor_select = sensor_select)
  }

  if (control_D3(mesh)) {
    mesh3 <- mesh$D3
  }

  if (missing(tri)) {
    if (control_D2(mesh)) {
      mesh2 <- mesh$D2
    }
    tri <- make_triangulation(mesh2)
  }

  newdata <- prepare_anim_structure(data, amp_name = amplitude, coords, mesh3)

  if (is.null(col_scale)) {

    if (is.null(col_range)) {
      if (all(is.na(newdata$amplitude_IM))) {
        stop("No valid values in amplitude_IM to create color scale.")
      }
      col_range <- range(newdata$amplitude_IM)
    }

    col_scale <- create_scale(col_range)
  }


  mesh0 <- rgl::mesh3d(x = mesh3$x, y = mesh3$y, z = mesh3$z, triangles = t(tri))

  plot_3d_time <- function(time_point) {
    time_data <- newdata |>
      filter(.data$time == time_point) # filter actual time point

    rgl::clear3d()
    y_cut <- cut(time_data$amplitude_IM, breaks = col_scale$breaks, include.lowest = TRUE)
    y_col <- col_scale$colors[y_cut]

    rgl::shade3d(mesh0, col = y_col, lit = FALSE)

  }

  if (is.null(frames_dir) && !is.null(output_path)) {
    warning("The 'frames_dir' setting is required for video export. Video was not exported.")
  }

  if (!is.null(frames_dir)) { # export frames (and mp4 video)
    export_video(frames_dir = frames_dir, plot_function = plot_3d_time,
                 time_points = unique(newdata$time),
                 output_path = output_path, framerate = framerate,
                 cleanup = cleanup)
  } else {

    for (t in unique(newdata$time)) {  # animation
      tryCatch({
        plot_3d_time(t)
        Sys.sleep(sec)
      }, error = function(e) {
        warning(paste("Failed to plot frame:", t, "with error:", e$message))
      })
    }
  }

}

#' Export EEG animation frames as video
#'
#' @description
#' Helper function to render a series of 3D EEG visualizations and export them as PNG frames, then encode them into an `.mp4` video using the `av` package.
#'
#' @param frames_dir A name of directory where frame images will be saved.
#' @param plot_function A function that draws a 3D EEG plot for a single time point with time index as input.
#' @param time_points A numeric vector of time indices or values to render sequentially.
#' @param output_path Optional file path to save the encoded `.mp4` video. If `NULL`, only PNG frames are generated.
#' @param framerate Frame rate (in frames per second) for the output video.
#' @param cleanup Logical. Indicates, if all the PNG files should be deleted after encoding video. Default value is `TRUE`.
#'
#' @return
#' Invisibly returns `NULL`. Saves PNG frames to `frames_dir` and, if specified, creates an `.mp4` video at `output_path`.
#'
#' @import rgl
#'
#' @noRd
export_video <- function(frames_dir,
                         plot_function,
                         time_points,
                         output_path,
                         framerate,
                         cleanup) {
  if (!is.function(plot_function)) {
    stop("plot_function must be a function.")
  }

  if (!dir.exists(frames_dir)) { # create a dir if not exists
    dir.create(frames_dir, recursive = TRUE)
  }

  if (file.access(frames_dir, 2) != 0) {
    stop(paste("Cannot write to directory:", frames_dir, " - Check permissions."))
  }

  if (!is.numeric(time_points)) {
    stop("Time points must be numeric.")
  }

  frame_index <- 1

  for (t in time_points) {
    plot_function(t)

    snapshot_filename <- sprintf("%s/frame_%03d.png", frames_dir, frame_index)

    tryCatch({
      # check if rgl context is active and then attempt the snapshot
      if (rgl::rgl.cur() != 0) {
        rgl::rgl.snapshot(snapshot_filename)
      } else {
        stop("No active rgl device.")
      }
    }, error = function(e) {
      warning(paste("Failed to take rgl snapshot:", e$message))
    })

    frame_index <- frame_index + 1
  }

  if (!is.null(output_path)) { # create video in mp4
    if (!requireNamespace("av", quietly = TRUE)) {
      stop("To export video, the 'av' package is required.")
    }

    img_list <- sort(list.files(frames_dir, pattern = "frame_\\d+\\.png", full.names = TRUE))
    av::av_encode_video(img_list, output = output_path, framerate = framerate)

    if (cleanup == TRUE) {
      # delete all the PNG files after encoding video
      file.remove(img_list)
    } else {
      message("Frames saved to: ", normalizePath(frames_dir))
    }

    message("Video created at: ", normalizePath(output_path))
  }
}



#' Prepare interpolated animation structure with confidence intervals
#'
#' @description
#' Internal helper function for generating a time-series of interpolated EEG topographic maps that include lower and upper confidence interval bounds.
#' The result is reshaped into long format to facilitate animation of both average and uncertainty ribbons.
#'
#' @param data A data frame, tibble or database table containing EEG signal data. Must include columns `average`, `ci_low`, `ci_up`, `time`, and `sensor`.
#' @param coords A data frame containing sensor coordinate information. Must include `sensor`, `x`, `y` (and `z` for 3D figure).
#' @param mesh_mat A numeric matrix of mesh coordinates (2D or 3D) over which the signal will be interpolated.
#'
#' @return
#' A tibble in long format with the following columns:
#' \itemize{
#'   \item{time}{Time point of the EEG signal.}
#'   \item{mesh_coord}{Coordinates of mesh grid (repeated across time and statistic type).}
#'   \item{stats}{Type of the statistics value — "Average", "CI, lower bound", or "CI, upper bound".}
#'   \item{stats_value}{Interpolated value at each mesh point.}
#' }
#'
#' @noRd
prepare_anim_structure_CI <- function(data,
                                      coords,
                                      mesh_mat) {

  if (inherits(data, "tbl_sql")) {
    data <- dplyr::collect(data)
  }

  if (all(c("x", "y", "z") %in% names(coords))) {
    coords_df <- data.frame(x = coords[["x"]], y = coords[["y"]], z = coords[["z"]])
  } else {
    coords_df <- data.frame(x = coords[["x"]], y = coords[["y"]])
  }

  if (!all(unique(coords$sensor) %in% data$sensor)) {
    stop("Mismatch between sensors in data and coords.")
  }

  sensor_order <- as.factor(coords$sensor) # reorder data according to sensor
  data_order <- data |>
    mutate(sensor = factor(.data$sensor, levels = sensor_order)) |>
    arrange(.data$sensor)

  # add coordinates of sensors to the tibble with time and signal data
  tib_signal <- data_order |>
    left_join(coords, by = "sensor")


  # splitting the tibble and computing IM model on splitted tibble
  tib_split <- split(tib_signal, tib_signal$time)

  tib_IM <- purrr::map_dfr(names(tib_split), function(t) {
    df_t <- tib_split[[t]]
    interpolated_values <- IM(coords_df, df_t[["average"]], mesh_mat)$Y_hat
    y_hat_low <- IM(coords_df, df_t[["ci_low"]], mesh_mat)$Y_hat
    y_hat_up <- IM(coords_df, df_t[["ci_up"]], mesh_mat)$Y_hat

    interp_tib <- tibble(
      time = as.numeric(df_t$time[1]),
      mesh_coord = mesh_mat,
      y_avg = interpolated_values[1:dim(mesh_mat)[1]],
      y_low = y_hat_low[1:dim(mesh_mat)[1]],
      y_up = y_hat_up[1:dim(mesh_mat)[1]]
    )

  })

  tib_IM <- tib_IM |>
    tidyr::pivot_longer(
          cols = c("y_low", "y_avg", "y_up"),
          names_to = "stats",
          values_to = "stats_value")
  tib_IM$stats <- factor(tib_IM$stats, levels = c("y_low", "y_avg", "y_up"),
                         labels = c("CI, lower bound", "Average", "CI, upper bound"))

  return(tib_IM)
}

#' Animate EEG average topographic map with confidence bounds
#'
#' @description
#' An animation of the average signal time course as a topographic map along with the lower and upper bounds of the confidence interval. In the output, three facets are plotted per frame: CI lower, average, CI upper.
#'
#'
#' @param data A data frame, tibble or a database table with input data to plot. It should be an output from \code{\link{compute_mean}} function or an object with the same structure. Required columns: `sensor` - sensor labels, `time` - numbers of time points, `average` - average signal values, `ci_low` and `ci_up` - lower and upper CI bounds.
#' @param t_lim Limits of time points (i.e., the length of the timeline displayed below the animation).
#' @param FS The sampling frequency. Default value is 250 Hz.
#' @param t0 Index of the zero time point, i.e. point, where 0 ms should be marked (most often time of the stimulus or time of the response).
#' @param mesh A `"mesh"` object (or a named list with the same structure) containing at least `D2` element with x and y coordinates of a point mesh used for computing IM model. If not defined, the point mesh with default settings from \code{\link{point_mesh}} function is used.
#' @param coords Sensor coordinates as a tibble or data frame with named `x`, `y` columns of sensor coordinates and `sensor` column with sensor names. If not defined, the HCGSN256 template is used.
#' @param template The kind of sensor template montage used. Currently the only available option is `"HCGSN256"` denoting the 256-channel HydroCel Geodesic Sensor Net v.1.0, which is also a default setting.
#' @param col_range A vector with minimum and maximum value of the amplitude used in the colour palette for plotting. If not defined, the range of the input signal is used.
#' @param col_scale Optionally, a colour scale to be utilised for plotting. If not defined, it is computed from `col_range`.
#' @param show_legend Logical. Indicates, whether legend should be displayed below the graph. Default value is `TRUE`.
#' @param contour Logical. Indicates, whether contours should be plotted in the graph. Default value is `FALSE`.
#' @param output_path File path where the animation will be saved using `gifski` renderer (optional). If not defined, the animation is plotted in the RStudio Viewer.
#' @param ... Additional parameters for animation according to [gganimate::animate].
#'
#' @details
#' Note: When specifying the `coords` and `template` at the same time, the `template` parameter takes precedence and the `coords` parameter is ignored.
#'
#' @returns
#' If `output_path` is `NULL`, the function prints the animation to the RStudio Viewer.
#' If `output_path` is specified, the animation is saved to the given file path and not displayed. The `gifski` and `magick` packages are required for animation export.
#'
#' @seealso \code{\link{animate_topo}}, \code{\link{compute_mean}}, \code{\link{baseline_correction}}, static version: \code{\link{plot_topo_mean}}
#'
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @import gganimate
#' @importFrom grDevices hsv
#' @importFrom scales rescale
#' @importFrom rlang .data
#' @examples
#' \donttest{
#' # This example may take a few seconds to render.
#' # Run only if you want to generate the full animation.
#'
#' # a) prepare data: compute the mean from baseline corrected signal for subject 2,
#' # first 10 points and only 13 epochs (epochs 14 and 15 are outliers)
#' edata <- pick_data(epochdata, subject_rg = 2, epoch_rg = 1:13, time_rg = 1:10)
#' data_base <- baseline_correction(edata, baseline_range = 1:10) # baseline correction
#' data_mean <- compute_mean(data_base, amplitude = "signal_base", subject = 2,
#'  type = "jack", group = "space") # compute mean
#' # b) render the animation
#' # (t0 = 10 because the time of the stimulus in epochdata is in time point 10)
#' animate_topo_mean(data_mean, t_lim = c(1,50), t0 = 10)
#' }
animate_topo_mean <- function(data,
                              t_lim,
                              FS = 250,
                              t0 = 1,
                              mesh,
                              coords = NULL,
                              template = NULL,
                              col_range = NULL,
                              col_scale = NULL,
                              show_legend = TRUE,
                              contour = FALSE,
                              output_path = NULL,
                              ...) {

  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("To render the animation, the 'magick' package is required.")
  }

  if (!(is.logical(contour))) {
    stop("Argument 'contour' has to be logical.")
  }

  if (!(is.logical(show_legend))) {
    stop("Argument 'show_legend' has to be logical.")
  }

  if (!is.numeric(FS) || FS <= 0) {
    stop("'FS' must be a positive number.")
  }

  if (!is.numeric(t0) || length(t0) != 1) {
    stop("'t0' must be a numeric value.")
  }

  stop_if_missing_cols(data, required_cols = c("average", "ci_low", "ci_up", "sensor", "time"))

  if (any(is.na(data[["average"]]))) {
    stop("There are NA's in the 'average' column.")
  }

  if (any(is.na(data[["ci_low"]]))) {
    stop("There are NA's in the 'ci_low' column.")
  }
  if (any(is.na(data[["ci_up"]]))) {
    stop("There are NA's in the 'ci_up' column.")
  }

  if (!is.null(template) && !is.null(coords)) {
    warning("Both 'template' and 'coords' were specified. Using 'template' and ignoring 'coords'.")
  }

  if (is.null(template) && is.null(coords)) {
    # use HCGSN256 template
    template <- "HCGSN256"
  }

  sensor_select <- unique(data$sensor)

  if (!is.null(template)) {
    coords_full <- switch(template,
                          "HCGSN256" = diegr::HCGSN256$D2,
                          stop("Unknown template.")
    )
    sensor_index <- which(coords_full$sensor %in% sensor_select)
    coords <- coords_full[sensor_index,]
  }

  stop_if_missing_cols(coords, required_cols = c("x", "y", "sensor"))

  if (missing(mesh)) {
    mesh <- point_mesh(dimension = 2, template = { template },
                       sensor_select = sensor_select)
  }

  if (control_D2(mesh)) {
    mesh_mat <- mesh$D2
  }

  newdata <- prepare_anim_structure_CI(data, coords, mesh_mat)

  M <- max(mesh_mat$y, na.rm = TRUE)
  Mm <- min(mesh_mat$y, na.rm = TRUE)
  y_l <- Mm - 0.1 * abs(Mm)
  x_range <- range(mesh_mat$x)
  x0 <- mean(mesh_mat$x, na.rm = TRUE)

  if (any(newdata$time <  range(t_lim)[1] | newdata$time > range(t_lim)[2])) {
    warning("Some values of 'time' are outside the 't_lim' range.")
  }

  k0 <- 1000 / FS
  t_range <- range(t_lim)
  k <- range(t_lim)[2] - range(t_lim)[1]
  x_marg <- 0.03 * k
  time_positions <- seq(t_range[1], t_range[2], length.out = k + 1)
  time_range <- sort(unique(newdata$time))
  timeline <- tibble(time = time_range, x = time_positions[time_range], y = 0)


  if (is.null(col_scale)) {

    if (is.null(col_range)) {
      if (all(is.na(newdata$stats_value))) {
        stop("No valid values in stats_value to create color scale.")
      }
      col_range <- range(newdata$stats_value)
    }

    col_scale <- create_scale(col_range)
  }

  g <- ggplot(newdata, aes(x = .data$mesh_coord$x, y = .data$mesh_coord$y)) +
    geom_raster(aes(fill = .data$stats_value)) +
    scale_fill_gradientn(
      colors = col_scale$colors,
      breaks = col_scale$breaks,
      limits = range(col_scale$breaks),
      labels = round(col_scale$breaks, 2),
      values = scales::rescale(col_scale$breaks)
    ) +
    facet_wrap(~ stats, ncol = 3) +
    coord_fixed(ratio = 1) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )

  if (show_legend == TRUE) {
    g <- g  +
      labs(fill = expression(paste("Amplitude (", mu, "V)"))) +
      guides(fill = guide_colorbar(barwidth = 20, barheight = 0.7)) +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 8)
      )
  } else {
    g <- g +
      theme(legend.position = "none")
  }

  if (contour == TRUE) {
    g <- g + geom_contour(aes(z = .data$stats_value), color = "gray", breaks = col_scale$breaks)
  }


  g <- g + gganimate::transition_manual(.data$time)


  timeline_plot_base <- ggplot(timeline, aes(x = .data$x, y = .data$y)) +
    geom_hline(yintercept = 0) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    ) +
    xlim(t_range[1] - x_marg , t_range[2] + x_marg) +
    ylim(-0.25,0.25) +
    annotate(geom = "point", x = t_range[1], y = 0, col = "black", pch = 3) +
    annotate(geom = "text", x = t_range[1], y = 0.2, label = paste0((min(t_lim) - t0) * k0)) +
    annotate(geom = "point", x = t_range[2], y = 0, col = "black", pch = 3) +
    annotate(geom = "text", x = t_range[2], y = 0.2, label = paste0((max(t_lim) - t0) * k0 , " ms")) +
    geom_point(data = timeline, aes(x = .data$x, y = .data$y, group = .data$time),
               color = "red", size = 3, inherit.aes = FALSE) +
    geom_text(data = timeline, aes(x = .data$x, y = .data$y, label = paste0("t = ", (.data$time - t0) * k0, " ms"), group = .data$time),
              vjust = 1.5, color = "red", inherit.aes = FALSE) +  # label for the point
  gganimate::transition_manual(.data$time)


  c_gif <- gganimate::animate(g, width = 480, height = 300)
  d_gif <- gganimate::animate(timeline_plot_base, width = 480, height = 60)

  c_gif <- magick::image_read(c_gif)
  d_gif <- magick::image_read(d_gif)

  i = 1
  new_gif <- magick::image_append(c(c_gif[i], d_gif[i]), stack = TRUE)

  for (i in 2:length(time_range)) {
    combined <- magick::image_append(c(c_gif[i], d_gif[i]), stack = TRUE)
    new_gif <- c(new_gif, combined)
  }

  if (!is.null(output_path)) {
    if (!requireNamespace("gifski", quietly = TRUE)) {
      stop("To export animation, the 'gifski' package is required.")
    }

    magick::image_write(new_gif, format = "gif", path = output_path)
    message("Animation saved into: ", output_path)
  } else {
    print(new_gif)
  }

}

