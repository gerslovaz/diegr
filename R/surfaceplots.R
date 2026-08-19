#' Interactive 3D surface topographic plot of EEG signal
#'
#' @description
#' Creates an interactive 3D mesh surface plot of EEG amplitude in one time point using \code{plotly}.
#' The dense point mesh forms the 2D base, while the interpolated signal amplitude determines the 3D elevation and surface color.
#'
#' @param data A data frame, tibble, or a database table with input data to plot. It must contain at least two columns: \code{sensor} with sensor labels, and the column with the EEG amplitude specified in the argument \code{amplitude}.
#' @param amplitude A character string specifying the name of the column from the input data containing EEG amplitude values.
#' @param mesh A \code{"mesh"} object (or a named list with the same structure) containing at least a \code{D2} element with x and y coordinates of a point mesh used for computing the IM model, and a \code{template} element specifying the sensor montage. If not defined, the point mesh with default settings from \code{\link{point_mesh}} function is used.
#' @param coords Sensor coordinates as a tibble or data frame with named `x`, `y` and `sensor` columns. The `sensor` labels must match the labels in sensor column in `data`. If not defined, the template specified in `mesh$template` (or the default `"HCGSN256"`) is used.
#' @param template The kind of sensor template montage used. Available options are `"HCGSN256"`, `"biosemi128"`, `"biosemi256"`, and `"system1005"`. Default setting is `"HCGSN256"`.
#' @param col_range A vector with minimum and maximum value of the amplitude used in the colour palette for plotting. If not defined, the range of interpolated signal is used.
#' @param col_scale Optionally, a colour scale to be utilised for plotting. If not defined, it is computed from `col_range`.
#' @param show_sensors Logical. Indicates whether original sensor locations should be visualized as black scatter points on the 3D surface. Default is \code{TRUE}.
#'
#' @return A \code{plotly} widget object containing the interactive 3D surface plot.
#'
#' Additionally, the returned object carries a `"diegr_metadata"` attribute with metadata such as details about the data and plot.
#'
#' @seealso \code{\link{interactive_surfaceplot_curves}}, \code{\link{scalp_plot}}
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom plotly plot_ly add_trace
#' @importFrom dplyr collect mutate arrange
#'
#' @examples
#' \donttest{
#' # Prepare a data structure:
#' edata <- pick_data(epochdata, subject_rg = 2, epoch_rg = 1:13, time_rg = 1:10)
#' data_base <- baseline_correction(edata, baseline_range = 1:9)
#' data_mean <- compute_mean(data_base, amplitude = "signal_base", type = "point", domain = "space")
#'
#' # Create an interactive 3D surface plot of average signal in time point 10:
#' data_mean |>
#' dplyr::filter(time == 10) |>
#' interactive_surfaceplot(amplitude = "average", col_range = c(-10, 10))
#' }
interactive_surfaceplot <- function(data,
                                    amplitude,
                                    mesh,
                                    coords = NULL,
                                    template = NULL,
                                    col_range = NULL,
                                    col_scale = NULL,
                                    show_sensors = TRUE
                                    ){
  if (nrow(data) == 0) {
    stop("Input data is empty.")
  }

  stop_if_missing_cols(data, required_cols = c(amplitude, "sensor"))

  if (any(is.na(data[[amplitude]]))) {
    stop("There are NA's in amplitude column.")
  }

  if (!(is.logical(show_sensors))) {
    stop("Argument 'show_sensors' has to be logical.")
  }

  if (!missing(mesh) && !is.null(mesh$template)) {
    if (!is.null(template) && template != mesh$template) {
      warning(paste0("Provided 'template' (", template, ") differs from 'mesh$template' (", mesh$template, "). Using 'mesh$template' to ensure consistency."))
    }
    active_template <- mesh$template
  } else if (!is.null(template)) {
    active_template <- template
  } else {
    active_template <- "HCGSN256"
  }

  if (inherits(data, "tbl_sql") || inherits(data, "tbl_dbi")) {
    data <- dplyr::collect(data) # collect data for DB table
  }

  sensor_select <- unique(data$sensor)

  if (is.null(coords)) {
    coords_full <- switch(active_template,
                          "HCGSN256" = diegr::HCGSN256,
                          "biosemi128" = diegr::biosemi128,
                          "biosemi256" = diegr::biosemi256,
                          "system1005" = diegr::system1005,
                          stop(
                            "Unknown template '", template, "'. Supported templates are: ",
                            paste(c("HCGSN256", "biosemi128", "biosemi256", "system1005"), collapse = ", "),
                            "."
                          )
    )

    missing_in_template <- setdiff(sensor_select, coords_full$D2$sensor)

    if (length(missing_in_template) > 0) {
      stop(paste0(
        "Mismatch between data and template. The following sensors are present in 'data' but missing from the template '", active_template, "': ",
        paste(missing_in_template, collapse = ", ")
      ))
    }

    sensor_index <- which(coords_full$D2$sensor %in% sensor_select)
    coords <- coords_full$D2[sensor_index,]
  } else {
    stop_if_missing_cols(coords, required_cols = c("x", "y", "sensor"))

    missing_in_coords <- setdiff(sensor_select, coords$sensor)

    if (length(missing_in_coords) > 0) {
      stop(paste0(
        "Mismatch between data and coords. The following sensors are present in 'data' but missing from 'coords': ",
        paste(missing_in_coords, collapse = ", ")
      ))
    }
    coords <- coords[coords$sensor %in% sensor_select, ]
  }

  if (missing(mesh)) {
    mesh <- point_mesh(dimension = 2, template = active_template,
                       sensor_select = sensor_select)
  }

  if (control_D2(mesh)) {
    mesh_mat <- mesh$D2
    tri <- make_triangulation(mesh_mat)
  }

  coords_df <- data.frame(x = coords[["x"]], y = coords[["y"]])

  sensor_order <- as.factor(coords$sensor) # reorder data according to sensor
  data_order <- data |>
    dplyr::mutate(sensor = factor(.data$sensor, levels = sensor_order)) |>
    dplyr::arrange(.data$sensor)

  y_hat <- IM(coords_df, data_order[[amplitude]], mesh_mat)$Y_hat
  ycp_IM <- y_hat[1:dim(mesh_mat)[1]]
  interp_data <- data.frame(x = mesh_mat[,1], y = mesh_mat[,2], ycp_IM = ycp_IM)

  if (is.null(col_scale)) {
    if (is.null(col_range)) {
      col_range <- range(interp_data$ycp_IM)
    }

    col_scale <- create_scale(col_range)
  }

  col_scale_plotly <- make_plotly_scale(col_scale)


  fig <- plotly::plot_ly() |>
    add_trace(
      type = "mesh3d",
      opacity = 0.85,

      x = interp_data$x,
      y = interp_data$y,
      z = interp_data$ycp_IM,

      i = tri[,1] - 1,
      j = tri[,2] - 1,
      k = tri[,3] - 1,

      intensity = interp_data$ycp_IM,
      colorscale = col_scale_plotly,
      cmin = min(col_scale$breaks),
      cmax = max(col_scale$breaks)
    )

  if (show_sensors == TRUE) {
    fig <- fig |>
      plotly::add_trace(
        type = "scatter3d",
        mode = "markers",

        x = coords$x,
        y = coords$y,
        z = data_order[[amplitude]],

        marker = list(
          size = 2,
          color = "black"
        )
      )
  }

  data_meta <- attr(data, "diegr_metadata")
  mesh_meta <- attr(mesh, "diegr_metadata")
  scale_meta <- attr(col_scale, "diegr_metadata")

  if (is.null(data_meta)) data_meta <- list(history = list())
  if (is.null(mesh_meta)) mesh_meta <- list(mesh_parameters = list())
  if (is.null(scale_meta)) scale_meta <- list(scale_parameters = list())

  plot_step <- list(
    step = "interactive_surfaceplot",
    timestamp = Sys.time(),
    params = list(
      amplitude_column = amplitude,
      template = active_template
    )
  )

  plot_metadata <- list(
    package_version = tryCatch(as.character(utils::packageVersion("diegr")), error = function(e) "unknown"),
    data_history = data_meta$history,
    mesh_info = mesh_meta$mesh_parameters,
    scale_info = scale_meta$scale_parameters,
    plot_info = plot_step
  )

  attr(fig, "diegr_metadata") <- plot_metadata

  return(fig)
}


#' Create plotly-compatible color scale
#'
#' @description
#' Internal helper function to convert a standard custom color scale (e.g., from \code{create_scale})
#' into the nested list structure required by the \code{plotly} package for surface rendering.
#'
#' @param col_scale A list containing \code{breaks} and \code{colors} vectors.
#'
#' @return A list of lists mapping values 0-1 to their corresponding colors.
#'
#' @noRd
make_plotly_scale <- function(col_scale) {

  vals <- (col_scale$breaks - min(col_scale$breaks)) / diff(range(col_scale$breaks))

  out <- list()

  for (i in seq_along(col_scale$colors)) {

    out[[2*i - 1]] <- list(vals[i],   col_scale$colors[i])
    out[[2*i    ]] <- list(vals[i + 1], col_scale$colors[i])
  }

  out
}


#' Interactive 3D surface plot of EEG signal over time
#'
#' @description
#' Creates an interactive 3D surface plot displaying the EEG signal amplitude across different sensors and time points.
#' The x-axis represents time, the y-axis represents the sensors, and the z-axis (along with surface color) represents the signal amplitude.
#'
#' @param data A data frame, tibble, or a database table containing the EEG data. Required columns are: \code{sensor}, \code{time}, and the column with the EEG amplitude specified in the argument \code{amplitude}.
#' @param amplitude A character string specifying the name of the column from the input data containing EEG amplitude values.
#' @param sensor_ticks A character vector specifying which sensor labels should be displayed on the y-axis. If \code{NULL}, no specific labels are forced, but mismatched names will throw an error if provided.
#' @param col_range A vector with minimum and maximum value of the amplitude used in the colour palette for plotting. If not defined, the range of interpolated signal is used.
#' @param col_scale Optionally, a colour scale to be utilised for plotting. If not defined, it is computed from `col_range`.
#'
#' @return A \code{plotly} widget object containing the interactive 3D surface plot.
#'
#' Additionally, the returned object carries a `"diegr_metadata"` attribute with metadata.
#'
#' @seealso \code{\link{interactive_surfaceplot}}, \code{\link{plot_topo_mean}}
#'
#' @export
#'
#' @importFrom stats xtabs as.formula
#' @importFrom plotly plot_ly add_surface layout
#' @importFrom dplyr collect
#'
#' @examples
#' \donttest{
#' # Prepare data: Mean across epochs 1:13 for Subject 2
#' edata <- pick_data(epochdata, subject_rg = 2, epoch_rg = 1:13)
#' data_base <- baseline_correction(edata, baseline_range = 1:9)
#' data_mean <- compute_mean(data_base, amplitude = "signal_base", type = "point", domain = "time")
#'
#' # Selected sensors to display on the axis
#' selected_sensors <- c("E1", "E21", "E41", "E61", "E81", "E101",
#'  "E121", "E141", "E161", "E181", "E201", "E221")
#'
#' # Render the interactive plot
#' interactive_surfaceplot_curves(data_mean, amplitude = "average", sensor_ticks = selected_sensors)
#' }

interactive_surfaceplot_curves <- function(data,
                                           amplitude,
                                           sensor_ticks = NULL,
                                           col_range = NULL,
                                           col_scale = NULL
                                           ){
  if (nrow(data) == 0) {
    stop("Input data is empty.")
  }

  stop_if_missing_cols(data, required_cols = c(amplitude, "sensor", "time"))

  if (any(is.na(data[[amplitude]]))) {
    stop("There are NA's in amplitude column.")
  }

  if (inherits(data, "tbl_sql") || inherits(data, "tbl_dbi")) {
    data <- dplyr::collect(data) # collect data for DB table
  }

  if (!all(unique(sensor_ticks) %in% data$sensor)) {
    stop("Mismatch between sensors in data and sensor ticks.")
  }

  if (is.null(col_scale)) {
    if (is.null(col_range)) {
      col_range <- range(data[[amplitude]], na.rm = TRUE)
    }

    col_scale <- create_scale(col_range)
  }

  col_scale_plotly <- make_plotly_scale(col_scale)

  # create data structure for plotly
  data$sensor <- factor(data$sensor, levels = unique(data$sensor))
  form <- stats::as.formula(paste(amplitude, "~ sensor + time"))
  mat <- stats::xtabs(form, data = data)
  times <- as.numeric(colnames(mat))
  sensors <- rownames(mat)

  fig <- plotly::plot_ly(z = ~as.matrix(mat))
  fig <- fig |>
    plotly::add_surface(
      colorscale = col_scale_plotly,
      cmin = min(col_scale$breaks),
      cmax = max(col_scale$breaks),
      colorbar = list(title = "Amplitude (\u00b5V)")
    )

  selected_tickvals <- which(sensors %in% sensor_ticks)
  selected_ticktext <- sensors[selected_tickvals]

  fig <- fig |>
    plotly::layout(
    scene = list(
      yaxis = list(
        title = "Sensors",
        tickmode = "array",
        tickvals = selected_tickvals,
        ticktext = selected_ticktext
      ),
      xaxis = list(
        title = "Time (ms)",
        autorange = TRUE
      ),
      zaxis = list(
        title = "Amplitude (\u00b5V)"
      )
    )
  )

  data_meta <- attr(data, "diegr_metadata")
  scale_meta <- attr(col_scale, "diegr_metadata")

  if (is.null(data_meta)) data_meta <- list(history = list())
  if (is.null(scale_meta)) scale_meta <- list(scale_parameters = list())

  plot_step <- list(
    step = "interactive_surfaceplot_curves",
    timestamp = Sys.time(),
    params = list(
      amplitude_column = amplitude
    )
  )

  plot_metadata <- list(
    package_version = tryCatch(as.character(utils::packageVersion("diegr")), error = function(e) "unknown"),
    data_history = data_meta$history,
    scale_info = scale_meta$scale_parameters,
    plot_info = plot_step
  )

  attr(fig, "diegr_metadata") <- plot_metadata

  return(fig)
}
