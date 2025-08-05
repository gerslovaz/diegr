#' Plot topographic map of EEG signal
#'
#' @description
#' Plot a topographic circle or polygon map of the EEG signal amplitude using topographic colour scale. The thin-plate spline interpolation model \eqn{\text{IM:}\; \mathbb{R}^2 \rightarrow \mathbb{R}} is used for signal interpolation between the sensor locations.
#' The output in the form of a ggplot object allows to easily edit the result image properties.
#'
#'
#' @param data A data frame, tibble or a database table with input data to plot with at least two columns: \code{sensor} with sensor labels and the column with the EEG amplitude specified in the argument \code{amplitude}.
#' @param amplitude A character string naming the column with EEG amplitude values.
#' @param mesh A \code{"mesh"} object (or a named list with the same structure) containing at least \code{D2} element with x and y coordinates of a point mesh used for computing IM model. If not defined, the point mesh with default settings from \code{\link{point_mesh}} function is used.
#' @param coords Sensor coordinates as a tibble or data frame with named \code{x}, \code{y} and \code{sensor} columns. The \code{sensor} labels must match the labels in sensor column in \code{data}. If not defined, the HCGSN256 template is used.
#' @param template The kind of sensor template montage used. Currently the only available option is \code{"HCGSN256"} denoting the 256-channel HydroCel Geodesic Sensor Net v.1.0, which is also a default setting.
#' @param col_range A vector with minimum and maximum value of the amplitude used in the colour palette for plotting. If not defined, the range of input signal expanded by a padding value equal to 5% is used.
#' @param col_scale Optionally, a colour scale to be utilised for plotting. If not defined, it is computed from \code{col_range}.
#' @param contour Logical. Indicates, whether contours should be plotted in the graph. Default value is \code{FALSE}.
#' @param show_legend Logical. Indicates, whether legend should be displayed beside the graph. Default value is \code{TRUE}.
#' @param label_sensors A logical value indicating whether the sensor labels should also be plotted (default value is \code{FALSE}).
#'
#' @details
#' For more details about required mesh structure see \code{\link{point_mesh}} function. If the input \code{mesh} structure does not match this format, an error or incorrect function behavior may occur.
#'
#' Be careful when choosing the argument \code{col_range}. If the amplitude in input data contains values outside the chosen range, this will cause "holes" in the resulting plot.
#' To compare results for different subjects or conditions, set the same values of \code{col_range} and \code{col_scale} arguments in all cases.
#' The default used scale is based on topographical colours with zero value always at the border of blue and green shades.
#'
#' @return A `ggplot` object showing an interpolated topographic map of EEG amplitude.
#' @export
#'
#' @seealso \code{\link{animate_topo}}, \code{\link{point_mesh}}, \code{\link{plot_topo_mean}}
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom grDevices hsv
#' @importFrom scales rescale
#' @importFrom stats influence
#' @importFrom rlang .data
#'
#' @examples
#' # Plot average topographic map of signal for subject 2 from the time point 10
#' # (the time of the stimulus) without the outliers (epoch 14 and 15)
#'
#' # a) preparing data
#' # a1) extract required data
#' edata <- epochdata |>
#' dplyr::filter(subject == 2 & time %in% 1:10 & epoch %in% 1:13)
#' # a2) baseline correction (needed for suitable topographic map)
#' data_base <- baseline_correction(edata, baseline_range = 1:10)
#' # a3) average computing
#' data_mean <- compute_mean(data_base, amplitude = "signal_base", subject = 2, time = 10,
#'  type = "jack", group = "space")
#'
#'
#' # b) plotting the topographic map with contours and legend
#' # interval (-30,15) is selected in consideration of the signal progress
#' topo_plot(data = data_mean, amplitude = "average", template = "HCGSN256",
#' col_range = c(-30, 15), contour = TRUE)
#'
#' \dontrun{
#' # c) plotting the same map without contours but with sensor labels
#' topo_plot(data = data_mean, amplitude = "average", template = "HCGSN256",
#'  col_range = c(-30, 15), label_sensors = TRUE)
#' }
#'
topo_plot <- function(data,
                      amplitude,
                      mesh,
                      coords = NULL,
                      template = NULL,
                      col_range = NULL,
                      col_scale = NULL,
                      contour = FALSE,
                      show_legend = TRUE,
                      label_sensors = FALSE) {

  if (!amplitude %in% names(data)) {
    stop(paste0("There is no column '", amplitude, "' in the input data."))
  }

  if (!is.numeric(data[[amplitude]])) {
    stop("The amplitude column must be numeric.")
  }

  if (!(is.logical(contour))) {
    stop("Argument 'contour' has to be logical.")
  }

  if (!(is.logical(show_legend))) {
    stop("Argument 'show_legend' has to be logical.")
  }

  if (!(is.logical(label_sensors))) {
    stop("Argument 'label_sensors' has to be logical.")
  }

  if (is.null(col_range)) {
    padding <- 0.05 * diff(range(data[[amplitude]]))
    col_range <- range(data[[amplitude]]) + c(-1, 1) * padding
  }
  if (is.null(col_scale)) {
    col_scale <- create_scale(col_range)
  }

  if (!is.null(template)) {
    coords <- switch(template,
                     "HCGSN256" = diegr::HCGSN256$D2,
                     stop("Unknown template.")
    )
  }

  if (!is.null(template) && !is.null(coords)) {
    warning("Both 'template' and 'coords' were specified. Using 'template' and ignoring 'coords'.")
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
    mesh <- point_mesh(dimension = 2, template = "HCGSN256")
  }

  if (control_D2(mesh)) {
    mesh_mat <- mesh$D2
  }

  M <- max(max(mesh_mat[,2], na.rm = TRUE), max(coords[["y"]]))
  x0 <- mean(mesh_mat[,1], na.rm = TRUE)

  coords_df <- data.frame(x = coords[["x"]], y = coords[["y"]])

  if (!all(unique(coords$sensor) %in% data$sensor)) {
    stop("Mismatch between sensors in data and coords.")
  }

  sensor_order <- as.factor(coords$sensor) # reorder data according to sensor
  data_order <- data |>
      mutate(sensor = factor(.data$sensor, levels = sensor_order)) |>
      arrange(.data$sensor)

  y_hat <- IM(coords_df, data_order[[amplitude]], mesh_mat)$Y_hat
  ycp_IM <- y_hat[1:dim(mesh_mat)[1]]
  interp_data <- data.frame(x = mesh_mat[,1], y = mesh_mat[,2], ycp_IM = ycp_IM)


  g <- ggplot(interp_data, aes(x = .data$x, y = .data$y)) +
    geom_raster(aes(fill = ycp_IM)) +  #, interpolate = TRUE
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
    )


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
      theme(legend.position = "none")
    }

  if (contour == TRUE) {
   g <- g + geom_contour(aes(z = ycp_IM), color = "gray", breaks = col_scale$breaks)
  }

  g <- g +
    geom_point(data = coords, aes(x = .data$x, y = .data$y), color = "black", cex = 0.7)

  if (label_sensors == TRUE) {
    coords_df$sensor <- coords[["sensor"]]
    g <- g + geom_text(data = coords_df, aes(label = .data$sensor), size = 2, vjust = -0.9)
  }

  g +
    annotate("segment", x = x0, y = M + 0.07 * abs(M), xend = x0 - 0.08 * M, yend = M + 0.01 * abs(M), col = "gray40") +
    annotate("segment", x = x0, y = M + 0.07 * abs(M), xend = x0 + 0.08 * M, yend = M + 0.01 * abs(M), col = "gray40")

}

