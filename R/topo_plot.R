#' Plot topographic map of EEG signal
#'
#' @description
#' Plot a topographic circle or polygon map of the EEG signal amplitude using topographic color scale. The thin-plate interpolation spline model (IM) is used for signal interpolation between the sensor locations.
#' The output in the form of a ggplot object allows to easily edit the result image properties.
#'
#'
#' @param signal A vector with signal to plot.
#' @param mesh A \code{"mesh"} object, data frame or matrix with x and y coordinates of a point mesh used for computing IM model. If not defined, the point mesh with default settings from \code{\link{point_mesh()}} function is used.
#' @param coords Sensor coordinates as a tibble or data frame with named x and y columns. If not defined, the HCGSN256 template is used.
#' @param col.range A vector with minimum and maximum value of the amplitude used in the color palette for plotting. If not defined, the range of input signal is used.
#' @param col.scale A color scale which should be used for plotting. If not defined, it is computed from col.range.
#' @param contour Logical. Indicates, whether contours should be plotted in the graph. Default value is \code{FALSE}.
#' @param legend Logical. Indicates, whether legend should be displayed beside the graph. Default value is \code{TRUE}.
#'
#' @details
#' Be careful when choosing the argument \code{col.range}. If the input \code{signal} contains values outside the chosen range, this will cause "holes" in the resulting plot.
#' To compare results for different subjects or conditions, set the same values of \code{col.range} and \code{col.scale} arguments in all cases.
#' The default used scale is based on topographical colors with zero value always at the border of blue and green shades.
#'
#' @return A plot.
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom grDevices hsv
#' @importFrom scales rescale
#'
#' @examples
#' data("HCGSN256")
#' data("epochdata")
#' # Plot average topographic map of signal for subject 1 from the time point 1 (the time of the stimulus)
#' # the outliers (epoch 14 and 15) are extracted before computing average
#'
#' # a) preparing data
#' s1 <- epochdata |>
#' dplyr::filter(time == 1 & subject == 1 & !epoch %in% c(14,15)) |>
#' dplyr::select(signal, sensor, epoch) |>
#' dplyr::group_by(sensor) |>
#' dplyr::mutate(average = mean(signal, na.rm = TRUE))
#' s1 <- s1$average[1:204]
#'
#' # b) plotting the topographic circle map with contours and legend
#' topo_plot(signal = s1, col.range = c(-40, 40))
#'
topo_plot <- function(signal, mesh, coords = NULL,
                      col.range = NULL, col.scale = NULL, contour = FALSE, legend = TRUE) {
  ## zamyslet se nad zjednodusenim ohledne n a r, slo by to automaticky vytahnout z mesh > uprava vystupu
  ## takto by bylo nutne pocitat mesh na kazde vykresleni, coz nechceme

  if (!(is.logical(contour))) {
    stop("Argument 'contour' has to be logical.")
  }

  if (!(is.logical(legend))) {
    stop("Argument 'legend' has to be logical.")
  }

  if (is.null(col.range)) {
    col.range <- range(data)
  }
  if (is.null(col.scale)) {
    col.scale <- create_scale(col.range)
  }
  if (is.null(coords)) {
    coords <- HCGSN256$D2
  }

  required_cols <- c("x", "y")
  missing_cols <- setdiff(required_cols, colnames(coords))

  if (length(missing_cols) > 0) {
    stop(paste("The following required columns in 'coords' are missing:",
               paste(missing_cols, collapse = ", ")))
  }

  if (length(coords$x) != length(signal)) {
    stop("Arguments 'signal' and 'coords' must be the same length.")
  }

  if (missing(mesh)) {
    mesh <- point_mesh(dim = 2, template = "HCGSN256")
  }

  if (inherits(mesh, "mesh")) {
    mesh.mat <- mesh$D2
  } else {
    mesh.mat <- mesh[,1:2]
  }

  M <- max(mesh.mat[,2], na.rm = TRUE)

  beta.hat <- IM(coords, signal)
  X.Pcp <- XP_IM(coords, mesh.mat)
  y.Pcp <- X.Pcp %*% beta.hat
  ycp.IM2 <- y.Pcp[1:dim(mesh.mat)[1]]
  interp_data <- data.frame(x = mesh.mat[,1], y = mesh.mat[,2], ycp.IM2 = ycp.IM2)


  g <- ggplot(interp_data, aes(x = x, y = y)) +
    geom_raster(aes(fill = ycp.IM2), interpolate = TRUE) +
    scale_fill_gradientn(
      colors = col.scale$colors,
      breaks = col.scale$breaks,
      limits = range(col.scale$breaks),
      labels = col.scale$breaks,
      values = scales::rescale(col.scale$breaks)
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
   g <- g + geom_contour(aes(z = ycp.IM2), color = "gray", breaks = col.scale$breaks)
  }

  g <- g +
    geom_point(data = coords, aes(x = x, y = y), color = "black", cex = 0.7)

  g +
    annotate("segment", x = 0, y = 1.07 * M, xend = -0.08 * M, yend = 1.01 * M, col = "gray40") +
    annotate("segment", x = 0, y = 1.07 * M, xend = 0.08 * M, yend = 1.01 * M, col = "gray40")

}


IM <- function(X, y) {
  ## interpolating using spline
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  if (!is.numeric(y)) {
    y <- as.numeric(y)
  }

  d <- ncol(X)
  X.P <- XP_IM(X)
  beta.hat <- solve(X.P) %*% c(y, rep(0, d + 1))
  return(beta.hat)
}




