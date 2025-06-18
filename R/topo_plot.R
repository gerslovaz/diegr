#' Plot topographic map of EEG signal
#'
#' @description
#' Plot a topographic circle or polygon map of the EEG signal amplitude using topographic colour scale. The thin-plate spline interpolation model \eqn{\text{IM:}\; \mathbb{R}^2 \rightarrow \mathbb{R}} is used for signal interpolation between the sensor locations.
#' The output in the form of a ggplot object allows to easily edit the result image properties.
#'
#'
#' @param data A data frame, tibble or a database table with input data to plot with at least two columns: \code{sensor} with sensor labels and the column with the EEG amplitude specified in the argument \code{amplitude}.
#' @param amplitude A character specifying the name of the column from input data with an EEG amplitude values.
#' @param mesh A \code{"mesh"} object, data frame or matrix with x and y coordinates of a point mesh used for computing IM model. If not defined, the point mesh with default settings from \code{\link{point_mesh}} function is used.
#' @param coords Sensor coordinates as a tibble or data frame with named \code{x}, \code{y} and \code{sensor} columns. The \code{sensor} labels must match the labels in sensor column in \code{data}. If not defined, the HCGSN256 template is used.
#' @param template The kind of sensor template montage used. Currently the only available option is \code{"HCGSN256"} denoting the 256-channel HydroCel Geodesic Sensor Net v.1.0, which is also a default setting.
#' @param col_range A vector with minimum and maximum value of the amplitude used in the colour palette for plotting. If not defined, the range of input signal is used.
#' @param col_scale Optionally, a colour scale to be utilised for plotting. If not defined, it is computed from \code{col_range}.
#' @param contour Logical. Indicates, whether contours should be plotted in the graph. Default value is \code{FALSE}.
#' @param legend Logical. Indicates, whether legend should be displayed beside the graph. Default value is \code{TRUE}.
#' @param names A logical value indicating whether the sensor names should also be plotted (default value is \code{FALSE}).
#' @param names_vec Optionally, a vector with sensor names. The argument is required when using own \code{coords} and setting \code{names = TRUE}.

#'
#' @details
#' Be careful when choosing the argument \code{col_range}. If the amplitude in input data contains values outside the chosen range, this will cause "holes" in the resulting plot.
#' To compare results for different subjects or conditions, set the same values of \code{col_range} and \code{col_scale} arguments in all cases.
#' The default used scale is based on topographical colours with zero value always at the border of blue and green shades.
#'
#' @return A plot.
#' @export
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
#' data_base <- baseline_correction(edata, base_int = 1:10)
#' # a3) average computing
#' data_mean <- compute_mean(data_base, amplitude = "signal_base", subject = 2, time = 10,
#'  type = "point")
#'
#'
#' # b) plotting the topographic circle map with contours and legend
#' # interval (-30,15) is selected in consideration of the signal progress
#' topo_plot(data = data_mean, amplitude = "average", template = "HCGSN256",
#' col_range = c(-30, 15), contour = TRUE)
#'
#' # c) plotting the same map without contours but with sensor labels
#' topo_plot(data = data_mean, amplitude = "average", template = "HCGSN256",
#'  col_range = c(-30, 15), names = TRUE)
#'
topo_plot <- function(data, amplitude, mesh, coords = NULL, template = NULL,
                      col_range = NULL, col_scale = NULL, contour = FALSE, legend = TRUE,
                      names = FALSE, names_vec = NULL) {

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

  if (!(is.logical(names))) {
    stop("Argument 'names' has to be logical.")
  }

  if (is.null(col_range)) {
    col_range <- 1.1 * range(data[[amp_name]])
  }
  if (is.null(col_scale)) {
    col_scale <- create_scale(col_range)
  }

  if (!is.null(template)) {
    coords <- switch(template,
                          "HCGSN256" = diegr::HCGSN256$D2)
    if (is.null(coords)) {
      stop("Unknown template.")
    }
  }

  if (is.null(template) && is.null(coords)) {
    coords <- diegr::HCGSN256$D2
  }


  #if (names == TRUE && is.null(names_vec)) {
  #  if (!is.null(coords)) {
  #    stop("With using own coordinates please define the 'names_vec' or set 'names' to FALSE.")
  #  } else {names_vec <- coords$sensor}
  #   }
  #if (is.null(coords)) {
  #  coords <- diegr::HCGSN256$D2
  #}


  required_cols <- c("x", "y", "sensor")
  missing_cols <- setdiff(required_cols, colnames(coords))

  if (length(missing_cols) > 0) {
    stop(paste("The following required columns in 'coords' are missing:",
               paste(missing_cols, collapse = ", ")))
  }

  if (names == TRUE && is.null(names_vec)) {
    names_vec <- coords$sensor
  }

  #if (length(coords[["x"]]) != length(signal)) { ## tohle asi pri novem pristupu match pres sensor nebude treba
  #  stop("Arguments 'signal' and 'coords' must be the same length.")
  #}

  if (missing(mesh)) {
    mesh <- point_mesh(dim = 2, template = "HCGSN256")
  }

  if (inherits(mesh, "mesh")) {
    mesh_mat <- mesh$D2
  } else {
    mesh_mat <- mesh[,1:2]
  }

  M <- max(max(mesh_mat[,2], na.rm = TRUE), max(coords[["y"]]))
  x0 <- mean(mesh_mat[,1], na.rm = TRUE)

  coords_df <- data.frame(x = coords[["x"]], y = coords[["y"]])

  sensor_order <- as.factor(coords$sensor) # reorder data according to sensor
  data_order <- data |>
      mutate(sensor = factor(.data$sensor, levels = sensor_order)) |>
      arrange(.data$sensor)

  y_hat <- IM(coords_df, data_order[[amp_name]], mesh_mat)$Y_hat
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
   g <- g + geom_contour(aes(z = ycp_IM), color = "gray", breaks = col_scale$breaks)
  }

  g <- g +
    geom_point(data = coords, aes(x = .data$x, y = .data$y), color = "black", cex = 0.7)

  if (names == TRUE) {
    g <- g + geom_text(data = coords_df, aes(label = names_vec), size = 2, vjust = -0.9)
  }

  g +
    annotate("segment", x = x0, y = M + 0.07 * abs(M), xend = x0 - 0.08 * M, yend = M + 0.01 * abs(M), col = "gray40") +
    annotate("segment", x = x0, y = M + 0.07 * abs(M), xend = x0 + 0.08 * M, yend = M + 0.01 * abs(M), col = "gray40")

}


IM <- function(X, Y, Xcp = X) {
  ## interpolating using spline
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  if (!is.matrix(Xcp)) {
    Xcp <- as.matrix(Xcp)
  }
  if (!is.matrix(Y)) {
    Y <- as.matrix(Y)
  }

  d1 <- ncol(X)
  d2 <- ncol(Y)

  X_P <- XP_IM(X)
  Y_P <- rbind(Y, matrix(0, d1 + 1, d2))
  beta_hat <- solve(X_P) %*% Y_P

  if (identical(X, Xcp)) {
    y_Pcp <- X_P %*% beta_hat
  } else {
    X_Pcp <- XP_IM(X, Xcp)
    y_Pcp <- X_Pcp %*% beta_hat
  }

  return(list(Y_hat = y_Pcp, beta_hat = beta_hat))
}


XP_PRM <- function(X, lambda) {
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }

  k <-  dim(X)[1]
  dv <- dim(X)[2]
  kk <- k + dv + 1

  S <- spline_matrix(X)
  S_P <- matrix(0, kk, kk)
  S_P[(dv + 2):kk, (dv + 2):kk] <- S

  decomp <- eigen(S, symmetric = T)
  U <- decomp$vectors
  eigval <- decomp$values
  eigval[eigval < 0] <- 0

  R <- matrix(0, kk, kk)
  R[(dv + 2):kk, (dv + 2):kk] <- U %*% diag(sqrt(eigval)) %*% t(U)

  X_P <- matrix(0, nrow = k + kk, ncol = kk)
  X_P[1:k, ] <- cbind(rep(1, k), X, S)
  X_P[(k + 1):(k + kk),] <- sqrt(lambda) * R
  return(X_P)
}

PRM <- function(X, Y, lambda) {

  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  if (!is.matrix(Y)) {
    Y <- as.matrix(Y)
  }

  k <-  dim(X)[1]
  dv <- dim(X)[2]
  do <- dim(as.matrix(Y))[2]

  XP <- XP_PRM(X, lambda)
  YP <- matrix(0, nrow = 2 * k + dv + 1, ncol = do)
  YP[1:k,] <- Y

  model <- lm(YP ~ XP - 1)
  hatY <- matrix(model$fitted.values, nrow = 2*k + dv + 1, ncol = do)
  hatY <- hatY[1:k,]
  Beta <- model$coefficients
  DiagHat <- influence(model)$hat[1:k]
  return(list(Y_hat = hatY, beta_hat = Beta, diag_hat = DiagHat))
}


GCV_score <- function(X, Y, lambda){
  model <- PRM(X, Y, lambda)
  k <- dim(as.matrix(X))[1]
  GCV <- k * sum((model$Y_hat - Y)^2) / (sum(rep(1,k) - model$diag_hat))^2
  return(GCV)
}

DCV_score <- function(X, Y, lambda){
  k <- dim(as.matrix(X))[1]
  model <- PRM(X, Y, lambda)
  DCV <- k * sum((model$Y_hat - Y)^2) / (k - 1.5 * sum(model$diag_hat))^2
  return(DCV)
}
