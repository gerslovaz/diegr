#' Plot topographic map of EEG signal
#'
#' @description
#' Plot a topographic circle or polygon map of the EEG signal amplitude using topographic colour scale. The thin-plate spline interpolation model \eqn{\text{IM:}\; \mathbb{R}^2 \rightarrow \mathbb{R}} is used for signal interpolation between the sensor locations.
#' The output in the form of a ggplot object allows to easily edit the result image properties.
#'
#'
#' @param signal A vector with signal to plot.
#' @param mesh A \code{"mesh"} object, data frame or matrix with x and y coordinates of a point mesh used for computing IM model. If not defined, the point mesh with default settings from \code{\link{point_mesh}} function is used.
#' @param coords Sensor coordinates as a tibble or data frame with named \code{x} and \code{y} columns. If not defined, the HCGSN256 template is used.
#' @param col.range A vector with minimum and maximum value of the amplitude used in the colour palette for plotting. If not defined, the range of input signal is used.
#' @param col.scale Optionally, a colour scale to be utilised for plotting. If not defined, it is computed from \code{col.range}.
#' @param contour Logical. Indicates, whether contours should be plotted in the graph. Default value is \code{FALSE}.
#' @param legend Logical. Indicates, whether legend should be displayed beside the graph. Default value is \code{TRUE}.
#' @param names A logical value indicating whether the sensor names should also be plotted (default value is \code{FALSE}).
#' @param names.vec Optionally, a vector with sensor names. The argument is required when using own \code{coords} and setting \code{names = TRUE}.

#'
#' @details
#' Be careful when choosing the argument \code{col.range}. If the input \code{signal} contains values outside the chosen range, this will cause "holes" in the resulting plot.
#' To compare results for different subjects or conditions, set the same values of \code{col.range} and \code{col.scale} arguments in all cases.
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
#' # (the time of the stimulus)
#' # the outliers (epoch 14 and 15) are extracted before computing average
#'
#' # a) preparing data
#' s1 <- epochdata |>
#' dplyr::filter(time == 10 & subject == 2 & !epoch %in% c(14,15)) |>
#' dplyr::select(signal, sensor, epoch) |>
#' dplyr::group_by(sensor) |>
#' dplyr::mutate(average = mean(signal, na.rm = TRUE))
#' s1 <- s1$average[1:204]
#'
#' # b) plotting the topographic circle map with contours and legend
#' # interval (-30,15) is selected in consideration of the signal progress
#' topo_plot(signal = s1, col.range = c(-30, 15), contour = TRUE)
#'
#' # c) plotting the same map without contours but with sensor labels
#' topo_plot(signal = s1, col.range = c(-30, 15), names = TRUE)
#'
topo_plot <- function(signal, mesh, coords = NULL,
                      col.range = NULL, col.scale = NULL, contour = FALSE, legend = TRUE,
                      names = FALSE, names.vec = NULL) {

  if (!(is.logical(contour))) {
    stop("Argument 'contour' has to be logical.")
  }

  if (!(is.logical(legend))) {
    stop("Argument 'legend' has to be logical.")
  }

  if (!(is.logical(names))) {
    stop("Argument 'names' has to be logical.")
  }

  if (is.null(col.range)) {
    col.range <- 1.1 * range(signal)
  }
  if (is.null(col.scale)) {
    col.scale <- create_scale(col.range)
  }
  if (names == TRUE && is.null(names.vec)) {
    if (!is.null(coords)) {
      stop("With using own coordinates please define the 'names.vec' or set 'names' to FALSE.")
    } else {names.vec <- diegr::HCGSN256$sensor}
      }
  if (is.null(coords)) {
    coords <- diegr::HCGSN256$D2
  }


  required_cols <- c("x", "y")
  missing_cols <- setdiff(required_cols, colnames(coords))

  if (length(missing_cols) > 0) {
    stop(paste("The following required columns in 'coords' are missing:",
               paste(missing_cols, collapse = ", ")))
  }

  if (length(coords[["x"]]) != length(signal)) {
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

  M <- max(max(mesh.mat[,2], na.rm = TRUE), max(coords[["y"]]))
  x0 <- mean(mesh.mat[,1], na.rm = TRUE)

  if (ncol(coords) > 2) {
    coords <- data.frame(x = coords[["x"]], y = coords[["y"]])
  }

  y.hat <- IM(coords, signal, mesh.mat)$Y.hat
  ycp.IM <- y.hat[1:dim(mesh.mat)[1]]
  interp_data <- data.frame(x = mesh.mat[,1], y = mesh.mat[,2], ycp.IM = ycp.IM)


  g <- ggplot(interp_data, aes(x = .data$x, y = .data$y)) +
    geom_raster(aes(fill = ycp.IM)) +  #, interpolate = TRUE
    scale_fill_gradientn(
      colors = col.scale$colors,
      breaks = col.scale$breaks,
      limits = range(col.scale$breaks),
      labels = round(col.scale$breaks, 2),
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
   g <- g + geom_contour(aes(z = ycp.IM), color = "gray", breaks = col.scale$breaks)
  }

  g <- g +
    geom_point(data = coords, aes(x = .data$x, y = .data$y), color = "black", cex = 0.7)

  if (names == TRUE) {
    g <- g + geom_text(data = coords, aes(label = names.vec), size = 2, vjust = -0.9)
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

  X.P <- XP_IM(X)
  Y.P <- rbind(Y, matrix(0, d1 + 1, d2))
  beta.hat <- solve(X.P) %*% Y.P

  if (identical(X, Xcp)) {
    y.Pcp <- X.P %*% beta.hat
  } else {
    X.Pcp <- XP_IM(X, Xcp)
    y.Pcp <- X.Pcp %*% beta.hat
  }

  return(list(Y.hat = y.Pcp, beta.hat = beta.hat))
}


XP_PRM <- function(X, lambda) {
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }

  k <-  dim(X)[1]
  d.v <- dim(X)[2]
  kk <- k + d.v + 1

  S <- spline_matrix(X)
  S.P <- matrix(0, kk, kk)
  S.P[(d.v + 2):kk, (d.v + 2):kk] <- S

  decomp <- eigen(S, symmetric = T)
  U <- decomp$vectors
  eigval <- decomp$values
  eigval[eigval < 0] <- 0

  R <- matrix(0, kk, kk)
  R[(d.v + 2):kk, (d.v + 2):kk] <- U %*% diag(sqrt(eigval)) %*% t(U)

  X.P <- matrix(0, nrow = k + kk, ncol = kk)
  X.P[1:k, ] <- cbind(rep(1, k), X, S)
  X.P[(k + 1):(k + kk),] <- sqrt(lambda) * R
  return(X.P)
}

PRM <- function(X, Y, lambda) {

  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  if (!is.matrix(Y)) {
    Y <- as.matrix(Y)
  }

  k <-  dim(X)[1]
  d.v <- dim(X)[2]
  d.o <- dim(as.matrix(Y))[2]

  XP <- XP_PRM(X, lambda)
  YP <- matrix(0, nrow = 2 * k + d.v + 1, ncol = d.o)
  YP[1:k,] <- Y

  model <- lm(YP ~ XP - 1)
  hatY <- matrix(model$fitted.values, nrow = 2*k + d.v + 1, ncol = d.o)
  hatY <- hatY[1:k,]
  Beta <- model$coefficients
  DiagHat <- influence(model)$hat[1:k]
  return(list(Y.hat = hatY, beta.hat = Beta, diag.hat = DiagHat))
}


GCV_score <- function(X, Y, lambda){
  model <- PRM(X, Y, lambda)
  k <- dim(as.matrix(X))[1]
  GCV <- k * sum((model$Y.hat - Y)^2) / (sum(rep(1,k) - model$diag.hat))^2
  return(GCV)
}

DCV_score <- function(X, Y, lambda){
  k <- dim(as.matrix(X))[1]
  model <- PRM(X, Y, lambda)
  DCV <- k * sum((model$Y.hat - Y)^2) / (k - 1.5 * sum(model$diag.hat))^2
  return(DCV)
}


