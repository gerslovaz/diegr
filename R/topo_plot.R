#' Plot topographic map of EEG signal
#'
#' @description
#' Doplnit podrobnosti o skale atd.
#'
#'
#' @param data A vector with signal to plot.
#' @param mesh A data frame with x and y coordinates of a point mesh used for computing IM model. If not defined, the point mesh with default settings from point_mesh() function is used.
#' @param coords Sensor coordinates. If not defined, the HCGSN256 template is used.
#' @param col.range A vector ... See details.
#' @param col.scale A color scale which should be used for plotting. If not defined, it is computed from col.range.
#'
#' @return A plot.
#' @export
#'
#' @examples
#' # Plot average topographic map of signal for subject 1 from the time point 251
#' # the outliers - epoch 47 and 48 are extracted before computing average
#' data <- epochdata |>
#' dplyr::filter(time == 251 & subject == 1 & !epoch %in% c(47,48)) |>
#' dplyr::select(signal, electrode, epoch) |>
#' dplyr::group_by(electrode) |>
#' mutate(average = mean(signal, na.rm = TRUE))
#'
#' data <- data$average[1:204]
#' topo_plot(data = data, col.range = c(-40, 40))
#'
topo_plot <- function(data, mesh, coords = HCGSN256$D2,
                      col.range = NULL, col.scale = NULL) {
  ## zamyslet se nad zjednodusenim ohledne n a r, slo by to automaticky vytahnout z mesh > uprava vystupu
  ## takto by bylo nutne pocitat mesh na kazde vykresleni, coz nechceme

  if (is.null(col.range)) {
    col.range <- range(data)
  }
  if (is.null(col.scale)) {
    col.scale <- create_scale(col.range)
  }

  if (missing(mesh)) {
    mesh <- point_mesh(dim = 2)
  }

  mesh.mat <- as.matrix(na.omit(mesh))
  mx <- sort(na.omit(unique(mesh$x)))
  my <- sort(na.omit(unique(mesh$y)))
  N <- max(length(mx), length(my))

  beta.hat <- IM(coords, data)
  X.Pcp <- XP_IM(coords, mesh.mat)
  y.Pcp <- X.Pcp %*% beta.hat
  ycp.IM2 <- mesh$x
  ycp.IM2[-which(is.na(ycp.IM2))] <- y.Pcp[1:dim(mesh.mat)[1]]
  ycp.IM2 <- matrix(ycp.IM2, nrow = N)

  plot_point_mesh(mesh, sensors = FALSE, type = "n")

  image(mx, my, ycp.IM2,
    xlab = "", ylab = "", xaxt = "n", yaxt = "n",
    col = col.scale$colors, breaks = col.scale$breaks,
    bty = "n", add = T
  )
  lvl <- col.scale$breaks[seq(1, length(col.scale$breaks), 2)]
  contour(mx, my, ycp.IM2, add = T, col = "gray", levels = lvl)
}

IM <- function(X, y) {
  ## interpolating using spline
  y <- as.numeric(y)
  d <- ncol(X)
  X.P <- XP_IM(X)
  beta.hat <- solve(X.P) %*% c(y, rep(0, d + 1))
  return(beta.hat)
}

TopoColorsNeg <- function(n, alpha = 1) {
  ## create negative part of color palette
  if ((n <- as.integer(n[1L])) > 0) {
    hsv(h = seq.int(from = 43 / 60, to = 31 / 60, length.out = n), alpha = alpha)
  } else {
    character()
  }
}

TopoColorsPos <- function(n, alpha = 1) {
  ## create positive part of color palette
  if ((n <- as.integer(n[1L])) > 0) {
    j <- n %/% 3
    k <- n %/% 3
    i <- n - j - k
    c(
      if (i > 0) hsv(h = seq.int(from = 23 / 60, to = 11 / 60, length.out = i), alpha = alpha),
      if (j > 0 & k > 0) hsv(h = seq.int(from = 10 / 60, to = 0, length.out = (j + k)), alpha = alpha, s = seq.int(from = 1, to = 0.95, length.out = (j + k)), v = seq.int(from = 1, to = 0.6, length.out = (j + k)))
    )
    # hsv(h = seq.int(from = 10/60, to = 6/60, length.out = (j + k)), alpha = alpha, s = seq.int(from = 1, to = 0.3, length.out = (j + k)), v = 1))
  } else {
    character()
  }
}

TopoColorsPos2 <- function(n, alpha = 1) {
  ## jiny pristup k poz. skale, jasnejsi cervena na konci
  if ((n <- as.integer(n[1L])) > 0) {
    hues <- seq(1 / 3, 0, length.out = n)
    hsv(hues, 1, 1)
  } else {
    character()
  }
}

create_scale <- function(col.range) {
  probs <- seq(0, 1, by = 0.02)
  k_probs <- length(probs)
  breaks <- unique(pretty(col.range, k_probs))
  breaks[length(breaks)] <- breaks[length(breaks)] + 1
  breaks_negative <- which(breaks <= 0)
  breaks_positive <- which(breaks >= 0)
  k_negbreaks <- length(breaks_negative)
  k_posbreaks <- length(breaks_positive)
  scale_color <- c(TopoColorsNeg(k_negbreaks - 1), TopoColorsPos(k_posbreaks - 1))
  return(list(colors = scale_color, breaks = breaks))
}

cut_scale <- function(signal, col.scale) { # asi bude stacit tak easy

  clr <- cut(signal, col.scale$breaks, include.lowest = TRUE)
  colour <- col.scale$colors[clr]
  return(colour)
}


PlotScale <- function(col.scale) {
  breaks <- col.scale$breaks
  col.range <- range(col.scale$breaks)

  windows(width = 1.2, height = 5)
  par(mar = c(1, 1, 1.2, 4))
  plot(0, 1,
    type = "n", xlim = c(0, 1), ylim = col.range, xaxs = "i", yaxs = "i",
    xlab = "", ylab = "", bty = "n", axes = FALSE
  )
  rect(0, breaks[-length(breaks)], 1, breaks[-1L],
    col = col.scale$colors
  )
  axis(4, breaks, cex.axis = 0.8, las = 1)
}
