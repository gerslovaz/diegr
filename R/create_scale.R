#' Create color scale used in topographic figures
#'
#' @param col.range A numeric vector with required range of the variable to be plotted in the color scale.
#' @param k A number from interval (0,1) indicating a sequence step for the color palette. The smaller number, the finer division of the data range interval.
#'
#' @details
#' The palette is created according to topographical colors: negative values correspond to shades of blue and purple and positive values to shades of green, yellow and red. The zero value of the variable is always at the border of blue and green shades.
#' To compare results for different subjects or conditions, set the same \code{col.range} for all cases.
#'
#' @return A list with two components:
#' \item{colors}{A vector with hexadecimal codes of palette colors.}
#' \item{breaks}{A vector with breaks for cutting the data range.}
#' @export
#'
#' @examples
#' # Creating scale on interval (-10,10) with default step number
#' create_scale(col.range = c(-10,10))
create_scale <- function(col.range, k = 0.02) {

  if (length(col.range) != 2 | !is.numeric(col.range)) {
    stop("The argument 'col.range' is supposed to be a numeric vector with two terms.")
  }

  if (k > 1 | k <= 0) {
    k <- 0.02
    warning("The argument 'k' is supposed to be from the (0,1) interval. The default value 0.02 was used instead.")
  }

  probs <- seq(0, 1, by = { k })
  k_probs <- length(probs)
  breaks <- pretty(col.range, k_probs)
  breaks_negative <- which(breaks <= 0)
  breaks_positive <- which(breaks >= 0)
  k_negbreaks <- length(breaks_negative)
  k_posbreaks <- length(breaks_positive)
  scale_color <- c(ColorsNeg(k_negbreaks - 1), ColorsPos(k_posbreaks - 1))

  return(list(colors = scale_color, breaks = breaks))
}

ColorsNeg <- function(n, alpha = 1) {
  ## create negative part of color palette
  if (n > 0) {
    hsv(h = seq.int(from = 43 / 60, to = 31 / 60, length.out = n), alpha = alpha)
  } else {
    character()
  }
}

ColorsPos <- function(n, alpha = 1) {
  ## create positive part of color palette
  if (n > 0) {
    j <- n %/% 3
    i <- n - 2 * j
    c(
      if (i > 0) hsv(h = seq.int(from = 23 / 60, to = 11 / 60, length.out = i), alpha = alpha),
      if (j > 0) hsv(h = seq.int(from = 10 / 60, to = 0, length.out = (2 * j)), alpha = alpha, s = seq.int(from = 1, to = 0.95, length.out = (2 * j)), v = seq.int(from = 1, to = 0.6, length.out = (2 * j)))
    )
  } else {
    character()
  }
}



cut_scale <- function(signal, col.scale) {

  clr <- cut(signal, col.scale$breaks, include.lowest = TRUE)
  colour <- col.scale$colors[clr]
  return(colour)
}


cut_signal <- function(signal, mesh, coords, col.scale) {
  #if (missing(coords)) {
  #  coords <- HCGSN256$D2
  #}

  #if (ncol(mesh) > 2) {
  #  mesh <- mesh[,1:2]
  #}

  y.Pcp <- IM(coords, signal, mesh)$Y.hat
  ycp.IM <- y.Pcp[1:length(mesh[,1])]

  y.cut <- cut(ycp.IM, breaks = col.scale$breaks, include.lowest = TRUE)
  y.color <- col.scale$colors[y.cut]
  return(y.color)
}
