#' Create colour scale used in topographic figures
#'
#' @param col_range A numeric vector with required range of the variable to be plotted in the colour scale.
#' @param k A number from interval (0,1) indicating a sequence step for the colour palette. The smaller number, the finer division of the data range interval.
#'
#' @details
#' The palette is created according to topographical colours: negative values correspond to shades of blue and purple and positive values to shades of green, yellow and red. The zero value of the variable is always at the border of blue and green shades.
#' To compare results for different subjects or conditions, set the same \code{col_range} for all cases.
#'
#' The parameter \code{k} is set by default with respect to the range of \code{col_range} as follows: \code{k = 0.1} for range \eqn{\leq 30}, \code{k = 0.03} for range \eqn{\geq 70} and \code{k = 0.04} otherwise.
#'
#' @return A list with two components:
#' \item{colors}{A vector with hexadecimal codes of palette colours.}
#' \item{breaks}{A vector with breaks for cutting the data range.}
#' @export
#'
#' @examples
#' # Create scale on interval (-10,10) with default step number
#' create_scale(col_range = c(-10,10))
#'
#' # Create scale on interval c(-5,10) with small k (finer division)
#' create_scale(col_range = c(-5, 10), k = 0.02)
create_scale <- function(col_range, k = NULL) {

  if (length(col_range) != 2 | !is.numeric(col_range)) {
    stop("The argument 'col_range' is supposed to be a numeric vector with two terms.")
  }

  if (is.null(k)) {
    maxmin <- max(col_range) - min(col_range)
    if (maxmin <= 30) {
      k <- 0.1
    } else if (maxmin >= 70) {
      k <- 0.03
    } else {
      k <- 0.04
    }
  }

 if (k > 1 | k <= 0) {
   k <- 0.04
   warning("The argument 'k' is supposed to be from the (0,1) interval. The user input value was replaced by 0.04.")
  }

  probs <- seq(0, 1, by = { k })
  k_probs <- length(probs)
  breaks <- pretty(col_range, k_probs)
  breaks_negative <- which(breaks <= 0)
  breaks_positive <- which(breaks >= 0)
  k_negbreaks <- length(breaks_negative)
  k_posbreaks <- length(breaks_positive)
  scale_color <- c(ColorsNeg(k_negbreaks - 1), ColorsPos(k_posbreaks - 1))

  return(list(colors = scale_color, breaks = breaks))
}

ColorsNeg <- function(n, alpha = 1) {
  ## create negative part of colour palette
  if (n > 0) {
    hsv(h = seq.int(from = 43 / 60, to = 31 / 60, length.out = n), alpha = alpha)
  } else {
    character()
  }
}

ColorsPos <- function(n, alpha = 1) {
  ## create positive part of colour palette
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



cut_scale <- function(signal, col_scale) {

  clr <- cut(signal, col_scale$breaks, include.lowest = TRUE)
  colour <- col_scale$colors[clr]
  return(colour)
}


cut_signal <- function(signal, mesh, coords, col_scale) {
  #if (missing(coords)) {
  #  coords <- HCGSN256$D2
  #}

  #if (ncol(mesh) > 2) {
  #  mesh <- mesh[,1:2]
  #}

  y_Pcp <- IM(coords, signal, mesh)$Y_hat
  ycp_IM <- y_Pcp[1:length(mesh[,1])]

  y_cut <- cut(ycp_IM, breaks = col_scale$breaks, include.lowest = TRUE)
  y_color <- col_scale$colors[y_cut]
  return(y_color)
}
