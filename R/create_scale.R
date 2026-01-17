#' Create colour scale used in topographic figures
#'
#' @param col_range A numeric vector with required range of the variable to be plotted in the colour scale.
#' @param k A number from interval (0,1) indicating a sequence step for the colour palette. The smaller number, the finer division of the data range interval. See Details for more information about auto-computing if `NULL`.
#' @param type A character indicating the type of color palette to create. Available options: `"topo"` (default value) for topographical palette and `"redblue"` for red-blue palette, see Details for more information.
#'
#' @details
#' The topographical palette (`type = "topo"`) is created according to topographical colours: negative values correspond to shades of blue and purple and positive values to shades of green, yellow and red. The zero value of the variable is always at the border of blue and green shades.
#'
#' The red-blue palette (`type = "redblue"`) has negative values corresponding to shades of blue and positive values corresponding to shades of red.
#'
#' To compare results for different subjects or conditions, set the same \code{col_range} for all cases. Otherwise, the colours are assigned separately in each plot and are not consistent with each other.
#'
#' The parameter \code{k} is set by default with respect to the range of \code{col_range} as follows:
#' - \code{k = 0.1} for range \eqn{\leq 30},
#' - \code{k = 0.03} for range \eqn{\geq 70},
#' - \code{k = 0.04} otherwise.
#'
#' @return A list with two components:
#' \item{colors}{A vector with hexadecimal codes of palette colours.}
#' \item{breaks}{A vector with breaks for cutting the data range.}
#' The list is intended for use in \code{\link[ggplot2]{scale_fill_gradientn}} or similar plotting calls.
#'
#' @export
#'
#' @examples
#' # Create red-blue scale on interval (-10,10) with default step number
#' create_scale(col_range = c(-10,10), type = "redblue")
#'
#' # Create topographic scale on interval c(-5,10) with small k (finer division)
#' CStopo <- create_scale(col_range = c(-5, 10), k = 0.02)
#' # plot colours of the scale as points
#' k_col <- length(CStopo$colors)
#' plot(1:k_col, rep(1, k_col), col = CStopo$colors, pch = 16,
#'  axes = FALSE, ylab = "", xlab = "")
#'
create_scale <- function(col_range,
                          k = NULL,
                          type = c("topo", "redblue")) {
  type <- match.arg(type)

  if (!is.numeric(col_range) || length(col_range) != 2 || col_range[1] == col_range[2]) {
    stop("The argument 'col_range' must be a numeric vector of two distinct values.")
  }

  if (!is.null(k) && (k > 1 | k <= 0)) {
    stop("The argument 'k' is supposed to be from the (0,1) interval.")
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

  probs <- seq(0, 1, by = k)
  k_probs <- length(probs)
  breaks <- pretty(col_range, k_probs)

  if (0 >= min(col_range) && 0 <= max(col_range) && !is.element(0, breaks)) {
    breaks <- sort(unique(c(breaks, 0)))
    }

  breaks_negative <- which(breaks <= 0)
  breaks_positive <- which(breaks >= 0)
  k_negbreaks <- length(breaks_negative)
  k_posbreaks <- length(breaks_positive)
  if (type == "topo") {
    scale_color <- c(ColorsNeg(k_negbreaks - 1), ColorsPos(k_posbreaks - 1))
  } else if (type == "redblue") {
    scale_color <- c(ColorsBlue(k_negbreaks - 1), ColorsRed(k_posbreaks - 1))
  } else {
    stop("Unknown 'type' argument.")
  }

  return(list(colors = scale_color, breaks = breaks))
}

#' Create HSV-based topo-color ramp for negative EEG values
#'
#' @param n Integer specifying the number of shades.
#' @param alpha Transparency level between 0 and 1.
#'
#' @return A character vector of HSV color hex codes.
#'
#' @keywords internal
#' @noRd
ColorsNeg <- function(n, alpha = 1) {
  if (n > 0) {
    hsv(h = seq.int(from = 43 / 60, to = 31 / 60, length.out = n), alpha = alpha)
  } else {
    character()
  }
}


#' Create HSV-based topo-color ramp for positive EEG values
#'
#' @param n Integer specifying the number of shades.
#' @param alpha Transparency level between 0 and 1.
#'
#' @return A character vector of HSV color hex codes.
#'
#' @keywords internal
#' @noRd
ColorsPos <- function(n, alpha = 1) {
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

#' Create blue color ramp for negative EEG values for red-blue palette
#'
#' @param n Integer specifying the number of shades.
#' @param alpha Transparency level between 0 and 1.
#'
#' @return A character vector of color hex codes.
#'
#' @keywords internal
#' @noRd
ColorsBlue <- function(n,
                       alpha = 1) {
  if (n > 0) {
    pal <- grDevices::colorRampPalette(c("#2E5A87", "#DFEDF6"))
    cols <- pal(n)

    if (alpha < 1) {
      cols <- grDevices::adjustcolor(cols, alpha.f = alpha)
    }

    cols
  } else {
    character()
  }
}

#' Create red color ramp for positive EEG values for red-blue palette
#'
#' @param n Integer specifying the number of shades.
#' @param alpha Transparency level between 0 and 1.
#'
#' @return A character vector of color hex codes.
#'
#' @keywords internal
#' @noRd
ColorsRed <- function(n,
                      alpha = 1) {
  if (n > 0) {
    pal <- grDevices::colorRampPalette(c("#E4CCC6", "#A90C38"))
    cols <- pal(n)

    if (alpha < 1) {
      cols <- grDevices::adjustcolor(cols, alpha.f = alpha)
    }

    cols
  } else {
    character()
  }
}

