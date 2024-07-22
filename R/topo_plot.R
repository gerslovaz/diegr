topo_plot <- function(data, coords, region, sensors, ) {

}

IM <- function(X,y){
  ## interpolating using spline
  y <- as.numeric(y)
  d <- ncol(X)
  X.P <- XP_IM(X)
  beta.hat <- solve(X.P) %*% c(y, rep(0,d+1))
  return(beta.hat)
}

TopoColorsNeg <- function(n, alpha = 1) {
  if ((n <- as.integer(n[1L])) > 0) {
    hsv(h = seq.int(from = 43/60, to = 31/60,length.out = n), alpha = alpha)
  }
  else character()
}

TopoColorsPos <- function(n, alpha = 1) {
  if ((n <- as.integer(n[1L])) > 0) {
    j <- n %/% 3
    k <- n %/% 3
    i <- n - j - k
    c(if (i > 0) hsv(h = seq.int(from = 23/60, to = 11/60, length.out = i), alpha = alpha),
      if (j > 0 & k > 0) hsv(h = seq.int(from = 10/60, to = 0, length.out = (j + k)), alpha = alpha, s = seq.int(from = 1, to = 0.95, length.out = (j + k)), v = seq.int(from = 1, to = 0.6, length.out = (j + k))) )
    #hsv(h = seq.int(from = 10/60, to = 6/60, length.out = (j + k)), alpha = alpha, s = seq.int(from = 1, to = 0.3, length.out = (j + k)), v = 1))
  }
  else character()
}

TopoColorsPos2 <- function(n, alpha = 1) {
  ## jiny pristup k poz. skale, jasnejsi cervena na konci
  if ((n <- as.integer(n[1L])) > 0) {
    hues <- seq(1/3, 0, length.out = n)
    hsv(hues, 1, 1)
      }
  else character()
}

create_scale <- function(col.range) {

  probs     <- seq(0, 1, by = 0.02)
  k_probs   <- length(probs)
  breaks    <- unique(pretty(col.range,k_probs))
  breaks[length(breaks)] <- breaks[length(breaks)] + 1
  breaks_negative   <- which(breaks <= 0)
  breaks_positive   <- which(breaks >= 0)
  k_negbreaks <- length(breaks_negative)
  k_posbreaks <- length(breaks_positive)
  scale_color <- c(TopoColorsNeg(k_negbreaks - 1),TopoColorsPos2(k_posbreaks - 1))
  return(list(colors = scale_color, breaks = breaks))

}

prumer <- epochdata %>%
  dplyr::filter(time == 251 & subject == 1 & epoch == 13) %>%
  dplyr::select(signal, electrode)
col.range <- c(-21, 55) ## col range pro grand average z avgHCvsP

cut_scale <- function(signal, col.scale) { # asi bude stacit tak easy

  clr <- cut(signal, col.scale$breaks, include.lowest = TRUE)
  colour <- col.scale$colors[clr]

}


## pridat NA kontrolu
#ind         <- !is.na(signal)
#colour      <- rep("white", length(colour))
#colour[ind] <- clr[ind]

clr         <- rep(0, nrow(prumer))
ind         <- !is.na(colour)
clr_nona    <- rep(0, length(clr[ind]))

colour_nona       <- colour[ind]
ID_negative       <- which(colour_nona < 0)
ID_positive       <- which(colour_nona >= 0)
breaks_negative   <- which(breaks <= 0)
breaks_positive   <- which(breaks >= 0)
k_negbreaks <- length(breaks_negative)
k_posbreaks <- length(breaks_positive)
negbreaks <- (breaks[breaks_negative])[-length(breaks_negative)]
clr_nona_negative <- cut(colour_nona[ID_negative], breaks[breaks_negative], labels = negbreaks, include.lowest = TRUE)
posbreaks <- (breaks[breaks_positive])[-length(breaks_positive)]
clr_nona_positive <- cut(colour_nona[ID_positive], breaks[breaks_positive], labels = posbreaks, include.lowest = TRUE)
clr_nona[ID_negative] <- TopoColorsNeg(k_negbreaks - 1)[clr_nona_negative]
clr_nona[ID_positive] <- TopoColorsPos(k_posbreaks - 1)[clr_nona_positive]
clr[ind]             <- clr_nona
colour      <- rep("white", length(colour))
colour[ind] <- clr[ind]




PlotScale <- function(col.scale) {
  breaks <- col.scale$breaks

  windows(width = 1.2, height = 5)
  par(mar = c(1,1,1.2,4))
  plot(0,1,type = "n",xlim = c(0,1), ylim = col.range, xaxs = "i", yaxs = "i",
       xlab = "", ylab = "",bty = "n", axes = FALSE)
  rect(0, breaks[-length(breaks)], 1, breaks[-1L],
       col =  col.scale$colors
  )
  axis(4,breaks, cex.axis = 0.8,las = 1)

}


PlotTopoSub <- function(data, t, df.vstup, col.scale = barvy, col.rg = range.pretty,
                        col.range = col.range, n = 141){
  ## topo-plot using IM model
  signal <- data %>%
    dplyr::filter(time == t)
  #signal$number <- gsub("E","",as.character(signal$electrode))
  #signal$number <- as.numeric(signal$number)
  #signal <- signal %>%
  #  dplyr::arrange(number)
  beta.hat <- IM(df.vstup,signal$signal)
  X.Pcp <- XP.IM(df.vstup, Xcp) # pocita to docela dlouho uz na 30tis. siti, chce to nejak optimalizovat
  y.Pcp <- X.Pcp%*%beta.hat
  ycp.IM2[-which(is.na(ycp.IM2))] <- y.Pcp[1:dim(Xcp)[1]]
  ycp.IM2 <- matrix(ycp.IM2, nrow=n)
  x <- x[1:ncol(ycp.IM2)]

  #signal.col <- cut(y.Pcp, col.rg, include.lowest=TRUE) #zakoduje hodnoty podle clr
  #y.col <- sit.ctverec2[,1]
  #y.col[-which(is.na(y.col))] <- signal.col
  #colr <- col.scale[sort(y.col)] ## dulezite je tady to SORT jinak to nefunguje spravne - jeste prozkoumat, jestli mam mit sort na y.col nebo staci mit to na signal?
  nf <- layout( matrix(c(1,2), nrow=2, byrow=TRUE),
                heights=c(5,1))

  par(mar = c(0,0,0,0))
  plot(df.vstup$x, df.vstup$y, ylim = c(-450, 450), asp = 1, type = "n", axes = F)
  image(seq(-r2, r2, length.out=n), x, ycp.IM2, xlab="", ylab="", xaxt="n", yaxt="n",
        col= barvy, breaks = col.rg,
        bty="n", add=T) # pozor, u image musi byt spravne nastaveny ylim a xlim, kde se to ma vykreslit
  contour(seq(-r2, r2, length.out=n), x, ycp.IM2, add=T, col = "gray")
  points(df.vstup$x, df.vstup$y, pch = 16, col = "black", cex = 0.6)
  segments(0,415,-30,396)
  segments(0,415,30,396)

  plot(1:750, rep(0, 750), type = "l", lty = 2, axes = F, xlab = "", ylab = "")
  points(t, 0, col = "red", pch = 18, cex = 1.6)
  points(251, 0, col = "black", pch = 4, cex = 1.5)
  text(t, 0.25, labels = paste("t = ", t*4 - 1004, " ms"), cex = 1.5)
}


