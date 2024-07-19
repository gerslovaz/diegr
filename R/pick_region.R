pick_region <- function(coords = NULL, hemisphere = NULL, region = NULL, ROI = NULL) {
  if (missing(coords)) {
    coords <- HCGSN256
  }

  if (missing(ROI)) {
    ROI <- ROIs
  }

  idx.reg <- grep(paste(region, collapse = "|"), ROI)
  new.coords <- coords$D2[idx.reg,]

  if (missing(region)) {
    new.coords <- coords$D2
  }

  x <- new.coords$x

  midline <- which(x == 0)
  idx.l <- c()
  idx.r <- c()

  if (any(hemisphere == "left")) {
    idx.l <- which(x < 0)
  }
  if (any(hemisphere == "right")) {
    idx.r <- which(x > 0)
  }
  idx <- c(idx.l, idx.r, midline)

  if (missing(hemisphere)) {
    idx <- 1:length(x)
  }

  new.coords <- new.coords[idx,]

  return(new.coords)
}

ROIs <- c(rep('frontal', 6), rep('central', 3), rep('frontal', 6), rep('central', 2),
          rep('frontal', 6), 'central', rep('frontal',5), 'central', rep('frontal', 9), rep('central', 5),
          rep('frontal', 3), rep('central', 5), rep('frontal', 2), rep('central', 5), rep('temporal',3),
          rep('central', 3), rep('temporal',3), rep('central', 2), rep('temporal',2), rep('central',6),
          rep('temporal',2), rep('parietal',6), rep('temporal',2), rep('parietal',7), 'temporal',
          rep('parietal',7), rep('occipital', 7), 'parietal', rep('occipital', 7), rep('parietal',3),
          rep('central',2), rep('occipital', 6), rep('parietal',3), rep('central',2), rep('occipital', 5),
          rep('parietal',3), rep('central',2), rep('occipital', 4), rep('parietal',3), rep('central',2), rep('occipital', 3),
          rep('parietal',3), rep('central',2), rep('occipital', 2), rep('parietal',2), rep('temporal',2),
          rep('central',6), 'occipital', 'parietal', rep('temporal', 4), rep('central',5), rep('temporal', 4), rep('central',4),
          rep('temporal', 2), rep('central', 4), 'temporal', rep('frontal', 4), 'central')
