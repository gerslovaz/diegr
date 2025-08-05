## helper functions

req_cols <- function(obj, required_cols) {
  # control required columns
  is.atomic(names(obj)) && all(required_cols %in% names(obj))
}
# in: animate_topo, control_D3, control_D2

control_D3 <- function(mesh){
  # control the D3 part of a mesh
  if (!is.list(mesh) || !"D3" %in% names(mesh)) {
    stop("The 'D3' is missing in the input mesh.")
  } else if (!req_cols(mesh$D3, c("x", "y", "z"))) {
    stop("Columns 'x', 'y', 'z' are required in 'D3' part of a mesh.")
  }
  return(invisible(TRUE))
}
# in: scalp_plot

control_D2 <- function(mesh){
  # control the D2 part of a mesh
  if (!is.list(mesh) || !"D2" %in% names(mesh)) {
    stop("The 'D2' is missing in the input mesh.")
  } else if (!req_cols(mesh$D2, c("x", "y"))) {
    stop("Columns 'x', 'y' are required in 'D2' part of a mesh.")
  }
  return(invisible(TRUE))
}
# in: scalp_plot


exclude_epoch <- function(data, ex_epoch){
  # exclude chosen epoch(s)
  newdata <- data |>
    dplyr::filter(!.data$epoch %in% {{ ex_epoch }})

  return(newdata)
}
# in: compute_mean

spline_matrix <- function(X, Xcp = X) {
  ## compute S matrix to using in spline methods for d in c(1,2,3)
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  if (!is.matrix(Xcp)) {
    Xcp <- as.matrix(Xcp)
  }

  if (ncol(X) != ncol(Xcp)) {
    stop("The matrices X and Xcp for spline matrix computing must have the same number of columns")
  }

  k <- dim(X)[1]
  kcp <- dim(Xcp)[1]
  d <- dim(X)[2]
  X1 <- t(matrix(rep(t(Xcp), k), nrow = d))
  X2 <- matrix(rep(X, each = kcp), ncol = d)

  diff_sq <- (X1 - X2)^2
  argument <- rowSums(diff_sq)
  if (d == 2) {
    S <- 1 / (8 * pi) * (argument) * log(sqrt(argument))
    S[argument == 0] <- 0
  } else if (d == 3) {
    S <- -1 / (8 * pi) * sqrt(argument)
    S[argument == 0] <- 0
  } else if (d == 1) {
    S <- 1/12 * abs(X1 - X2)^3
  } else {
    stop("X input in spline_matrix must be dim 1, 2 or 3")
  }

  S <- matrix(S, kcp, k, byrow = FALSE)
  return(S)
}

XP_IM <- function(X, Xcp) {
  ## compute X_p for interpolation spline method
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  if (missing(Xcp)) {
    Xcp <- X
  }
  if (!is.matrix(Xcp)) {
    Xcp <- as.matrix(Xcp)
  }

  if (ncol(X) != ncol(Xcp)) {
    stop("The matrices X and Xcp for X.P computing must have the same number of columns")
  }

  k <- dim(X)[1]
  kcp <- dim(Xcp)[1]
  d <- dim(X)[2]

  X_P <- matrix(0, nrow = kcp + d + 1, ncol = k + d + 1)
  X_P[1:kcp, 1:k] <- spline_matrix(X, Xcp)
  X_P[1:kcp, (k + 1):(k + d + 1)] <- cbind(1, Xcp)
  X_P[(kcp + 1):(kcp + d + 1), 1:k] <- rbind(1, t(X))

  return(X_P)
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

  X_P <- XP_IM(X, X)
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

recompute_3d <- function(X2D, X3D, mesh) {
  ## recompute 3D net from 2D coordinates
  if (!is.matrix(X2D)) {
    X2D <- as.matrix(X2D)
  }

  if (!is.matrix(X3D)) {
    X3D <- as.matrix(X3D)
  }

  if (is.list(mesh) && "D2" %in% names(mesh)) {
    mesh <- mesh$D2
  }

  if (any(is.na(mesh))) {
    mesh <- na.omit(mesh)
  }

  kcp <- dim(mesh)[1]
  Y_Pcp <- IM(X2D, X3D, mesh)$Y_hat
  Y <- data.frame(x = Y_Pcp[1:kcp, 1], y = Y_Pcp[1:kcp, 2], z = Y_Pcp[1:kcp, 3])
  return(Y)
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


#' Subsets data based on specified criteria
#'
#' @description
#' This function filters a dataset based on a series of optional
#' conditions related to subject, sensor, time, and epoch.
#'
#' @param data A data frame, tibble or database table with input data. It should have at least columns named 'subject', 'sensor', 'time', and 'epoch'.
#' @param subject_rg A vector specifying the subjects to include. If `NULL` (the default), no filtering is applied based on subject.
#' @param sensor_rg A vector specifying the sensors to include. If `NULL` (the default), no filtering is applied based on sensor.
#' @param time_rg A vector specifying the time points to include. If `NULL` (the default), no filtering is applied based on time.
#' @param epoch_rg A vector specifying the epochs to include. If `NULL` (the default), no filtering is applied based on epoch.
#'
#' @return A subset of original data according to chosen parameters.
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @noRd
pick_data <- function(data, subject_rg = NULL, sensor_rg = NULL,
                      time_rg = NULL, epoch_rg = NULL) {

  conditions <- list()

  if (!is.null(subject_rg)) {
    conditions <- append(conditions, expr(.data$subject %in% {{ subject_rg }}))
  }
  if (!is.null(sensor_rg)) {
    conditions <- append(conditions, expr(.data$sensor %in% {{ sensor_rg }}))
  }

  if (!is.null(time_rg)) {
    conditions <- append(conditions, expr(.data$time %in% {{ time_rg }}))
  }

  if (!is.null(epoch_rg)) {
    conditions <- append(conditions, expr(.data$epoch %in% {{ epoch_rg }}))
  }

  data |>
    dplyr::filter(!!!conditions)

}

#make_latex_title <- function(use_latex, yaxis_title_latex, yaxis_title_nolatex) {
#  if (use_latex) {
#    yaxis_title <- yaxis_title_latex
#    mathjax_config <- 'cdn'
#  } else {
#    yaxis_title <- yaxis_title_nolatex
#    mathjax_config <- NULL
#  }

#}

