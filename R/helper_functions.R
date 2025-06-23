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
