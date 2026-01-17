#' Check for required column names
#'
#' @description
#' Internal utility to check whether required column names are present in an object.
#'
#' @param obj An object with named columns (e.g., data frame or list).
#' @param required_cols A character vector of column names to check.
#'
#' @return Logical value indicating whether all required columns are present.
#' @keywords internal
#' @noRd
req_cols <- function(obj,
                     required_cols) {

  is.atomic(names(obj)) && all(required_cols %in% colnames(obj))
}
# in: animate_topo, control_D3, control_D2

#' Check missing columns in data
#'
#' @description
#' Verifies that all required columns are present in a data object.
#' If any required columns are missing, the function stops with an informative error message.
#'
#' @param data A data frame, tibble, or database table.
#' @param required_cols A character vector of required column names.
#' @param obj_name Name of the object used in the error message. Defaults to the name of \code{data}.
#'
#' @return Invisibly returns \code{TRUE} if all required columns are present.
#' @keywords internal
#' @noRd
stop_if_missing_cols <- function(data,
                                 required_cols,
                                 obj_name = deparse(substitute(data))) {

  cols <- dplyr::tbl_vars(data)
  miss_req <- setdiff(required_cols, cols)
  if (length(miss_req) > 0) {
    stop(
      "Columns missing in ", obj_name, ": ", paste(miss_req, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Validate grouping variables for NA-only columns
#'
#' Checks whether specified grouping variables present in the data contain only \code{NA} values.
#' Works with both in-memory data frames and database tables (via \pkg{dbplyr}).
#'
#' @param data A data frame or a database table (e.g. a \code{tbl_sql}).
#' @param vars Character vector of grouping variable names to check. Variables that are not present in \code{data} are silently ignored.
#' @param action Character string specifying how to handle grouping variables that contain only \code{NA} values. One of:
#'   \describe{
#'     \item{\code{"warn"}}{Issue a warning (default).}
#'     \item{\code{"stop"}}{Throw an error and stop execution.}
#'     \item{\code{"none"}}{Do nothing.}
#'   }
#'
#' @details
#' A grouping variable is considered invalid if it contains zero non-missing
#' values (i.e. all entries are \code{NA}). This condition is detected using a
#' database-safe check based on counting non-\code{NA} values.
#'
#' Variables with a single unique non-\code{NA} value are allowed and do not
#' trigger any message.
#'
#' @return Invisibly returns \code{NULL}. The function is used for its side effects
#' (warnings or errors).
#' @keywords internal
#' @noRd
check_grouping_vars <- function(data,
                                vars,
                                action = c("warn", "stop", "none")) {
  action <- match.arg(action)

  vars <- intersect(vars, colnames(data))
  if (length(vars) == 0) {
    return(invisible(NULL))
  }

  summary <- data |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(vars),
        ~ sum(!is.na(.)),
        .names = "{.col}__n_non_na"
      )
    ) |>
    dplyr::collect()

  bad_vars <- vars[
    vapply(
      vars,
      function(v) summary[[paste0(v, "__n_non_na")]] == 0,
      logical(1)
    )
  ]

  if (length(bad_vars) > 0 && action != "none") {
    msg <- paste0(
      "Grouping variable(s) ",
      paste(sprintf("`%s`", bad_vars), collapse = ", "),
      " are present but contain only NA values. ",
      "This will likely lead to invalid grouping results."
    )

    if (action == "warn") {
      warning(msg, call. = FALSE)
    } else if (action == "stop") {
      stop(msg, call. = FALSE)
    }
  }

  invisible(NULL)
}



#' Validate D3 part of mesh object
#'
#' @description
#' Internal helper to ensure 3D coordinate structure exists in mesh object and contains `x`, `y`, `z`.
#'
#' @param mesh A list expected to contain `D3`, typically from sensor layout.
#'
#' @return Invisible TRUE if checks pass; otherwise, stops with error.
#' @keywords internal
#' @noRd
control_D3 <- function(mesh){

  if (!is.list(mesh) || !"D3" %in% names(mesh)) {
    stop("The 'D3' is missing in the input mesh.")
  } else if (!is.data.frame(mesh$D3)) {
    stop("The 'D3' must be a data.frame or tibble.")
  } else if (!req_cols(mesh$D3, c("x", "y", "z"))) {
    stop("Columns 'x', 'y', 'z' are required in 'D3' part of a mesh.")
  } else if (!all(sapply(mesh$D3[, c("x", "y", "z")], is.numeric))) {
    stop("Coordinates must be numeric.")
  }
  return(invisible(TRUE))

}
# in: scalp_plot

#' Validate D2 part of mesh object
#'
#' @description
#' Internal helper to ensure 2D coordinate structure exists in mesh object and contains `x` and `y`.
#'
#' @param mesh A list expected to contain `D2`, typically from sensor layout.
#'
#' @return Invisible TRUE if checks pass; otherwise, stops with error.
#' @keywords internal
#' @noRd
control_D2 <- function(mesh){

  if (!is.list(mesh) || !"D2" %in% names(mesh)) {
    stop("The 'D2' is missing in the input mesh.")
  } else if (!is.data.frame(mesh$D2)) {
    stop("The 'D2' must be a data.frame or tibble.")
  } else if (!req_cols(mesh$D2, c("x", "y"))) {
    stop("Columns 'x', 'y' are required in 'D2' part of a mesh.")
  } else if (!all(sapply(mesh$D2[, c("x", "y")], is.numeric))) {
    stop("Coordinates must be numeric.")
  }
  return(invisible(TRUE))

}
# in: scalp_plot

#' Generate spline matrix for interpolation
#'
#' @description
#' Computes the thin-plate spline matrix for dimensions 1, 2, or 3 between crossing points.
#'
#' @param X Matrix of crossing points.
#' @param Xcp Optional matrix of target crossing points; defaults to `X`.
#'
#' @return A numeric matrix encoding pairwise spline distances.
#' @keywords internal
#' @noRd
spline_matrix <- function(X,
                          Xcp = X) {

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


#' Construct augmented design matrix for interpolation
#'
#' @description
#' Generates the augmented matrix used in spline interpolation using `spline_matrix()`.
#'
#' @param X Matrix of input coordinates.
#' @param Xcp Optional matrix of evaluation coordinates.
#'
#' @return A matrix of appropriate structure for spline interpolation estimation.
#' @keywords internal
#' @noRd
XP_IM <- function(X,
                  Xcp) {

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
    stop("The matrices X and Xcp for X_P computing must have the same number of columns.")
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


#' Interpolate spline model to target crossing points
#'
#' @description
#' Performs interpolation of Y-values using thin-plate spline fit on control points.
#'
#' @param X Matrix of input coordinates.
#' @param Y Matrix of response values.
#' @param Xcp Optional matrix of target crossing points.
#'
#' @return A list with following components:
#' - `Y_hat`: interpolated values,
#' - `beta_hat`: coefficients.
#'
#' @keywords internal
#' @noRd
IM <- function(X,
               Y,
               Xcp = X) {

  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  if (!is.matrix(Xcp)) {
    Xcp <- as.matrix(Xcp)
  }
  if (!is.matrix(Y)) {
    Y <- as.matrix(Y)
  }

  if (nrow(X) < 2 || ncol(X) < 1 || !is.numeric(X)) {
    stop("Matrix X must have at least 2 rows and 1 column, and contain numeric values.")
  }
  if (nrow(Y) < 2 || ncol(Y) < 1 || !is.numeric(Y)) {
    stop("Matrix Y must have at least 2 rows and 1 column, and contain numeric values.")
  }
  if (nrow(Xcp) < 2 || ncol(Xcp) < 1 || !is.numeric(Xcp)) {
    stop("Matrix Xcp must have at least 2 rows and 1 column, and contain numeric values.")
  }

  d1 <- ncol(X)
  d2 <- ncol(Y)

  X_P <- XP_IM(X, X)
  condition_num <- kappa(X_P)

  if (condition_num > 1e+12) { # Check ill-conditioned matrix
    warning(paste("The X_P matrix is ill-conditioned (kappa =",
                  format(condition_num, scientific = TRUE, digits = 3),
                  ") and the results from solve() could be inaccurate."))
  }

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

#' Reconstruct 3D mesh from 2D layout
#'
#' @description
#' Interpolates 3D coordinates from 2D layout using a spline model fitted to known correspondence.
#'
#' @param X2D Matrix of 2D coordinates.
#' @param X3D Matrix of matching 3D coordinates.
#' @param mesh A mesh or mesh$D2 target for interpolation.
#'
#' @return A data frame with columns `x`, `y`, and `z`.
#' @keywords internal
#' @noRd
recompute_3d <- function(X2D,
                         X3D,
                         mesh) {

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



#' Construct penalized regression matrix for smoothing
#'
#' @description
#' Helper to construct design matrix for penalized regression model using spline basis.
#'
#' @param X Matrix of input coordinates.
#' @param lambda Numeric smoothing parameter.
#'
#' @return Design matrix for penalized regression.
#' @keywords internal
#' @noRd
XP_PRM <- function(X,
                   lambda) {
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }

  if (!is.numeric(lambda) || lambda <= 0) {
    stop("'lambda' must be a positive number.")
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

#' Penalized regression model using spline basis
#'
#' @description
#' Fits a penalized regression spline model using lambda as regularization parameter.
#'
#' @param X Matrix of input coordinates.
#' @param Y Matrix of target coordinates.
#' @param lambda Smoothing parameter.
#'
#' @return List with following components:
#' - `Y_hat`: fitted values,
#' - `beta_hat`: coefficients, hat diagonals
#' - `diag_hat`: hat diagonals.
#'
#' @keywords internal
#' @noRd
PRM <- function(X,
                Y,
                lambda) {

  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  if (!is.matrix(Y)) {
    Y <- as.matrix(Y)
  }

  if (!is.numeric(lambda) || lambda <= 0) {
    stop("'lambda' must be a positive number.")
  }

  if (nrow(X) < 2 || ncol(X) < 1 || !is.numeric(X)) {
    stop("Matrix X must have at least 2 rows and 1 column, and contain numeric values.")
  }
  if (nrow(Y) < 2 || ncol(Y) < 1 || !is.numeric(Y)) {
    stop("Matrix Y must have at least 2 rows and 1 column, and contain numeric values.")
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

#' Generalized Cross-Validation (GCV) score
#'
#' @description
#' Computes GCV score for assessing smoothing parameter in spline regression.
#'
#' @param X Matrix of predictors.
#' @param Y Matrix of response values.
#' @param lambda Smoothing parameter.
#'
#' @return Numeric scalar with GCV score.
#' @keywords internal
#' @noRd
GCV_score <- function(X,
                      Y,
                      lambda){

  model <- PRM(X, Y, lambda)

  if (length(model$diag_hat) == 0 || any(is.na(model$diag_hat))) {
    stop("The output of PRM model has zero length or contain NAs.")
  }

  k <- dim(as.matrix(X))[1]
  denom <- sum(rep(1,k) - model$diag_hat)
  if (denom == 0) {
    stop("Degenerate model fit - denominator in GCV score is zero.")
  }
  GCV <- k * sum((model$Y_hat - Y)^2) / denom^2
  return(GCV)
}


#' Degrees of freedom adjusted cross-validation (DCV) score
#'
#' @description
#' Computes DCV score as an alternative to GCV for spline model fit.
#'
#' @param X Matrix of predictors.
#' @param Y Matrix of response values.
#' @param lambda Smoothing parameter.
#'
#' @return Numeric scalar with DCV score.
#' @keywords internal
#' @noRd
DCV_score <- function(X,
                      Y,
                      lambda){

  k <- dim(as.matrix(X))[1]
  model <- PRM(X, Y, lambda)

  if (length(model$diag_hat) == 0 || any(is.na(model$diag_hat))) {
    stop("The output of PRM model has zero length or contain NAs.")
  }
  DCV <- k * sum((model$Y_hat - Y)^2) / (k - 1.5 * sum(model$diag_hat))^2
  return(DCV)
}


