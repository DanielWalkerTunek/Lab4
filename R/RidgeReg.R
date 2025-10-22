#' @importFrom stats model.matrix resid coef setNames sd
NULL
#' Fit a ridge regression model
#'
#' This function fits a multiple linear regression model using
#' ridge regularization and QR decomposition.
#'
#' @param formula A formula specifying the model.
#' @param data A data frame containing the variables in the model.
#' @param lambda A numeric value for the regularization parameter.
#'
#' @return An object of class `ridgereg` containing:
#' \item{coefficients}{Estimated regression coefficients (un-scaled).}
#' \item{fitted.values}{Fitted (predicted) values.}
#' \item{residuals}{Residuals.}
#' \item{formula}{The model formula.}
#' \item{lambda}{The regularization parameter used.}
#' @export
#'
#' @examples
#' data(iris)
#' fit_ridge <- ridgereg(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris, lambda = 0.5)
#' print(fit_ridge)

ridgereg <- function(formula, data, lambda) {
  formula_str <- deparse(formula)
  data_name   <- deparse(substitute(data))

  if (!is.data.frame(data)) stop("`data` must be a data.frame")

  # build model matrix X and response y
  X <- model.matrix(formula, data = data)
  y_name <- all.vars(formula)[1]
  y <- data[[y_name]]

  y_mean <- mean(y)
  y_scaled <- y - y_mean

  # Identify covariate columns (everything except the intercept)
  cov_cols <- 2:ncol(X)

  # Store their means and standard deviations
  X_means <- apply(X[, cov_cols, drop = FALSE], 2, mean)
  X_sds   <- apply(X[, cov_cols, drop = FALSE], 2, sd)

  # Create the scaled X matrix
  X_scaled <- X
  X_scaled[, cov_cols] <- scale(X[, cov_cols, drop = FALSE])

  # number of obs (n) and parameters (p)
  n <- nrow(X_scaled)
  p <- ncol(X_scaled)

  # Create penalty matrix I
  I_pen <- diag(p)
  I_pen[1, 1] <- 0 # Do not penalize the intercept

  # Create augmented matrices
  X_aug <- rbind(X_scaled, sqrt(lambda) * I_pen)
  y_aug <- c(y_scaled, rep(0, p))

  # Update 'n' for the augmented matrix
  n_aug <- nrow(X_aug)

  # QR decomposition
  Q <- matrix(0, n_aug, p)
  R <- matrix(0, p, p)

  for (j in 1:p) {
    v <- X_aug[, j]
    if (j > 1) {
      for (i in 1:(j-1)) {
        R[i, j] <- sum(Q[, i] * X_aug[, j]) # Use X_aug
        v <- v - R[i, j] * Q[, i]
      }
    }
    R[j, j] <- sqrt(sum(v^2))
    Q[, j] <- v / R[j, j]
  }

  # estimate beta with QR
  beta_hat_scaled <- solve(R, t(Q) %*% y_aug)

  beta_hat <- beta_hat_scaled

  # Un-scale coefficients
  beta_hat[cov_cols] <- beta_hat_scaled[cov_cols] / X_sds

  # Un-scale intercept
  beta_hat[1] <- y_mean + beta_hat_scaled[1] - sum(beta_hat_scaled[cov_cols] * X_means / X_sds)

  # fitted values and residuals
  y_hat <- X %*% beta_hat
  residuals <- y - y_hat


  # create S3 object
  res <- list(
    coefficients = setNames(as.vector(beta_hat), colnames(X)),
    fitted.values = as.vector(y_hat),
    residuals = as.vector(residuals),
    formula = formula,
    formula_str = formula_str,
    data_name = data_name,
    lambda = lambda
  )
  class(res) <- "ridgereg"
  res
}

################################################################################
# Methods
################################################################################

#' @export
print.ridgereg <- function(x, ...) {
  cat(sprintf("ridgereg(formula = %s, data = %s, lambda = %g)\n\n",
              x$formula_str, x$data_name, x$lambda))
  cat("Coefficients:\n")
  print(x$coefficients)
  invisible(x)
}

#' Predicted values for a model
#'
#' Returns the predicted values from a model.
#'
#' @param object A model object.
#' @param ... Arguments passed to methods.
#' @export
predict <- function(object, ...) UseMethod("predict")


#' @rdname predict
#' @param newdata A data frame for which to predict values.
#' @export
predict.ridgereg <- function(object, newdata, ...) {

  if (missing(newdata)) {
    return(object$fitted.values)
  }

  X_new <- model.matrix(object$formula, data = newdata)
  y_pred <- X_new %*% object$coefficients

  return(as.vector(y_pred))
}


#' @export
coef.ridgereg <- function(object, ...) {
  object$coefficients
}


