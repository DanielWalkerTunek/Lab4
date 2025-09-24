#' Fit a linear regression model (QR decomposition)
#'
#' This function fits a multiple linear regression model using
#' a QR decomposition rather than the normal equations.
#'
#' @param formula A formula specifying the model.
#' @param data A data frame containing the variables in the model.
#'
#' @return An object of class `linreg` with QR-based estimates.
#' @export
#'
#' @examples
#' data(iris)
#' fit <- linreg_qr(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)
#' summary(fit)


linreg_qr <- function(formula, data) {

  # build model matrix X and response y
  X <- model.matrix(formula, data = data)
  y_name <- all.vars(formula)[1]
  y <- data[[y_name]]

  # number of obs (n) and parameters (p)
  n <- nrow(X)
  p <- ncol(X)

  # QR decomposition of X
    # QR factorization
  qrX <- qr(X)
    # orthonormal Q
  Q <- qr.Q(qrX)
    # upper triangular R
  R <- qr.R(qrX)

  # estimate beta wwith QR
  beta_hat <- solve(R, t(Q) %*% y)

  # fitted values and resids
  y_hat <- X %*% beta_hat
  residuals <- y - y_hat

  df <- n - p
  sigma2 <- sum(residuals^2) / df

  # VAR and CoVAR for beta using R
  R_inv <- solve(R)
  var_beta <- sigma2 * R_inv %*% t(R_inv)

  se_beta <- sqrt(diag(var_beta))
  tvalues <- as.vector(beta_hat) / se_beta
  pvalues <- 2 * pt(abs(tvalues), df = df, lower.tail = FALSE)

  # return S3 object
  res <- list(
    coefficients = as.vector(beta_hat),
    fitted.values = as.vector(y_hat),
    residuals = as.vector(residuals),
    df = df,
    sigma2 = sigma2,
    var_beta = var_beta,
    tvalues = tvalues,
    pvalues = pvalues,
    formula = formula,
    data = data
  )

  class(res) <- "linreg"
  return(res)

}
