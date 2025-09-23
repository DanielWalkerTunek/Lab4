#' Fit a linear regression model (ordinary least squares)
#'
#' This function fits a multiple linear regression model using
#' ordinary least squares. It mimics the behaviour of `lm()` but
#' returns an object of class `linreg` with its own print,
#' plot, coef, resid, pred and summary methods.
#'
#' @param formula A formula specifying the model.
#' @param data A data frame containing the variables in the model.
#'
#' @return An object of class `linreg` containing:
#' \item{coefficients}{Estimated regression coefficients.}
#' \item{fitted.values}{Fitted (predicted) values.}
#' \item{residuals}{Residuals.}
#' \item{df}{Degrees of freedom.}
#' \item{sigma2}{Residual variance estimate.}
#' \item{var_beta}{Variance–covariance matrix of coefficients.}
#' \item{tvalues}{t statistics for coefficients.}
#' \item{pvalues}{p-values for coefficients.}
#' @export
#'
#' @examples
#' data(iris)
#' fit <- linreg(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)
#' summary(fit)


linreg <- function(formula, data) {

  # build model matrix X and response y
  X <- model.matrix(formula, data = data)
  y_name <- all.vars(formula)[1]
  y <- data[[y_name]]

  # number of obs (n) and parameters (p)
  n <- nrow(X)
  p <- ncol(X)

  # OLS estimates beta_hat = (X'X)^(-1) X'y
  XtX_inv <- solve(t(X) %*% X)
  beta_hat <- XtX_inv %*% t(X) %*% y

  # fitted values and residuals
  y_hat <- X %*% beta_hat
  residuals <- y - y_hat

  # degrees of freedom
  df <- n - p

  # residual variance
  sigma2 <- sum(residuals^2) / df

  # variance of coefficients
  var_beta <- as.numeric(sigma2) * XtX_inv

  # t-values and p-values
  se_beta <- sqrt(diag(var_beta))
  tvalues <- as.vector(beta_hat) / se_beta
  pvalues <- 2 * pt(abs(tvalues), df = df, lower.tail = FALSE)

  # create S3 object
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
