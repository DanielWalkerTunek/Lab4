#' @importFrom stats model.matrix pt printCoefmat resid coef setNames
NULL
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
#' \item{var_beta}{Variance-covariance matrix of coefficients.}
#' \item{tvalues}{t statistics for coefficients.}
#' \item{pvalues}{p-values for coefficients.}
#' @export
#'
#' @examples
#' data(iris)
#' fit <- linreg(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)
#' summary(fit)

linreg <- function(formula, data) {
  formula_str <- deparse(formula)
  data_name   <- deparse(substitute(data))

  if (!is.data.frame(data)) stop("`data` must be a data.frame")

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

  # fitted values and residuals
  y_hat <- X %*% beta_hat
  residuals <- y - y_hat

  # degrees of freedom
  df <- n - p

  # residual variance
  sigma2 <- sum(residuals^2) / df

  # VAR and CoVAR for beta using R
  R_inv <- solve(R)
  var_beta <- sigma2 * R_inv %*% t(R_inv)

  # t-values and p-values
  se_beta <- sqrt(diag(var_beta))
  tvalues <- as.vector(beta_hat) / se_beta
  pvalues <- 2 * pt(abs(tvalues), df = df, lower.tail = FALSE)

  # create S3 object
  res <- list(
    coefficients = setNames(as.vector(beta_hat), colnames(X)),
    fitted.values = as.vector(y_hat),
    residuals = as.vector(residuals),
    df = df,
    sigma2 = sigma2,
    var_beta = var_beta,
    tvalues = setNames(as.vector(tvalues), colnames(X)),
    pvalues = setNames(as.vector(pvalues), colnames(X)),
    formula = formula,
    formula_str = formula_str,
    data_name = data_name
  )
  class(res) <- "linreg"
  res
}

################################################################################
# Methods
################################################################################

#' @export
print.linreg <- function(x, ...) {
  cat(sprintf("linreg(formula = %s, data = %s)\n\n",
              x$formula_str, x$data_name))
  cat("Coefficients:\n")
  print(x$coefficients)
  invisible(x)
}

#' @export
#' @importFrom graphics plot
plot.linreg <- function(x, ...) {
  stopifnot(inherits(x, "linreg"))
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' must be installed to plot linreg objects.")
  }

  fitted <- x$fitted.values
  resid  <- x$residuals
  sigma  <- sqrt(x$sigma2)
  std_resid <- resid / sigma

  df1 <- data.frame(fitted = fitted, resid = resid)
  df2 <- data.frame(fitted = fitted, sres = sqrt(abs(std_resid)))

  p1 <- ggplot2::ggplot(df1, ggplot2::aes(fitted, resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_smooth(se = FALSE, formula = y ~ x) +
    ggplot2::labs(
      title = "Residuals vs Fitted",
      x = paste("Fitted values\n", deparse(x$formula)),
      y = "Residuals"
    )

  p2 <- ggplot2::ggplot(df2, ggplot2::aes(fitted, sres)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE, formula = y ~ x) +
    ggplot2::labs(
      title = "Scale-Location",
      x = paste("Fitted values\n", deparse(x$formula)),
      y = expression(sqrt("|Standardized residuals|"))
    )

  print(p1)
  print(p2)
  invisible(x)
}


#' Predicted/fitted values for a linreg model
#'
#' Returns the fitted values from a `linreg` model.
#'
#' @param object A model object.
#' @param ... Ignored.
#' @return A numeric vector of fitted values.
#' @export
pred <- function(object, ...) UseMethod("pred")


#' @rdname pred
#' @export
pred.linreg <- function(object, ...) object$fitted.values


# stats::resid is already a generic; provide our method
#' @export
resid.linreg <- function(object, ...) {
  object$residuals
}

# stats::coef is a generic; provide our method
#' @export
coef.linreg <- function(object, ...) {
  object$coefficients
}

#' @export
summary.linreg <- function(object, ...) {
  est <- object$coefficients
  se  <- sqrt(diag(object$var_beta))
  t   <- object$tvalues
  p   <- object$pvalues

  tab <- cbind(
    Estimate = est,
    `Std. Error` = se,
    `t value` = t,
    `Pr(>|t|)` = p
  )

  stats::printCoefmat(tab, P.values = TRUE, has.Pvalue = TRUE, signif.stars = TRUE)

  cat(sprintf("\nResidual standard error: %.3f on %d degrees of freedom\n",
              sqrt(object$sigma2), object$df))
  invisible(object)
}
