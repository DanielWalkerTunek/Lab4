library(MASS)
data("iris")

test_that("ridgereg coefficients match lm.ridge", {
  lambda_val <- 0.5

  our_mod <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = lambda_val)

  X_covariates <- model.matrix(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)[, -1]
  y <- iris$Petal.Length

  mass_mod <- lm.ridge(y ~ X_covariates, lambda = lambda_val)

  mass_coefs <- coef(mass_mod)

  names(mass_coefs) <- c("(Intercept)", "Sepal.Width", "Sepal.Length")

  expect_equal(coef(our_mod), mass_coefs, tolerance = 1e-4)
})

test_that("predict.ridgereg works", {
  lambda_val <- 1.0
  our_mod <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = lambda_val)

  preds_train <- predict(our_mod)
  preds_new <- predict(our_mod, newdata = iris[1:10, ])

  expect_equal(preds_train, our_mod$fitted.values)
  expect_equal(length(preds_new), 10)
})
