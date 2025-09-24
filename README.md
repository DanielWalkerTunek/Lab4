
# Lab4

<!-- badges: start -->
<!-- badges: end -->

The goal of Lab4 is to implement a Linear regression model

## Installation

You can install the development version of Lab4 from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("DanielWalkerTunek/Lab4")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(Lab4)
data(iris)
fit <- linreg(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)
summary(fit)
```

