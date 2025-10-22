
################################################################################
# Seminar demo
################################################################################

# install.packages("pak")
# pak::pak("DanielWalkerTunek/Lab4")
library(Lab4)

devtools::check()
devtools::test()

# Check available functions
ls("package:Lab4")

# Check available methods for linreg class
methods(class = "linreg")

fit <- linreg(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)

# print.linreg()
fit

# summary.linreg()
summary(fit)

# plot.linreg()
plot(fit)

# resid.linreg() and coef.linreg()
resid(fit)
coef(fit)

# pred.linreg()
pred(fit)

# vignettes
vignette("Lab4", package = "Lab4")

