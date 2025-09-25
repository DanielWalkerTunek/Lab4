################################################################################
# Seminar Demo Script for Lab4 Package
################################################################################

# Load packages
library(devtools)
library(Lab4)


# Run tests to show they work
devtools::check()

# Load your package
library(Lab4)

# Show available vignettes
vignette(package = "Lab4")

# Open your vignette
vignette("Lab4-intro", package = "Lab4")

# Run an example function from Lab4
example_function()  # replace with one of your real functions
