################################################################################
# Seminar Demo Script for Lab4 Package
################################################################################

# Load devtools
library(devtools)

# Install Lab4 (if not already installed)
# install("path/to/Lab4")  # if local
# or load it directly for development
load_all("path/to/Lab4")

# Run tests to show they work
test("path/to/Lab4")  # runs tests in tests/testthat/

# Load your package
library(Lab4)

# Show available vignettes
vignette(package = "Lab4")

# Open your vignette
vignette("Lab4-intro", package = "Lab4")

# Run an example function from Lab4
example_function()  # replace with one of your real functions
