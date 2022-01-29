# inputs
# NA

# outputs
# NA

# notes on improvements and additions to function
# none

#-------------------------------------------------------------------------------
startup_function <- function() {

  # create vector of CRAN packages to install
  cran_packages_vector <- c(
    "ggplot2",
    "viridis",
    "plyr",
    "gridEdxtra",
    "grid"
  )

  # create a vector of non-CRAN packages (had to load from GITHUB)
  non_cran_packages_vector <- c("ggpattern")

  # install cran packages
  install.packages(cran_packages_vector)

  # install non-cran packages
  install.packages(non_cran_packages_vector)

  # load cran packages to library
  library(
    ggplot2,
    viridis,
    plyr,
    gridExtra,
    grid
  )

  # load non-cran packages to library
  library(ggpattern)
}