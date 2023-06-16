
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ccdR

<!-- badges: start -->
<!-- badges: end -->

The goal of ccdR is to provide R users a set of functions to access the
[CCTE API](https://api-ccte.epa.gov/docs/index.html) without requiring
extensive experience interacting directly with APIs.

## Installation

You can install the development version of ccdR like so:

``` r
if (!library(devtools, logical.return = TRUE)){
  install.packages(devtools)
  library(devtools)
}

# Set API key to build vignette properly. If not run in this order, the Vignette will not build correctly!!!
Sys.setenv('CCTE_API_KEY' = '<YOUR API KEY>')

devtools::install_git(url = 'https://bitbucket.epa.gov/scm/ccdr1/ccdr.git',
                      build_vignettes = TRUE)
```
