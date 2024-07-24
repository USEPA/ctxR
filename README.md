
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ctxR

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/ctxR)](https://cran.r-project.org/package=ctxR)
[![Monthly
Downloads](https://cranlogs.r-pkg.org/badges/last-month/ctxR?color=7BAFD4)](https://cranlogs.r-pkg.org/badges/last-month/ctxR?color=7BAFD4)
<!-- badges: end -->

The goal of ctxR is to provide R users a set of functions to access the
[CTX
APIs](https://www.epa.gov/comptox-tools/computational-toxicology-and-exposure-apis)
without requiring extensive experience interacting directly with APIs.

## Installation

You can install the development version of ctxR like so:

``` r
if (!library(devtools, logical.return = TRUE)){
  install.packages(devtools)
  library(devtools)
}

devtools::install_gitub("USEPA/ctxR")
```

You can install from CRAN using the following:

``` r
install.packages('ctxR')
```

Disclaimer: You won’t need a API key to install the ctxR package, but
will need to supply an API key to use ctxR. Please visit [CTX
APIs](https://www.epa.gov/comptox-tools/computational-toxicology-and-exposure-apis)
to request an API key.
