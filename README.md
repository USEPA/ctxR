
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ctxR: Utilities for Interacting with the CTX APIs <a href="https://cran.r-project.org/web/packages/ctxR/index.html"><img src="vignettes/Pictures/ctxR_hex_link.png" width="200" align="right" /></a>

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/ctxR)](https://cran.r-project.org/package=ctxR)
[![Active](http://img.shields.io/badge/Status-Active-green.svg)](https://cran.r-project.org/package=ctxR)
[![Monthly
Downloads](https://cranlogs.r-pkg.org/badges/last-month/ctxR?color=7BAFD4)](https://cranlogs.r-pkg.org/badges/last-month/ctxR?color=7BAFD4)
[![DOI](https://zenodo.org/badge/doi/10.32614/CRAN.package.ctxR.svg)](http://dx.doi.org/10.32614/CRAN.package.ctxR)

<!-- badges: end -->

## Welcome to the GitHub repository for the ctxR package

ctxR was developed to streamline the process of accessing the
information available through the [Computational Toxicology and Exposure
(CTX)
APIs](https://www.epa.gov/comptox-tools/computational-toxicology-and-exposure-apis)
without requiring prior knowledge of how to use APIs. Chemical, hazard,
bioactivity, and exposure data in available from the CTX APIs. Most data
is also available on the [CompTox Chemical Dashboard
(CCD)](https://comptox.epa.gov/dashboard/) or within other
[Computational Toxicology and Exposure Online
Resources](https://www.epa.gov/comptox-tools).

If you are interested in contributing or want to report a bug, please
submit a issue or start a discussion. See
[CONTRIBUTING](CONTRIBUTING.md) for more information.

To install the current development version, run the following command:

    devtools::install_github("USEPA/ctxR")

Disclaimer: Users don’t need a API key to install ctxR, but will need to
supply an API key to use ctxR and access data. A *FREE* API key can be
obtained by emailing the [CTX API Admins](mailto:ccte_api@epa.gov).
