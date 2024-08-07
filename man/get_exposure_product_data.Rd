% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exposure-APIs.R
\name{get_exposure_product_data}
\alias{get_exposure_product_data}
\title{Retrieve product data for exposure purposes}
\usage{
get_exposure_product_data(
  DTXSID = NULL,
  API_key = NULL,
  Server = exposure_api_server,
  verbose = FALSE
)
}
\arguments{
\item{DTXSID}{The chemical identifier DTXSID}

\item{API_key}{The user-specific API key}

\item{Server}{The root address for the API endpoint}

\item{verbose}{A logical indicating if some “progress report” should be given.}
}
\value{
A data.frame with product information relating to exposure to the
given chemical
}
\description{
Retrieve product data for exposure purposes
}
\examples{
\dontshow{if (has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Pull exposure product data for BPA
bpa <- get_exposure_product_data(DTXSID = 'DTXSID7020182')
\dontshow{\}) # examplesIf}
}
