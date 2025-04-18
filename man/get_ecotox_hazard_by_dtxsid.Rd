% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hazard-APIs.R
\name{get_ecotox_hazard_by_dtxsid}
\alias{get_ecotox_hazard_by_dtxsid}
\title{Get ecotox hazard data by DTXSID}
\usage{
get_ecotox_hazard_by_dtxsid(
  DTXSID = NULL,
  API_key = NULL,
  Server = hazard_api_server,
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
A data.frame containing chemical (ecotox) hazard data
}
\description{
Get ecotox hazard data by DTXSID
}
\examples{
\dontshow{if (has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Pull ecotox hazard data for BPA
bpa_ecotox <- get_ecotox_hazard_by_dtxsid(DTXSID = 'DTXSID7020182')
\dontshow{\}) # examplesIf}
}
