% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hazard-APIs.R
\name{get_hazard_endpoint_status}
\alias{get_hazard_endpoint_status}
\title{Hazard API Endpoint status}
\usage{
get_hazard_endpoint_status()
}
\value{
Status of Hazard API Endpoints
}
\description{
Hazard API Endpoint status
}
\examples{
\dontshow{if (has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
status <- get_hazard_endpoint_status()
print(status)
\dontshow{\}) # examplesIf}
}
