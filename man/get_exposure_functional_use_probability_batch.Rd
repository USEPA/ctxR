% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exposure-APIs-batch.R
\name{get_exposure_functional_use_probability_batch}
\alias{get_exposure_functional_use_probability_batch}
\title{Retrieve exposure functional use probability data batch}
\usage{
get_exposure_functional_use_probability_batch(
  DTXSID = NULL,
  API_key = NULL,
  rate_limit = 0L,
  Server = exposure_api_server,
  verbose = FALSE
)
}
\arguments{
\item{DTXSID}{Chemical identifier DTXSID}

\item{API_key}{The user-specific API key}

\item{rate_limit}{Number of seconds to wait between each request}

\item{Server}{The root address for the API endpoint}

\item{verbose}{A logical indicating if some “progress report” should be
given.}
}
\value{
A data.table, with each row containing exposure functional use
probability data for each input DTXSID. NA values are filled in for
categories that have probability of 0
}
\description{
Retrieve exposure functional use probability data batch
}
\examples{
\dontshow{if (has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Pull exposure functional use probability data for multiple chemicals
dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
dtxsid_func_use_prob <- get_exposure_functional_use_batch(DTXSID = dtxsid)
\dontshow{\}) # examplesIf}
}
