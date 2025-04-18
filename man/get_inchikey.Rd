% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chemical-APIs.R
\name{get_inchikey}
\alias{get_inchikey}
\title{Get InChIKey}
\usage{
get_inchikey(
  name = NULL,
  API_key = NULL,
  Server = chemical_api_server,
  verbose = FALSE
)
}
\arguments{
\item{name}{Chemical name}

\item{API_key}{The user-specific API key}

\item{Server}{The root address for the API endpoint}

\item{verbose}{A logical indicating if some “progress report” should be
given.}
}
\value{
A string giving the associated InChIKey.
}
\description{
Get InChIKey
}
\examples{
\dontshow{if (has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
bpa_inchikey <- get_inchikey(name = "Bisphenol A")
\dontshow{\}) # examplesIf}
}
