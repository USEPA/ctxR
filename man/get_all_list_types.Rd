% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chemical-APIs.R
\name{get_all_list_types}
\alias{get_all_list_types}
\title{Get all list types}
\usage{
get_all_list_types(
  API_key = NULL,
  Server = chemical_api_server,
  verbose = FALSE
)
}
\arguments{
\item{API_key}{The user-specific API key.}

\item{Server}{The root address for the API endpoint.}

\item{verbose}{A logical indicating if some "progress report" should be given.}
}
\value{
A character list of types of lists.
}
\description{
Get all list types
}
\examples{
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
get_all_list_types()
\dontshow{\}) # examplesIf}
}
