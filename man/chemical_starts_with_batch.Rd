% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chemical-APIs-batch.R
\name{chemical_starts_with_batch}
\alias{chemical_starts_with_batch}
\title{Chemical starts with batch search}
\usage{
chemical_starts_with_batch(
  word_list = NULL,
  API_key = NULL,
  rate_limit = 0L,
  verbose = FALSE,
  top = NULL
)
}
\arguments{
\item{word_list}{A list of character strings of chemical names or portion of
chemical names}

\item{API_key}{User-specific API key}

\item{rate_limit}{Number of seconds to wait between each request}

\item{verbose}{A logical indicating if some “progress report” should be given.}

\item{top}{The number of results to return if there are multiple results
available}
}
\value{
A named list of data.frames of chemicals and related values matching
the query parameters. The data.frames under the 'valid' entry contain
chemical information for successful requests while the data.frames under
the 'invalid' entry contain data.frames with chemical name suggestions
based on the input search values.
}
\description{
Chemical starts with batch search
}
\examples{
\dontshow{if (has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Pull chemicals that start with given substrings
bpa_substrings <- chemical_starts_with_batch(word_list = c('DTXSID702018',
                                                           'DTXCID3018'))
\dontshow{\}) # examplesIf}
}
\author{
Paul Kruse, Kristin Issacs
}
