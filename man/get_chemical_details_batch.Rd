% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chemical-APIs-batch.R
\name{get_chemical_details_batch}
\alias{get_chemical_details_batch}
\title{Retrieve chemical details from DTXSID of DTXCID in batch search}
\usage{
get_chemical_details_batch(DTXSID = NULL, DTXCID = NULL, API_key = NULL)
}
\arguments{
\item{DTXSID}{The chemical identifier DTXSID}

\item{DTXCID}{The chemical identifier DTXCID}

\item{API_key}{The user-specific API key}
}
\value{
A named list of data.tables containing chemical information for the
  chemicals with DTXSID or DTXCID matching the input parameter.
}
\description{
Retrieve chemical details from DTXSID of DTXCID in batch search
}