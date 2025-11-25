# Retrieve bioactivity data from DTXSID or AEID batch

Retrieve bioactivity data from DTXSID or AEID batch

## Usage

``` r
get_bioactivity_details_batch(
  DTXSID = NULL,
  AEID = NULL,
  SPID = NULL,
  m4id = NULL,
  API_key = NULL,
  Server = NULL,
  rate_limit = 0L,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  A list of chemical identifier DTXSIDs.

- AEID:

  A list of assay endpoint identifiers AEIDs.

- SPID:

  A list of ChemSpider chemical inputs

- m4id:

  A list of chemical identifier m4ids

- API_key:

  The user-specific API key.

- Server:

  The root address for the API endpoint

- rate_limit:

  Number of seconds to wait between each request

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A named list of data.frames containing bioactivity information for the
chemicals with DTXSID or assays with AEID matching the input parameter.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull bioactivity details for multiple chemicals
dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
batch_bioactivity <- get_bioactivity_details_batch(DTXSID = dtxsid)
# Pull bioactivity details for multiple assays
batch_bioactivity <- get_bioactivity_details_batch(AEID = c(159, 160))
}
```
