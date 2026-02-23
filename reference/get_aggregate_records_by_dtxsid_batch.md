# Get Aggregate Records by DTXSID via batch

Get Aggregate Records by DTXSID via batch

## Usage

``` r
get_aggregate_records_by_dtxsid_batch(
  DTXSID = NULL,
  API_key = NULL,
  rate_limit = 0L,
  Server = "https://comptox.epa.gov/ctx-api/exposure",
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  Chemical identifier DTXSID

- API_key:

  The user-specific API key

- rate_limit:

  Number of seconds to wait between each request

- Server:

  The root address for the API endpoint

- verbose:

  A logicial indicating if some "progress report" should be given.

## Value

A list of data.frames containing aggregate records data for each input
DTXSID.

## Examples

``` r
if (FALSE) {
# Retrieve aggregate records data for BPA and Caffeine
get_aggregate_records_by_dtxsid_batch(DTXSID = c('DTXSID0020232',
  'DTXSID7020182'))
}
```
