# Get biomonitoring data via batch

Get biomonitoring data via batch

## Usage

``` r
get_biomonitoring_data_batch(
  DTXSID = NULL,
  API_key = NULL,
  Projection = "",
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

- Projection:

  Optional parameter controlling return type.

- rate_limit:

  Number of seconds to wait between requests

- Server:

  The root address for the API endpoint.

- verbose:

  A logical indicating if some "progress report" should be given.

## Value

A list of data.frames of biomonitoring data corresponding to the input
DTXSIDs.

## Examples

``` r
if (FALSE) {
# Retrieve biomonitoring data for BPA and Caffeine
get_biomonitoring_data_batch(DTXSID = c('DTXSID7020182', 'DTXSID0020232'))
}
```
