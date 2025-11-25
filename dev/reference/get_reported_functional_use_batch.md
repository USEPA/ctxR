# Get reported functional use via batch

Get reported functional use via batch

## Usage

``` r
get_reported_functional_use_batch(
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

  Number of seconds to wait between requests

- Server:

  The root address for the API endpoint.

- verbose:

  A logical indicating if some "progress report" should be given.

## Value

A list of data.frames of reported functional use corresponding to the
input DTXSIDs.

## Examples

``` r
if (FALSE) {
# Retrieve reported functional use for BPA and Caffeine
get_reported_functional_use_batch(DTXSID = c('DTXSID7020182',
                                             'DTXSID0020232'))
}
```
