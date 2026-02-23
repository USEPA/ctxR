# Retrieve chemical fate data in batch search

Retrieve chemical fate data in batch search

## Usage

``` r
get_fate_by_dtxsid_batch(
  DTXSID = NULL,
  API_key = NULL,
  rate_limit = 0L,
  Server = chemical_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  A vector of chemicals identifier DTXSIDs

- API_key:

  The user-specific API key

- rate_limit:

  Number of seconds to wait between each request

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A data.table containing chemical fate information for the chemicals with
DTXSID matching the input parameter.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemical fate by dtxsids
chemical_fates <- get_fate_by_dtxsid_batch(DTXSID = c('DTXSID7020182',
                                                      'DTXSID2021315'))
}
```
