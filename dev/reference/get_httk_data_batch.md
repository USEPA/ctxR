# Retrieve httk data via batch search

Retrieve httk data via batch search

## Usage

``` r
get_httk_data_batch(
  DTXSID = NULL,
  API_key = NULL,
  rate_limit = 0L,
  Server = exposure_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  The chemical identifier DTXSID

- API_key:

  The user-specific API key

- rate_limit:

  Number of seconds to wait between each request

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some "progress report" should be given.

## Value

A named list of httk data corresponding to the input chemicals

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Retrieve information for BPA and Caffeine
dtxsids <- c('DTXSID7020182', 'DTXSID0020232')
httk_data <- get_httk_data_batch(DTXSID = dtxsids)
}
```
