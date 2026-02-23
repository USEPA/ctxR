# Retrieve product data for exposure purposes batch

Retrieve product data for exposure purposes batch

## Usage

``` r
get_exposure_product_data_batch(
  DTXSID = NULL,
  API_key = NULL,
  rate_limit = 0L,
  Server = exposure_api_server,
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

  A logical indicating if some “progress report” should be given.

## Value

A named list of data.frames, each containing exposure product data for
each input DTXSID.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull exposure functional use data for multiple chemicals
dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
dtxsid_product_data <- get_exposure_product_data_batch(DTXSID = dtxsid)
}
```
