# Retrieve exposure functional use probability data batch

Retrieve exposure functional use probability data batch

## Usage

``` r
get_exposure_functional_use_probability_batch(
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

A data.table, with each row containing exposure functional use
probability data for each input DTXSID. NA values are filled in for
categories that have probability of 0

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull exposure functional use probability data for multiple chemicals
dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
dtxsid_func_use_prob <- get_exposure_functional_use_batch(DTXSID = dtxsid)
}
```
