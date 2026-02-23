# Get hazard data by DTXSID batch

The function was deprecated due to updates and restructing of the CTX
APIs.

## Usage

``` r
get_hazard_by_dtxsid_batch(
  DTXSID = NULL,
  API_key = NULL,
  rate_limit = 0L,
  Server = hazard_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  A list of chemical identifier DTXSIDs

- API_key:

  The user-specific API key

- rate_limit:

  Number of seconds to wait between each request

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A data.table containing chemical (human and eco) hazard data for each
input chemical.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull hazard data for multiple chemicals
dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
batch_hazard <- get_hazard_by_dtxsid_batch(DTXSID = dtxsid)
}
```
