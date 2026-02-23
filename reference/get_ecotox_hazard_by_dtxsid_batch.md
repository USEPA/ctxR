# Get ecotox hazard data by DTXSID batch

**\[deprecated\]**

The function was deprecated due to updates and restructing of the CTX
APIs.

## Usage

``` r
get_ecotox_hazard_by_dtxsid_batch(
  DTXSID = NULL,
  API_key = NULL,
  rate_limit = 0L,
  Server = hazard_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  A list of chemical identifier DTXSIDs.

- API_key:

  The user-specific API key.

- rate_limit:

  Number of seconds to wait between each request

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A data.table containing chemical ecotox hazard data.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull ecotox hazard data for multiples chemicals
dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
dtxsid_ecotox_hazard <- get_ecotox_hazard_by_dtxsid_batch(DTXSID = dtxsid)
}
```
