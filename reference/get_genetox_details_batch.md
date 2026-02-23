# Get genetox details batch

Get genetox details batch

## Usage

``` r
get_genetox_details_batch(
  DTXSID = NULL,
  API_key = NULL,
  rate_limit = 0L,
  Server = hazard_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  The chemical identifier DTXSIDs

- API_key:

  The user-specific API key.

- rate_limit:

  Number of seconds to wait between requests

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A data.table of genetox detail data for each input DTXSID.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull genetox details data for multiples chemicals
dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
dtxsid_genetox_details_hazard <- get_genetox_details_batch(DTXSID = dtxsid)
}
```
