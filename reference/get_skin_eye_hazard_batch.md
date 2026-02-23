# Get skin and eye hazard batch

Get skin and eye hazard batch

## Usage

``` r
get_skin_eye_hazard_batch(
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

  Number of seconds to wait between each request

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A named list of data.frames containing skin and eye hazard data for each
input DTXSID.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull skin eye hazard data for multiples chemicals
dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
dtxsid_skin_eye_hazard <- get_skin_eye_hazard_batch(DTXSID = dtxsid)
}
```
