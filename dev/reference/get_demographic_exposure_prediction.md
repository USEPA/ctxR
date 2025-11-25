# Get demographic exposure prediction data

Get demographic exposure prediction data

## Usage

``` r
get_demographic_exposure_prediction(
  DTXSID = NULL,
  API_key = NULL,
  Server = exposure_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  The chemical identifier DTXSID

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some "progress report" should be given.

## Value

A data.table of demographic exposure prediction data.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull general exposure prediction data for BPA
bpa <- get_demographic_exposure_prediction(DTXSID = 'DTXSID7020182')
}
```
