# Retrieve exposure related functional use data

Retrieve exposure related functional use data

## Usage

``` r
get_exposure_functional_use(
  DTXSID = NULL,
  API_key = NULL,
  Server = exposure_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  Chemical identifier DTXSID

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A data.frame of functional use data.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull functional use data for BPA
bpa <- get_exposure_functional_use(DTXSID = 'DTXSID7020182')
}
```
