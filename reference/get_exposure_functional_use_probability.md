# Retrieve probability of exposure for functional use category

Retrieve probability of exposure for functional use category

## Usage

``` r
get_exposure_functional_use_probability(
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

  A logical indicating if some “progress report” should be given.

## Value

A data.frame with probabilities corresponding to various routes of
exposure related to functional use.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull functional use probability data for BPA
bpa <- get_exposure_functional_use_probability(DTXSID = 'DTXSID7020182')
}
```
