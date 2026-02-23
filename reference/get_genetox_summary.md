# Get genetox summary

Get genetox summary

## Usage

``` r
get_genetox_summary(
  DTXSID = NULL,
  API_key = NULL,
  Server = hazard_api_server,
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

A data.frame of genetox summary data related to the input DTXSID.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull genetox summary for BPA
bpa_genetox_summary <- get_genetox_summary(DTXSID = 'DTXSID7020182')
}
```
