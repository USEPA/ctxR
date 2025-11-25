# Get cancer hazard

Get cancer hazard

## Usage

``` r
get_cancer_hazard(
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

A data.frame of cancer hazard data related to the input DTXSID.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull cancer hazard data for BPA
bpa_cancer <- get_cancer_hazard(DTXSID = 'DTXSID7020182')
}
```
