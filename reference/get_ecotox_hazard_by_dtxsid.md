# Get ecotox hazard data by DTXSID

**\[deprecated\]**

The function was deprecated due to updates and restructing of the CTX
APIs.

## Usage

``` r
get_ecotox_hazard_by_dtxsid(
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

A data.frame containing chemical (ecotox) hazard data

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull ecotox hazard data for BPA
bpa_ecotox <- get_ecotox_hazard_by_dtxsid(DTXSID = 'DTXSID7020182')
}
```
