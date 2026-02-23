# Retrieve product data for exposure purposes

Retrieve product data for exposure purposes

## Usage

``` r
get_exposure_product_data(
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

A data.frame with product information relating to exposure to the given
chemical

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull exposure product data for BPA
bpa <- get_exposure_product_data(DTXSID = 'DTXSID7020182')
}
```
