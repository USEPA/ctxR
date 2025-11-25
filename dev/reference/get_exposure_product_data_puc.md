# Retrieve product use categories related to exposure

Retrieve product use categories related to exposure

## Usage

``` r
get_exposure_product_data_puc(
  API_key = NULL,
  Server = exposure_api_server,
  verbose = FALSE
)
```

## Arguments

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A data.frame consisting of all the product use categories

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull product data use categories for BPA
puc_categories <- get_exposure_product_data_puc()
}
```
