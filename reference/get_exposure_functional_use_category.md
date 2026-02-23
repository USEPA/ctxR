# Retrieve functional use categories

Retrieve functional use categories

## Usage

``` r
get_exposure_functional_use_category(
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

A data.frame of functional use categories.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull functional use category data for BPA
functional_use_categories <- get_exposure_functional_use_category()
}
```
