# Get Product Use Categories

Get Product Use Categories

## Usage

``` r
get_product_use_category(
  DTXSID = NULL,
  API_key = NULL,
  Server = "https://comptox.epa.gov/ctx-api/exposure",
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

A data.frame containing product use categories.

## Examples

``` r
if (FALSE) {
# Get product use categories for Caffeine
get_product_use_category('DTXSID0020232')
}
```
