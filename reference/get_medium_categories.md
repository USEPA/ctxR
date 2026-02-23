# Retrieve MMDB medium categories

Retrieve MMDB medium categories

## Usage

``` r
get_medium_categories(
  API_key = NULL,
  Server = "https://comptox.epa.gov/ctx-api/exposure",
  verbose = FALSE
)
```

## Arguments

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some "progress report" should be given.

## Value

A data.frame of harmonized medium categories from MMDB and relevant
descriptions.

## Examples

``` r
if (FALSE) {
# Retrieve medium categories and descriptions
get_medium_categories()
}
```
