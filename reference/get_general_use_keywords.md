# Get General Use Keywords

Get General Use Keywords

## Usage

``` r
get_general_use_keywords(
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

A data.frame containing general use keywords.

## Examples

``` r
if (FALSE) {
# Get general use keywords for Caffeine
get_general_use_keywords('DTXSID0020232')
}
```
