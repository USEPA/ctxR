# Get Production Volume

Get Production Volume

## Usage

``` r
get_production_volume(
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

A data.frame containing production volume data.

## Examples

``` r
if (FALSE) {
# Get production volume data for Caffeine
get_production_volume('DTXSID0020232')
}
```
