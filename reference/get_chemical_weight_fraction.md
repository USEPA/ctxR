# Get Chemical Weight Fractions

Get Chemical Weight Fractions

## Usage

``` r
get_chemical_weight_fraction(
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

A data.frame containing chemical weight fraction data.

## Examples

``` r
if (FALSE) {
# Get chemical weight fraction data for Caffeine
get_chemical_weight_fraction('DTXSID0020232')
}
```
