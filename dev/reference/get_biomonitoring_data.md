# Get Biomonitoring data

Get Biomonitoring data

## Usage

``` r
get_biomonitoring_data(
  DTXSID = NULL,
  API_key = NULL,
  Projection = "",
  Server = "https://comptox.epa.gov/ctx-api/exposure",
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  The chemical identifier DTXSID

- API_key:

  The user-specific API key

- Projection:

  Optional parameter controlling return type.

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some "progress report" should be given.

## Value

A data.frame containing general use keywords.

## Examples

``` r
if (FALSE) {
# Get biomonitoring data for Caffeine
get_biomonitoring_data('DTXSID0020232')
}
```
