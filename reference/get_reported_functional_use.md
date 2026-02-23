# Get Reported Functional Use

Get Reported Functional Use

## Usage

``` r
get_reported_functional_use(
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

A data.frame containing reported functional use data.

## Examples

``` r
if (FALSE) {
# Get reported functional use data for Caffeine
get_reported_functional_use('DTXSID0020232')
}
```
