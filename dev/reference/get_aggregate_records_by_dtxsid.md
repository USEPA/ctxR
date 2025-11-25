# Get aggregate records by DTXSID

Get aggregate records by DTXSID

## Usage

``` r
get_aggregate_records_by_dtxsid(
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

A data.frame of aggregate record data by DTXSID.

## Examples

``` r
if (FALSE) {
#Pull aggregate records for BPA by DTXSID
bpa_agg_records <- get_aggregate_records_by_dtxsid(DTXSID = 'DTXSID7020182')
}
```
