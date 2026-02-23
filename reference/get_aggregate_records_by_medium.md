# Get aggregate records by medium

Get aggregate records by medium

## Usage

``` r
get_aggregate_records_by_medium(
  Medium = NULL,
  API_key = NULL,
  Server = "https://comptox.epa.gov/ctx-api/exposure",
  pageNumber = 1,
  verbose = FALSE
)
```

## Arguments

- Medium:

  The mmdb medium of exposure.

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- pageNumber:

  Parameter for how to return data records.

- verbose:

  A logical indicating if some "progress report" should be given.

## Value

A list of search parameters and data of aggregate record data by medium.

## Examples

``` r
if (FALSE) {
#Pull aggregate records for BPA by medium
bpa_agg_records <- get_aggregate_records_by_medium(Medium = 'surface water')
}
```
