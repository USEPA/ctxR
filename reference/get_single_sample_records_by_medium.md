# Get single sample records by medium

Get single sample records by medium

## Usage

``` r
get_single_sample_records_by_medium(
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

A list of search parameters and data of single sample record data by
medium.

## Examples

``` r
if (FALSE) {
#Pull single records for BPA by medium
bpa_sample_records <- get_single_sample_records_by_medium(Medium = 'surface water')
}
```
