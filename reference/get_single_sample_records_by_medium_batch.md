# Get Single Sample Records by medium via batch

Get Single Sample Records by medium via batch

## Usage

``` r
get_single_sample_records_by_medium_batch(
  Medium = NULL,
  API_key = NULL,
  rate_limit = 0L,
  Server = "https://comptox.epa.gov/ctx-api/exposure",
  verbose = FALSE
)
```

## Arguments

- Medium:

  The MMDB medium of exposure

- API_key:

  The user-specific API key

- rate_limit:

  Number of seconds to wait between each request

- Server:

  The root address for the API endpoint

- verbose:

  A logicial indicating if some "progress report" should be given.

## Value

A list of data.frames containing single sample records data for each
input medium.

## Examples

``` r
if (FALSE) {
# Retrieve single sample records data for 'surface water' and 'soil'
get_single_sample_records_by_medium_batch(Medium = c('surface water', 'soil'))
}
```
