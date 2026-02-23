# Get single sample records by DTXSID

Get single sample records by DTXSID

## Usage

``` r
get_single_sample_records_by_dtxsid(
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

A data.frame of single sample record data by DTXSID.

## Examples

``` r
if (FALSE) {
#Pull single sample records for BPA by DTXSID
bpa_sample_records <- get_single_sample_records_by_dtxsid(DTXSID = 'DTXSID7020182')
}
```
