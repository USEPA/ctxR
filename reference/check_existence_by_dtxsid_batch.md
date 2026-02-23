# Check existence by DTXSID batch

Check existence by DTXSID batch

## Usage

``` r
check_existence_by_dtxsid_batch(
  DTXSID = NULL,
  API_key = NULL,
  rate_limit = 0L,
  Server = chemical_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  The chemical identifier DTXSIDs

- API_key:

  The user-specific API key

- rate_limit:

  Number of seconds to wait between each request.

- Server:

  The root address of the API endpoint

- verbose:

  A logical indicating whether some "progress report" should be given.

## Value

A data.table of information detailing valid and invalid DTXSIDs.

## Examples

``` r
if (FALSE) {
dtxsids <- c('DTXSID7020182F', 'DTXSID7020182', 'DTXSID0020232F')
existence <- check_existence_by_dtxsid_batch(DTXSID = dtxsids)
}
```
