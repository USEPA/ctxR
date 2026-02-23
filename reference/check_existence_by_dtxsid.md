# Check existence by DTXSID

Check existence by DTXSID

## Usage

``` r
check_existence_by_dtxsid(
  DTXSID = NULL,
  API_key = NULL,
  Server = chemical_api_server,
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

  A logical indicating whether some "progress report" should be given.

## Value

A data.table with information on whether the input DTXSID is valid, and
where to find more information on the chemical when the DTXSID is valid.

## Examples

``` r
if (FALSE) {
# DTXSID for bpa
bpa <- check_existence_by_dtxsid('DTXSID7020182')
# False DTXSID
false_res <- check_existence_by_Dtxsid('DTXSID7020182f')
}
```
