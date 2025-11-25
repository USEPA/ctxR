# Get fate by DTXSID

Get fate by DTXSID

## Usage

``` r
get_fate_by_dtxsid(
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

  A logical indicating if some “progress report” should be given.

## Value

A data.frame containing chemical information for the chemical with
DTXSID matching the input parameter.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemical fate data for BPA
bpa <- get_fate_by_dtxsid(DTXSID = 'DTXSID7020182')
}
```
