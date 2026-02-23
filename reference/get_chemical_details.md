# Retrieve chemical details from DTXSID of DTXCID

Retrieve chemical details from DTXSID of DTXCID

## Usage

``` r
get_chemical_details(
  DTXSID = NULL,
  DTXCID = NULL,
  Projection = "chemicaldetailstandard",
  API_key = NULL,
  Server = chemical_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  The chemical identifier DTXSID

- DTXCID:

  The chemical identifier DTXCID

- Projection:

  The format and chemical detail data returned. Allowed values are
  'chemicaldetailall', 'chemicaldetailstandard', 'chemicalidentifier',
  'chemicalstructure', 'ntatoolkit', 'ccdchemicaldetails', 'compact'. If
  left empty or there is a mismatch, the default format will be
  'chemicaldetailstandard'.

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A data.table containing chemical information for the chemical with
DTXSID matching the input parameter.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemical details for BPA
bpa <- get_chemical_details(DTXSID = 'DTXSID7020182')
}
```
