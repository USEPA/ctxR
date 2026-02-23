# Retrieve chemical information

Retrieve chemical information

## Usage

``` r
get_chem_info(
  DTXSID = NULL,
  type = "",
  API_key = NULL,
  Server = chemical_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  The chemical identifier DTXSID

- type:

  This specifies whether to only grab predicted or experimental results.
  If not specified, it will grab all details. The allowable input values
  are "predicted" or "experimental".

- API_key:

  The user-specific API Key

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
# Pull chemical information for BPA
bpa <- get_chem_info(DTXSID = 'DTXSID7020182')
}
```
