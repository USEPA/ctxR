# Get experimental physical-chemical property data

Get experimental physical-chemical property data

## Usage

``` r
get_chem_props_exp(
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

  A logical indicating if some "progress report" should be given.

## Value

A data.frame of experimental physchem property data

## Examples

``` r
if (FALSE) {
# Get experimental physchem properties for BPA
bpa_exp_props <- get_chem_props_exp(DTXSID = 'DTXSID7020182')
}
```
