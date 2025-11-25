# Get predicted physical-chemical property data

Get predicted physical-chemical property data

## Usage

``` r
get_chem_props_pred(
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

A data.frame of predicted physchem property data

## Examples

``` r
if (FALSE) {
# Get predicted physchem properties for BPA
bpa_pred_props <- get_chem_props_pred(DTXSID = 'DTXSID7020182')
}
```
