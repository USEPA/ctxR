# Get Summary information on chemical properties

Get Summary information on chemical properties

## Usage

``` r
get_chem_props_summary(
  DTXSID = NULL,
  API_key = NULL,
  Server = chemical_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  The chemical identifier DTXSID.

- API_key:

  The user-specific API key.

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some "progress report" should be given.

## Value

A data.frame of summary data for chemical properties.

## Examples

``` r
if (FALSE) {
# Get summary data for BPA
bpa_props_summary <- get_chem_props_summary(DTXSID = 'DTXSID7020182')
}
```
