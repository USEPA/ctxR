# Get predicted physical-chemical property data via batch

Get predicted physical-chemical property data via batch

## Usage

``` r
get_chem_props_pred_batch(
  DTXSID = NULL,
  API_key = NULL,
  rate_limit = 0L,
  Server = chemical_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  The chemical identifier DTXSID

- API_key:

  The user-specific API key

- rate_limit:

  Number of seconds to wait between each request

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some "progress report" should be given.

## Value

A data.table of predicted physchem property data

## Examples

``` r
if (FALSE) {
# Get predicted physchem properties for BPA and Caffeine
chem_props_pred <- get_chem_props_pred(DTXSID = c('DTXSID7020182',
                                                  'DTXSID0020232'))
}
```
