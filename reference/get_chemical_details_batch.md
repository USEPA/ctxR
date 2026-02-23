# Retrieve chemical details from DTXSID of DTXCID in batch search

Retrieve chemical details from DTXSID of DTXCID in batch search

## Usage

``` r
get_chemical_details_batch(
  DTXSID = NULL,
  DTXCID = NULL,
  Projection = "chemicaldetailstandard",
  API_key = NULL,
  rate_limit = 0L,
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
  'chemicaldetailall', 'chemicaldetailstandard', chemicalidentifier',
  'chemicalstructure', 'ntatoolkit', ccdchemicaldetails', 'compact'. If
  left empty or there is a mismatch, the default format will be
  'chemicaldetailstandard'.

- API_key:

  The user-specific API key

- rate_limit:

  Number of seconds to wait between each request

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A data.table (DTXSID) or a named list of data.tables (DTXCID) containing
chemical information for the chemicals with DTXSID or DTXCID matching
the input parameter.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemical details for multiple chemicals by dtxsid
dtxsids <- c('DTXSID7020182', 'DTXSID2021315')
dtxsid_details <- get_chemical_details_batch(DTXSID = dtxsid)
# Pull chemical details for multiple chemicals by dtxcid
dtxcids <- c('DTXCID30182', 'DTXCID001315')
dtxcid_details <- get_chemical_details_batch(DTXCID = dtxcids)
}
```
