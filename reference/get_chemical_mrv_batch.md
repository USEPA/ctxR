# Ger mrv file by DTXSID or DTXCID batch

Ger mrv file by DTXSID or DTXCID batch

## Usage

``` r
get_chemical_mrv_batch(
  DTXSID = NULL,
  DTXCID = NULL,
  API_key = NULL,
  rate_limit = 0L,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  A list of the chemical identifier DTXSIDs.

- DTXCID:

  A list of the chemical identifier DTXCIDs.

- API_key:

  The user-specific API key.

- rate_limit:

  Number of seconds to wait between each request

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A named list of XML file format for representing a mrv file for each
chemicals.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull mrv files for multiple chemicals by DTXSID
dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
mrv_files <- get_chemical_mrv_batch(DTXSID = dtxsid)
# Pull mrv files for multiple chemicals by DTXCID
dtxcid <- c('DTXCID30182', 'DTXCID001315')
mrv_files <- get_chemical_mrv_batch(DTXCID = dtxcid)
}
```
