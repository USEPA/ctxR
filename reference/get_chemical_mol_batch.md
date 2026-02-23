# Get mol file by DTXSID or DTXCID batch

Get mol file by DTXSID or DTXCID batch

## Usage

``` r
get_chemical_mol_batch(
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

A named list of character strings giving a mol file representations of
the given input chemicals.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull mol files for multiple chemicals by DTXSID
dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
mol_files <- get_chemical_mol_batch(DTXSID = dtxsid)
# Pull mol files for multiple chemicals by DTXCID
dtxcid <- c('DTXCID30182', 'DTXCID001315')
mol_files <- get_chemical_mol_batch(DTXCID = dtxcid)
}
```
