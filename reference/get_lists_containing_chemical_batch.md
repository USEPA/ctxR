# Get chemical lists containing given chemical batch

Get chemical lists containing given chemical batch

## Usage

``` r
get_lists_containing_chemical_batch(
  chemical_list = NULL,
  API_key = NULL,
  rate_limit = 0L,
  verbose = FALSE
)
```

## Arguments

- chemical_list:

  A list of the chemical identifier DTXSIDs.

- API_key:

  The user-specific API key.

- rate_limit:

  Number of seconds to wait between each request

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A named list of chemical lists that contain the given chemicals.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull lists containing chemicals for multiple chemicals
lists <- get_lists_containing_chemical_batch(chemical_list = c('DTXSID7020182',
                                                               'DTXSID2021315'))
}
```
