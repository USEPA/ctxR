# Get chemical synonym batch

Get chemical synonym batch

## Usage

``` r
get_chemical_synonym_batch(
  DTXSID = NULL,
  API_key = NULL,
  rate_limit = 0L,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  A list of chemical identifier DTXSIDs

- API_key:

  The user-specific API key.

- rate_limit:

  The number of seconds to wait between requests.

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A named list of lists containing synonym information for each input
DTXSID.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull synonyms for multiple chemicals
dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
batch_synonyms <- get_chemical_synonym_batch(DTXSID = dtxsid)
}
```
