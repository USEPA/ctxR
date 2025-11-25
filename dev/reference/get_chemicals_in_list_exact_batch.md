# Get chemicals in a list specified by exact characters batch search

Get chemicals in a list specified by exact characters batch search

## Usage

``` r
get_chemicals_in_list_exact_batch(
  list_names = NULL,
  words = NULL,
  API_key = NULL,
  rate_limit = 0L,
  verbose = FALSE
)
```

## Arguments

- list_names:

  The names of the lists to search.

- words:

  The search words, one for each list.

- API_key:

  The user-specific API key.

- rate_limit:

  Number of seconds to wait between each request.

- verbose:

  A logical indicating if some "progress report" should be given.

## Value

A named list of lists, with names corresponding to search terms and
lists corresponding to DTXSIDs associated to the search terms

## Examples

``` r
if (FALSE) {
# Search `CCL4` for chemicals exactly matching with 'Bisphenol A' and
# `BIOSOLIDS2021` for chemicals exactly matching with 'Bisphenol A'.
bisphenol_a <- get_chemicals_in_list_exact_batch(list_names = c('CCL4',
                                                              'BIOSOLIDS2021'),
                                                 words = rep('Bisphenol A', 2))
}
```
