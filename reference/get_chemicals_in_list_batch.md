# Get chemicals in a given chemical list batch

Get chemicals in a given chemical list batch

## Usage

``` r
get_chemicals_in_list_batch(
  list_names = NULL,
  API_key = NULL,
  rate_limit = 0L,
  verbose = FALSE
)
```

## Arguments

- list_names:

  A list of names of chemical lists.

- API_key:

  The user-specific API key.

- rate_limit:

  Number of seconds to wait between each request

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A named list of data.frames each containing chemicals in the
corresponding chemical lists.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemicals in lists for multiple lists
chemicals_in_lists <- get_chemicals_in_list_batch(list_names = c('CCL4', 'NATADB'))
}
```
