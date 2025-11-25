# Get chemical list by name batch

Get chemical list by name batch

## Usage

``` r
get_public_chemical_list_by_name_batch(
  name_list = NULL,
  Projection = "",
  API_key = NULL,
  rate_limit = 0L,
  verbose = FALSE
)
```

## Arguments

- name_list:

  A list of chemical list names.

- Projection:

  Optional parameter controlling return type. It takes values
  'chemicallistall' and 'chemicallistname' with the former as the
  default value.

- API_key:

  The user-specific API key.

- rate_limit:

  Number of seconds to wait between each request

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A named list of data.frames containing information about the chemical
lists. Note, these are not the chemical lists themselves. To access the
chemicals in a given list, use
[`get_chemicals_in_list`](https://usepa.github.io/ctxR/dev/reference/get_chemicals_in_list.md).

## See also

[`get_chemicals_in_list`](https://usepa.github.io/ctxR/dev/reference/get_chemicals_in_list.md)

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemical list information by list names
list_info <- get_public_chemical_list_by_name_batch(name_list = c('CCL4',
                                                                  'NATADB'))
}
```
