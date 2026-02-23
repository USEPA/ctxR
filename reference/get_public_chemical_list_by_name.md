# Get chemical list by name

Get chemical list by name

## Usage

``` r
get_public_chemical_list_by_name(
  list_name = NULL,
  Projection = "",
  API_key = NULL,
  Server = chemical_api_server,
  verbose = FALSE
)
```

## Arguments

- list_name:

  The name of the list of chemicals

- Projection:

  Optional parameter controlling return type. It takes values
  chemicallistall' and 'chemicallistname' with the former as the default
  value.

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A data.frame containing information about the chemical list. Note, this
is not the chemical list itself. To access the chemicals in the list,
use
[`get_chemicals_in_list`](https://usepa.github.io/ctxR/reference/get_chemicals_in_list.md).

## See also

[`get_chemicals_in_list`](https://usepa.github.io/ctxR/reference/get_chemicals_in_list.md)

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemical list by list name
ccl4 <- get_public_chemical_list_by_name(list_name = 'CCL4')
}
```
