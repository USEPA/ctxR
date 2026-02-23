# Get chemicals in a given chemical list

Get chemicals in a given chemical list

## Usage

``` r
get_chemicals_in_list(
  list_name = NULL,
  API_key = NULL,
  Server = chemical_api_server,
  verbose = FALSE
)
```

## Arguments

- list_name:

  The name of the list of chemicals

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A data.frame of the chemical list

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Retrieve chemicals contained in chemical list 'CCL4'
ccl4_chemicals <- get_chemicals_in_list(list_name = 'CCL4')
}
```
