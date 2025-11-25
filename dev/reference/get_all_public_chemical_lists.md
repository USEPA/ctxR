# Get all public chemical lists

Get all public chemical lists

## Usage

``` r
get_all_public_chemical_lists(
  Projection = "",
  API_key = NULL,
  Server = chemical_api_server,
  verbose = FALSE
)
```

## Arguments

- Projection:

  Optional parameter controlling return type. It takes values
  chemicallistall' and 'chemicallistname' with the former as the default
  value.

- API_key:

  The user-specific api key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A data.frame containing information on all public chemical lists
available from the CTX chemical api.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull all chemical lists
all_lists <- get_all_public_chemical_lists()
}
```
