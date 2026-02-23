# Get chemical lists by type

Get chemical lists by type

## Usage

``` r
get_chemical_lists_by_type(
  type = NULL,
  Projection = "",
  API_key = NULL,
  Server = chemical_api_server,
  verbose = FALSE
)
```

## Arguments

- type:

  The type of list. This is a case sensitive parameter and returns lists
  only for values of "federal", "international", "state", and "other".

- Projection:

  Optional parameter controlling return type. It takes values
  'chemicallistall' and 'chemicallistname' with the former as the
  default value.

- API_key:

  The user-specified API key.

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A data.frame containing information about lists that meet the search
criteria.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemical lists by type
federal <- get_chemical_lists_by_type(type = 'Federal')
}
```
