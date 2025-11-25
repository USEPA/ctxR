# Get chemical lists by type batch search

Get chemical lists by type batch search

## Usage

``` r
get_chemical_lists_by_type_batch(
  type_list = NULL,
  Projection = "",
  API_key = NULL,
  rate_limit = 0L,
  verbose = FALSE
)
```

## Arguments

- type_list:

  A list of list types. This is a case sensitive parameter and returns
  lists only for values of "federal", "international", "state", and
  "other".

- Projection:

  Optional parameter controlling return type. It takes values
  'chemicallistall' and 'chemicallistname' with the former as the
  default value.

- API_key:

  The user-specified API key.

- rate_limit:

  Number of seconds to wait between each request

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A named list of data.frames containing information about lists that meet
the search criteria.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemical lists by type
federal_state <- get_chemical_lists_by_type_batch(type_list = c('federal',
                                                                'state'))
}
```
