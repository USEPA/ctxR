# Retrieve chemicals by property and value range in batch search

Retrieve chemicals by property and value range in batch search

## Usage

``` r
get_chemical_by_property_range_batch(
  start_list = NULL,
  end_list = NULL,
  property_list = NULL,
  API_key = NULL,
  rate_limit = 0L,
  verbose = FALSE
)
```

## Arguments

- start_list:

  Numeric values, the beginning of the range

- end_list:

  Numeric values, the end of the range

- property_list:

  Strings, the properties being queried

- API_key:

  The user-specific API key

- rate_limit:

  Number of seconds to wait between each request

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A named list of data.frames containing chemical information for the
chemicals matching the search criteria.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemicals by property ranges
prop_ranges <- get_chemical_by_property_range_batch(start_list = c(1.311,
                                                                   211.99),
                                                    end_list = c(1.313,
                                                                     212.01),
                                                    property_list = c('Density',
                                                                      'Boiling Point'))
}
```
