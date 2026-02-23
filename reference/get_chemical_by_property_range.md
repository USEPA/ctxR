# Get chemicals by property and its value range

Get chemicals by property and its value range

## Usage

``` r
get_chemical_by_property_range(
  start = NULL,
  end = NULL,
  property = NULL,
  API_key = NULL,
  Server = chemical_api_server,
  verbose = FALSE
)
```

## Arguments

- start:

  A numeric value, the beginning of the range

- end:

  A numeric value, the end of the range

- property:

  A string, the property in question

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A data.frame containing chemical information for chemicals matching the
search criteria.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemicals with a given property in a set range
density <- get_chemical_by_property_range(start = 1.311, end = 1.313,
                                          property = 'Density')
}
```
