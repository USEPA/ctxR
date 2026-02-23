# Get ms ready by mass batch search

Get ms ready by mass batch search

## Usage

``` r
get_msready_by_mass_batch(
  start_list = NULL,
  end_list = NULL,
  API_key = NULL,
  rate_limit = 0L,
  verbose = FALSE
)
```

## Arguments

- start_list:

  A numeric list of starting values for mass range

- end_list:

  A numeric list of ending values for mass range

- API_key:

  The user-specific API key

- rate_limit:

  Number of seconds to wait between each request

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A named list of character lists with DTXSIDs with msready masses falling
within the given ranges.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull msready chemicals by mass ranges
msready_data <- get_msready_by_mass_batch(start_list = c(200.9, 200.95),
                                          end_list = c(200.95, 201.00))
}
```
