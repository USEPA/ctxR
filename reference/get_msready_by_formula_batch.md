# Get msready by formula batch search

Get msready by formula batch search

## Usage

``` r
get_msready_by_formula_batch(
  formula_list = NULL,
  API_key = NULL,
  rate_limit = 0L,
  verbose = FALSE
)
```

## Arguments

- formula_list:

  A list of strings denoting the input chemicals formulas

- API_key:

  The user-specific API key

- rate_limit:

  Number of seconds to wait between each request

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A named list of character lists of DTXSIDs with chemical formulas
matching the search criteria

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull msready data for several chemical formulas
msready_data <- get_msready_by_formula_batch(formula_list = c('C16H24N2O5S',
                                                              'C15H16O2'))
}
```
