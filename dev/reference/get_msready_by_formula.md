# Get msready by formula

Get msready by formula

## Usage

``` r
get_msready_by_formula(
  formula = NULL,
  API_key = NULL,
  Server = chemical_api_server,
  verbose = FALSE
)
```

## Arguments

- formula:

  A string denoting the input chemical formula

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A character list of DTXSIDs with chemical formulas matching the search
criteria

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemicals that match input formula
mass_formula <- get_msready_by_formula(formula = 'C16H24N2O5S')
}
```
