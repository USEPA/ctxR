# Get msready by mass and error offset

Get msready by mass and error offset

## Usage

``` r
get_msready_by_mass_with_error_batch(
  masses = NULL,
  error = NULL,
  API_key = NULL,
  rate_limit = 0,
  verbose = FALSE
)
```

## Arguments

- masses:

  A numeric list of masses.

- error:

  The mass offset value.

- API_key:

  The user-specific API key.

- rate_limit:

  Number of seconds to wait between each request

- verbose:

  A logical indicating if some "progress report" should be given.

## Value

A list (of lists) of DTXSIDs, with a list returned for each input mass
value.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
#Pull chemicals by msready mass and error offset
msready_data <- get_msready_by_mass_with_error_batch(masses = c(226, 228),
                                                     error = 4)
}
```
