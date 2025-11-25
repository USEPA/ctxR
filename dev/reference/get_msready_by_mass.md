# Get msready by mass

Get msready by mass

## Usage

``` r
get_msready_by_mass(
  start = NULL,
  end = NULL,
  API_key = NULL,
  Server = chemical_api_server,
  verbose = FALSE
)
```

## Arguments

- start:

  The starting value for mass range

- end:

  The ending value for mass range

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A list of DTXSIDs with msready mass falling within the given range.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemicals with msready mass in given range
mass_range <- get_msready_by_mass(start = 200.9, end = 200.95)
}
```
