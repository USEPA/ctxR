# Get httk data

Get httk data

## Usage

``` r
get_httk_data(
  DTXSID = NULL,
  API_key = NULL,
  Server = exposure_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  The chemical identifier DTXSID

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some "progress report" should be given.

## Value

A data.table of httk data for the given input chemical.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull httk data for BPA
bpa_httk <- get_httk_data(DTXSID = 'DTXSID7020182')
}
```
