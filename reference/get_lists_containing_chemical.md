# Get chemical lists containing given chemical

Get chemical lists containing given chemical

## Usage

``` r
get_lists_containing_chemical(
  DTXSID = NULL,
  API_key = NULL,
  Server = chemical_api_server,
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

  A logical indicating if some “progress report” should be given.

## Value

A list of names of chemical lists that contain the given chemical

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemical lists containing BPA
bpa_lists <- get_lists_containing_chemical(DTXSID = 'DTXSID7020182')
}
```
