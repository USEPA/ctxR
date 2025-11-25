# Get chemical synonym

Get chemical synonym

## Usage

``` r
get_chemical_synonym(
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

A named list of synonym information for the input DTXSID

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull synonyms for BPA
bpa_synonym <- get_chemical_synonym(DTXSID = 'DTXSID7020182')
}
```
