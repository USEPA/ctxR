# Get mol file by DTXSID or DTXCID

Get mol file by DTXSID or DTXCID

## Usage

``` r
get_chemical_mol(
  DTXSID = NULL,
  DTXCID = NULL,
  API_key = NULL,
  Server = chemical_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  Chemical identifier DTXSID

- DTXCID:

  Chemical identifier DTXCID

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A character string giving a mol file representation

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull mol file for BPA by dtxsid
bpa_mol <- get_chemical_mol(DTXSID = 'DTXSID7020182')
# Pull mol file for BPA by dtxcid
bpa_mol <- get_chemical_mol(DTXCID = 'DTXCID30182')
}
```
