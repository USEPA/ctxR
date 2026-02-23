# Get mrv file by DTXSID or DTXCID

Get mrv file by DTXSID or DTXCID

## Usage

``` r
get_chemical_mrv(
  DTXSID = NULL,
  DTXCID = NULL,
  API_key = NULL,
  Server = chemical_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  The chemical identifier DTXSID

- DTXCID:

  The chemical identifier DTXCID

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

XML file format for representing a mrv file.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull mrv file for BPA by dtxsid
bpa_mrv <- get_chemical_mrv(DTXSID = 'DTXSID7020182')
# Pull mrv file for BPA by dtxcid
bpa_mrv <- getchemical_mrv(DTXCID = 'DTXCID30182')
}
```
