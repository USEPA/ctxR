# Retrieve bioactivity data from DTXSID, AEID, SPID, or m4id

Retrieve bioactivity data from DTXSID, AEID, SPID, or m4id

## Usage

``` r
get_bioactivity_details(
  DTXSID = NULL,
  AEID = NULL,
  SPID = NULL,
  m4id = NULL,
  API_key = NULL,
  Server = bioactivity_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  The chemical identifier DTXSID

- AEID:

  The assay endpoint identifier AEID

- SPID:

  The ChemSpider chemical input

- m4id:

  The chemical identifier m4id

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A data.frame containing bioactivity information for the chemical or
assay endpoint with identifier matching the input parameter.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull BPA bioactivity details
bpa <- get_bioactivity_details(DTXSID = 'DTXSID7020182')
# Pull assay bioactivity details
assay <- get_bioactivity_details(AEID = 159)
}
```
