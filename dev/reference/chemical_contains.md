# Chemical contains

Chemical contains

## Usage

``` r
chemical_contains(
  word = NULL,
  API_key = NULL,
  Server = chemical_api_server,
  verbose = FALSE,
  top = NULL
)
```

## Arguments

- word:

  A character string of a chemical identifier or portion of a chemical
  identifier. Identifiers can be a chemical name, dtxsid, dtxcid, casrn,
  or inchikey.

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

- top:

  The number of results to return if there are multiple results
  available

## Value

A data.frame of chemicals and related values matching the query
parameters

## Author

Paul Kruse, Kristin Issacs

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemicals that contain substring
substring_chemicals <- chemical_contains(word = 'TXSID702018')
}
```
