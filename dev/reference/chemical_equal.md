# Chemical equal

Chemical equal

## Usage

``` r
chemical_equal(
  word = NULL,
  API_key = NULL,
  Server = chemical_api_server,
  verbose = FALSE
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

## Value

A data.frame of chemicals and related values matching the query
parameters

## Author

Paul Kruse, Kristin Issacs

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemicals with matching DTXSID
bpa_dtxsid <- chemical_equal(word = 'DTXSID7020182')
}
```
