# Get InChI

Get InChI

## Usage

``` r
get_inchi(
  name = NULL,
  API_key = NULL,
  Server = chemical_api_server,
  verbose = FALSE
)
```

## Arguments

- name:

  Chemical name

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A string giving the associated inchi string.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
bpa_inchi <- get_inchi(name = "Bisphenol A")
}
```
