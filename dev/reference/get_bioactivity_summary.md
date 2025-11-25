# Retrieve bioactivity summary for AEID

Retrieve bioactivity summary for AEID

## Usage

``` r
get_bioactivity_summary(
  AEID = NULL,
  API_key = NULL,
  Server = bioactivity_api_server,
  verbose = FALSE
)
```

## Arguments

- AEID:

  The assay endpoint indentifier AEID

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A data.frame containing summary information corresponding to the input
AEID

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull an assay bioactivity summary
aeid_1386 <- get_bioactivity_summary(AEID = 1386)
}
```
