# Retrieve annotations for AEID

Retrieve annotations for AEID

## Usage

``` r
get_annotation_by_aeid(
  AEID = NULL,
  API_key = NULL,
  Server = bioactivity_api_server,
  verbose = FALSE
)
```

## Arguments

- AEID:

  The assay endpoint identifier AEID

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A data.frame containing the annotated assays corresponding to the input
AEID parameter

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Retrieve annotation for an assay
annotation <- get_annotation_by_aeid(AEID = 159)
}
```
