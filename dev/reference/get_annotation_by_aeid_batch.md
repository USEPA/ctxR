# Retrieve annotations for AEID batch

Retrieve annotations for AEID batch

## Usage

``` r
get_annotation_by_aeid_batch(
  AEID = NULL,
  API_key = NULL,
  Server = NULL,
  rate_limit = 0L,
  verbose = FALSE
)
```

## Arguments

- AEID:

  A list of AEID identifiers

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- rate_limit:

  Number of seconds to wait between each request

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A named list of data.frames containing annotation information for the
assays with AEID matching the input parameter.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Get annotations for multiple aeids
aeid_annotations <- get_annotation_by_aeid_batch(AEID = c(159, 160))
}
```
