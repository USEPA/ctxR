# Retrieve all assays

Retrieve all assays

## Usage

``` r
get_all_assays(
  API_key = NULL,
  Server = bioactivity_api_server,
  verbose = FALSE
)
```

## Arguments

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A data.frame containing all the assays and associated information

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Retrieve all assays
assays <- get_all_assays()
}
```
