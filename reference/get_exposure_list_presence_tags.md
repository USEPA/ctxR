# Retrieve list presence tags

Retrieve list presence tags

## Usage

``` r
get_exposure_list_presence_tags(
  API_key = NULL,
  Server = exposure_api_server,
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

A data.frame with all the list presence tags and associated data.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull list presence tags
tags <- get_exposure_list_presence_tags()
}
```
