# Retrieve document data and list presence tags for a chemical

Retrieve document data and list presence tags for a chemical

## Usage

``` r
get_exposure_list_presence_tags_by_dtxsid(
  DTXSID = NULL,
  API_key = NULL,
  Server = exposure_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  The chemical identifier DTXSID

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A data.frame of document information and list presence tags

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull list presence tags for BPA
bpa <- get_exposure_list_presence_tags(DTXSID = 'DTXSID7020182')
}
```
