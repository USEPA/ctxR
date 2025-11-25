# Get msready by DTXCID

Get msready by DTXCID

## Usage

``` r
get_msready_by_dtxcid(
  DTXCID = NULL,
  API_key = NULL,
  Server = chemical_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXCID:

  The chemical identifier DTXCID

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A character list of DTXSIDs with DTXCIDs matching the search criteria

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemicals with matching DTXCID
dtxcid_msready <- get_msready_by_dtxcid(DTXSID = 'DTXCID30182')
}
```
