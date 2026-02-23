# Retrieve document data and list presence tags for chemicals batch

Retrieve document data and list presence tags for chemicals batch

## Usage

``` r
get_exposure_list_presence_tags_by_dtxsid_batch(
  DTXSID = NULL,
  API_key = NULL,
  rate_limit = 0L,
  Server = exposure_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  Chemical identifier DTXSID

- API_key:

  The user-specific API key

- rate_limit:

  Number of seconds to wait between each request

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A named list of data.frames, each containing exposure list presence tags
use data for each input DTXSID.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull exposure functional use data for multiple chemicals
dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
exp_list_tags <- get_exposure_list_presence_tags_by_dtxsid_batch(DTXSID = dtxsid)
}
```
