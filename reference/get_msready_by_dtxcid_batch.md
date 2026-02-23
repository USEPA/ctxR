# Get msready by DTXCID batch search

Get msready by DTXCID batch search

## Usage

``` r
get_msready_by_dtxcid_batch(
  DTXCID = NULL,
  API_key = NULL,
  rate_limit = 0L,
  verbose = FALSE
)
```

## Arguments

- DTXCID:

  A list of chemical identifier DTXCIDs

- API_key:

  A user-specific API key

- rate_limit:

  Number of seconds to wait between each request

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A named list of character lists of DTXSIDs with DTXCIDs matching the
search criteria

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull msready chemicals matching specific DTXCID
dtxcid_msready <- get_msready_by_dtxcid_batch(DTXCID = c('DTXCID30182',
                                                         'DTXCID001315'))
}
```
