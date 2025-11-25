# Retrieve chemical information in batch search

Retrieve chemical information in batch search

## Usage

``` r
get_chem_info_batch(
  DTXSID = NULL,
  type = "",
  API_key = NULL,
  rate_limit = 0L,
  Server = chemical_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  A vector of chemical identifier DTXSIDs

- type:

  A vector of type used in get_chem_info(). This specifies whether to
  only grab predicted or experimental results. If not specified, it will
  grab all details. The allowable input values are "", predicted", or
  "experimental".

- API_key:

  The user-specific API key.

- rate_limit:

  Number of seconds to wait between each request

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A data.table containing chemical information for the chemicals with
DTXSID matching the input parameter.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemical info for multiple chemicals
chem_info <- get_chem_info_batch(DTXSID = c('DTXSID7020182',
                                            'DTXSID2021315'))
}
```
