# Chemical contains batch search

Chemical contains batch search

## Usage

``` r
chemical_contains_batch(
  word_list = NULL,
  API_key = NULL,
  rate_limit = 0L,
  verbose = FALSE,
  top = NULL
)
```

## Arguments

- word_list:

  A list of character strings of chemical names or portion of chemical
  names

- API_key:

  User-specific API key

- rate_limit:

  Number of seconds to wait between each request

- verbose:

  A logical indicating if some “progress report” should be given.

- top:

  The number of results to return if there are multiple results
  available

## Value

A named list of data.frames of chemicals and related values matching the
query parameters. The data.frames under the 'valid' entry contain
chemical information for successful requests while the data.frames under
the 'invalid' entry contain data.frames with chemical name suggestions
based on the input search values.

## Author

Paul Kruse, Kristin Issacs

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemicals that contain substrings
substring_chemicals <- chemical_contains_batch(word_list = c('TXDIS702018',
                                                             'DTXSID70201'))
}
```
