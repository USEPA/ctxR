# Chemical equal batch search

Chemical equal batch search

## Usage

``` r
chemical_equal_batch(
  word_list = NULL,
  API_key = NULL,
  rate_limit = 0L,
  verbose = FALSE
)
```

## Arguments

- word_list:

  A list of character strings of chemical names or portion of chemical
  names, DTXSIDs, CASRNs, InChIKeys.

- API_key:

  User-specific API key

- rate_limit:

  Number of seconds to wait between each request

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A named list of data.tables of chemicals and related values matching the
query parameters. The list contains two entries, 'valid' and 'invalid';
'valid', contains a data.table of the results of the the searched
chemical that were found in the databases; 'invalid' contains a
data.table with 'suggestions' for each searched valued that did not
return a chemical.

## Author

Paul Kruse, Kristin Issacs

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemicals that match input strings
bpa <- chemical_equal_batch(word_list = c('DTXSID7020182', 'DTXCID30182'))
}
```
