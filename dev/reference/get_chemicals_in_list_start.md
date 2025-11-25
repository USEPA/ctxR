# Get chemicals in a list specified by starting characters

Get chemicals in a list specified by starting characters

## Usage

``` r
get_chemicals_in_list_start(
  list_name = NULL,
  word = NULL,
  API_key = NULL,
  Server = chemical_api_server,
  verbose = FALSE
)
```

## Arguments

- list_name:

  The name of the list to search

- word:

  The starting characters to match chemicals in the given list

- API_key:

  The user-specific api key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some "progress report" should be given.

## Value

A list of DTXSIDs matching the list and search word criteria

## Examples

``` r
if (FALSE) {
bis_biosolids_2021 <- get_chemicals_in_list_start(list_name = 'BIOSOLIDS2021',
                                                  word = 'Bi')
}
```
