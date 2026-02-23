# ctxR Options

ctxR stores options as a named list in R's global options, i.e.
`getOption('ctxR')`. It currently stores two such options, one for CCTE
credentialing and one to suppress private API information in the URLs
printed to the screen when web queries are placed. For both of those,
see
[`register_ctx_api_key()`](https://usepa.github.io/ctxR/reference/register_ctx_api_key.md).

## Usage

``` r
set_ctxR_option(...)

has_ctxR_options()

has_ctxR_option(option)
```

## Arguments

- ...:

  a named listing of options to set

- option:

  a specific option to query, e.g. `display_api_key`

## Value

- `set_ctxR_option()` does not have a return value but has the side
  effect of setting options used by other functions.

- `has_ctxR_option()` returns a Boolean.

- `has_ctxR_options()` returns a Boolean.

## See also

[`register_ctx_api_key()`](https://usepa.github.io/ctxR/reference/register_ctx_api_key.md)

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Set ctxR options
set_ctxR_option('display_api_key' = FALSE)
}
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Check if there are options registered to 'ctxR'
has_ctxR_options()
}
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Check if a specific option is registered for 'ctxR'
has_ctxR_option('display_api_key')
}
```
