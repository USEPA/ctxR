# Register CTX API Key for ctxR

This page contains documentation tools related to enabling CTX API
services in R.

## Usage

``` r
showing_key()

ctxR_show_api_key()

ctxR_hide_api_key()

register_ctx_api_key(key, write = FALSE)

# S3 method for class 'ctx_credentials'
print(...)

ctx_key()

has_ctx_key()
```

## Arguments

- key:

  an API key

- write:

  if TRUE, stores the secrets provided in the .Renviron file

- ...:

  a dumped formal argument to the generic print method

## Value

- `showing_key` returns a Boolean.

- `ctxR_show_api_key()` has no return value but has the side effect of
  changing the display settings of the API key.

- `ctxR_hide_api_key()` has no return value but has the side effect of
  changing the display settings of the API key.

- `register_ctx_api_key()` has no return value but has the side effect
  of storing the API key.

- `print.ctx_credentials()` has no return value and is an S3 method for
  printing the `ctx_credentials` class.

- `ctx_key()` returns a string, either the stored API key or
  `NA_character_`.

- `has_ctx_key()` returns a Boolean.

## Details

To obtain an API key and enable services, go to
<https://www.epa.gov/comptox-tools/computational-toxicology-and-exposure-apis>.
This documentation shows you how to obtain an API key to allow access to
the CTX APIs.

To tell ctxR about your API key, use `register_ctx_api_key()`, e.g.
`register_ctx_api_key(key = 'grbwigbwoginrowgbwibgdibdvinrginiwgo')`
(that's a fake key). This will set your API key for the current session,
but if you restart R, you'll need to do it again. You can set it
permanently by setting `write = TRUE`m see the examples. If you set it
permanently it will be stored in a local file, and that will be accessed
by ctxR persistently across sessions.

Users should be aware that the API key, a string of garbled
characters/numbers/symbols, is a PRIVATE key - it uniquely identifies
and authenticates you to CTX's services. If anyone gets your API key,
they can use it to masquerade as you to CTX. To mitigate against users
inadvertently sharing their keys, by default ctxR never displays a
user's key in messages displayed to the console.

Users should be aware that ctxR has no mechanism with which to safeguard
the private key once registered with R. That is to say, once you
register your API key, any R function will have access to it. As a
consequence, ctxR will not know if another function, potentially from a
compromised package, accesses the key and uploads it to a third party.
For this reason, when using ctxR we recommend a heightened sense of
security and self-awareness: only use trusted packages, do not save the
API keys in script files, etc.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Check if API key is showing
showing_key()
}
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Toggle API key to display
ctxR_show_api_key()
}
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Toggle API key to be hidden
ctxR_hide_api_key()
}
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Register key for this session
register_ctx_api_key(key = 'YOUR API KEY')
# Register key over sessions
register_ctx_api_key(key = 'YOUR API KEY', write = TRUE)
}
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Print function for ctx_credentials class
print.ctx_credentials()
}
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Display ctx API key
ctx_key()
}
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Check whether API key is registered
has_ctx_key()
}
```
