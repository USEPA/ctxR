# Get skin and eye hazard

Get skin and eye hazard

## Usage

``` r
get_skin_eye_hazard(
  DTXSID = NULL,
  API_key = NULL,
  Server = hazard_api_server,
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

A data.frame containing skin and eye hazard data.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull skin and eye hazard data for BPA
bpa_skin_eye <- get_skin_eye_hazard_batch(DTXSID = 'DTXSID7020182')
}
```
