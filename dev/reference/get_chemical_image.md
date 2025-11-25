# Get image file by DTXSID or DTXCID

Get image file by DTXSID or DTXCID

## Usage

``` r
get_chemical_image(
  DTXSID = NULL,
  DTXCID = NULL,
  gsid = NULL,
  SMILES = NULL,
  format = "",
  API_key = NULL,
  Server = chemical_api_server,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  Chemical identifier DTXSID

- DTXCID:

  Chemical identifier DTXCID

- gsid:

  DSSTox Generic Substance Identifier

- SMILES:

  Chemical identifier SMILES

- format:

  The image type, either "png" or "svg". If left blank, will default to
  "png".

- API_key:

  The user-specific API key

- Server:

  The root address for the API endpoint

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A Large array of three dimensions representing an image. For displaying
this, one may use
[`png::writePNG()`](https://rdrr.io/pkg/png/man/writePNG.html) or
[`countcolors::plotArrayAsImage()`](https://rdrr.io/pkg/countcolors/man/plotArrayAsImage.html)
among many such functions.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull chemical image for BPA by dtxsid
bpa_image_matrix <- get_chemical_image(DTXSID = 'DTXSID7020182')
if (requireNamespace("countcolors", quietly = TRUE)){
  countcolors::plotArrayAsImage(bpa_image_matrix)
}
# Pull chemical image for BPA by dtxcid
bpa_image_matrix <- get_chemical_image(DTXCID = 'DTXCID30182')
if (requireNamespace("countcolors", quietly = TRUE)){
  countcolors::plotArrayAsImage(bpa_image_matrix)
}
}
```
