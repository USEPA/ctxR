# Get image file by DTXSID or DTXCID batch

Get image file by DTXSID or DTXCID batch

## Usage

``` r
get_chemical_image_batch(
  DTXSID = NULL,
  DTXCID = NULL,
  SMILES = NULL,
  format = "",
  API_key = NULL,
  rate_limit = 0L,
  verbose = FALSE
)
```

## Arguments

- DTXSID:

  A list of chemical identifier DTXSIDs.

- DTXCID:

  A list of chemical identifier DTXCIDs.

- SMILES:

  A list of chemical identifier SMILES.

- format:

  The image type, either "png" or "svg". If left blank, will default to
  "png".

- API_key:

  The user-specific API key.

- rate_limit:

  Number of seconds to wait between each request

- verbose:

  A logical indicating if some “progress report” should be given.

## Value

A named list of Large arrays of three dimensions representing an image.
For displaying an image, one may use
[`png::writePNG()`](https://rdrr.io/pkg/png/man/writePNG.html) or
[`countcolors::plotArrayAsImage()`](https://rdrr.io/pkg/countcolors/man/plotArrayAsImage.html)
among many such functions.

## Examples

``` r
if (FALSE) { # has_ctx_key() & is.na(ctx_key() == "FAKE_KEY")
# Pull images for multiple chemicals
dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
images <- get_chemical_image_batch(DTXSID = dtxsid)
if (requireNamespace("countcolors", quietly = TRUE)){
  countcolors::plotArrayAsImage(images[[1]])
  countcolors::plotArrayAsImage(images[[2]])
}
}
```
