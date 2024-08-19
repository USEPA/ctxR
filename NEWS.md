# ctxR (development version)


## Minor improvements and fixes

* Adjusted `chemical_contains()`, `chemical_equal()`, and 
`chemical_starts_with()` to handle http 400 errors and returned information from
those requests. Adjusted the `chemical_contains_batch()`, 
`chemical_equals_batch()`, and `chemical_starts_with_batch()` to return named
lists of valid and invalid search results, with invalid results containing 
information from 400 errors (@kisaacs1, #11).

## New features

* Added `get_httk_data()`, `get_httk_data_batch()`, 
`get_general_exposure_prediction()`, `get_general_exposure_prediction_batch()`,
`get_demographic_exposure_prediction()`, 
`get_demographic_exposure_prediction_batch()` functions. Updated the 
`Exposure.Rmd` vignette to include examples of how to use these functions (#6).


# ctxR 1.0.0

* Initial release. Renamed package from `ccdR` package for better alignment 
with US EPA CTX APIs.

