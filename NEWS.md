# ctxR 1.1.0


## Minor improvements and fixes

* Updated exposition, corrected typos, and added references to the vignettes (#37).

* Fixed broken link to `CONTRIBUTING.md` in README (@c1au6i0, #37)

* Updated `get_chemical_synonym()`, `get_chemical_synonym_batch()` to handle returned data more efficiently (#30).

* Updated documentation of internal helper functions `create_data.table_chemical_details()`, `prepare_word()` (#24).

* Fixed `get_bioactivity_details()` to handle case when `mc6Param` field of returned data is NULL (#18).

* Fixed `get_bioactivity_details()` to handle `m4id` parameter values that do not exist (#16).

* Adjusted `chemical_contains()`, `chemical_equal()`, and 
`chemical_starts_with()` to handle http 400 errors and returned information from
those requests. Adjusted the `chemical_contains_batch()`, 
`chemical_equals_batch()`, and `chemical_starts_with_batch()` to return named lists of valid and invalid search results, with invalid results containing information from 400 errors (@kisaacs1, #12).

* Added examples in `Chemical.Rmd` vignette for  `get_chemical_details_batch()` (#8).

## New features

* Added warning for missing `API_key` parameter and missing stored API key to all functions that wrap API endpoints (#35).

* Added error handling for invalid API key inputs to all functions that wrap an API endpoint (#33).

* Added functions `get_all_list_types()`, `get_chemicals_in_list_start()`, `get_chemicals_in_list_start_batch()` `get_chemicals_in_list_exact()`, `get_chemicals_in_list_exact_batch()`, `get_chemicals_in_lists_contain()`, `get_chemicals_in_lists_contain_batch`(). Added `gsid` parameter to `get_chemical_image()`. Updated `Chemical.rmd` vignette to include examples for new chemical functions and new features (#30).

* Added `check_existence_by_dtxsid()`, `check_existence_by_dtxsid_batch()` functions. Updated the `Chemical.Rmd` vignette to include examples of how to use these functions and `ctxR` hex logo location (#28).

* Added parameter `limit` and default value 200 to `generate_ranges()` function and fixed. Fixed request limit in `chemical_equal_batch()` function (@seanthimons, #26).

* Added pkgdown website for development version of package (#22).

* Added monthly download and CRAN version badges to `README.md` and `README.Rmd` files (#14).

* Added `get_httk_data()`, `get_httk_data_batch()`, 
`get_general_exposure_prediction()`, `get_general_exposure_prediction_batch()`,
`get_demographic_exposure_prediction()`, 
`get_demographic_exposure_prediction_batch()` functions. Updated the 
`Exposure.Rmd` vignette to include examples of how to use these functions (#10).

* Added pkgdown site for CRAN version of `ctxR` (#20).


# ctxR 1.0.0

* Initial release. Renamed package from `ccdR` package for better alignment 
with US EPA CTX APIs.

