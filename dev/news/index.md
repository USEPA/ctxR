# Changelog

## ctxR (development version)

### Minor improvements and fixes

- Removed columns no longer returned from API in
  [`get_fate_by_dtxsid_batch()`](https://usepa.github.io/ctxR/dev/reference/get_fate_by_dtxsid_batch.md)
  ([\#81](https://github.com/USEPA/ctxR/issues/81))
- Correct endpoints used in experimental and predicted property batch
  functions ([\#83](https://github.com/USEPA/ctxR/issues/83))
- Update columns returned from API in `get_chemical_synonym` and
  `get_chemical_synonym_batch` functions
  ([\#85](https://github.com/USEPA/ctxR/issues/85))

## ctxR 1.1.3

CRAN release: 2025-10-08

### Major improvements and fixes

- Update api host from cloud.gov to local server throughout package
  ([\#70](https://github.com/USEPA/ctxR/issues/70))

### New Features

- Added functions for the Exposure domain CCD endpoints
  [`get_product_use_category()`](https://usepa.github.io/ctxR/dev/reference/get_product_use_category.md),
  [`get_product_use_categories_batch()`](https://usepa.github.io/ctxR/dev/reference/get_product_use_categories_batch.md),
  [`get_production_volume()`](https://usepa.github.io/ctxR/dev/reference/get_production_volume.md),
  [`get_production_volume_batch()`](https://usepa.github.io/ctxR/dev/reference/get_production_volume_batch.md),
  [`get_biomonitoring_data()`](https://usepa.github.io/ctxR/dev/reference/get_biomonitoring_data.md),
  [`get_biomonitoring_data_batch()`](https://usepa.github.io/ctxR/dev/reference/get_biomonitoring_data_batch.md),
  [`get_general_use_keywords()`](https://usepa.github.io/ctxR/dev/reference/get_general_use_keywords.md),
  [`get_general_use_keywords_batch()`](https://usepa.github.io/ctxR/dev/reference/get_general_use_keywords_batch.md),
  [`get_reported_functional_use()`](https://usepa.github.io/ctxR/dev/reference/get_reported_functional_use.md),
  [`get_reported_functional_use_batch()`](https://usepa.github.io/ctxR/dev/reference/get_reported_functional_use_batch.md),
  [`get_chemical_weight_fraction()`](https://usepa.github.io/ctxR/dev/reference/get_chemical_weight_fraction.md),
  and
  [`get_chemical_weight_fraction_batch()`](https://usepa.github.io/ctxR/dev/reference/get_chemical_weight_fraction_batch.md)
  ([\#71](https://github.com/USEPA/ctxR/issues/71)).

- Added functions for Exposure domain MMDB endpoints
  [`get_medium_categories()`](https://usepa.github.io/ctxR/dev/reference/get_medium_categories.md),
  [`get_single_sample_records_by_dtxsid()`](https://usepa.github.io/ctxR/dev/reference/get_single_sample_records_by_dtxsid.md),
  [`get_single_sample_records_by_dtxsid_batch()`](https://usepa.github.io/ctxR/dev/reference/get_single_sample_records_by_dtxsid_batch.md),
  [`get_single_sample_records_by_medium()`](https://usepa.github.io/ctxR/dev/reference/get_single_sample_records_by_medium.md),
  [`get_single_sample_records_by_medium_batch()`](https://usepa.github.io/ctxR/dev/reference/get_single_sample_records_by_medium_batch.md),
  [`get_aggregate_records_by_dtxsid()`](https://usepa.github.io/ctxR/dev/reference/get_aggregate_records_by_dtxsid.md),
  `get_aggregate_records_by_dtxsid_batch`(),
  [`get_aggregate_records_by_medium()`](https://usepa.github.io/ctxR/dev/reference/get_aggregate_records_by_medium.md),
  `get_aggregate_records_by_medium_batch`()
  ([\#71](https://github.com/USEPA/ctxR/issues/71)).

- Added examples to `ctxR_04_Exposure.Rmd` vignette for new CCD and MMDB
  endpoint functions ([\#71](https://github.com/USEPA/ctxR/issues/71)).

## ctxR 1.1.2

CRAN release: 2025-04-07

### Minor improvements and fixes

- Updated roles in `DESCRIPTION` file and Roxygen version
  ([\#57](https://github.com/USEPA/ctxR/issues/57)).

## ctxR 1.1.1

CRAN release: 2025-04-01

### Minor improvements and fixes

- Updated roles in `DESCRIPTION` file
  ([\#55](https://github.com/USEPA/ctxR/issues/55)).

## ctxR 1.1.0

CRAN release: 2025-01-08

### Minor improvements and fixes

- Updated moved URLs per CRAN request
  ([\#40](https://github.com/USEPA/ctxR/issues/40)).

- Updated exposition, corrected typos, and added references to the
  vignettes ([\#37](https://github.com/USEPA/ctxR/issues/37)).

- Fixed broken link to `CONTRIBUTING.md` in README
  ([@c1au6i0](https://github.com/c1au6i0),
  [\#37](https://github.com/USEPA/ctxR/issues/37))

- Updated
  [`get_chemical_synonym()`](https://usepa.github.io/ctxR/dev/reference/get_chemical_synonym.md),
  [`get_chemical_synonym_batch()`](https://usepa.github.io/ctxR/dev/reference/get_chemical_synonym_batch.md)
  to handle returned data more efficiently
  ([\#30](https://github.com/USEPA/ctxR/issues/30)).

- Updated documentation of internal helper functions
  [`create_data.table_chemical_details()`](https://usepa.github.io/ctxR/dev/reference/create_data.table_chemical_details.md),
  [`prepare_word()`](https://usepa.github.io/ctxR/dev/reference/prepare_word.md)
  ([\#24](https://github.com/USEPA/ctxR/issues/24)).

- Fixed
  [`get_bioactivity_details()`](https://usepa.github.io/ctxR/dev/reference/get_bioactivity_details.md)
  to handle case when `mc6Param` field of returned data is NULL
  ([\#18](https://github.com/USEPA/ctxR/issues/18)).

- Fixed
  [`get_bioactivity_details()`](https://usepa.github.io/ctxR/dev/reference/get_bioactivity_details.md)
  to handle `m4id` parameter values that do not exist
  ([\#16](https://github.com/USEPA/ctxR/issues/16)).

- Adjusted
  [`chemical_contains()`](https://usepa.github.io/ctxR/dev/reference/chemical_contains.md),
  [`chemical_equal()`](https://usepa.github.io/ctxR/dev/reference/chemical_equal.md),
  and
  [`chemical_starts_with()`](https://usepa.github.io/ctxR/dev/reference/chemical_starts_with.md)
  to handle http 400 errors and returned information from those
  requests. Adjusted the
  [`chemical_contains_batch()`](https://usepa.github.io/ctxR/dev/reference/chemical_contains_batch.md),
  `chemical_equals_batch()`, and
  [`chemical_starts_with_batch()`](https://usepa.github.io/ctxR/dev/reference/chemical_starts_with_batch.md)
  to return named lists of valid and invalid search results, with
  invalid results containing information from 400 errors
  ([@kisaacs1](https://github.com/kisaacs1),
  [\#12](https://github.com/USEPA/ctxR/issues/12)).

- Added examples in `Chemical.Rmd` vignette for
  [`get_chemical_details_batch()`](https://usepa.github.io/ctxR/dev/reference/get_chemical_details_batch.md)
  ([\#8](https://github.com/USEPA/ctxR/issues/8)).

### New features

- Added warning for missing `API_key` parameter and missing stored API
  key to all functions that wrap API endpoints
  ([\#35](https://github.com/USEPA/ctxR/issues/35)).

- Added error handling for invalid API key inputs to all functions that
  wrap an API endpoint
  ([\#33](https://github.com/USEPA/ctxR/issues/33)).

- Added functions
  [`get_all_list_types()`](https://usepa.github.io/ctxR/dev/reference/get_all_list_types.md),
  [`get_chemicals_in_list_start()`](https://usepa.github.io/ctxR/dev/reference/get_chemicals_in_list_start.md),
  [`get_chemicals_in_list_start_batch()`](https://usepa.github.io/ctxR/dev/reference/get_chemicals_in_list_start_batch.md)
  [`get_chemicals_in_list_exact()`](https://usepa.github.io/ctxR/dev/reference/get_chemicals_in_list_exact.md),
  [`get_chemicals_in_list_exact_batch()`](https://usepa.github.io/ctxR/dev/reference/get_chemicals_in_list_exact_batch.md),
  `get_chemicals_in_lists_contain()`,
  `get_chemicals_in_lists_contain_batch`(). Added `gsid` parameter to
  [`get_chemical_image()`](https://usepa.github.io/ctxR/dev/reference/get_chemical_image.md).
  Updated `Chemical.rmd` vignette to include examples for new chemical
  functions and new features
  ([\#30](https://github.com/USEPA/ctxR/issues/30)).

- Added
  [`check_existence_by_dtxsid()`](https://usepa.github.io/ctxR/dev/reference/check_existence_by_dtxsid.md),
  [`check_existence_by_dtxsid_batch()`](https://usepa.github.io/ctxR/dev/reference/check_existence_by_dtxsid_batch.md)
  functions. Updated the `Chemical.Rmd` vignette to include examples of
  how to use these functions and `ctxR` hex logo location
  ([\#28](https://github.com/USEPA/ctxR/issues/28)).

- Added parameter `limit` and default value 200 to `generate_ranges()`
  function and fixed. Fixed request limit in
  [`chemical_equal_batch()`](https://usepa.github.io/ctxR/dev/reference/chemical_equal_batch.md)
  function ([@seanthimons](https://github.com/seanthimons),
  [\#26](https://github.com/USEPA/ctxR/issues/26)).

- Added pkgdown website for development version of package
  ([\#22](https://github.com/USEPA/ctxR/issues/22)).

- Added monthly download and CRAN version badges to `README.md` and
  `README.Rmd` files ([\#14](https://github.com/USEPA/ctxR/issues/14)).

- Added
  [`get_httk_data()`](https://usepa.github.io/ctxR/dev/reference/get_httk_data.md),
  [`get_httk_data_batch()`](https://usepa.github.io/ctxR/dev/reference/get_httk_data_batch.md),
  [`get_general_exposure_prediction()`](https://usepa.github.io/ctxR/dev/reference/get_general_exposure_prediction.md),
  [`get_general_exposure_prediction_batch()`](https://usepa.github.io/ctxR/dev/reference/get_general_exposure_prediction_batch.md),
  [`get_demographic_exposure_prediction()`](https://usepa.github.io/ctxR/dev/reference/get_demographic_exposure_prediction.md),
  [`get_demographic_exposure_prediction_batch()`](https://usepa.github.io/ctxR/dev/reference/get_demographic_exposure_prediction_batch.md)
  functions. Updated the `Exposure.Rmd` vignette to include examples of
  how to use these functions
  ([\#10](https://github.com/USEPA/ctxR/issues/10)).

- Added pkgdown site for CRAN version of `ctxR`
  ([\#20](https://github.com/USEPA/ctxR/issues/20)).

## ctxR 1.0.0

CRAN release: 2024-07-04

- Initial release. Renamed package from `ccdR` package for better
  alignment with US EPA CTX APIs.
