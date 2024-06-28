# ccdR (development version)

## Breaking changes

* Currently no reverse dependencies and no breaking changes.

## New features

* New `get_bioactivity_endpoint_status()` and `get_hazard_endpoint_status()` 
retrieve Bioactivity and Hazard endpoint statuses, resepctively (#61).

* New `get_chemical_endpoint_status()` provides Chemical API status (#49).

* New `get_inchi()` and `get_inchikey()` functions retrieve inchi and inchikey
data (#49).

* New `get_msready_by_mass_with_error_batch()` retrieves msready mass data with
mass error option (#49).

* New `get_smiles()` retrieves smiles data (#49).

* New tests included for  `chemical_equal_batch()`, `chemical_contains()`,
`chemical_contains_batch()`, `get_inchi()`, `get_inchikey()`, `get_smiles()`,
and `get_msready_by_mass_with_error_batch()` (#49).

* New `get_exposure_endpoint_status()` provides Exposure API status (#46).

* New `get_exposure_functional_use()` and `get_exposure_functional_use_batch()`
retrieves functional use data (#46) and `get_exposure_functional_use_category()`
retrieves functional use categories (#46).

* New `get_exposure_functional_use_probability()` and 
`get_exposure_functional_use_probability_batch()` retrieves functional use
probability data (#46).

* New `get_exposure_list_presence_tags_by_dtxsid()` and 
`get_exposure_list_presence_tags_by_dtxsid_batch()` retrieve list presence tag 
data and `get_exposure_list_presence_tags()` retrieves list presence tag details 
(#46).

* New `get_exposure_product_data()` and `get_exposure_product_data_batch()` 
retrieve exposure product data and `get_exposure_product_data_puc()` 
retrieves exposure product data PUCs (#46).

* New `test-exposure-APIs.R` and `test-exposure-APIs-batch.R` for Exposure API 
functions (#46).

* New `Exposure.Rmd` vignette (#46).



## Minor improvements and fixes

* Added badges to `Reamdme.Rmd` and `Readme.md` (#61).

* Renamed `ccte_key()` to `ctx_key()` and `has_ccte_key()` to `has_ctx_key()`. 
Also renamed instances of 'CCTE' and 'CTED' to 'CTX' when referring to APIs and
updated `DESCRIPTION` file (#61).

* Updated `bootstrap_ccdr()` to reflect new API name (#61).

* Fixed bug in `get_bioactifity_details()` for handling different nested field 
types (#57).

* Fixed bug in `get_bioactivity_details()` for unnesting returned data.frame 
(#52).

* Fixed bug in `get_bioactivity_details()` for handling m4id mismatch (#50).


* New projection values for `get_chemical_details()` and 
`get_chemical_details_batch()` (#49).

* New parameter in `chemical_starts_with()`, `get_chemical_starts_with_batch()`, 
`chemical_equal_batch()`, `chemical_contains()`, and `chemical_contains_batch()` 
controlling number of returned results (#49).

* New parameter in `get_chemical_image()` and `get_chemical_image_batch()` for
SMILES input (#49).

* Updated `Chemical.Rmd` with code examples for new functions (#49).

* Updated `Bioactivity.Rmd` to reflect new CTX API name, added references to 
introduction, improved exposition and code examples (#47).

* Updated `Bioactivity.Rmd`, `Chemical.Rmd`, `Hazard.Rmd`, and 
`Introduction.Rmd` to include current references, change names to reflect new 
CTX API branding (#45).

* Updated attach message and added '.onLoad()` function to fix API key bug when
loading package from another package (#43).

# ccdR 1.0.0

# ccdR 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Initial release.
