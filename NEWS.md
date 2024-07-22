# ctxR (development version)

## Minor improvements and fixes

* Adjusted `chemical_contains()`, `chemical_equal()`, and 
`chemical_starts_with()` to handle http 400 errors and returned information from
those requests. Adjusted the `chemical_contains_batch()`, 
`chemical_equals_batch()`, and `chemical_starts_with_batch()` to return named
lists of valid and invalid search results, with invalid results containing 
information from 400 errors (@kisaacs1, #11).

# ctxR 1.0.0

* Initial release. Renamed package from `ccdR` package for better alignment 
with US EPA CTX APIs.

