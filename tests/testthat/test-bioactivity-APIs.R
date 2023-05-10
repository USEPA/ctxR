test_that("Catch missing API", {
  expect_error(get_bioactivity_details(DTXSID = 'DTXSID7020182'), 'Please input an API_key!')
})

test_that("DTXSID/AEID errors", {
  expect_error(get_bioactivity_details(API_key = 'test_key'), 'Please input a DTXSID or AEID!')
  expect_error(get_bioactivity_details(DTXSID = 'DTXSID7020182', AEID = 1234, API_key = 'test_key'), 'Please input either a DTXSID or AEID, but not both!')
})

test_that('Return data type', {
  expect_type(get_bioactivity_details(DTXSID = 'DTXSID7020182', API_key = Sys.getenv('CCTE_API_KEY')), 'list')
  expect_type(get_bioactivity_details(DTXSID = '', API_key = Sys.getenv('CCTE_API_KEY')), 'NULL')
  expect_type(get_bioactivity_details(AEID = 42, API_key = Sys.getenv('CCTE_API_KEY')), 'list')
})
