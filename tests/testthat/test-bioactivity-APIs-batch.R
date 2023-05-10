test_that("catch missing API", {
  expect_error(get_bioactivity_details_batch(), 'Please input a character string containing a valid API key!')
  expect_error(get_bioactivity_details_batch(API_key = 1), 'Please input a character string containing a valid API key!')
})

test_that('DTXSID/AEID errors', {
  expect_error(get_bioactivity_details_batch(DTXSID = 1, API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_bioactivity_details_batch(DTXSID = list('first' = '1', 'second' = 2), API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_bioactivity_details_batch(API_key = 'test_key'), 'Please input a list of DTXSIDs or AEIDs!')

})

test_that('Rate limit warnings', {
  expect_warning(get_bioactivity_details_batch(DTXSID = c('DTXSID7020182'), API_key = Sys.getenv('CCTE_API_KEY'), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_bioactivity_details_batch(DTXSID = c('DTXSID7020182'), API_key = Sys.getenv('CCTE_API_KEY'), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
})

test_that('Return data types', {
  expect_type(get_bioactivity_details_batch(DTXSID = c('DTXSID7020182'), API_key = 'test_key'), 'list')
  expect_type(get_bioactivity_details_batch(DTXSID = c('DTXSID7020182'), API_key = Sys.getenv('CCTE_API_KEY')), 'list')
  expect_type(get_bioactivity_details_batch(AEID = c(42), API_key = 'test_key'), 'list')


})
