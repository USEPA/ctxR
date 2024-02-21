with_mock_dir("bioactivity-batch",{
test_that("catch missing API", {
  # Run register_ccdr(key = 'YOUR KEY', write = TRUE) prior to running tests

  #store env variable so tests don't overwrite
  tmp <- Sys.getenv("CCTE_API_KEY")
  on.exit(Sys.setenv("CCTE_API_KEY" = tmp))
  if(Sys.getenv("CCTE_API_KEY") == ""){
    #set env variable temporarily for testing
    Sys.setenv("CCTE_API_KEY" = "stored_api_key")
  }
  expect_message(get_bioactivity_details_batch(DTXSID = c('DTXSID7020182'), verbose = TRUE), 'Using stored API key!')
  expect_message(get_bioactivity_details_batch(DTXSID = c('DTXSID7020182'), API_key = 1, verbose = TRUE), 'Using stored API key!')
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
  expect_type(get_bioactivity_details_batch(DTXSID = c('DTXSID8031865'), API_key = ''), 'list')
  expect_type(get_bioactivity_details_batch(DTXSID = c('DTXSID8031865'), API_key = Sys.getenv('CCTE_API_KEY')), 'list')
  expect_type(get_bioactivity_details_batch(AEID = c(42), API_key = 'test_key'), 'list')
})})
