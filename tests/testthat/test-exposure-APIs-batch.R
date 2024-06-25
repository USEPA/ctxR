with_mock_dir("exposure-batch",{

test_that("DTXSID errors", {
  expect_error(get_exposure_functional_use_batch(DTXSID = 1, API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_exposure_functional_use_batch(DTXSID = list('first' = '1', 'second' = 2), API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_exposure_functional_use_batch(API_key = 'test_key'), 'Please input a list of DTXSIDs!')
  expect_error(get_exposure_functional_use_probability_batch(DTXSID = 1, API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_exposure_functional_use_probability_batch(DTXSID = list('first' = '1', 'second' = 2), API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_exposure_functional_use_probability_batch(API_key = 'test_key'), 'Please input a list of DTXSIDs!')
  expect_error(get_exposure_product_data_batch(DTXSID = 1, API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_exposure_product_data_batch(DTXSID = list('first' = '1', 'second' = 2), API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_exposure_product_data_batch(API_key = 'test_key'), 'Please input a list of DTXSIDs!')
  expect_error(get_exposure_list_presence_tags_by_dtxsid_batch(DTXSID = 1, API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_exposure_list_presence_tags_by_dtxsid_batch(DTXSID = list('first' = '1', 'second' = 2), API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_exposure_list_presence_tags_by_dtxsid_batch(API_key = 'test_key'), 'Please input a list of DTXSIDs!')
})

test_that("Rate limit warnings", {
  expect_warning(get_exposure_functional_use_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_exposure_functional_use_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_exposure_functional_use_probability_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_exposure_functional_use_probability_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_exposure_product_data_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_exposure_product_data_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_exposure_list_presence_tags_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_exposure_list_presence_tags_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
})

test_that("Return data type", {
  expect_type(get_exposure_functional_use_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key()), 'list')
  expect_type(get_exposure_functional_use_batch(DTXSID = c(''), API_key = ctx_key()), 'list')
  expect_type(get_exposure_functional_use_probability_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key()), 'list')
  expect_type(get_exposure_functional_use_probability_batch(DTXSID = c(''), API_key = ctx_key()), 'list')
  expect_type(get_exposure_product_data_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key()), 'list')
  expect_type(get_exposure_product_data_batch(DTXSID = c(''), API_key = ctx_key()), 'list')
  expect_type(get_exposure_list_presence_tags_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key()), 'list')
  expect_type(get_exposure_list_presence_tags_by_dtxsid_batch(DTXSID = c(''), API_key = ctx_key()), 'list')
})})
