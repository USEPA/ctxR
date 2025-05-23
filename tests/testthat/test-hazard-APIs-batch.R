with_mock_dir("hazard-batch",{
# test_that("catch missing APIs", {
#   #store env variable so tests don't overwrite
#   # tmp <- Sys.getenv("CTX_API_KEY")
#   # on.exit(Sys.setenv("CTX_API_KEY" = tmp))
#   # if(Sys.getenv("CTX_API_KEY") == ""){
#   #   #set env variable temporarily for testing
#   #   Sys.setenv("CTX_API_KEY" = "stored_api_key")
#   # }
#   # Run register_ctxR(key = 'YOUR KEY', write = TRUE) prior to running tests
#   expect_message(get_hazard_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), verbose = TRUE), 'Using stored API key!')
#   expect_message(get_hazard_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), API_key = 1, verbose = TRUE), 'Using stored API key!')
#   expect_message(get_human_hazard_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), verbose = TRUE), 'Using stored API key!')
#   expect_message(get_human_hazard_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), API_key = 1, verbose = TRUE), 'Using stored API key!')
#   expect_message(get_ecotox_hazard_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), verbose = TRUE), 'Using stored API key!')
#   expect_message(get_ecotox_hazard_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), API_key = 1, verbose = TRUE), 'Using stored API key!')
#   expect_message(get_skin_eye_hazard_batch(DTXSID = c('DTXSID7020182'), verbose = TRUE), 'Using stored API key!')
#   expect_message(get_skin_eye_hazard_batch(DTXSID = c('DTXSID7020182'), API_key = 1, verbose = TRUE), 'Using stored API key!')
#   expect_message(get_cancer_hazard_batch(DTXSID = c('DTXSID7020182'), verbose = TRUE), 'Using stored API key!')
#   expect_message(get_cancer_hazard_batch(DTXSID = c('DTXSID7020182'), API_key = 1, verbose = TRUE), 'Using stored API key!')
#   expect_message(get_genetox_summary_batch(DTXSID = c('DTXSID7020182'), verbose = TRUE), 'Using stored API key!')
#   expect_message(get_genetox_summary_batch(DTXSID = c('DTXSID7020182'), API_key = 1, verbose = TRUE), 'Using stored API key!')
#   expect_message(get_genetox_details_batch(DTXSID = c('DTXSID7020182'), verbose = TRUE), 'Using stored API key!')
#   expect_message(get_genetox_details_batch(DTXSID = c('DTXSID7020182'), API_key = 1, verbose = TRUE), 'Using stored API key!')
# })

test_that('DTXSID errors', {
  expect_error(get_hazard_by_dtxsid_batch(DTXSID = 1, API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_hazard_by_dtxsid_batch(DTXSID = list('first' = '1', 'second' = 2), API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_hazard_by_dtxsid_batch(API_key = 'test_key'), 'Please input a list of DTXSIDs!')
  expect_error(get_human_hazard_by_dtxsid_batch(DTXSID = 1, API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_human_hazard_by_dtxsid_batch(DTXSID = list('first' = '1', 'second' = 2), API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_human_hazard_by_dtxsid_batch(API_key = 'test_key'), 'Please input a list of DTXSIDs!')
  expect_error(get_ecotox_hazard_by_dtxsid_batch(DTXSID = 1, API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_ecotox_hazard_by_dtxsid_batch(DTXSID = list('first' = '1', 'second' = 2), API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_ecotox_hazard_by_dtxsid_batch(API_key = 'test_key'), 'Please input a list of DTXSIDs!')
  expect_error(get_skin_eye_hazard_batch(DTXSID = 1, API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_skin_eye_hazard_batch(DTXSID = list('first' = '1', 'second' = 2), API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_skin_eye_hazard_batch(API_key = 'test_key'), 'Please input a list of DTXSIDs!')
  expect_error(get_cancer_hazard_batch(DTXSID = 1, API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_cancer_hazard_batch(DTXSID = list('first' = '1', 'second' = 2), API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_cancer_hazard_batch(API_key = 'test_key'), 'Please input a list of DTXSIDs!')
  expect_error(get_genetox_summary_batch(DTXSID = 1, API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_genetox_summary_batch(DTXSID = list('first' = '1', 'second' = 2), API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_genetox_summary_batch(API_key = 'test_key'), 'Please input a list of DTXSIDs!')
  expect_error(get_genetox_details_batch(DTXSID = 1, API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_genetox_details_batch(DTXSID = list('first' = '1', 'second' = 2), API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_genetox_details_batch(API_key = 'test_key'), 'Please input a list of DTXSIDs!')
})

test_that('Rate limit warnings', {
  expect_warning(get_hazard_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_hazard_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_human_hazard_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_human_hazard_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_ecotox_hazard_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_ecotox_hazard_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_skin_eye_hazard_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_skin_eye_hazard_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_cancer_hazard_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_cancer_hazard_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_genetox_summary_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_genetox_summary_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_genetox_details_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_genetox_details_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
})

test_that('Return data types', {
  expect_type(get_hazard_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key()), 'list')
  expect_type(get_hazard_by_dtxsid_batch(DTXSID = c(''), API_key = ctx_key()), 'list')
  #expect_type(get_hazard_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), API_key = ''), 'list')
  expect_type(get_human_hazard_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key()), 'list')
  expect_type(get_human_hazard_by_dtxsid_batch(DTXSID = c(''), API_key = ctx_key()), 'list')
  #expect_type(get_human_hazard_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), API_key = ''), 'list')
  expect_type(get_ecotox_hazard_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key()), 'list')
  expect_type(get_ecotox_hazard_by_dtxsid_batch(DTXSID = c(''), API_key = ctx_key()), 'list')
  #expect_type(get_ecotox_hazard_by_dtxsid_batch(DTXSID = c('DTXSID7020182'), API_key = ''), 'list')
  expect_type(get_skin_eye_hazard_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key()), 'list')
  expect_type(get_skin_eye_hazard_batch(DTXSID = c(''), API_key = ctx_key()), 'list')
  #expect_type(get_skin_eye_hazard_batch(DTXSID = c('DTXSID7020182'), API_key = ''), 'list')
  expect_type(get_cancer_hazard_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key()), 'list')
  expect_type(get_cancer_hazard_batch(DTXSID = c(''), API_key = ctx_key()), 'list')
  #expect_type(get_cancer_hazard_batch(DTXSID = c('DTXSID7020182'), API_key = ''), 'list')
  expect_type(get_genetox_summary_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key()), 'list')
  expect_type(get_genetox_summary_batch(DTXSID = c(''), API_key = ctx_key()), 'list')
  #expect_type(get_genetox_summary_batch(DTXSID = c('DTXSID7020182'), API_key = ''), 'list')
  expect_type(get_genetox_details_batch(DTXSID = c('DTXSID7020182'), API_key = ctx_key()), 'list')
  expect_type(get_genetox_details_batch(DTXSID = c(''), API_key = ctx_key()), 'list')
  #expect_type(get_genetox_details_batch(DTXSID = c('DTXSID7020182'), API_key = ''), 'list')
})})
