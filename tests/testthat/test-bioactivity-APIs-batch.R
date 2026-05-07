with_mock_dir("bioactivity-batch",{
# test_that("catch missing API", {
#   # Run register_ctxR(key = 'YOUR KEY', write = TRUE) prior to running tests
#
#   #store env variable so tests don't overwrite
#   #tmp <- Sys.getenv("CTX_API_KEY")
#   #on.exit(Sys.setenv("CTX_API_KEY" = tmp))
#   #if(Sys.getenv("CTX_API_KEY") == ""){
#   #  #set env variable temporarily for testing
#   #  Sys.setenv("CTX_API_KEY" = "stored_api_key")
#   #}
#   expect_message(get_bioactivity_details_batch(DTXSID = c('DTXSID8031865'), verbose = TRUE), 'Using stored API key!')
#   expect_message(get_bioactivity_details_batch(DTXSID = c('DTXSID8031865'), API_key = 1, verbose = TRUE), 'Using stored API key!')
# })

test_that('DTXSID/AEID errors', {
  #expect_error(get_bioactivity_details_batch(DTXSID = list('first' = '1', 'second' = 2), API_key = 'test_key'), 'Please input a character list for DTXSID!')
  expect_error(get_bioactivity_details_batch(API_key = 'test_key'), 'Please input a list of DTXSIDs, AEIDs, SPIDs, or m4ids!')
  expect_error(get_aed_data_batch(API_key = 'test_key'), 'Please input a list of DTXSIDs!')
  expect_error(get_single_concentration_batch(API_key = 'test_key'), 'Please input an AEID!')
  expect_error(get_predictions_by_dtxsid_batch(API_key = 'test_key'), 'Please input a list of DTXSIDs!')
  expect_error(get_predictions_by_dtxsid_and_model_batch(API_key = 'test_key'), 'Please input a list of DTXSIDs!')
  expect_error(get_predictions_by_dtxsid_and_model_batch(DTXSID = '', API_key = 'test_key'), 'Please input a Model!')
  expect_error(get_chemicals_by_assay_batch(API_key = 'test_key'), 'Please input a list of AEIDs!')
  expect_error(get_bioactivity_summary_by_tissue_batch(API_key = 'test_key'), 'Please input a list of DTXSIDs!')
  expect_error(get_bioactivity_summary_batch(API_key = 'test_key'), 'Please input a list of AEIDs or DTXSIDs!')
  expect_error(get_assay_summary_by_gene_batch(API_key = 'test_key'), 'Please input an geneSymbol!')
  expect_error(get_aop_data_batch(API_key = 'test_key'), 'Please input a list of AEIDs, KeyEvents, or EntrezGeneIds!')
  expect_error(get_analytical_qc_batch(API_key = 'test_key'), 'Please input a list of DTXSIDs!')
})

test_that('Word search errors', {
  expect_error(assay_contains_batch(API_key = 'test_key'), 'Please input a list of strings!')
  expect_error(assay_equal_batch(API_key = 'test_key'), 'Please input a list of strings!')
  expect_error(assay_starts_with_batch(API_key = 'test_key'), 'Please input a list of strings!')
})

test_that('Rate limit warnings', {
  expect_warning(get_bioactivity_details_batch(DTXSID = c(''), API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_bioactivity_details_batch(DTXSID = c(''), API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_aed_data_batch(DTXSID = c(''), API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_aed_data_batch(DTXSID = c(''), API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_single_concentration_batch(AEID = c(''), API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_single_concentration_batch(AEID = c(''), API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_predictions_by_dtxsid_batch(DTXSID = c(''), API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_predictions_by_dtxsid_batch(DTXSID = c(''), API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_predictions_by_dtxsid_and_model_batch(DTXSID = c(''), Model = '', API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_predictions_by_dtxsid_and_model_batch(DTXSID = c(''), Model = '', API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_chemicals_by_assay_batch(AEID = c(''), API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_chemicals_by_assay_batch(AEID = c(''), API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_bioactivity_summary_by_tissue_batch(DTXSID = c(''), Tissue = '', API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_bioactivity_summary_by_tissue_batch(DTXSID = c(''), Tissue = '', API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_bioactivity_summary_batch(DTXSID = c(''), API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_bioactivity_summary_batch(DTXSID = c(''), API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_assay_summary_by_gene_batch(geneSymbol = c(''), API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_assay_summary_by_gene_batch(geneSymbol =  c(''), API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_aop_data_batch(AEID = c(''), API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_aop_data_batch(AEID = c(''), API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_analytical_qc_batch(DTXSID = c(''), API_key = ctx_key(), rate_limit = '0'), 'Setting rate limit to 0 seconds between requests!')
  expect_warning(get_analytical_qc_batch(DTXSID = c(''), API_key = ctx_key(), rate_limit = -1), 'Setting rate limit to 0 seconds between requests!')
})

test_that('Return data types', {
  ##expect_type(get_bioactivity_details_batch(DTXSID = c('DTXSID8031865'), API_key = ''), 'list')
  expect_type(get_bioactivity_details_batch(DTXSID = c(''), API_key = ctx_key()), 'list')
  ##expect_type(get_bioactivity_details_batch(AEID = c(1), API_key = 'test_key'), 'list')
  expect_type(get_aed_data_batch(DTXSID = '', API_key = ctx_key()), 'list')
  expect_type(get_single_concentration_batch(AEID = 1, API_key = ctx_key()), 'list')
  expect_type(get_predictions_by_dtxsid_batch(DTXSID = '', API_key = ctx_key()), 'list')
  expect_type(get_predictions_by_dtxsid_and_model_batch(DTXSID = '', Model = '', API_key = ctx_key()), 'list')
  expect_type(get_chemicals_by_assay_batch(AEID = 1, API_key = ctx_key()), 'list')
  expect_type(get_bioactivity_summary_by_tissue_batch(DTXSID = '', Tissue = '', API_key = ctx_key()), 'list')
  expect_type(get_bioactivity_summary_batch(DTXSID = '', API_key = ctx_key()), 'list')
  expect_type(get_assay_summary_by_gene_batch(geneSymbol = '', API_key = ctx_key()), 'list')
  expect_type(get_aop_data_batch(AEID = 1, API_key = ctx_key()), 'list')
  expect_type(get_aop_data_batch(KeyEvent = '', API_key = ctx_key()), 'list')
  expect_type(get_aop_data_batch(EntrezGeneId = 1, API_key = ctx_key()), 'list')
  expect_type(get_analytical_qc_batch(DTXSID = '', API_key = ctx_key()), 'list')
  expect_type(assay_contains_batch(word = '', API_key = ctx_key()), 'list')
  expect_type(assay_equal_batch(word = '', API_key = ctx_key()), 'list')
  expect_type(assay_starts_with_batch(word = '', API_key = ctx_key()), 'list')
})})
