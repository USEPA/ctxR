with_mock_dir("bioactivity",{

# test_that("Catch missing API", {
#   # Run register_ctxR(key = 'YOUR KEY', write = TRUE) prior to running tests
#   #store env variable so tests don't overwrite
#   # tmp <- Sys.getenv("CTX_API_KEY")
#   # on.exit(Sys.setenv("CTX_API_KEY" = tmp))
#   # if(Sys.getenv("CTX_API_KEY") == ""){
#   # #set env variable temporarily for testing
#   # Sys.setenv("CTX_API_KEY" = "stored_api_key")
#   # }
#
#   expect_message(get_bioactivity_details(DTXSID = 'DTXSID8031865', verbose = TRUE), 'Using stored API key!')
# })

test_that("DTXSID/AEID errors", {
  expect_error(get_bioactivity_details(API_key = 'test_key'), 'Please input a DTXSID, AEID, SPID, or m4id!')
  expect_error(get_bioactivity_details(DTXSID = 'DTXSID8031865', AEID = 1, API_key = 'test_key'), 'Please input a value for only one of DTXSID, AEID, SPID, or m4id, but not multiple!')
  expect_error(get_single_concentration(API_key = 'test_key'), 'Please input an AEID!')
  expect_error(get_assay_summary_by_gene(API_key = 'test_key'), 'Please input an geneSymbol!')
  expect_error(get_aeid_by_endpoint(API_key = 'test_key'), 'Please input an endpoint!')
  expect_error(get_chemicals_by_assay(API_key = 'test_key'), 'Please input an AEID!')
  expect_error(get_bioactivity_summary_by_tissue(API_key = 'test_key'), 'Please input an DTXSID!')
  expect_error(get_aed_data(API_key = 'test_key'), 'Please input an DTXSID!')
  expect_error(get_analytical_qc(API_key = 'test_key'), 'Please input an DTXSID!')
  expect_error(get_aop_data(API_key = 'test_key'), 'Please input a AEID, KeyEvent, or EntrezGeneId!')
  expect_error(get_predictions_by_dtxsid(API_key = 'test_key'), 'Please input an DTXSID!')
  expect_error(get_predictions_by_dtxsid_and_model(API_key = 'test_key'), 'Please input an DTXSID!')
})

test_that('Word search errors', {
  expect_error(assay_contains(API_key = 'test_key'), 'Please input a character value for word!')
  expect_error(assay_equal(API_key = 'test_key'), 'Please input a character value for word!')
  expect_error(assay_starts_with(API_key = 'test_key'), 'Please input a character value for word!')
})

test_that('Return data type', {
  #expect_type(get_bioactivity_details(DTXSID = 'DTXSID8031865', API_key = ctx_key()), 'list')
  expect_type(get_bioactivity_details(DTXSID = '', API_key = ctx_key()), 'NULL')
  #expect_type(get_bioactivity_details(DTXSID = 'DTXSID7020182', API_key = ''), 'NULL')
  expect_type(get_bioactivity_details(AEID = 1, API_key = ctx_key()), 'list')
  #expect_type(get_bioactivity_details(AEID = 42, API_key = ''), 'NULL')
  expect_type(get_single_concentration(AEID = 1, API_key = ctx_key()), 'list')
  expect_type(get_assay_summary_by_gene(geneSymbol = '', API_key = ctx_key()), 'NULL')
  expect_type(get_aeid_by_endpoint(endpoint = '', API_key = ctx_key()), 'NULL')
  expect_type(get_chemicals_by_assay(AEID = 1, API_key = ctx_key()), 'list')
  expect_type(get_bioactivity_summary_by_tissue(DTXSID = '', Tissue = '', API_key = ctx_key()), 'list')
  expect_type(get_aed_data(DTXSID = '', API_key = ctx_key()), 'NULL')
  expect_type(get_analytical_qc(DTXSID = '', API_key = ctx_key()), 'NULL')
  expect_type(get_aop_data(AEID = 1, API_key = ctx_key()), 'list')
  expect_type(get_aop_data(KeyEvent = '', API_key = ctx_key()), 'NULL')
  expect_type(get_aop_data(EntrezGeneId = 1, API_key = ctx_key()), 'list')
  expect_type(get_predictions_by_dtxsid(DTXSID = '', API_key = ctx_key()), 'NULL')
  expect_type(get_predictions_by_dtxsid_and_model(DTXSID = '', Model = '', API_key = ctx_key()), 'list')
  expect_type(assay_contains(word = '', API_key = ctx_key()), 'NULL')
  expect_type(assay_equal(word = '', API_key = ctx_key()), 'NULL')
  expect_type(assay_starts_with(word = '', API_key = ctx_key()), 'NULL')
})
})
