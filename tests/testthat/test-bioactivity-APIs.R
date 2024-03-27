with_mock_dir("bioactivity",{

test_that("Catch missing API", {
  # Run register_ccdr(key = 'YOUR KEY', write = TRUE) prior to running tests
  #store env variable so tests don't overwrite
  # tmp <- Sys.getenv("CCTE_API_KEY")
  # on.exit(Sys.setenv("CCTE_API_KEY" = tmp))
  # if(Sys.getenv("CCTE_API_KEY") == ""){
  # #set env variable temporarily for testing
  # Sys.setenv("CCTE_API_KEY" = "stored_api_key")
  # }

  expect_message(get_bioactivity_details(DTXSID = 'DTXSID8031865', verbose = TRUE), 'Using stored API key!')
})

test_that("DTXSID/AEID errors", {
  expect_error(get_bioactivity_details(API_key = 'test_key'), 'Please input a DTXSID, AEID, SPID, or m4id!')
  expect_error(get_bioactivity_details(DTXSID = 'DTXSID8031865', AEID = 1, API_key = 'test_key'), 'Please input a value for only one of DTXSID, AEID, SPID, or m4id, but not multiple!')
})

test_that('Return data type', {
  expect_type(get_bioactivity_details(DTXSID = 'DTXSID8031865', API_key = ccte_key()), 'list')
  expect_type(get_bioactivity_details(DTXSID = '', API_key = ccte_key()), 'NULL')
  #expect_type(get_bioactivity_details(DTXSID = 'DTXSID7020182', API_key = ''), 'NULL')
  expect_type(get_bioactivity_details(AEID = 1), 'list')
  #expect_type(get_bioactivity_details(AEID = 42, API_key = ''), 'NULL')
})
})
