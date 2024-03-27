with_mock_dir("hazard",{
# test_that("catch missing API", {
#    # Run register_ccdr(key = 'YOUR KEY', write = TRUE) prior to running tests
#     #store env variable so tests don't overwrite
#     # tmp <- Sys.getenv("CCTE_API_KEY")
#     # on.exit(Sys.setenv("CCTE_API_KEY" = tmp))
#     # if(Sys.getenv("CCTE_API_KEY") == ""){
#     #   #set env variable temporarily for testing
#     #   Sys.setenv("CCTE_API_KEY" = "stored_api_key")
#     # }
#    expect_message(get_hazard_by_dtxsid(DTXSID = 'DTXSID7020182', verbose = TRUE), 'Using stored API key!')
#    expect_message(get_human_hazard_by_dtxsid(DTXSID = 'DTXSID7020182', verbose = TRUE), 'Using stored API key!')
#    expect_message(get_ecotox_hazard_by_dtxsid(DTXSID = 'DTXSID7020182', verbose = TRUE), 'Using stored API key!')
#    expect_message(get_skin_eye_hazard(DTXSID = 'DTXSID7020182', verbose = TRUE), 'Using stored API key!')
#    expect_message(get_cancer_hazard(DTXSID = 'DTXSID7020182', verbose = TRUE), 'Using stored API key!')
#    expect_message(get_genetox_summary(DTXSID = 'DTXSID7020182', verbose = TRUE), 'Using stored API key!')
#    expect_message(get_genetox_details(DTXSID = 'DTXSID7020182', verbose = TRUE), 'Using stored API key!')
#  })

test_that("catch missing DTXSID", {
  expect_error(get_hazard_by_dtxsid(API_key = 'test_key'), 'Please input a DTXSID!')
  expect_error(get_human_hazard_by_dtxsid(API_key = 'test_key'), 'Please input a DTXSID!')
  expect_error(get_ecotox_hazard_by_dtxsid(API_key = 'test_key'), 'Please input a DTXSID!')
  expect_error(get_skin_eye_hazard(API_key = 'test_key'), 'Please input a DTXSID!')
  expect_error(get_cancer_hazard(API_key = 'test_key'), 'Please input a DTXSID!')
  expect_error(get_genetox_summary(API_key = 'test_key'), 'Please input a DTXSID!')
  expect_error(get_genetox_details(API_key = 'test_key'), 'Please input a DTXSID!')
})

test_that("Return data type", {
  expect_type(get_hazard_by_dtxsid(DTXSID = 'DTXSID7020182', API_key = ccte_key()), 'list')
  expect_type(get_hazard_by_dtxsid(DTXSID = '', API_key = ccte_key()), 'NULL')
  #expect_type(get_hazard_by_dtxsid(DTXSID = 'DTXSID7020182', API_key = ''), 'NULL')
  expect_type(get_human_hazard_by_dtxsid(DTXSID = 'DTXSID7020182', API_key = ccte_key()), 'list')
  expect_type(get_human_hazard_by_dtxsid(DTXSID = '', API_key = ccte_key()), 'NULL')
  #expect_type(get_human_hazard_by_dtxsid(DTXSID = 'DTXSID7020182', API_key = ''), 'NULL')
  expect_type(get_ecotox_hazard_by_dtxsid(DTXSID = 'DTXSID7020182', API_key = ccte_key()), 'list')
  expect_type(get_ecotox_hazard_by_dtxsid(DTXSID = '', API_key = ccte_key()), 'NULL')
  #expect_type(get_ecotox_hazard_by_dtxsid(DTXSID = 'DTXSID7020182', API_key = ''), 'NULL')
  expect_type(get_skin_eye_hazard(DTXSID = 'DTXSID7020182', API_key = ccte_key()), 'list')
  expect_type(get_skin_eye_hazard(DTXSID = '', API_key = ccte_key()), 'NULL')
  #expect_type(get_skin_eye_hazard(DTXSID = 'DTXSID7020182', API_key = ''), 'NULL')
  expect_type(get_cancer_hazard(DTXSID = 'DTXSID7020182', API_key = ccte_key()), 'list')
  expect_type(get_cancer_hazard(DTXSID = '', API_key = ccte_key()), 'NULL')
  #expect_type(get_cancer_hazard(DTXSID = 'DTXSID7020182', API_key = ''), 'NULL')
  expect_type(get_genetox_summary(DTXSID = 'DTXSID7020182', API_key = ccte_key()), 'list')
  expect_type(get_genetox_summary(DTXSID = '', API_key = ccte_key()), 'NULL')
  #expect_type(get_genetox_summary(DTXSID = 'DTXSID7020182', API_key = ''), 'NULL')
  expect_type(get_genetox_details(DTXSID = 'DTXSID7020182', API_key = ccte_key()), 'list')
  expect_type(get_genetox_details(DTXSID = '', API_key = ccte_key()), 'NULL')
  #expect_type(get_genetox_details(DTXSID = 'DTXSID7020182', API_key = ''), 'NULL')
})
})
