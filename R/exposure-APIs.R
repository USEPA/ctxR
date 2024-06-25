#' Retrieve exposure related functional use data
#'
#' @param DTXSID Chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame of functional use data.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull functional use data for BPA
#' bpa <- get_exposure_functional_use(DTXSID = 'DTXSID7020182')

get_exposure_functional_use <- function(DTXSID = NULL,
                                        API_key = NULL,
                                        Server = exposure_api_server,
                                        verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input an DTXSID!')
  else if (is.null(API_key)){
    if (has_ctx_key()){
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
  }

  response <- httr::GET(url = paste0(Server, '/functional-use/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
  )
  if(response$status_code == 401){
    stop('Please input an API_key!')
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()
}


#' Retrieve probability of exposure for functional use category
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame with probabilities corresponding to various routes of
#'   exposure related to functional use.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull functional use probability data for BPA
#' bpa <- get_exposure_functional_use_probability(DTXSID = 'DTXSID7020182')

get_exposure_functional_use_probability <- function(DTXSID = NULL,
                                                    API_key = NULL,
                                                    Server = exposure_api_server,
                                                    verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input an DTXSID!')
  else if (is.null(API_key)){
    if (has_ctx_key()){
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
  }

  response <- httr::GET(url = paste0(Server, '/functional-use/probability/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
  )
  if(response$status_code == 401){
    stop('Please input an API_key!')
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()
}

#' Retrieve functional use categories
#'
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame of functional use categories.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull functional use category data for BPA
#' functional_use_categories <- get_exposure_functional_use_category()

get_exposure_functional_use_category <- function(API_key = NULL,
                                                 Server = exposure_api_server,
                                                 verbose = FALSE){
   if (is.null(API_key)){
    if (has_ctx_key()){
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
  }

  response <- httr::GET(url = paste0(Server, '/functional-use/category'),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
  )
  if(response$status_code == 401){
    stop('Please input an API_key!')
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()
}

#' Retrieve product data for exposure purposes
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame with product information relating to exposure to the
#'   given chemical
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull exposure product data for BPA
#' bpa <- get_exposure_product_data(DTXSID = 'DTXSID7020182')

get_exposure_product_data <- function(DTXSID = NULL,
                                      API_key = NULL,
                                      Server = exposure_api_server,
                                      verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input an DTXSID!')
  else if (is.null(API_key)){
    if (has_ctx_key()){
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
  }

  response <- httr::GET(url = paste0(Server, '/product-data/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
  )
  if(response$status_code == 401){
    stop('Please input an API_key!')
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()
}

#' Retrieve product use categories related to exposure
#'
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame consisting of all the product use categories
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull product data use categories for BPA
#' puc_categories <- get_exposure_product_data_puc()

get_exposure_product_data_puc <- function(API_key = NULL,
                                          Server = exposure_api_server,
                                          verbose = FALSE){
  if (is.null(API_key)){
    if (has_ctx_key()){
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
  }

  response <- httr::GET(url = paste0(Server, '/product-data/puc'),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
                        )
  if(response$status_code == 401){
    stop('Please input an API_key!')
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()
}


#' Retrieve list presence tags
#'
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame with all the list presence tags and associated data.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull list presence tags
#' tags <- get_exposure_list_presence_tags()

get_exposure_list_presence_tags <- function(API_key = NULL,
                                            Server = exposure_api_server,
                                            verbose = FALSE){
  if (is.null(API_key)){
    if (has_ctx_key()){
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
  }

  response <- httr::GET(url = paste0(Server, '/list-presence/tags'),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
                        )
  if(response$status_code == 401){
    stop('Please input an API_key!')
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()
}

#' Retrieve document data and list presence tags for a chemical
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame of document information and list presence tags
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull list presence tags for BPA
#' bpa <- get_exposure_list_presence_tags(DTXSID = 'DTXSID7020182')

get_exposure_list_presence_tags_by_dtxsid <- function(DTXSID = NULL,
                                                      API_key = NULL,
                                                      Server = exposure_api_server,
                                                      verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input an DTXSID!')
  else if (is.null(API_key)){
    if (has_ctx_key()){
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
  }

  response <- httr::GET(url = paste0(Server, '/list-presence/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
                        )
  if(response$status_code == 401){
    stop('Please input an API_key!')
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()
}


#' Exposure API Endpoint status
#'
#' @return Status of Exposure API Endpoints
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' status <- get_exposure_endpoint_status()
#' print(status)

get_exposure_endpoint_status <- function(){
  request <- httr::GET(url = "https://api-ccte.epa.gov/exposure/health")
  return(request$status_code)
}
