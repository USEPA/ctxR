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

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/functional-use/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
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

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/functional-use/probability/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
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
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/functional-use/category'),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
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

#' Get httk data
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some "progress report" should be given.
#'
#' @return A data.table of httk data for the given input chemical.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull httk data for BPA
#' bpa_httk <- get_httk_data(DTXSID = 'DTXSID7020182')

get_httk_data <- function(DTXSID = NULL,
                          API_key = NULL,
                          Server = exposure_api_server,
                          verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input an DTXSID!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/httk/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
   if(response$status_code == 401){
     stop(httr::content(response)$detail)
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

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/product-data/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
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
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/product-data/puc'),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
                        )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
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
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/list-presence/tags'),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
                        )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
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

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/list-presence/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
                        )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
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

#' Get general exposure prediction data
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some "progress report" should be given.
#'
#' @return A data.table of general exposure prediction data or NULL if data is
#' missing.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull general exposure prediction data for BPA
#' bpa <- get_general_exposure_prediction(DTXSID = 'DTXSID7020182')

get_general_exposure_prediction <- function(DTXSID = NULL,
                                            API_key = NULL,
                                            Server = exposure_api_server,
                                            verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input an DTXSID!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/seem/general/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    content <- httr::content(response, as = 'text', encoding = 'UTF-8')
    if (nchar(content) > 0){
      parsed_data <- jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8"))
      non_null_data <- null_to_na(parsed_data)
      return(data.table::as.data.table(non_null_data, rm.na = FALSE))
    }
    if (verbose) {
      print('The request was successful but returned no data')
    }
    return()
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()
}

#' Get demographic exposure prediction data
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some "progress report" should be given.
#'
#' @return A data.table of demographic exposure prediction data.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull general exposure prediction data for BPA
#' bpa <- get_demographic_exposure_prediction(DTXSID = 'DTXSID7020182')


get_demographic_exposure_prediction <- function(DTXSID = NULL,
                                                API_key = NULL,
                                                Server = exposure_api_server,
                                                verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input an DTXSID!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/seem/demographic/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
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

null_to_na <- function(data_list){
  lapply(data_list, function(t){
    if (is.null(t)){
      return(NA_character_)
    }
    return(t)
  })
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
  request <- httr::GET(url = paste0(exposure_api_server, "/health"))
  return(request$status_code)
}
