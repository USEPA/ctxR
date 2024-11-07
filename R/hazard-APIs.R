#' Get hazard data by DTXSID
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame containing chemical (human and eco) hazard data
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull hazard data for BPA
#' bpa <- get_hazard_by_dtxsid(DTXSID = 'DTXSID7020182')

get_hazard_by_dtxsid <- function(DTXSID = NULL,
                                 API_key = NULL,
                                 Server = hazard_api_server,
                                 verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key)){
    if (has_ctx_key()) {
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
  }


  response <- httr::GET(url = paste0(Server, '/search/by-dtxsid/', DTXSID),
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

#' Get human hazard data by DTXSID
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame containing chemical human hazard data
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull human hazard data for BPA
#' bpa_human <- get_human_hazard_by_dtxsid(DTXSID = 'DTXSID7020182')

get_human_hazard_by_dtxsid <- function(DTXSID = NULL,
                                       API_key = NULL,
                                       Server = hazard_api_server,
                                       verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key)){
    if (has_ctx_key()) {
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
  }

  response <- httr::GET(url = paste0(Server, '/human/search/by-dtxsid/', DTXSID),
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

#' Get ecotox hazard data by DTXSID
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame containing chemical (ecotox) hazard data
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull ecotox hazard data for BPA
#' bpa_ecotox <- get_ecotox_hazard_by_dtxsid(DTXSID = 'DTXSID7020182')

get_ecotox_hazard_by_dtxsid <- function(DTXSID = NULL,
                                        API_key = NULL,
                                        Server = hazard_api_server,
                                        verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key)){
    if (has_ctx_key()) {
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
  }

  response <- httr::GET(url = paste0(Server, '/eco/search/by-dtxsid/', DTXSID),
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


#' Get skin and eye hazard
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame containing skin and eye hazard data.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull skin and eye hazard data for BPA
#' bpa_skin_eye <- get_skin_eye_hazard_batch(DTXSID = 'DTXSID7020182')

get_skin_eye_hazard <- function(DTXSID = NULL,
                                API_key = NULL,
                                Server = hazard_api_server,
                                verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key)){
    if (has_ctx_key()) {
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
  }

  response <- httr::GET(url = paste0(Server, '/skin-eye/search/by-dtxsid/', DTXSID),
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

#' Get cancer hazard
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame of cancer hazard data related to the input DTXSID.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull cancer hazard data for BPA
#' bpa_cancer <- get_cancer_hazard(DTXSID = 'DTXSID7020182')

get_cancer_hazard <- function(DTXSID = NULL,
                              API_key = NULL,
                              Server = hazard_api_server,
                              verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key)){
    if (has_ctx_key()) {
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
  }


  response <- httr::GET(url = paste0(Server, '/cancer-summary/search/by-dtxsid/', DTXSID),
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

#' Get genetox summary
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame of genetox summary data related to the input DTXSID.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull genetox summary for BPA
#' bpa_genetox_summary <- get_genetox_summary(DTXSID = 'DTXSID7020182')

get_genetox_summary <- function(DTXSID = NULL,
                                API_key = NULL,
                                Server = hazard_api_server,
                                verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key)){
    if (has_ctx_key()) {
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
  }


  response <- httr::GET(url = paste0(Server, '/genetox/summary/search/by-dtxsid/', DTXSID),
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

#' Get genetox details
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame of genetox detail data related to the input DTXSID.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull genetox details for BPA
#' bpa_genetox_details <- get_genetox_details(DTXSID = 'DTXSID7020182')

get_genetox_details <- function(DTXSID = NULL,
                                API_key = NULL,
                                Server = hazard_api_server,
                                verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key)){
    if (has_ctx_key()) {
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
  }


  response <- httr::GET(url = paste0(Server, '/genetox/details/search/by-dtxsid/', DTXSID),
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


#' Hazard API Endpoint status
#'
#' @return Status of Hazard API Endpoints
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' status <- get_hazard_endpoint_status()
#' print(status)

get_hazard_endpoint_status <- function(){
  request <- httr::GET(url = paste0(hazard_api_server,"/health"))
  return(request$status_code)
}
