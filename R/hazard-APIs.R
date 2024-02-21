#' Get hazard data by DTXSID
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#'
#' @return A data.frame containing chemical (human and eco) hazard data
#' @export


get_hazard_by_dtxsid <- function(DTXSID = NULL,
                               API_key = NULL,
                               Server = hazard_api_server){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    }
  }


  response <- httr::GET(url = paste0(Server, '/search/by-dtxsid/', DTXSID),
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
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()

}

#' Get human hazard data by DTXSID
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#'
#' @return A data.frame containing chemical human hazard data
#' @export


get_human_hazard_by_dtxsid <- function(DTXSID = NULL,
                                 API_key = NULL,
                                 Server = hazard_api_server){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    }
  }

  response <- httr::GET(url = paste0(Server, '/human/search/by-dtxsid/', DTXSID),
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
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()

}

#' Get ecotox hazard data by DTXSID
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#'
#' @return A data.frame containing chemical (ecotox) hazard data
#' @export


get_ecotox_hazard_by_dtxsid <- function(DTXSID = NULL,
                                 API_key = NULL,
                                 Server = hazard_api_server){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    }
  }

  response <- httr::GET(url = paste0(Server, '/eco/search/by-dtxsid/', DTXSID),
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
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()

}


#' Get skin and eye hazard
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#'
#' @return A data.frame containing skin and eye hazard data.
#' @export


get_skin_eye_hazard <- function(DTXSID = NULL,
                                API_key = NULL,
                                Server = hazard_api_server){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    }
  }

  response <- httr::GET(url = paste0(Server, '/skin-eye/search/by-dtxsid/', DTXSID),
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
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()

}

#' Get cancer hazard
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#'
#' @return A data.frame of cancer hazard data related to the input DTXSID.
#' @export


get_cancer_hazard <- function(DTXSID = NULL,
                              API_key = NULL,
                              Server = hazard_api_server){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    }
  }


  response <- httr::GET(url = paste0(Server, '/cancer-summary/search/by-dtxsid/', DTXSID),
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
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()

}

#' Get genetox summary
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#'
#' @return A data.frame of genetox summary data related to the input DTXSID.
#' @export


get_genetox_summary <- function(DTXSID = NULL,
                                API_key = NULL,
                                Server = hazard_api_server){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    }
  }


  response <- httr::GET(url = paste0(Server, '/genetox/summary/search/by-dtxsid/', DTXSID),
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
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()

}

#' Get genetox details
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#'
#' @return A data.frame of genetox detail data related to the input DTXSID.
#' @export


get_genetox_details <- function(DTXSID = NULL,
                                API_key = NULL,
                                Server = hazard_api_server){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    }
  }


  response <- httr::GET(url = paste0(Server, '/genetox/details/search/by-dtxsid/', DTXSID),
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
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()

}
