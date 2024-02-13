#' Retrieve exposure related functional use data
#'
#' @param DTXSID Chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#'
#' @return A data.frame of functional use data.


get_exposure_functional_use <- function(DTXSID = NULL,
                                        API_key = NULL,
                                        Server = "https://api-ccte-stg.epa.gov/exposure"){
  if (is.null(DTXSID))
    stop('Please input an DTXSID!')
  else if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input an API_key!')
    }
  }

  response <- httr::GET(url = paste0(Server, '/functional-use/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
  )

  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()
}


#' Retrieve probability of exposure for functional use category
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#'
#' @return A data.frame with probabilities corresponding to various routes of
#'   exposure related to functional use.


get_exposure_functional_use_probability <- function(DTXSID = NULL,
                                                    API_key = NULL,
                                                    Server = "https://api-ccte-stg.epa.gov/exposure"){
  if (is.null(DTXSID))
    stop('Please input an DTXSID!')
  else if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input an API_key!')
    }
  }

  response <- httr::GET(url = paste0(Server, '/functional-use/probability/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
  )

  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()
}

#' Retrieve functional use categories
#'
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#'
#' @return A data.frame of functional use categories.


get_exposure_functional_use_category <- function(API_key = NULL,
                                                 Server = "https://api-ccte-stg.epa.gov/exposure"){
   if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input an API_key!')
    }
  }

  response <- httr::GET(url = paste0(Server, '/functional-use/category'),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
  )

  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()
}

#' Retrieve product data for exposure purposes
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#'
#' @return A data.frame with product information relating to exposure to the
#'   given chemical


get_exposure_product_data <- function(DTXSID = NULL,
                                      API_key = NULL,
                                      Server = "https://api-ccte-stg.epa.gov/exposure"){
  if (is.null(DTXSID))
    stop('Please input an DTXSID!')
  else if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input an API_key!')
    }
  }

  response <- httr::GET(url = paste0(Server, '/product-data/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
  )

  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()
}

#' Retrieve product use categories related to exposure
#'
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#'
#' @return A data.frame consisting of all the product use categories


get_exposure_product_data_puc <- function(API_key = NULL,
                                          Server = "https://api-ccte-stg.epa.gov/exposure"){
  if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input an API_key!')
    }
  }

  response <- httr::GET(url = paste0(Server, '/product-data/puc'),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
                        )

  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()
}


#' Retrieve list presence tags
#'
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#'
#' @return A data.frame with all the list presence tags and associated data.


get_exposure_list_presence_tags <- function(API_key = NULL,
                                            Server = "https://api-ccte-stg.epa.gov/exposure"){
  if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input an API_key!')
    }
  }

  response <- httr::GET(url = paste0(Server, '/list-presence/tags'),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
                        )

  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()
}

#' Retrieve document data and list presence tags for a chemical
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#'
#' @return A data.frame of document information and list presence tags


get_exposure_list_presence_tags_by_dtxsid <- function(DTXSID = NULL,
                                                      API_key = NULL,
                                                      Server = "https://api-ccte-stg.epa.gov/exposure"){
  if (is.null(DTXSID))
    stop('Please input an DTXSID!')
  else if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input an API_key!')
    }
  }

  response <- httr::GET(url = paste0(Server, '/list-presence/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
                        )

  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()
}
