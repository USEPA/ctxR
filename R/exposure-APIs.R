get_exposure_functional_use <- function(DTXSID = NULL,
                                        API_key = NULL,
                                        Server = "https://api-ccte-stg.epa.gov/exposure"){
  if (is.null(DTXSID))
    stop('Please input an DTXSID!')
  else if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      message('Using stored API key!')
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
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()
}


get_exposure_functional_use_probability <- function(DTXSID = NULL,
                                                    API_key = NULL,
                                                    Server = "https://api-ccte-stg.epa.gov/exposure"){
  if (is.null(DTXSID))
    stop('Please input an DTXSID!')
  else if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      message('Using stored API key!')
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
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()
}

get_exposure_functional_use_category <- function(API_key = NULL,
                                                 Server = "https://api-ccte-stg.epa.gov/exposure"){
   if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      message('Using stored API key!')
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
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()
}

get_exposure_product_data <- function(DTXSID = NULL,
                                      API_key = NULL,
                                      Server = "https://api-ccte-stg.epa.gov/exposure"){
  if (is.null(DTXSID))
    stop('Please input an DTXSID!')
  else if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      message('Using stored API key!')
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
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()
}

get_exposure_product_data_puc <- function(API_key = NULL,
                                          Server = "https://api-ccte-stg.epa.gov/exposure"){
  if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      message('Using stored API key!')
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
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()
}


get_exposure_list_presence_tags <- function(API_key = NULL,
                                            Server = "https://api-ccte-stg.epa.gov/exposure"){
  if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      message('Using stored API key!')
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
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()
}

get_exposure_list_presence_tags_by_dtxsid <- function(DTXSID = NULL,
                                                      API_key = NULL,
                                                      Server = "https://api-ccte-stg.epa.gov/exposure"){
  if (is.null(DTXSID))
    stop('Please input an DTXSID!')
  else if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      message('Using stored API key!')
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
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()
}
