#' Get hazard data by DTXSID
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#'
#' @return A data.frame containing chemical (human and eco) hazard data
#' @export


get_hazard_by_dtxsid <- function(DTXSID = NULL,
                               API_key = NULL){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key))
    stop('Please input an API_key')

  response <- httr::GET(url = paste0('http://api-ccte.epa.gov/hazard/search/by-dtxsid/', DTXSID),
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

#' Get human hazard data by DTXSID
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#'
#' @return A data.frame containing chemical human hazard data
#' @export


get_human_hazard_by_dtxsid <- function(DTXSID = NULL,
                                 API_key = NULL){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key))
    stop('Please input an API_key')

  response <- httr::GET(url = paste0('http://api-ccte.epa.gov/hazard/human/search/by-dtxsid/', DTXSID),
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

#' Get ecotox hazard data by DTXSID
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#'
#' @return A data.frame containing chemical (ecotox) hazard data
#' @export


get_ecotox_hazard_by_dtxsid <- function(DTXSID = NULL,
                                 API_key = NULL){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key))
    stop('Please input an API_key')

  response <- httr::GET(url = paste0('http://api-ccte.epa.gov/hazard/eco/search/by-dtxsid/', DTXSID),
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
