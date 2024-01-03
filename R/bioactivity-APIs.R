#' Retrieve bioactivity data from DTXSID or AEID
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param AEID The chemical identifier AEID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#'
#' @return A data.frame containing bioactivity information for the chemical with
#'   DTXSID or assay with AEID matching the input parameter.
#' @export
#'
get_bioactivity_details <- function(DTXSID = NULL,
                                 AEID = NULL,
                                 API_key = NULL,
                                 Server = bioactivity_api_server){
  if (is.null(DTXSID) & is.null(AEID))# if (all(sapply(list(DTXSID, AEID, SPID, m4ID), is.null)))
    stop('Please input a DTXSID or AEID!') # " or SPID or m4ID"
  else if (!is.null(DTXSID) & !is.null(AEID)) # else if (length(which(!sapply(list(DTXSID, AEID, SPID, m4ID), is.null))) > 1)
    stop('Please input either a DTXSID or AEID, but not both!') #  or SPID or m4ID, but not multiple!
  else if (is.null(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input an API_key!')
    }
  }

  # data_index <- which(!sapply(list(DTXSID, AEID, SPID, m4ID), is.null))
  # data_endpoint <- paste0('by-', c('dtxsid', 'aeid', 'spid', 'm4id')[data_index])

  if (!is.null(DTXSID)){
    response <- httr::GET(url = paste0(Server, '/search/by-dtxsid/', DTXSID),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  } else {
    response <- httr::GET(url = paste0(Server, '/search/by-aeid/', AEID),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  }


  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()
}

get_bioactivity_summary <- function(AEID = NULL,
                                    API_key = NULL,
                                    Server = bioactivity_api_server){
  print("This is broken currently!")
  return()
  if (is.null(AEID))
    stop('Please input an AEID!')
  else if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input an API_key!')
    }
  }

  response <- httr::GET(url = paste0(Server, '/data/summary/search/by-aeid/', AEID),
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
