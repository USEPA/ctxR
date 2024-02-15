#' Retrieve bioactivity data from DTXSID, AEID, SPID, or m4id
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param AEID The assay endpoint identifier AEID
#' @param SPID The ChemSpider chemical input
#' @param m4id The chemical identifier m4id
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#'
#' @return A data.frame containing bioactivity information for the chemical or assay endpoint with
#'   identifier matching the input parameter.
#' @export
#'
get_bioactivity_details <- function(DTXSID = NULL,
                                 AEID = NULL,
                                 SPID = NULL,
                                 m4id = NULL,
                                 API_key = NULL,
                                 Server = bioactivity_api_server){
  #if (is.null(DTXSID) & is.null(AEID))#
  if (all(sapply(list(DTXSID, AEID, SPID, m4id), is.null)))
    stop('Please input a DTXSID, AEID, SPID, or m4id!')
  #else if (!is.null(DTXSID) & !is.null(AEID))
  else if (length(which(!sapply(list(DTXSID, AEID, SPID, m4id), is.null))) > 1)
    stop('Please input a value for only one of DTXSID, AEID, SPID, or m4id, but not multiple!')
  else if (is.null(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    }
  }

  data_index <- which(!sapply(list(DTXSID, AEID, SPID, m4id), is.null))
  data_endpoint <- paste0('by-', c('dtxsid', 'aeid', 'spid', 'm4id')[data_index])
  data_input <- unlist(list(DTXSID, AEID, SPID, m4id)[data_index])

  print(data_index)
  print(data_endpoint)
  print(data_input)

  response <- httr::GET(url = paste0(Server, '/search/', data_endpoint, '/', ifelse(data_index == 3, prepare_word(data_input), data_input)),
                        httr::add_headers(.headers = c(
                          'Content-Type' = 'application/json',
                          'x-api-key' = API_key)
                          )
                        )

  # if (!is.null(DTXSID)){
  #   response <- httr::GET(url = paste0(Server, '/search/by-dtxsid/', DTXSID),
  #                         httr::add_headers(.headers = c(
  #                           'Content-Type' =  'application/json',
  #                           'x-api-key' = API_key)
  #                         )
  #   )
  # } else {
  #   response <- httr::GET(url = paste0(Server, '/search/by-aeid/', AEID),
  #                         httr::add_headers(.headers = c(
  #                           'Content-Type' =  'application/json',
  #                           'x-api-key' = API_key)
  #                         )
  #   )
  # }

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

#' Retrieve bioactivity summary for AEID
#'
#' @param AEID The assay endpoint indentifier AEID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#'
#' @return A data.frame containing summary information corresponding to the
#'   input AEID
#' @export
#'
get_bioactivity_summary <- function(AEID = NULL,
                                    API_key = NULL,
                                    Server = bioactivity_api_server){
  #print("This is broken currently!")
  #return()
  if (is.null(AEID))
    stop('Please input an AEID!')
  else if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      message('Using stored API key!')
    }
  }

  response <- httr::GET(url = paste0(Server, '/data/summary/search/by-aeid/', AEID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
  if(response$status_code == 401){
    stop('Please input an API_key!')
  }
  if(response$status_code == 200){
    if (length(response$content) > 0){
      return(jsonlite::fromJSON(httr::content(response, as = 'text')))
    } else if (length(response$content) == 0){
      return(list(aeid = NA_integer_,
                  activeMc = NA_integer_,
                  totalMc = NA_integer_,
                  activeSc = as.pairlist(NULL),
                  totalSc = as.pairlist(NULL)))
    }
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()

}

#' Retrieve all assays
#'
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#'
#' @return A data.frame containing all the assays and associated information
#' @export
#'
get_all_assays <- function(API_key = NULL,
                           Server = bioactivity_api_server){

  if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      message('Using stored API key!')
    }
  }

  response <-  httr::GET(url = paste0(Server, '/assay/'),
                         httr::add_headers(.headers = c(
                           'Content-Type' =  'application/json',
                           'x-api-key' = API_key)
                         )
  )
  if(response$status_code == 401){
    stop('Please input an API_key!')
  }
  if(response$status_code == 200){
    res <- jsonlite::fromJSON(httr::content(response, as = 'text'))
    res[c('gene', 'assayList', 'citations')] <- lapply(res[c('gene', 'assayList', 'citations')],
                                                       function(df) do.call('mapply', c(list, df,
                                                                                        SIMPLIFY = FALSE,
                                                                                        USE.NAMES = FALSE)))
    return(res)
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()
}

#' Retrieve annotations for AEID
#'
#' @param AEID The assay endpoint identifier AEID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#'
#' @return A data.frame containing the annotated assays corresponding to the
#'   input AEID parameter
#' @export
#'
get_annotation_by_aeid <- function(AEID = NULL,
                                   API_key = NULL,
                                   Server = bioactivity_api_server){
  if (is.null(AEID))
    stop('Please input an AEID!')
  else if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      message('Using stored API key!')
    }
  }

  response <- httr::GET(url = paste0(Server, '/assay/search/by-aeid/', AEID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
  if(response$status_code == 401){
    stop('Please input an API_key!')
  }
  if(response$status_code == 200){
    if (length(response$content) > 0){
      return(jsonlite::fromJSON(httr::content(response, as = 'text')))
    } else {
      print('The request was successful but there is no information to return...')
      return(list())
    }
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()


}
