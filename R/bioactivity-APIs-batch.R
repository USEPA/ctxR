#' Retrieve bioactivity data from DTXSID or AEID batch
#'
#' @param DTXSID A list of chemical identifier DTXSIDs.
#' @param AEID A list of assay endpoint identifiers AEIDs.
#' @param API_key The user-specific API key.
#' @param Server The root address for the API endpoint
#' @param rate_limit Number of seconds to wait between each request
#'
#' @return A named list of data.frames containing bioactivity information for
#'   the chemicals with DTXSID or assays with AEID matching the input parameter.
#' @export


get_bioactivity_details_batch <- function(DTXSID = NULL,
                                          AEID = NULL,
                                          API_key = NULL,
                                          Server = NULL,
                                          rate_limit = 0L){
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    }
  }
  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }

  if (is.null(Server)){
    Server <- bioactivity_api_server
  }

  if (!is.null(DTXSID)){
    if (!is.character(DTXSID) & !all(sapply(DTXSID, is.character))){
      stop('Please input a character list for DTXSID!')
    }
    DTXSID <- unique(DTXSID)
    print('Using DTXSID!')
    results <- lapply(DTXSID, function(d){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_bioactivity_details(DTXSID = d,
                                  API_key = API_key,
                                  Server = Server)
        },
        error = function(cond){
          message(d)
          message(cond$message)
          return(NA)
        }
      )
      return(attempt)
    }
    )
    names(results) <- DTXSID
    return(results)
  } else if (!is.null(AEID)){
    AEID <- unique(AEID)
    print('Using AEID!')
    results <- lapply(AEID, function(a){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_bioactivity_details(AEID = a,
                                  API_key = API_key)
        },
        error = function(cond){
          message(a)
          message(cond$message)
          return(NA)
        }
      )
      return(attempt)
    }
    )
    names(results) <- AEID
    return(results)
  } else {
    stop('Please input a list of DTXSIDs or AEIDs!')
  }
}

#' Retrieve bioactivity summary data from AEID batch
#'
#' @param AEID A list of AEID identifiers
#' @param API_key The user-specific API key.
#' @param Server The root address for the API endpoint
#' @param rate_limit Number of seconds to wait between each request
#'
#' @return A named list of data.frames containing bioactivity summary
#'   information for the assays with AEID matching the input parameter.



get_bioactivity_summary_batch <- function(AEID = NULL,
                                          API_key = NULL,
                                          Server = NULL,
                                          rate_limit = 0L){
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    }
  }
  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }
  if (is.null(Server)){
    Server <- bioactivity_api_server
  }

  if (!is.null(AEID)){
    AEID <- unique(AEID)
    print('Using AEID!')
    results <- lapply(AEID, function(a){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_bioactivity_summary(AEID = a,
                                  API_key = API_key,
                                  Server = Server)
        },
        error = function(cond){
          message(a)
          message(cond$message)
          return(NA)
        }
      )
      return(attempt)
    }
    )
    names(results) <- AEID
    return(results)
  } else {
    stop('Please input a list of AEIDs!')
  }
}


#' Retrieve annotations for AEID batch
#'
#' @param AEID A list of AEID identifiers
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param rate_limit Number of seconds to wait between each request
#'
#' @return A named list of data.frames containing annotation information for the
#'   assays with AEID matching the input parameter.


get_annotation_by_aeid_batch <- function(AEID = NULL,
                                         API_key = NULL,
                                         Server = NULL,
                                         rate_limit = 0L){
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    }
  }
  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }
  if (is.null(Server)){
    Server <- bioactivity_api_server
  }

  if (!is.null(AEID)){
    AEID <- unique(AEID)
    print('Using AEID!')
    results <- lapply(AEID, function(a){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_annotation_by_aeid(AEID = a,
                                 API_key = API_key,
                                 Server = Server)
        },
        error = function(cond){
          message(a)
          message(cond$message)
          return(NA)
        }
      )
      return(attempt)
    }
    )
    names(results) <- AEID
    return(results)
  } else {
    stop('Please input a list of AEIDs!')
  }

}
