#' Retrieve bioactivity data from DTXSID or AEID batch
#'
#' @param DTXSID A list of chemical identifier DTXSIDs.
#' @param AEID A list of assay endpoint identifiers AEIDs.
#' @param SPID A list of ChemSpider chemical inputs
#' @param m4id A list of chemical identifier m4ids
#' @param API_key The user-specific API key.
#' @param Server The root address for the API endpoint
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of data.frames containing bioactivity information for
#'   the chemicals with DTXSID or assays with AEID matching the input parameter.
#' @export
#' @examplesIf ccte_key() == 'FAKE_KEY'
#' # Pull bioactivity details for multiple chemicals
#' dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
#' batch_bioactivity <- get_bioactivity_details_batch(DTXSID = dtxsid)
#' # Pull bioactivity details for multiple assays
#' batch_bioactivity <- get_bioactivity_details_batch(AEID = c(159, 160))

get_bioactivity_details_batch <- function(DTXSID = NULL,
                                          AEID = NULL,
                                          SPID = NULL,
                                          m4id = NULL,
                                          API_key = NULL,
                                          Server = NULL,
                                          rate_limit = 0L,
                                          verbose = FALSE){
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      if (verbose) {
        message('Using stored API key!')
      }
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
    if (verbose) {
      print('Using DTXSID!')
    }
    results <- lapply(DTXSID, function(d){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_bioactivity_details(DTXSID = d,
                                  API_key = API_key,
                                  Server = Server,
                                  verbose = verbose)
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
    if (verbose){
      print('Using AEID!')
    }
    results <- lapply(AEID, function(a){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_bioactivity_details(AEID = a,
                                  API_key = API_key,
                                  Server = Server,
                                  verbose = verbose)
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
  } else if (!is.null(SPID)){
    SPID <- unique(SPID)
    if (verbose){
      print('Using SPID!')
    }
    results <- lapply(SPID, function(a){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_bioactivity_details(SPID = a,
                                  API_key = API_key,
                                  Server = Server,
                                  verbose = verbose)
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
    names(results) <- SPID
    return(results)
  } else if (!is.null(m4id)){
    m4id <- unique(m4id)
    if (verbose){
      print('Using m4id!')
    }
    results <- lapply(m4id, function(a){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_bioactivity_details(m4id = a,
                                  API_key = API_key,
                                  Server = Server,
                                  verbose = verbose)
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
    names(results) <- m4id
    return(results)
  } else {
    stop('Please input a list of DTXSIDs, AEIDs, SPIDs, or m4ids!')
  }
}

#' Retrieve bioactivity summary data from AEID batch
#'
#' @param AEID A list of AEID identifiers
#' @param API_key The user-specific API key.
#' @param Server The root address for the API endpoint
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of data.frames containing bioactivity summary
#'   information for the assays with AEID matching the input parameter.
#' @export
#' @examplesIf ccte_key() == 'FAKE_KEY'
#' # Get bioactivity summary for multiple aeids
#' aeids <- get_bioactivity_summary_batch(AEID = c(159, 160))



get_bioactivity_summary_batch <- function(AEID = NULL,
                                          API_key = NULL,
                                          Server = NULL,
                                          rate_limit = 0L,
                                          verbose = FALSE){
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      if (verbose) {
        message('Using stored API key!')
      }
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
    if (verbose){
      print('Using AEID!')
    }
    results <- lapply(AEID, function(a){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_bioactivity_summary(AEID = a,
                                  API_key = API_key,
                                  Server = Server,
                                  verbose = verbose)
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
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of data.frames containing annotation information for the
#'   assays with AEID matching the input parameter.
#' @export
#' @examplesIf ccte_key() == 'FAKE_KEY'
#' # Get annotations for multiple aeids
#' aeid_annotations <- get_annotation_by_aeid_batch(AEID = c(159, 160))

get_annotation_by_aeid_batch <- function(AEID = NULL,
                                         API_key = NULL,
                                         Server = NULL,
                                         rate_limit = 0L,
                                         verbose = FALSE){
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      if (verbose) {
        message('Using stored API key!')
      }
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
    if (verbose) {
      print('Using AEID!')
    }
    results <- lapply(AEID, function(a){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_annotation_by_aeid(AEID = a,
                                 API_key = API_key,
                                 Server = Server,
                                 verbose = verbose)
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
