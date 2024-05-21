#' Retrieve bioactivity data from DTXSID, AEID, SPID, or m4id
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param AEID The assay endpoint identifier AEID
#' @param SPID The ChemSpider chemical input
#' @param m4id The chemical identifier m4id
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame containing bioactivity information for the chemical or assay endpoint with
#'   identifier matching the input parameter.
#' @export
#' @examplesIf has_ccte_key() & is.na(ccte_key() == 'FAKE_KEY')
#' # Pull BPA bioactivity details
#' bpa <- get_bioactivity_details(DTXSID = 'DTXSID7020182')
#' # Pull assay bioactivity details
#' assay <- get_bioactivity_details(AEID = 159)
get_bioactivity_details <- function(DTXSID = NULL,
                                 AEID = NULL,
                                 SPID = NULL,
                                 m4id = NULL,
                                 API_key = NULL,
                                 Server = bioactivity_api_server,
                                 verbose = FALSE){
  #if (is.null(DTXSID) & is.null(AEID))#
  if (all(sapply(list(DTXSID, AEID, SPID, m4id), is.null)))
    stop('Please input a DTXSID, AEID, SPID, or m4id!')
  #else if (!is.null(DTXSID) & !is.null(AEID))
  else if (length(which(!sapply(list(DTXSID, AEID, SPID, m4id), is.null))) > 1)
    stop('Please input a value for only one of DTXSID, AEID, SPID, or m4id, but not multiple!')
  else if (is.null(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
  }

  data_index <- which(!sapply(list(DTXSID, AEID, SPID, m4id), is.null))
  data_endpoint <- paste0('by-', c('dtxsid', 'aeid', 'spid', 'm4id')[data_index])
  data_input <- unlist(list(DTXSID, AEID, SPID, m4id)[data_index])

  if (verbose){
    print(data_index)
    print(data_endpoint)
    print(data_input)
  }


  response <- httr::GET(url = paste0(Server, '/data/search/', data_endpoint, '/', ifelse(data_index == 3, prepare_word(data_input), data_input)),
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
    req_content <- httr::content(response, as = 'text', encoding = "UTF-8")
    if (nchar(req_content) == 0){
      return(data.table::data.table())
    }
    res <- jsonlite::fromJSON(req_content)
    if (!is.data.frame(res) & (length(res) != 0)){
      for (i in 1:length(res)){
        if (is.null(res[[i]])) res[[i]] <- NA # set any NULLs to NA
        if (length(res[[i]]) > 1) {
          res[[i]] <- list(res[[i]]) # put lengths > 1 into a list to be just length 1, will unnest after
        }
      }
      res <- tibble::as_tibble_row(res)
    }
    param_cols <- c('mc3Param', 'mc4Param', 'mc5Param', 'mc6Param')
    col_index <- which(param_cols %in% names(res))
    if (length(col_index) > 0){
      # In some cases, columns are given by data.frames and we will not try to unnest these
      non_df_cols <- param_cols[col_index][which(sapply(param_cols[col_index], function(t){!is.data.frame(res[[t]])}))]
      if (length(non_df_cols) > 0)
        # In some cases, columns will be NA (in the m4id cases) and we will not try to unnest these
        col_index <- which(unname(!sapply(res[which(names(res) %in% non_df_cols)], is.na)))
      if (length(col_index) > 0){
        res <- tidyr::unnest_wider(data = res, col = param_cols[col_index])
      }
     }
    res_dt <- data.table::data.table(res)
    return(res_dt)
  } else {
    if (verbose){
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()
}

#' Retrieve bioactivity summary for AEID
#'
#' @param AEID The assay endpoint indentifier AEID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame containing summary information corresponding to the
#'   input AEID
#' @export
#' @examplesIf has_ccte_key() & is.na(ccte_key() == 'FAKE_KEY')
#' # Pull an assay bioactivity summary
#' aeid_1386 <- get_bioactivity_summary(AEID = 1386)
get_bioactivity_summary <- function(AEID = NULL,
                                    API_key = NULL,
                                    Server = bioactivity_api_server,
                                    verbose = FALSE){
  #print("This is broken currently!")
  #return()
  if (is.null(AEID))
    stop('Please input an AEID!')
  else if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      if (verbose) {
        message('Using stored API key!')
      }
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
      res <- jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8"))
      for (i in 1:length(res)){
        if (is.null(res[[i]])) res[[i]] <- NA # set any NULLs to NA
        if (length(res[[i]]) > 1) {
          res[[i]] <- list(res[[i]]) # put lengths > 1 into a list to be just length 1, will unnest after
        }
      }

      res_dt <- data.table::as.data.table(res)

      return(res_dt)
      return(res)
    } else if (length(response$content) == 0){
      return(data.table::data.table(aeid = NA_integer_,
                  activeMc = NA_integer_,
                  totalMc = NA_integer_,
                  activeSc = NA_integer_,
                  totalSc = NA_integer_))
    }
  } else {
    if (verbose){
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()

}

#' Retrieve all assays
#'
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame containing all the assays and associated information
#' @export
#' @examplesIf has_ccte_key() & is.na(ccte_key() == 'FAKE_KEY')
#' # Retrieve all assays
#' assays <- get_all_assays()
get_all_assays <- function(API_key = NULL,
                           Server = bioactivity_api_server,
                           verbose = FALSE){

  if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      if (verbose) {
        message('Using stored API key!')
      }
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
    res <- jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8"))
    res[c('gene', 'assayList', 'citations')] <- lapply(res[c('gene', 'assayList', 'citations')],
                                                       function(df) do.call('mapply', c(list, df,
                                                                                        SIMPLIFY = FALSE,
                                                                                        USE.NAMES = FALSE)))
    return(res)
  } else {
    if (verbose){
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()
}

#' Retrieve annotations for AEID
#'
#' @param AEID The assay endpoint identifier AEID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame containing the annotated assays corresponding to the
#'   input AEID parameter
#' @export
#' @examplesIf has_ccte_key() & is.na(ccte_key() == 'FAKE_KEY')
#' # Retrieve annotation for an assay
#' annotation <- get_annotation_by_aeid(AEID = 159)
get_annotation_by_aeid <- function(AEID = NULL,
                                   API_key = NULL,
                                   Server = bioactivity_api_server,
                                   verbose = FALSE){
  if (is.null(AEID))
    stop('Please input an AEID!')
  else if (is.null(API_key)){
    if (has_ccte_key()){
      API_key <- ccte_key()
      if (verbose) {
        message('Using stored API key!')
      }
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
      res <- jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8"))
      for (i in 1:length(res)){
        if (is.null(res[[i]])) res[[i]] <- NA # set any nulls to NA
        if (length(res[[i]]) > 1) {
          res[[i]] <- list(res[[i]]) # put lengths > 1 into a list to be just length 1, will unnest after
        }
      }

      res_dt <- data.table::as.data.table(res)

      return(res_dt)
    } else {
      if (verbose){
        print('The request was successful but there is no information to return...')
      }
      return(list())
    }
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()


}
