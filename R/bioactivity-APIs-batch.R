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
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
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

  #if (is.null(DTXSID) & is.null(AEID))#
  if (all(sapply(list(DTXSID, AEID, SPID, m4id), is.null)))
    stop('Please input a DTXSID, AEID, SPID, or m4id!')
  #else if (!is.null(DTXSID) & !is.null(AEID))
  else if (length(which(!sapply(list(DTXSID, AEID, SPID, m4id), is.null))) > 1)
    stop('Please input a value for only one of DTXSID, AEID, SPID, or m4id, but not multiple!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }

  if (is.null(Server)){
    Server <- bioactivity_api_server
  }

  data_index <- which(!sapply(list(DTXSID, AEID, SPID, m4id), is.null))
  data_endpoint <- paste0('by-', c('dtxsid', 'aeid', 'spid', 'm4id')[data_index])
  data_input <- unlist(list(DTXSID, AEID, SPID, m4id)[data_index])

  data_input <- unique(data_input)

  num_input <- length(data_input)
  indices <- generate_ranges(num_input)

  dt <- data.table::data.table()

  if (verbose){
    print(paste('Using', c('DTXSID', 'AEID', 'SPID', 'm4id')[data_index], '!'))
  }

  for (i in seq_along(indices)){
    print(i)
    print(data_endpoint)
    print(paste0(Server, '/data/search/',data_endpoint, '/'))
    print(indices[[i]])
    response <- httr::POST(url = paste0(Server, '/data/search/',data_endpoint, '/'),
                           httr::add_headers(.headers = c(
                             'Accept' = 'application/json',
                             'Content-Type' = 'application/json',
                             'x-api-key' = API_key
                           )),
                           body = jsonlite::toJSON(data_input[indices[[i]]], auto_unbox = ifelse(length(data_input[indices[[i]]]) > 1, 'T', 'F')))

    if (response$status_code == 401){
      stop(httr::content(response)$detail)
    }

    if (response$status_code == 200){
      #print(str(jsonlite::fromJSON(httr::content(response, as = 'text'))))
      if (i == 1){
        dt <- data.table::data.table(jsonlite::fromJSON(httr::content(response,
                                                                      as = 'text',
                                                                      encoding = "UTF-8")))
      }
      else {
        dt <- suppressWarnings(data.table::rbindlist(list(dt,
                                                          data.table::data.table(jsonlite::fromJSON(httr::content(response,
                                                                                                                  as = 'text',
                                                                                                                  encoding = "UTF-8")))),
                                                     fill = TRUE))
      }
      #return(data.frame(jsonlite::fromJSON(httr::content(response, as = 'text'))))
    }
    Sys.sleep(rate_limit)
  }

  return(dt)

  # if (!is.null(DTXSID)){
  #   if (!is.character(DTXSID) & !all(sapply(DTXSID, is.character))){
  #     stop('Please input a character list for DTXSID!')
  #   }
  #   DTXSID <- unique(DTXSID)
  #   if (verbose) {
  #     print('Using DTXSID!')
  #   }
  #
  #   num_dtxsid <- length(DTXSID)
  #   indices <- generate_ranges(num_dtxsid)
  #
  #   dt <- data.table::data.table()
  #
  #   for (i in seq_along(indices)){
  #     print(i)
  #     print(data_endpoint)
  #     print(paste0(Server, '/data/search/',data_endpoint))
  #     print(indices[[i]])
  #     response <- httr::POST(url = paste0(Server, '/data/search/',data_endpoint, '/'),
  #                          httr::add_headers(.headers = c(
  #                            'Accept' = 'application/json',
  #                            'Content-Type' = 'application/json',
  #                            'x-api-key' = API_key
  #                          )),
  #                          body = jsonlite::toJSON(DTXSID[indices[[i]]], auto_unbox = ifelse(length(DTXSID[indices[[i]]]) > 1, 'T', 'F')))
  #
  #     if (response$status_code == 401){
  #       stop(httr::content(response)$detail)
  #     }
  #
  #     if (response$status_code == 200){
  #       #print(str(jsonlite::fromJSON(httr::content(response, as = 'text'))))
  #       if (i == 1){
  #         dt <- data.table::data.table(jsonlite::fromJSON(httr::content(response,
  #                                                                       as = 'text',
  #                                                                       encoding = "UTF-8")))
  #       }
  #       else {
  #       dt <- suppressWarnings(data.table::rbindlist(list(dt,
  #                                                         data.table::data.table(jsonlite::fromJSON(httr::content(response,
  #                                                                                                                 as = 'text',
  #                                                                                                                 encoding = "UTF-8")))),
  #                                                    fill = TRUE))
  #       }
  #       #return(data.frame(jsonlite::fromJSON(httr::content(response, as = 'text'))))
  #     }
  #     Sys.sleep(rate_limit)
  #   }
  #
  #   return(dt)
  #
  #   results <- lapply(DTXSID, function(d){
  #     Sys.sleep(rate_limit)
  #     attempt <- tryCatch(
  #       {
  #         get_bioactivity_details(DTXSID = d,
  #                                 API_key = API_key,
  #                                 Server = Server,
  #                                 verbose = verbose)
  #       },
  #       error = function(cond){
  #         if (verbose){
  #           message(d)
  #           message(cond$message)
  #         }
  #         return(cond)
  #       }
  #     )
  #     return(attempt)
  #   }
  #   )
  #
  #   error_index <- which(sapply(results, function(t) {
  #     return('simpleError' %in% class(t))
  #   }))
  #   if (length(error_index) > 0){
  #     error <- results[[error_index[[1]]]]
  #     stop(error$message)
  #   }
  #
  #   names(results) <- DTXSID
  #   return(results)
  # } else if (!is.null(AEID)){
  #   AEID <- unique(AEID)
  #   if (verbose){
  #     print('Using AEID!')
  #   }
  #   results <- lapply(AEID, function(a){
  #     Sys.sleep(rate_limit)
  #     attempt <- tryCatch(
  #       {
  #         get_bioactivity_details(AEID = a,
  #                                 API_key = API_key,
  #                                 Server = Server,
  #                                 verbose = verbose)
  #       },
  #       error = function(cond){
  #         if (verbose){
  #           message(a)
  #           message(cond$message)
  #         }
  #         return(cond)
  #       }
  #     )
  #     return(attempt)
  #   }
  #   )
  #
  #   error_index <- which(sapply(results, function(t) {
  #     return('simpleError' %in% class(t))
  #   }))
  #   if (length(error_index) > 0){
  #     error <- results[[error_index[[1]]]]
  #     stop(error$message)
  #   }
  #
  #   names(results) <- AEID
  #   return(results)
  # } else if (!is.null(SPID)){
  #   SPID <- unique(SPID)
  #   if (verbose){
  #     print('Using SPID!')
  #   }
  #   results <- lapply(SPID, function(a){
  #     Sys.sleep(rate_limit)
  #     attempt <- tryCatch(
  #       {
  #         get_bioactivity_details(SPID = a,
  #                                 API_key = API_key,
  #                                 Server = Server,
  #                                 verbose = verbose)
  #       },
  #       error = function(cond){
  #         if (verbose) {
  #           message(a)
  #           message(cond$message)
  #         }
  #         return(cond)
  #       }
  #     )
  #     return(attempt)
  #   }
  #   )
  #
  #   error_index <- which(sapply(results, function(t) {
  #     return('simpleError' %in% class(t))
  #   }))
  #   if (length(error_index) > 0){
  #     error <- results[[error_index[[1]]]]
  #     stop(error$message)
  #   }
  #
  #   names(results) <- SPID
  #   return(results)
  # } else if (!is.null(m4id)){
  #   m4id <- unique(m4id)
  #   if (verbose){
  #     print('Using m4id!')
  #   }
  #   results <- lapply(m4id, function(a){
  #     Sys.sleep(rate_limit)
  #     attempt <- tryCatch(
  #       {
  #         get_bioactivity_details(m4id = a,
  #                                 API_key = API_key,
  #                                 Server = Server,
  #                                 verbose = verbose)
  #       },
  #       error = function(cond){
  #         if (verbose){
  #           message(a)
  #           message(cond$message)
  #         }
  #         return(cond)
  #       }
  #     )
  #     return(attempt)
  #   }
  #   )
  #
  #   error_index <- which(sapply(results, function(t) {
  #     return('simpleError' %in% class(t))
  #   }))
  #   if (length(error_index) > 0){
  #     error <- results[[error_index[[1]]]]
  #     stop(error$message)
  #   }
  #
  #   names(results) <- m4id
  #   return(results)
  # } else {
  #   stop('Please input a list of DTXSIDs, AEIDs, SPIDs, or m4ids!')
  # }
}

#' Get administered equivalent dose (AED) data for a given chemical batch
#'
#' @param DTXSID The chemical identifier DTXSIDs
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some "progress report" should be
#'   given.
#'
#' @returns A data.table of AED data derived from ToxCast in virto bioactivity
#'   data for given DTXSIDs.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Get data for DTXSID5021209 and DTXSID7020182
#' aed <- get_aed_data_batch(DTXSID = c('DTXSID5021209', 'DTXSID7020182'))
#' aed

get_aed_data_batch <- function(DTXSID = NULL,
                               API_key = NULL,
                               Server = NULL,
                               rate_limit = 0L,
                               verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input a list of DTXSIDs!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }
  if (is.null(Server)){
    Server <- bioactivity_api_server
  }

  DTXSID <- unique(DTXSID)

  num_input <- length(DTXSID)
  indices <- generate_ranges(num_input)

  dt <- data.table::data.table()

  for (i in seq_along(indices)){
    print(i)
    print(indices[[i]])
    response <- httr::POST(url = paste0(Server, '/data/aed/search/by-dtxsid/'),
                           httr::add_headers(.headers = c(
                             'Accept' = 'application/json',
                             'Content-Type' = 'application/json',
                             'x-api-key' = API_key
                           )),
                           body = jsonlite::toJSON(DTXSID[indices[[i]]], auto_unbox = ifelse(length(DTXSID[indices[[i]]]) > 1, 'T', 'F')))

    if (response$status_code == 401){
      stop(httr::content(response)$detail)
    }

    if (response$status_code == 200){
      #print(str(jsonlite::fromJSON(httr::content(response, as = 'text'))))
      if (i == 1){
        dt <- data.table::data.table(jsonlite::fromJSON(httr::content(response,
                                                                      as = 'text',
                                                                      encoding = "UTF-8")))
      }
      else {
        dt <- suppressWarnings(data.table::rbindlist(list(dt,
                                                          data.table::data.table(jsonlite::fromJSON(httr::content(response,
                                                                                                                  as = 'text',
                                                                                                                  encoding = "UTF-8")))),
                                                     fill = TRUE))
      }
      #return(data.frame(jsonlite::fromJSON(httr::content(response, as = 'text'))))
    }
    Sys.sleep(rate_limit)
  }

  if (dim(dt)[[1]] > 0){
    dt <- dt |> tidyr::unnest_longer(col = c('aedVal', 'aedType', 'aedValUnit', 'httkModel', 'httkVersion', 'potencyValType', 'invitrodbVersion', 'interindividualVarPerc'))
  }

  return(dt)

}

#' Retrieve bioactivity summary data from AEID batch
#'
#' @param DTXSID A list of chemical identifier DTXSIDs
#' @param AEID A list of AEID identifiers
#' @param API_key The user-specific API key.
#' @param Server The root address for the API endpoint
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of data.frames containing bioactivity summary
#'   information for the assays with AEID matching the input parameter.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Get bioactivity summary for multiple aeids
#' aeids <- get_bioactivity_summary_batch(AEID = c(159, 160))



get_bioactivity_summary_batch <- function(DTXSID = NULL,
                                          AEID = NULL,
                                          API_key = NULL,
                                          Server = NULL,
                                          rate_limit = 0L,
                                          verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }
  if (is.null(Server)){
    Server <- bioactivity_api_server
  }

  if (!is.null(DTXSID)){
    DTXSID <- unique(DTXSID)
    if (verbose){
      print('Using DTXSID!')
    }
    results <- lapply(DTXSID, function(d){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_bioactivity_summary(DTXSID = d,
                                  API_key = API_key,
                                  Server = Server,
                                  verbose = verbose)
        },
        error = function(cond){
          if (verbose) {
            message(d)
            message(cond$message)
          }
          return(cond)
        }
      )
      return(attempt)
    }
    )

    error_index <- which(sapply(results, function(t) {
      return('simpleError' %in% class(t))
    }))
    if (length(error_index) > 0){
      error <- results[[error_index[[1]]]]
      stop(error$message)
    }

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
          get_bioactivity_summary(AEID = a,
                                  API_key = API_key,
                                  Server = Server,
                                  verbose = verbose)
        },
        error = function(cond){
          if (verbose) {
            message(a)
            message(cond$message)
          }
          return(cond)
        }
      )
      return(attempt)
    }
    )

    error_index <- which(sapply(results, function(t) {
      return('simpleError' %in% class(t))
    }))
    if (length(error_index) > 0){
      error <- results[[error_index[[1]]]]
      stop(error$message)
    }

    names(results) <- AEID
    return(results)
  } else {
    stop('Please input a list of AEIDs or DTXSIDs!')
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
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Get annotations for multiple aeids
#' aeid_annotations <- get_annotation_by_aeid_batch(AEID = c(159, 160))

get_annotation_by_aeid_batch <- function(AEID = NULL,
                                         API_key = NULL,
                                         Server = NULL,
                                         rate_limit = 0L,
                                         verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
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
          if (verbose) {
            message(a)
            message(cond$message)
          }
          return(cond)
        }
      )
      return(attempt)
    }
    )

    error_index <- which(sapply(results, function(t) {
      return('simpleError' %in% class(t))
    }))
    if (length(error_index) > 0){
      error <- results[[error_index[[1]]]]
      stop(error$message)
    }

    names(results) <- AEID
    return(results)
  } else {
    stop('Please input a list of AEIDs!')
  }

}
