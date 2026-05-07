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
    stop('Please input a list of DTXSIDs, AEIDs, SPIDs, or m4ids!')
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
    print(paste0('Using ', c('DTXSID', 'AEID', 'SPID', 'm4id')[data_index], '!'))
  }

  for (i in seq_along(indices)){
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

#' Get single concentration data batch
#'
#' @param AEID The assay endpoint identifier AEIDs
#' @param Projection The format and concentration data returned. Allowed values
#'   are 'single-conc' and 'ccd-single-conc'. The default format is
#'   'single-conc'.
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some "progress report" should be
#'   given.
#'
#' @returns A named list of data.frames of single concentration screening data
#' for requested ToxCast assay component endpoint IDs (AEID).
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Get single conc data for AEID 3032 and 743
#' aeid_concs <- get_single_concentration_batch(AEID = c(3032, 743))
#'

get_single_concentration_batch <- function(AEID = NULL,
                                           Projection = 'single-conc',
                                           API_key = NULL,
                                           Server = bioactivity_api_server,
                                           rate_limit = 0L,
                                           verbose = FALSE){

  if (is.null(AEID))
    stop('Please input an AEID!')


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

  AEID <- unique(AEID)


  results <- lapply(AEID, function(a){
    Sys.sleep(rate_limit)
    attempt <- tryCatch(
      {
        get_single_concentration(AEID = a,
                                 Projection = Projection,
                                 API_key = API_key,
                                 Server = Server,
                                 verbose = verbose)
      },
      error = function(cond){
        if (verbose){
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
}

#' Get assay summary data by gene symbol batch
#'
#' @param geneSymbol The gene symbol.
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some "progress report" should be given.
#'
#' @returns A named list of data.frames of assay summary data for applicable
#' assays for requested offical gene symbols.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Retrieve summary data for gene symbol TUBA1A and IYD
#' summary <- get_assay_summary_by_gene_batch(geneSymbol = c('TUBA1A', 'IYD'))
#' summary
get_assay_summary_by_gene_batch <- function(geneSymbol = NULL,
                                      API_key = NULL,
                                      Server = bioactivity_api_server,
                                      rate_limit = 0L,
                                      verbose = FALSE){
  if (is.null(geneSymbol))
    stop('Please input an geneSymbol!')

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }
  if (is.null(Server)){
    Server <- bioactivity_api_server
  }

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }


  geneSymbol <- unique(geneSymbol)


  results <- lapply(geneSymbol, function(g){
    Sys.sleep(rate_limit)
    attempt <- tryCatch(
      {
        get_assay_summary_by_gene(geneSymbol = g,
                                  API_key = API_key,
                                  Server = Server,
                                  verbose = verbose)
      },
      error = function(cond){
        if (verbose){
          message(g)
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

  names(results) <- geneSymbol
  return(results)
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

#' Get list of chemical DTXSIDs for a given assay batch
#'
#' @param AEID The assay endpoint identifier AEIDs
#' @param Projection The format and DTXSID data returned. Allowed values are
#'   'dtxsidonly' and 'ccdassaydetails'. The default format
#'   is 'dtxsidonly'.
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be
#'   given.
#'
#' @returns A named list of DTXSIDs or data.frame of assay information.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' dtxsid_list <- get_chemicals_by_assay_batch(AEID = c(3032, 743))
#' dtxsid_list
get_chemicals_by_assay_batch <- function(AEID = NULL,
                                         Projection = 'dtxsidonly',
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
    results <- lapply(AEID, function(a){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_chemicals_by_assay(AEID = a,
                                 Projection = Projection,
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

#' Get summary data by DTXSID and assay tissue origin batch
#'
#' @param DTXSID The chemical identifier DTXSIDs
#' @param Tissue The tissue of origin for the assay
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @returns A named list of data.frames of summary data for the given chemicals and tissue.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Get data for DTXSID7020182 and DTXSID7024241 and liver
#' liver_summary <- get_bioactivity_summary_by_tissue(DTXSID = c('DTXSID7020182', 'DTXSID7024241'),
#'                                                    Tissue = 'liver')
#' liver_summary
#'

get_bioactivity_summary_by_tissue_batch <- function(DTXSID = NULL,
                                                    Tissue = NULL,
                                                    API_key = NULL,
                                                    Server = NULL,
                                                    rate_limit = 0L,
                                                    verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input a list of DTXSIDs!')

  if (is.null(Tissue))
    stop('Please input a Tissue!')

  if (length(Tissue) > 1){
    stop('Please specify only one tissue per list of DTXSIDs!')
  }


  if (is.null(Server)){
    Server = bioactivity_api_server
  }

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }

  DTXSID <- unique(DTXSID)

  results <- lapply(DTXSID, function(d){
    Sys.sleep(rate_limit)
    attempt <- tryCatch(
      {
        get_bioactivity_summary_by_tissue(DTXSID = d,
                                          Tissue = Tissue,
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

  names(results) <- paste0(DTXSID, '_', Tissue)
  return(results)
}

#' Get bioactivity model predictions by DTXSID batch
#'
#' @param DTXSID The chemical identifier DTXSIDs
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be
#'   given.
#'
#' @returns A named list of data.frames of ToxCast model prediction data
#' for given DTXSIDs.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Get predictions for DTXSID70201082 and DTXSID0020232
#' predictions <- get_predictions_by_dtxsid_batch(DTXSID = c('DTXSID7020182',
#'                                                           'DTXSID0020232'))
#' predictions
get_predictions_by_dtxsid_batch <- function(DTXSID = NULL,
                                            API_key = NULL,
                                            Server = NULL,
                                            rate_limit = 0L,
                                            verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input a list of DTXSIDs!')

  if (is.null(Server)){
    Server = bioactivity_api_server
  }

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }

  DTXSID <- unique(DTXSID)

  results <- lapply(DTXSID, function(d){
    Sys.sleep(rate_limit)
    attempt <- tryCatch(
      {
        get_predictions_by_dtxsid(DTXSID = d,
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

}

#' Get bioactivity model predictions by DTXSID and Model batch
#'
#' @param DTXSID The chemical identifier DTXSIDs
#' @param Model The ToxCast model type. Model type options include: 'CERAPP
#'   Potency Level (Consensus)', 'CERAPP Potency Level (From Literature)',
#'   'COMPARA (Consensus)', and 'ToxCast Pathway Model (AUC)'.
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be
#'   given.
#'
#' @returns A named list of data.frames of ToxCast model prediction data
#' for given DTXSIDs and Model.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Get predictions for DTXSID70201082 and DTXSID0020232 and Model CERAPP
#' predictions <- get_predictions_by_dtxsid_batch(DTXSID = c('DTXSID7020182',
#'                                                           'DTXSID0020232'),
#'                                                Model = 'CERAPP')
#' predictions
get_predictions_by_dtxsid_and_model_batch <- function(DTXSID = NULL,
                                                      Model = NULL,
                                                      API_key = NULL,
                                                      Server = NULL,
                                                      rate_limit = 0L,
                                                      verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input a list of DTXSIDs!')

  if (is.null(Model))
    stop('Please input a Model!')

  if (length(Model) > 1){
    stop('Please specify only one Model per list of DTXSIDs!')
  }

  if (is.null(Server)){
    Server = bioactivity_api_server
  }

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }

  DTXSID <- unique(DTXSID)

  results <- lapply(DTXSID, function(d){
    Sys.sleep(rate_limit)
    attempt <- tryCatch(
      {
        get_predictions_by_dtxsid_and_model(DTXSID = d,
                                            Model = Model,
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

  names(results) <- paste0(DTXSID, '_', Model)
  return(results)

}

#' Get analytical QC data for a chemical batch
#'
#' @param DTXSID The chemical identifier DTXSIDs
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be
#'   given.
#'
#' @returns A named list of data.frames of analytical QC data for the requested
#' DTXSIDs.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Get QC data for DTXSID7020182 and DTXSID0020232
#' aq <- get_analytical_qc(DTXSID = c('DTXSID7020182', 'DTXSID0020232'))
#' aq

get_analytical_qc_batch <- function(DTXSID = NULL,
                                    API_key = NULL,
                                    Server = NULL,
                                    rate_limit = 0L,
                                    verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input a list of DTXSIDs!')

  if (is.null(Server)){
    Server = bioactivity_api_server
  }

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }

  DTXSID <- unique(DTXSID)

  results <- lapply(DTXSID, function(d){
    Sys.sleep(rate_limit)
    attempt <- tryCatch(
      {
        get_analytical_qc(DTXSID = d,
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
}

#' Retrieve assays by starting characters batch
#'
#' @param word_list A vector of character strings of an assay names
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#' @param top Limit the number of returned entries.
#'
#' @returns A named list of data.frames of assay information for the given input.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Retrieve assays that start with the character string `ATG_S` and `APR`
#' assays <- assay_starts_with_batch(word_list = c('ATG_S', 'APR'))
#' assays
assay_starts_with_batch <- function(word_list = NULL,
                                    API_key = NULL,
                                    Server = bioactivity_api_server,
                                    rate_limit = 0L,
                                    verbose = FALSE,
                                    top = NULL){
  if (is.null(word_list))
    stop('Please input a list of strings!')

  if (is.null(Server)){
    Server = bioactivity_api_server
  }

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  word_list <- unique(word_list)

  results <- lapply(word_list, function(w){
    Sys.sleep(rate_limit)
    attempt <- tryCatch(
      {
        assay_starts_with(word = w,
                          API_key = API_key,
                          Server = Server,
                          verbose = verbose,
                          top = top)
      },
      error = function(cond){
        if (verbose) {
          message(w)
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

  names(results) <- word_list
  return(results)
}

#' Retrieve assays by exact match batch
#'
#' @param word_list A vector of character strings of an assay names
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @returns A named list of data.frames of assay information for the given input.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Retrieve assays that start with the character string `ATG_STAT3_CIS` and
#' # `APR_HepG2_MicrotubuleCSK_1hr`
#' assays <- assay_starts_with_batch(word_list = c('ATG_STAT3_CIS',
#'                                                 'APR_HepG2_MicrotubuleCSK_1hr'))
#' assays
assay_equal_batch <- function(word_list = NULL,
                              API_key = NULL,
                              Server = bioactivity_api_server,
                              rate_limit = 0L,
                              verbose = FALSE){
  if (is.null(word_list))
    stop('Please input a list of strings!')

  if (is.null(Server)){
    Server = bioactivity_api_server
  }

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  word_list <- unique(word_list)

  results <- lapply(word_list, function(w){
    Sys.sleep(rate_limit)
    attempt <- tryCatch(
      {
        assay_equal(word = w,
                    API_key = API_key,
                    Server = Server,
                    verbose = verbose)
      },
      error = function(cond){
        if (verbose) {
          message(w)
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

  names(results) <- word_list
  return(results)
}

#' Retrieve assays by substring batch
#'
#' @param word_list A vector of character strings of an assay names
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#' @param top Limit the number of returned entries.
#'
#' @returns A named list of data.frames of assay information for the given input.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Retrieve assays that contain the character string `ATG_S` or `APR`
#' assays <- assay_contains_batch(word_list = c('ATG_S', 'APR'))
#' assays
assay_contains_batch <- function(word_list = NULL,
                                 API_key = NULL,
                                 Server = bioactivity_api_server,
                                 rate_limit = 0L,
                                 verbose = FALSE,
                                 top = NULL){
  if (is.null(word_list))
    stop('Please input a list of strings!')

  if (is.null(Server)){
    Server = bioactivity_api_server
  }

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  word_list <- unique(word_list)

  results <- lapply(word_list, function(w){
    Sys.sleep(rate_limit)
    attempt <- tryCatch(
      {
        assay_contains(word = w,
                       API_key = API_key,
                       Server = Server,
                       verbose = verbose,
                       top = top)
      },
      error = function(cond){
        if (verbose) {
          message(w)
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

  names(results) <- word_list
  return(results)
}

#' Get ToxCast-mapped AOP data batch
#'
#' @param AEID The assay endpoint identifier AEIDs
#' @param KeyEvent The Key Event numbers
#' @param EntrezGeneId The Entrez Gene IDs
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @returns A named list of data.frames of ToxCast-mapped AOP data for the
#' given input.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # By AEID, Key Event, and Entrez Gene ID
#' aop_entrez <- get_aop_data_batch(EntrezGeneId = 196)
#' aop_entrez
#' aop_ke <- get_aop_data_batch(KeyEvent = 18)
#' aop_ke
#' aop_aeid <- get_aop_data_batch(AEID = 63)
#' aop_aeid
get_aop_data_batch <- function(AEID = NULL,
                         KeyEvent = NULL,
                         EntrezGeneId = NULL,
                         API_key = NULL,
                         Server = NULL,
                         rate_limit = 0L,
                         verbose = FALSE){

  #if (is.null(AEID) & is.null(KeyEvent) & is.null(EntrezGeneId))#
  if (all(sapply(list(AEID, KeyEvent, EntrezGeneId), is.null)))
    stop('Please input a list of AEIDs, KeyEvents, or EntrezGeneIds!')
  #else if (!is.null(AEID) & !is.null(KeyEvent) & !is.null(EntrezGeneId))
  else if (length(which(!sapply(list(AEID, KeyEvent, EntrezGeneId), is.null))) > 1)
    stop('Please input a value for only one of AEID,  KeyEvent, or EntrezGeneId but not multiple!')

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

  data_index <- which(!sapply(list(AEID, KeyEvent, EntrezGeneId), is.null))

  data_input <- unlist(list(AEID, KeyEvent, EntrezGeneId)[data_index])

  data_input <- unique(data_input)

  results <- lapply(data_input, function(d){
    Sys.sleep(rate_limit)
    attempt <- tryCatch(
      {
        if (data_index == 1){
          get_aop_data(AEID = d,
                       API_key = API_key,
                       Server = Server,
                       verbose = verbose)
        } else if (data_index == 2) {
          get_aop_data(KeyEvent = d,
                       API_key = API_key,
                       Server = Server,
                       verbose = verbose)
        } else {
          get_aop_data(EntrezGeneId = d,
                       API_key = API_key,
                       Server = Server,
                       verbose = verbose)
        }

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

  names(results) <- data_input
  return(results)

  }
