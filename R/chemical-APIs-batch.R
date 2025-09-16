

#' Retrieve chemical details from DTXSID of DTXCID in batch search
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param DTXCID The chemical identifier DTXCID
#' @param Projection The format and chemical detail data returned. Allowed
#'   values are 'chemicaldetailall', 'chemicaldetailstandard',
#'   chemicalidentifier', 'chemicalstructure', 'ntatoolkit',
#'   ccdchemicaldetails', 'compact'. If left empty or there is a
#'   mismatch, the default format will be 'chemicaldetailstandard'.
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be
#' given.
#'
#' @return A data.table (DTXSID) or a named list of data.tables (DTXCID)
#'   containing chemical information for the chemicals with DTXSID or DTXCID
#'   matching the input parameter.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemical details for multiple chemicals by dtxsid
#' dtxsids <- c('DTXSID7020182', 'DTXSID2021315')
#' dtxsid_details <- get_chemical_details_batch(DTXSID = dtxsid)
#' # Pull chemical details for multiple chemicals by dtxcid
#' dtxcids <- c('DTXCID30182', 'DTXCID001315')
#' dtxcid_details <- get_chemical_details_batch(DTXCID = dtxcids)

get_chemical_details_batch <- function(DTXSID = NULL,
                                       DTXCID = NULL,
                                       Projection = 'chemicaldetailstandard',
                                       API_key = NULL,
                                       rate_limit = 0L,
                                       verbose = FALSE){

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.null(DTXSID)){
    t <- get_chemical_details_batch_2(DTXSID = DTXSID,
                                      Projection = Projection,
                                      API_key = API_key,
                                      rate_limit = rate_limit,
                                      verbose = verbose)
    return(t)
  }


  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }
  if (!is.null(DTXSID)){
    if (!is.character(DTXSID) & !all(sapply(DTXSID, is.character))){
      stop('Please input a character list for DTXSID!')
    }
    DTXSID <- unique(DTXSID)
    if (verbose) {
      print('Using DTXSID!')
    }
    results <- lapply(DTXSID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_chemical_details(DTXSID = t,
                               Projection = Projection,
                               API_key = API_key,
                               verbose = verbose)
        },
        error = function(cond){
          message(t)
          message(cond$message)
          return(NA)
        }
      )
      return(attempt)
    }
    )
    names(results) <- DTXSID
  } else if (!is.null(DTXCID)){
    if (!is.character(DTXCID) & !all(sapply(DTXCID, is.character))){
      stop('Please input a character list for DTXCID!')
    }
    DTXCID <- unique(DTXCID)
    if (verbose) {
      print('Using DTXCID!')
    }
    results <- lapply(DTXCID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_chemical_details(DTXCID = t,
                               Projection = Projection,
                               API_key = API_key,
                               verbose = verbose)
        },
        error = function(cond){
          message(t)
          message(cond$message)
          return(NA)
        }
      )
      return(attempt)
    }
    )
    names(results) <- DTXCID
  } else {
    stop('Please input a list of DTXSIDs or DTXCIDs!')
  }
  return(results)
}

get_chemical_details_batch_2 <- function(DTXSID = NULL,
                                         Projection = 'chemicaldetailstandard',
                                         API_key = NULL,
                                         rate_limit = 0L,
                                         Server = chemical_api_server,
                                         verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }
  if (!is.null(DTXSID)){
    if (!is.character(DTXSID) & !all(sapply(DTXSID, is.character))){
      stop('Please input a character list for DTXSID!')
    }

    projection_entries <- c('chemicaldetailall',
                            'chemicaldetailstandard',
                            'chemicalidentifier',
                            'chemicalstructure',
                            'ntatoolkit',
                            'ccdchemicaldetails',
                            'compact')
    index <- 2
    if (!is.character(Projection)){
      warning('Setting `Projection` to `chemicaldetailstandard`')
      Projection <- 'chemicaldetailstandard'
    } else {
      Projection <- tolower(Projection)
      index <- which(projection_entries %in% Projection)
      if (length(index) == 0){
        stop('Please input a correct value for `Projection`!')
      } else if (length(index) > 1){
        warning('Setting `Projection` to `chemicaldetailstandard`')
        Projection <- 'chemicaldetailstandard'
        index <- 2
      } else {
        if (length(Projection) > 1){
          if (verbose){
            message(paste0('Using `Projection` = ', projection_entries[index], '!'))
          }
        }
        Projection <- projection_entries[index]
      }
    }

    projection_url <- paste0('?projection=', Projection)


    DTXSID <- unique(DTXSID)
    num_dtxsid <- length(DTXSID)
    indices <- generate_ranges(num_dtxsid)
    if (verbose) {
      print(indices)
    }

    dt <- create_data.table_chemical_details(index = index)

    for (i in seq_along(indices)){
      #dtxsid_list <- list(DTXSID[indices[[i]]])
        #generate_dtxsid_string(DTXSID[indices[[i]]])
      #print(dtxsid_list[1])
      # print(paste('The current index is i =', i))
      # print(DTXSID[indices[[i]]])
      # print(jsonlite::toJSON(DTXSID[indices[[i]]], auto_unbox = T, pretty = T))

      response <- httr::POST(url = paste0(Server, '/detail/search/by-dtxsid/', projection_url),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'application/json',
                               'x-api-key' = API_key
                             )),
                             body = jsonlite::toJSON(DTXSID[indices[[i]]], auto_unbox = ifelse(length(DTXSID[indices[[i]]]) > 1, 'T', 'F')))
      # response <- httr::GET(url = paste0(Server, '/detail/search/by-dtxsid/'),
      #                                   httr::add_headers(.headers = c(
      #                                     'Content-Type' =  'application/json',
      #                                     'x-api-key' = API_key)),
      #                                     query = list(d = paste0('[', DTXSID[indices[[i]]], ']'))
      #                                   )


    # print(paste('The response code is', response$status_code, 'for index i =', i))


    if (response$status_code == 401){
      stop(httr::content(response)$detail)
    }

    if (response$status_code == 200){
      #print(str(jsonlite::fromJSON(httr::content(response, as = 'text'))))
      dt <- suppressWarnings(data.table::rbindlist(list(dt,
                                                        data.table::data.table(jsonlite::fromJSON(httr::content(response,
                                                                                                                as = 'text',
                                                                                                                encoding = "UTF-8")))),
                                                   fill = TRUE))
      #return(data.frame(jsonlite::fromJSON(httr::content(response, as = 'text'))))
    }
  Sys.sleep(rate_limit)
    }

    #return(response)

    # results <- lapply(DTXSID, function(t){
    #   Sys.sleep(rate_limit)
    #   attempt <- tryCatch(
    #     {
    #       get_chemical_details(DTXSID = t,
    #                            Projection = Projection,
    #                            API_key = API_key)
    #     },
    #     error = function(cond){
    #       message(t)
    #       message(cond$message)
    #       return(NA)
    #     }
    #   )
    #   return(attempt)
    # }
    # )
    # names(results) <- DTXSID
  }
  return(dt)
}

generate_ranges <- function(end, limit = 200){
  if (!is.numeric(end) || end < 1) return(list())

  int_seq <- c(1:(as.integer(end)))

  indices = list(ceiling(length(int_seq)/limit))

  if (length(int_seq) > limit){
    for (i in 1:(ceiling(length(int_seq)/limit) - 1)){
      start <- limit*(i-1) + 1
      end <- limit*i
      indices[[i]] <- c(start:end)
      #print(200*(i-1) + 1)
      #print(200*i)
      #print(c((200*(i-1) + 1):(200*i)))
    }
    j <- length(indices)
    indices[[j+1]] <- c((limit*j + 1):length(int_seq))
    return(indices)
  }
  indices[[1]] <- int_seq
  return(indices)

}

generate_dtxsid_string <- function(items){
  dtxsid_string <- paste0('[', paste0('"', items, '"', collapse = ', '), ']')
  return(dtxsid_string)
}

#' Check existence by DTXSID batch
#'
#' @param DTXSID The chemical identifier DTXSIDs
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between each request.
#' @param Server The root address of the API endpoint
#' @param verbose A logical indicating whether some "progress report" should be
#' given.
#'
#' @return A data.table of information detailing valid and invalid DTXSIDs.
#' @export
#'
#' @examplesIf FALSE
#' dtxsids <- c('DTXSID7020182F', 'DTXSID7020182', 'DTXSID0020232F')
#' existence <- check_existence_by_dtxsid_batch(DTXSID = dtxsids)
check_existence_by_dtxsid_batch <- function(DTXSID = NULL,
                                            API_key = NULL,
                                            rate_limit = 0L,
                                            Server = chemical_api_server,
                                            verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }



  if (!is.null(DTXSID)){
    if (!is.character(DTXSID) & !all(sapply(DTXSID, is.character))){
      stop('Please input a character list for DTXSID!')
    }

    DTXSID <- unique(DTXSID)
    num_DTXSID <- length(DTXSID)
    indices <- generate_ranges(num_DTXSID)

    dt <- data.table::data.table(dtxsid = character(),
                                 isSafetyData = logical(),
                                 safetyUrl = character())

    #names(dt) <- names

    for (i in seq_along(indices)){
      if (verbose) {
        print(paste('The current index is i =', i, 'out of', length(indices)))
      }

      response <- httr::POST(url = paste0(Server, '/ghslink/to-dtxsid/'),
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
        if (length(response$content) > 0){
          res_content <- jsonlite::fromJSON(httr::content(response,
                                                          as = 'text',
                                                          encoding = "UTF-8"))
          if (length(res_content$safetyUrl) > 0){


          null_indices <- which(sapply(res_content$safetyUrl, is.null))
          if (length(null_indices) > 0){
            res_content$safetyUrl[null_indices] <- NA_character_
          }
          dt <- suppressWarnings(data.table::rbindlist(list(dt,
                                                            data.table::rbindlist(list(res_content))),
                                                       fill = TRUE))

          }
        }
      }
      Sys.sleep(rate_limit)
    }

    # Fix for bug in endpoint. DTXSIDs that are not valid do not have information
    # returned. To overcome this, the single search on the missing DTXSIDs is
    # exectued and combined with the valid responses.
    missing <- setdiff(DTXSID, dt$dtxsid)
    if (length(missing) > 0){
      missing_info <- lapply(missing, function(t){
          Sys.sleep(rate_limit)
          attempt <- tryCatch({
            check_existence_by_dtxsid(DTXSID = t,
                                      API_key = API_key,
                                      verbose = verbose)
          },
          error = function(cond){
            if (verbose){
              message('There was an error!')
              message(cond$message)
            }
            return(cond)
          }
          )
          return(attempt)
        })

      error_index <- which(sapply(missing_info, function(t){
        return('simpleError' %in% class(t))
      }))
      if (length(error_index) > 0){
        message <- missing_info[[error_index[[1]]]]
        stop(message$message)
      }

      missing_info <- data.table::rbindlist(missing_info)

      final <- data.table::rbindlist(list(dt, missing_info), fill = TRUE)
      return(final[match(DTXSID, final$dtxsid),])
    }



  } else {
    stop('Please input a list of DTXSIDs!')
  }
  return(dt)
}

get_smiles_batch <- function(names = NULL,
                             API_key = NULL,
                             rate_limit = 0L,
                             Server = chemical_api_server,
                             verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }
  if (!is.null(names)){
    if (!is.character(names) & !all(sapply(names, is.character))){
      stop('Please input a character list for names!')
    }

    names <- unique(names)
    num_names <- length(names)
    indices <- generate_ranges(num_names)

    dt <- character(length = num_names)
    names(dt) <- names

    for (i in seq_along(indices)){
      if (verbose) {
        print(paste('The current index is i =', i, 'out of', length(indices)))
      }

      response <- httr::POST(url = paste0(Server, '/indigo/to-smiles'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'text/plain',
                               'x-api-key' = API_key
                             )),
                             body = c(names[indices[[i]]]))

      if (response$status_code == 200){
        if (length(response$content) > 0){
          response_list <- list(httr::content(response, as = 'text', encoding = "UTF-8"))
        } else {
          response_list <- rep('', times = length(indices[[i]]))
        }
        dt[indices[[i]]] <- response_list
        }
      Sys.sleep(rate_limit)
    }

  }
  return(dt)

}

get_molecular_weight_batch <- function(names = NULL,
                                       API_key = NULL,
                                       rate_limit = 0L,
                                       Server = chemical_api_server,
                                       verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }
  if (!is.null(names)){
    if (!is.character(names) & !all(sapply(names, is.character))){
      stop('Please input a character list for names!')
    }

    names <- unique(names)
    num_names <- length(names)
    indices <- generate_ranges(num_names)

    dt <- character(length = num_names)
    names(dt) <- names

    for (i in seq_along(indices)){
      if (verbose) {
        print(paste('The current index is i =', i, 'out of', length(indices)))
      }

      response <- httr::POST(url = paste0(Server, '/indigo/to-molweight'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'text/plain',
                               'x-api-key' = API_key
                             )),
                             body = c(names[indices[[i]]]))

      if (response$status_code == 200){
        if (length(response$content) > 0){
          response_list <- list(httr::content(response, as = 'text', encoding = "UTF-8"))
        } else {
          response_list <- rep('', times = length(indices[[i]]))
        }
        dt[indices[[i]]] <- response_list
      }
      Sys.sleep(rate_limit)
    }

  }
  return(dt)

}

get_mol_v3000_batch <- function(names = NULL,
                                API_key = NULL,
                                rate_limit = 0L,
                                Server = chemical_api_server,
                                verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }
  if (!is.null(names)){
    if (!is.character(names) & !all(sapply(names, is.character))){
      stop('Please input a character list for names!')
    }

    names <- unique(names)
    num_names <- length(names)
    indices <- generate_ranges(num_names)

    dt <- character(length = num_names)
    names(dt) <- names

    for (i in seq_along(indices)){
      if (verbose) {
        print(paste('The current index is i =', i, 'out of', length(indices)))
      }

      response <- httr::POST(url = paste0(Server, '/indigo/to-mol3000'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'text/plain',
                               'x-api-key' = API_key
                             )),
                             body = c(names[indices[[i]]]))

      if (response$status_code == 200){
        if (length(response$content) > 0){
          response_list <- list(httr::content(response, as = 'text', encoding = "UTF-8"))
        } else {
          response_list <- rep('', times = length(indices[[i]]))
        }
        dt[indices[[i]]] <- response_list
      }
      Sys.sleep(rate_limit)
    }

  }
  return(dt)

}

get_mol_v2000_batch <- function(names = NULL,
                                API_key = NULL,
                                rate_limit = 0L,
                                Server = chemical_api_server,
                                verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }
  if (!is.null(names)){
    if (!is.character(names) & !all(sapply(names, is.character))){
      stop('Please input a character list for names!')
    }

    names <- unique(names)
    num_names <- length(names)
    indices <- generate_ranges(num_names)

    dt <- character(length = num_names)
    names(dt) <- names

    for (i in seq_along(indices)){
      if (verbose) {
        print(paste('The current index is i =', i, 'out of', length(indices)))
      }

      response <- httr::POST(url = paste0(Server, '/indigo/to-mol2000'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'text/plain',
                               'x-api-key' = API_key
                             )),
                             body = c(names[indices[[i]]]))

      if (response$status_code == 200){
        if (length(response$content) > 0){
          response_list <- list(httr::content(response, as = 'text', encoding = "UTF-8"))
        } else {
          response_list <- rep('', times = length(indices[[i]]))
        }
        dt[indices[[i]]] <- response_list
      }
      Sys.sleep(rate_limit)
    }

  }
  return(dt)

}

get_InChI_batch <- function(names = NULL,
                             API_key = NULL,
                             rate_limit = 0L,
                             Server = chemical_api_server,
                            verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }
  if (!is.null(names)){
    if (!is.character(names) & !all(sapply(names, is.character))){
      stop('Please input a character list for names!')
    }

    names <- unique(names)
    num_names <- length(names)
    indices <- generate_ranges(num_names)

    dt <- character(length = num_names)
    names(dt) <- names

    for (i in seq_along(indices)){
      if (verbose) {
        print(paste('The current index is i =', i, 'out of', length(indices)))
      }

      response <- httr::POST(url = paste0(Server, '/indigo/to-inchi'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'text/plain',
                               'x-api-key' = API_key
                             )),
                             body = c(names[indices[[i]]]))

      if (response$status_code == 200){
        if (length(response$content) > 0){
          response_list <- list(httr::content(response, as = 'text', encoding = "UTF-8"))
        } else {
          response_list <- rep('', times = length(indices[[i]]))
        }
        dt[indices[[i]]] <- response_list
      }
      Sys.sleep(rate_limit)
    }

  }
  return(dt)

}

get_canonical_smiles_batch <- function(names = NULL,
                                       API_key = NULL,
                                       rate_limit = 0L,
                                       Server = chemical_api_server,
                                       verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }
  if (!is.null(names)){
    if (!is.character(names) & !all(sapply(names, is.character))){
      stop('Please input a character list for names!')
    }

    names <- unique(names)
    num_names <- length(names)
    indices <- generate_ranges(num_names)

    dt <- character(length = num_names)
    names(dt) <- names

    for (i in seq_along(indices)){
      if (verbose) {
        print(paste('The current index is i =', i, 'out of', length(indices)))
      }

      response <- httr::POST(url = paste0(Server, '/indigo/to-canonicalsmiles'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'text/plain',
                               'x-api-key' = API_key
                             )),
                             body = c(names[indices[[i]]]))

      if (response$status_code == 200){
        if (length(response$content) > 0){
          response_list <- list(httr::content(response, as = 'text', encoding = "UTF-8"))
        } else {
          response_list <- rep('', times = length(indices[[i]]))
        }
        dt[indices[[i]]] <- response_list
      }
      Sys.sleep(rate_limit)
    }

  }
  return(dt)

}

#' Retrieve chemicals by property and value range in batch search
#'
#' @param start_list Numeric values, the beginning of the range
#' @param end_list Numeric values, the end of the range
#' @param property_list Strings, the properties being queried
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of data.frames containing chemical information for the
#'   chemicals matching the search criteria.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemicals by property ranges
#' prop_ranges <- get_chemical_by_property_range_batch(start_list = c(1.311,
#'                                                                    211.99),
#'                                                     end_list = c(1.313,
#'                                                                      212.01),
#'                                                     property_list = c('Density',
#'                                                                       'Boiling Point'))

get_chemical_by_property_range_batch <- function(start_list = NULL,
                                                 end_list = NULL,
                                                 property_list = NULL,
                                                 API_key = NULL,
                                                 rate_limit = 0L,
                                                 verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (is.null(start_list) || is.null(end_list)){
    stop('Please input a list for both `start_list` and `end_list`!')
  } else if ((length(start_list)) != (length(end_list))){
    stop('Mismatch in length of `start_list` and `end_list`!')
  } else if (!all(sapply(c(start_list, end_list), is.numeric))){
    stop('Only numeric values allowed in each list!')
  }

  if (is.null(property_list)){
    stop('Please input a list for `property_list`!')
  } else if ((length(property_list) != (length(start_list)))){
    if (length(property_list) == 1){
      if (verbose) {
        message('Setting `property_list` to repeat to match length of `start_list/end_list`!')
      }
      property_list_ <- rep(unlist(property_list), length(start_list))
    } else {
      stop('Mismatch in length of `property_list` and `start_list/end_list`!')
    }
  } else {
    property_list_ <- property_list
  }

  if (!is.character(property_list_) | !all(sapply(property_list_, is.character))){
    stop('Please input a character list for `property_list`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }

  start_list_ <- pmin(start_list, end_list, na.rm = TRUE)
  end_list_ <- pmax(start_list, end_list, na.rm = TRUE)

  results <- purrr::pmap(.l = list(start_list_, end_list_, property_list_), function(s, e, p){
    Sys.sleep(rate_limit)
    attempt <- tryCatch({
      get_chemical_by_property_range(start = s,
                                     end = e,
                                     property = p,
                                     API_key = API_key,
                                     verbose = verbose)
    },
    error = function(cond){
      if (verbose){
        message('There was an error!')
        message(paste('Start:', s))
        message(paste('End:', e))
        message(paste('Property:', p))
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

  names(results) <- paste0('(Start, End, Property) = (', start_list_, ', ', end_list_, ', ', property_list_, ')')
  return(results)
}


#' Retrieve chemical information in batch search
#'
#' @param DTXSID A vector of chemical identifier DTXSIDs
#' @param type A vector of type used in get_chem_info(). This specifies whether
#'   to only grab predicted or experimental results. If not specified, it will
#'   grab all details. The allowable input values are "", predicted", or
#'   "experimental".
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between each request
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.table containing chemical information for the chemicals with
#'   DTXSID matching the input parameter.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemical info for multiple chemicals
#' chem_info <- get_chem_info_batch(DTXSID = c('DTXSID7020182',
#'                                             'DTXSID2021315'))

get_chem_info_batch <- function(DTXSID = NULL,
                                type = '',
                                API_key = NULL,
                                rate_limit = 0L,
                                Server = chemical_api_server,
                                verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }
  if (!is.null(DTXSID)){
    if (!is.character(DTXSID) & !all(sapply(DTXSID, is.character))){
      stop('Please input a character list for DTXSID!')
    }
    DTXSID <- unique(DTXSID)

    num_dtxsid <- length(DTXSID)
    indices <- generate_ranges(num_dtxsid)

    dt <- data.table::data.table(name = character(),
                                 value = numeric(),
                                 id = integer(),
                                 source = character(),
                                 description = character(),
                                 propType = character(),
                                 unit = character(),
                                 propertyId = character(),
                                 dtxsid = character(),
                                 dtxcid = character())

    for (i in seq_along(indices)){

      # print(paste('The current index is i =', i, 'out of', length(indices)))

      response <- httr::POST(url = paste0(Server, '/property/search/by-dtxsid/'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'application/json',
                               'x-api-key' = API_key
                             )),
                             body = jsonlite::toJSON(DTXSID[indices[[i]]], auto_unbox = ifelse(length(DTXSID[indices[[i]]]) > 1, 'T', 'F')))

      # print(paste('The response code is', response$status_code, 'for index i =', i))


      if (response$status_code == 401){
        stop(httr::content(response)$detail)
      }

      if (response$status_code == 200){
        dt <- suppressWarnings(data.table::rbindlist(list(dt,
                                                          data.table::data.table(jsonlite::fromJSON(httr::content(response,
                                                                                                                  as = 'text',
                                                                                                                  encoding = "UTF-8")))),
                                                     fill = TRUE))
      }
      Sys.sleep(rate_limit)
    }


  if (!is.character(type)){
    return(dt)
  } else if (length(type) != 1){
    warning("Setting type to ''!")
    return(dt)
  } else {
    type_index <- c('predicted', 'experimental')[which(c('predicted', 'experimental') %in% tolower(type))]
    if (length(type_index) == 0){
      warning("Setting type to ''!")
      return(dt)
    }
    index_subset <- which(dt$propType %in% type_index)
    return(dt[index_subset, ])
  }

  # return(dt)
  } else {
    stop('Please input a character list for DTXSID!')
  }
}



#' Retrieve chemical fate data in batch search
#'
#' @param DTXSID A vector of chemicals identifier DTXSIDs
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.table containing chemical fate information for the chemicals
#'   with DTXSID matching the input parameter.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemical fate by dtxsids
#' chemical_fates <- get_fate_by_dtxsid_batch(DTXSID = c('DTXSID7020182',
#'                                                       'DTXSID2021315'))

get_fate_by_dtxsid_batch <- function(DTXSID = NULL,
                                     API_key = NULL,
                                     rate_limit = 0L,
                                     Server = chemical_api_server,
                                     verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }
  if (!is.null(DTXSID)){
    if (!is.character(DTXSID) & !all(sapply(DTXSID, is.character))){
      stop('Please input a character list for DTXSID!')
    }
    DTXSID <- unique(DTXSID)
    num_dtxsid <- length(DTXSID)
    indices <- generate_ranges(num_dtxsid)

    dt <- data.table::data.table(id = integer(),
                                 description = character(),
                                 minValue = numeric(),
                                 maxValue = numeric(),
                                 valueType = character(),
                                 unit = character(),
                                 dtxsid = character(),
                                 dtxcid = character(),
                                 endpointName = character(),
                                 resultValue = numeric(),
                                 modelSource = character())

    for (i in seq_along(indices)){

      # print(paste('The current index is i =', i, 'out of', length(indices)))

      response <- httr::POST(url = paste0(Server, '/fate/search/by-dtxsid/'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'application/json',
                               'x-api-key' = API_key
                             )),
                             body = jsonlite::toJSON(DTXSID[indices[[i]]], auto_unbox = ifelse(length(DTXSID[indices[[i]]]) > 1, 'T', 'F')))

      # print(paste('The response code is', response$status_code, 'for index i =', i))

      if (response$status_code == 401){
        stop(httr::content(response)$detail)
      }


      if (response$status_code == 200){
        dt <- suppressWarnings(data.table::rbindlist(list(dt,
                                                          data.table::data.table(jsonlite::fromJSON(httr::content(response,
                                                                                                                  as = 'text',
                                                                                                                  encoding = "UTF-8")))),
                                                     fill = TRUE))
      }
      Sys.sleep(rate_limit)
    }
    return(dt)
  } else {
    stop('Please input a list of DTXSIDs!')
  }
}



#' Chemical starts with batch search
#'
#' @param word_list A list of character strings of chemical names or portion of
#'   chemical names
#' @param API_key User-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#' @param top The number of results to return if there are multiple results
#'   available
#'
#' @return A named list of data.frames of chemicals and related values matching
#'   the query parameters. The data.frames under the 'valid' entry contain
#'   chemical information for successful requests while the data.frames under
#'   the 'invalid' entry contain data.frames with chemical name suggestions
#'   based on the input search values.
#' @author Paul Kruse, Kristin Issacs
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemicals that start with given substrings
#' bpa_substrings <- chemical_starts_with_batch(word_list = c('DTXSID702018',
#'                                                            'DTXCID3018'))

chemical_starts_with_batch <- function(word_list = NULL,
                                       API_key = NULL,
                                       rate_limit = 0L,
                                       verbose = FALSE,
                                       top = NULL){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }

  if (!is.null(top)){
    if (!is.numeric(top)) {
      warning("Setting 'top' to NULL")
      top <- NULL
    } else {
      top <- max(-1, as.integer(top))
      if (top < 1){
        warning("Setting 'top' to NULL")
        top <- NULL
      }
    }
  }

  return_list <- list()

  if (!is.null(word_list)){
    if (!is.character(word_list) & !all(sapply(word_list, is.character))){
      stop('Please input a character list for word_list!')
    }
    word_list <- unique(word_list)
    results <- lapply(word_list, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          chemical_starts_with(word = t, API_key = API_key, verbose = verbose,
                               top = top)
        },
        error = function(cond){
          if (verbose){
            message(t)
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

    index_200 <- which(unlist(lapply(results, check_search_dtxsid)))

    if (length(index_200) < length(word_list)){
      return_list$invalid <- results[-index_200]
      if (length(index_200) > 0){
        return_list$valid <- results[index_200]
      }
    } else {
      return_list$valid <- results
    }
    return(return_list)

  } else {
    stop('Please input a list of chemical names!')
  }
}

#' Chemical equal batch search
#'
#' @param word_list A list of character strings of chemical names or portion of
#'   chemical names, DTXSIDs, CASRNs, InChIKeys.
#' @param API_key User-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of data.tables of chemicals and related values matching
#' the query parameters. The list contains two entries, 'valid' and 'invalid';
#' 'valid', contains a data.table of the results of the the searched chemical
#' that were found in the databases; 'invalid' contains a data.table with
#' 'suggestions' for each searched valued that did not return a chemical.
#' @author Paul Kruse, Kristin Issacs
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemicals that match input strings
#' bpa <- chemical_equal_batch(word_list = c('DTXSID7020182', 'DTXCID30182'))

chemical_equal_batch <- function(word_list = NULL,
                                 API_key = NULL,
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

  return_list <- list()

  if (!is.null(word_list)){
    if (!is.character(word_list) & !all(sapply(word_list, is.character))){
      stop('Please input a character list for word_list!')
    }


    word_list <- unique(word_list)
    num_words <- length(word_list)
    indices <- generate_ranges(num_words, limit = 100)
    if (verbose) {
      print(indices)
    }

    dt <- data.table::data.table(casrn = character(),
                                 dtxsid = character(),
                                 dtxcid = character(),
                                 preferredName = character(),
                                 hasStructureImage = integer(),
                                 smiles = character(),
                                 isMarkush = logical(),
                                 searchName = character(),
                                 searchValue = character(),
                                 rank = integer(),
                                 searchMsgs = character(),
                                 suggestions = character(),
                                 isDuplicate = logical())

    for (i in seq_along(indices)){
      response <- httr::POST(url = paste0(chemical_api_server, '/search/equal/'),
                             httr::add_headers(.headers = c(
                               'accept' = 'application/json',
                               'content-type' = 'application/json',
                               'x-api-key' = API_key
                             )),
                             body = c(word_list[indices[[i]]])#c(word_list[indices[i]]
      )

      if (response$status_code == 401){
        stop(httr::content(response)$detail)
      }

      if (response$status_code == 200){
        dt <- suppressWarnings(data.table::rbindlist(list(dt,
                                                          data.table::data.table(jsonlite::fromJSON(httr::content(response,
                                                                                                                  as = 'text',
                                                                                                                  encoding = "UTF-8")))),
                                                     fill = TRUE))
      }

      Sys.sleep(rate_limit)
    }

    if (dim(dt)[[1]] > 0){


      search_index <- which(unlist(lapply(dt$searchValue, function(t) {!is.null(t)})))
      return_list <- data.table::copy(dt)[search_index, -c(11:12)]

      return(return_list)
    }

    return(return_list)

  } else {
    stop('Please input a list of chemical names!')
  }
}

#' Chemical contains batch search
#'
#' @param word_list A list of character strings of chemical names or portion of
#'   chemical names
#' @param API_key User-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#' @param top The number of results to return if there are multiple results
#'   available
#'
#' @return A named list of data.frames of chemicals and related values matching
#'   the query parameters. The data.frames under the 'valid' entry contain
#'   chemical information for successful requests while the data.frames under
#'   the 'invalid' entry contain data.frames with chemical name suggestions
#'   based on the input search values.
#' @author Paul Kruse, Kristin Issacs
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemicals that contain substrings
#' substring_chemicals <- chemical_contains_batch(word_list = c('TXDIS702018',
#'                                                              'DTXSID70201'))

chemical_contains_batch <- function(word_list = NULL,
                                    API_key = NULL,
                                    rate_limit = 0L,
                                    verbose = FALSE,
                                    top = NULL){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }

  if (!is.null(top)){
    if (!is.numeric(top)) {
      warning("Setting 'top' to NULL")
      top <- NULL
    } else {
      top <- max(-1, as.integer(top))
      if (top < 1){
        warning("Setting 'top' to NULL")
        top <- NULL
      }
    }
  }

  return_list <- list()

  if (!is.null(word_list)){
    if (!is.character(word_list) & !all(sapply(word_list, is.character))){
      stop('Please input a character list for word_list!')
    }
    word_list <- unique(word_list)
    results <- lapply(word_list, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          chemical_contains(word = t, API_key = API_key, verbose = verbose,
                            top = top)
        },
        error = function(cond){
          if (verbose){
            message(t)
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
    index_200 <- which(unlist(lapply(results, check_search_dtxsid)))

    if (length(index_200) < length(word_list)){
      return_list$invalid <- results[-index_200]
      if (length(index_200) > 0){
        return_list$valid <- results[index_200]
      }
    } else {
      return_list$valid <- results
    }
    return(return_list)
  }

  stop('Please input a list of chemical names!')

}

check_search_dtxsid <- function(list){
  # Check if chemical search returned chemical information or suggestions
  return('dtxsid' %in% names(list))
}


#' Get msready by mass and error offset
#'
#' @param masses A numeric list of masses.
#' @param error The mass offset value.
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some "progress report" should be given.
#'
#' @return A list (of lists) of DTXSIDs, with a list returned for each input
#' mass value.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' #Pull chemicals by msready mass and error offset
#' msready_data <- get_msready_by_mass_with_error_batch(masses = c(226, 228),
#'                                                      error = 4)

get_msready_by_mass_with_error_batch <- function(masses = NULL,
                                                 error = NULL,
                                                 API_key = NULL,
                                                 rate_limit = 0,
                                                 verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (is.null(masses) || is.null(error)){
    stop('Please input a list of masses and an error value!')
  } else if (!all(sapply(c(masses, error), is.numeric))) {
    stop('Please input only numeric values for masses and error!')
  }

  masses <- unique(masses)
  masses <- masses[masses > 0]

  if (length(masses) == 0) {
    stop('Please input only positive values for masses!')
  }

  error <- unique(error)
  error <- error[error > 0]

  if (length(error) == 0){
    stop('Value for error must be positive!')
  } else if (length(error) > 1){
    warning('Using the first positive value contained in error!')
    error <- error[[1]]
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }

  json_body <- jsonlite::toJSON(x = list(masses = masses,
                                error = error),
                                auto_unbox = TRUE)

  response <- httr::POST(url = paste0(chemical_api_server, '/msready/search/by-mass/'),
                         httr::add_headers(.headers = c(
                           'Accept' = 'application/json',
                           'Content-Type' = 'application/json',
                           'x-api-key' = API_key)),
                         body = json_body)

  if (response$status_code == 401){
    stop(httr::content(response)$detail)
  }

  if (response$status_code == 200){
    return(httr::content(response))
  }

  return(list())

}

#' Get ms ready by mass batch search
#'
#' @param start_list A numeric list of starting values for mass range
#' @param end_list A numeric list of ending values for mass range
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of character lists with DTXSIDs with msready masses
#'   falling within the given ranges.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull msready chemicals by mass ranges
#' msready_data <- get_msready_by_mass_batch(start_list = c(200.9, 200.95),
#'                                           end_list = c(200.95, 201.00))

get_msready_by_mass_batch <- function(start_list = NULL,
                                      end_list = NULL,
                                      API_key = NULL,
                                      rate_limit = 0L,
                                      verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if(is.null(start_list) || is.null(end_list)){
    stop('Please input a list for both `start_list` and `end_list`!')
  } else if (length(start_list) != length(end_list)) {
    stop('Mismatch in length of `start_list` and `end_list`!')
  } else if (!all(sapply(c(start_list, end_list), is.numeric))) {
    stop('Only numeric values allowed in `start_list` and `end_list`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }

  # Sort min/max from lists to avoid errors
  start_list_ <- pmin(start_list, end_list, na.rm = TRUE)
  end_list_ <- pmax(start_list, end_list, na.rm = TRUE)

  results <- purrr::map2(.x = start_list_, .y = end_list_, function(d, t){
    Sys.sleep(rate_limit)
    attempt <- tryCatch(
      {
        get_msready_by_mass(start = d,
                            end = t,
                            API_key = API_key,
                            verbose = verbose)
      },
      error = function(cond){
        if (verbose){
          message('There was an error!')
          message(paste('Start:', d))
          message(paste('End:', t))
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

  names(results) <- paste0('(Start, End) = (', start_list_, ', ', end_list_, ')')
  return(results)
}

#' Get msready by formula batch search
#'
#' @param formula_list A list of strings denoting the input chemicals formulas
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of character lists of DTXSIDs with chemical formulas
#'   matching the search criteria
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull msready data for several chemical formulas
#' msready_data <- get_msready_by_formula_batch(formula_list = c('C16H24N2O5S',
#'                                                               'C15H16O2'))

get_msready_by_formula_batch <- function(formula_list = NULL,
                                         API_key = NULL,
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
  if (!is.null(formula_list)){
    if (!is.character(formula_list) & !all(sapply(formula_list, is.character))){
      stop('Please input a character list for formula_list!')
    }
    formula_list <- unique(formula_list)
    results <- lapply(formula_list, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_msready_by_formula(formula = t, API_key = API_key, verbose = verbose)
        },
        error = function(cond){
          if (verbose){
            message(t)
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

    names(results) <- formula_list
    return(results)
  } else {
    stop('Please input a list of chemical formulas!')
  }
}

#' Get msready by DTXCID batch search
#'
#' @param DTXCID A list of chemical identifier DTXCIDs
#' @param API_key A user-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of character lists of DTXSIDs with DTXCIDs matching the
#'   search criteria
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull msready chemicals matching specific DTXCID
#' dtxcid_msready <- get_msready_by_dtxcid_batch(DTXCID = c('DTXCID30182',
#'                                                          'DTXCID001315'))

get_msready_by_dtxcid_batch <- function(DTXCID = NULL,
                                        API_key = NULL,
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
  if(!is.null(DTXCID)){
    if (!is.character(DTXCID) & !all(sapply(DTXCID, is.character))){
      stop('Please input a character list for DTXCID!')
    }
    DTXCID <- unique(DTXCID)
    results <- lapply(DTXCID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_msready_by_dtxcid(DTXCID = t, API_key = API_key, verbose = verbose)
        },
        error = function(cond){
          if(verbose){
            message(t)
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

    names(results) <- DTXCID
    return(results)
  } else {
    stop('Please input a list of DTXCIDs!')
  }
}

#' Get chemical lists by type batch search
#'
#' @param type_list A list of list types. This is a case sensitive parameter and
#'   returns lists only for values of "federal", "international", "state", and
#'   "other".
#' @param Projection Optional parameter controlling return type. It takes values
#'   'chemicallistall' and 'chemicallistname' with the former as the default
#'   value.
#' @param API_key The user-specified API key.
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of data.frames containing information about lists that
#'   meet the search criteria.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemical lists by type
#' federal_state <- get_chemical_lists_by_type_batch(type_list = c('federal',
#'                                                                 'state'))

get_chemical_lists_by_type_batch <- function(type_list = NULL,
                                             Projection = '',
                                             API_key = NULL,
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
  if (!is.null(type_list)){
    if (!is.character(type_list) & !all(sapply(type_list, is.character))){
      stop('Please input a character list for type_list!')
    }
    type_list <- unique(type_list)
    results <- lapply(type_list, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_chemical_lists_by_type(type = t,
                                     Projection = Projection,
                                     API_key = API_key,
                                     verbose = verbose)
        },
        error = function(cond){
          if (verbose){
            message(t)
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

    names(results) <- type_list
    return(results)
  } else {
    stop('Please input a list of list types!')
  }
}

#' Get chemical list by name batch
#'
#' @param name_list A list of chemical list names.
#' @param Projection Optional parameter controlling return type. It takes values
#'   'chemicallistall' and 'chemicallistname' with the former as the default
#'   value.
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of data.frames containing information about the chemical
#'   lists. Note, these are not the chemical lists themselves. To access the
#'   chemicals in a given list, use \code{\link{get_chemicals_in_list}}.
#' @seealso \code{\link{get_chemicals_in_list}}
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemical list information by list names
#' list_info <- get_public_chemical_list_by_name_batch(name_list = c('CCL4',
#'                                                                   'NATADB'))

get_public_chemical_list_by_name_batch <- function(name_list = NULL,
                                                   Projection = '',
                                                   API_key = NULL,
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
  if (!is.null(name_list)){
    if (!is.character(name_list) & !all(sapply(name_list, is.character))){
      stop('Please input a character list for name_list!')
    }
    name_list <- unique(name_list)
    results <- lapply(name_list, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_public_chemical_list_by_name(list_name = t,
                                           Projection = Projection,
                                           API_key = API_key,
                                           verbose = verbose)
        },
        error = function(cond){
          if (verbose){
            message(t)
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

    names(results) <- name_list
    return(results)
  } else {
    stop('Please input a list of list names!')
  }
}

#' Get chemical lists containing given chemical batch
#'
#' @param chemical_list A list of the chemical identifier DTXSIDs.
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of chemical lists that contain the given chemicals.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull lists containing chemicals for multiple chemicals
#' lists <- get_lists_containing_chemical_batch(chemical_list = c('DTXSID7020182',
#'                                                                'DTXSID2021315'))

get_lists_containing_chemical_batch <- function(chemical_list = NULL,
                                                API_key = NULL,
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
  if (!is.null(chemical_list)){
    if (!is.character(chemical_list) & !all(sapply(chemical_list, is.character))){
      stop('Please input a character list for chemical_list!')
    }
    chemical_list <- unique(chemical_list)
    results <- lapply(chemical_list, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_lists_containing_chemical(DTXSID = t,
                                        API_key = API_key,
                                        verbose = verbose)
        },
        error = function(cond){
          if (verbose){
            message(t)
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

    names(results) <- chemical_list
    return(results)
  } else {
    stop('Please input a list of DTXSIDs!')
  }
}


#' Get chemicals in a list specified by starting characters batch search
#'
#' @param list_names The names of the lists to search.
#' @param words The search words, one for each list.
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between each request.
#' @param verbose A logical indicating if some "progress report" should be given.
#'
#' @return A named list of lists, with names corresponding to search terms and
#' lists corresponding to DTXSIDs associated to the search terms
#' @export
#'
#' @examplesIf FALSE
#' # Search `CCL4` for chemicals starting with 'Bis' and `BIOSOLIDS2021` for
#' # chemicals starting with 'Tri'.
#' bis_and_tri <- get_chemicals_in_list_start_batch(list_names = c('CCL4',
#'                                                               'BIOSOLIDS2021'),
#'                                                  words = c('Bis', 'Tri'))

get_chemicals_in_list_start_batch <- function(list_names = NULL,
                                              words = NULL,
                                              API_key = NULL,
                                              rate_limit = 0L,
                                              verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if(is.null(list_names) || is.null(words)){
    stop('Please input a list for both `list_names` and `words`!')
  } else if (length(list_names) != length(words)) {
    stop('Mismatch in length of `list_names` and `words`!')
  } else if (!all(sapply(c(list_names, words), is.character))) {
    stop('Only character values allowed in `list_names` and `words`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }



  results <- purrr::map2(.x = list_names, .y = words, function(d, t){
    Sys.sleep(rate_limit)
    attempt <- tryCatch(
      {
        get_chemicals_in_list_start(list_name = d,
                                    word = t,
                                    API_key = API_key,
                                    verbose = verbose)
      },
      error = function(cond){
        if (verbose) {
          message('There was an error!')
          message(paste('List name:', d))
          message(paste('Word:', t))
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

  names(results) <- paste0('(List name, Word) = (', list_names, ', ', words, ')')
  return(results)
}

#' Get chemicals in a list specified by exact characters batch search
#'
#' @param list_names The names of the lists to search.
#' @param words The search words, one for each list.
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between each request.
#' @param verbose A logical indicating if some "progress report" should be given.
#'
#' @return A named list of lists, with names corresponding to search terms and
#' lists corresponding to DTXSIDs associated to the search terms
#' @export
#'
#' @examplesIf FALSE
#' # Search `CCL4` for chemicals exactly matching with 'Bisphenol A' and
#' # `BIOSOLIDS2021` for chemicals exactly matching with 'Bisphenol A'.
#' bisphenol_a <- get_chemicals_in_list_exact_batch(list_names = c('CCL4',
#'                                                               'BIOSOLIDS2021'),
#'                                                  words = rep('Bisphenol A', 2))

get_chemicals_in_list_exact_batch <- function(list_names = NULL,
                                              words = NULL,
                                              API_key = NULL,
                                              rate_limit = 0L,
                                              verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if(is.null(list_names) || is.null(words)){
    stop('Please input a list for both `list_names` and `words`!')
  } else if (length(list_names) != length(words)) {
    stop('Mismatch in length of `list_names` and `words`!')
  } else if (!all(sapply(c(list_names, words), is.character))) {
    stop('Only character values allowed in `list_names` and `words`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }



  results <- purrr::map2(.x = list_names, .y = words, function(d, t){
    Sys.sleep(rate_limit)
    attempt <- tryCatch(
      {
        get_chemicals_in_list_exact(list_name = d,
                                    word = t,
                                    API_key = API_key,
                                    verbose = verbose)
      },
      error = function(cond){
        if (verbose) {
          message('There was an error!')
          message(paste('List name:', d))
          message(paste('Word:', t))
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

  names(results) <- paste0('(List name, Word) = (', list_names, ', ', words, ')')
  return(results)
}

#' Get chemicals in a list specified by characters contained batch search
#'
#' @param list_names The names of the lists to search.
#' @param words The search words, one for each list.
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between each request.
#' @param verbose A logical indicating if some "progress report" should be given.
#'
#' @return A named list of lists, with names corresponding to search terms and
#' lists corresponding to DTXSIDs associated to the search terms
#' @export
#'
#' @examplesIf FALSE
#' # Search `CCL4` for chemicals containing with 'Bis' and `BIOSOLIDS2021` for
#' # chemicals containing with 'Zyle'.
#' bis_and_zyle <- get_chemicals_in_list_contain_batch(list_names = c('CCL4',
#'                                                               'BIOSOLIDS2021'),
#'                                                  words = c('Bis', 'Zyle'))

get_chemicals_in_list_contain_batch <- function(list_names = NULL,
                                                words = NULL,
                                                API_key = NULL,
                                                rate_limit = 0L,
                                                verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if(is.null(list_names) || is.null(words)){
    stop('Please input a list for both `list_names` and `words`!')
  } else if (length(list_names) != length(words)) {
    stop('Mismatch in length of `list_names` and `words`!')
  } else if (!all(sapply(c(list_names, words), is.character))) {
    stop('Only character values allowed in `list_names` and `words`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }



  results <- purrr::map2(.x = list_names, .y = words, function(d, t){
    Sys.sleep(rate_limit)
    attempt <- tryCatch(
      {
        get_chemicals_in_list_contain(list_name = d,
                                      word = t,
                                      API_key = API_key,
                                      verbose = verbose)
      },
      error = function(cond){
        if (verbose) {
          message('There was an error!')
          message(paste('List name:', d))
          message(paste('Word:', t))
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

  names(results) <- paste0('(List name, Word) = (', list_names, ', ', words, ')')
  return(results)
}



#' Get chemicals in a given chemical list batch
#'
#' @param list_names A list of names of chemical lists.
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of data.frames each containing chemicals in the
#'   corresponding chemical lists.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemicals in lists for multiple lists
#' chemicals_in_lists <- get_chemicals_in_list_batch(list_names = c('CCL4', 'NATADB'))

get_chemicals_in_list_batch <- function(list_names = NULL,
                                        API_key = NULL,
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
  if (!is.null(list_names)){
    if (!is.character(list_names) & !all(sapply(list_names, is.character))){
      stop('Please input a character list for list_names!')
    }
    list_names <- unique(list_names)
    results <- lapply(list_names, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_chemicals_in_list(list_name = t,
                                API_key = API_key,
                                verbose = verbose)
        },
        error = function(cond){
          if (verbose){
            message(t)
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

    names(results) <- list_names
    return(results)
  } else {
    stop('Please input a list of names of chemical lists!')
  }
}

#' Ger mrv file by DTXSID or DTXCID batch
#'
#' @param DTXSID A list of the chemical identifier DTXSIDs.
#' @param DTXCID A list of the chemical identifier DTXCIDs.
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of XML file format for representing a mrv file for each
#'   chemicals.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull mrv files for multiple chemicals by DTXSID
#' dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
#' mrv_files <- get_chemical_mrv_batch(DTXSID = dtxsid)
#' # Pull mrv files for multiple chemicals by DTXCID
#' dtxcid <- c('DTXCID30182', 'DTXCID001315')
#' mrv_files <- get_chemical_mrv_batch(DTXCID = dtxcid)

get_chemical_mrv_batch <- function(DTXSID = NULL,
                                   DTXCID = NULL,
                                   API_key = NULL,
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
  if (!is.null(DTXSID)){
    if (!is.character(DTXSID) & !all(sapply(DTXSID, is.character))){
      stop('Please input a character list for DTXSID!')
    }
    DTXSID <- unique(DTXSID)
    if (verbose) {
      print('Using DTXSID!')
    }
    results <- lapply(DTXSID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_chemical_mrv(DTXSID = t, API_key = API_key, verbose = verbose)
        },
        error = function(cond){
          if (verbose){
            message(t)
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
  } else if (!is.null(DTXCID)){
    if (!is.character(DTXCID) & !all(sapply(DTXCID, is.character))){
      stop('Please input a character list for DTXCID!')
    }
    DTXCID <- unique(DTXCID)
    if (verbose) {
      print('Using DTXCID!')
    }
    results <- lapply(DTXCID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_chemical_mrv(DTXCID = t, API_key = API_key)
        },
        error = function(cond){
          if (verbose){
            message(t)
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

    names(results) <- DTXCID
    return(results)
  } else {
    stop('Please input a list of DTXSIDs or DTXCIDs!')
  }
}

#' Get mol file by DTXSID or DTXCID batch
#'
#' @param DTXSID A list of the chemical identifier DTXSIDs.
#' @param DTXCID A list of the chemical identifier DTXCIDs.
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of character strings giving a mol file representations
#'   of the given input chemicals.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull mol files for multiple chemicals by DTXSID
#' dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
#' mol_files <- get_chemical_mol_batch(DTXSID = dtxsid)
#' # Pull mol files for multiple chemicals by DTXCID
#' dtxcid <- c('DTXCID30182', 'DTXCID001315')
#' mol_files <- get_chemical_mol_batch(DTXCID = dtxcid)

get_chemical_mol_batch <- function(DTXSID = NULL,
                                   DTXCID = NULL,
                                   API_key = NULL,
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
  if (!is.null(DTXSID)){
    if (!is.character(DTXSID)& !all(sapply(DTXSID, is.character))){
      stop('Please input a character list for DTXSID!')
    }
    DTXSID <- unique(DTXSID)
    if (verbose) {
      print('Using DTXSID!')
    }
    results <- lapply(DTXSID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_chemical_mol(DTXSID = t, API_key = API_key, verbose = verbose)
        },
        error = function(cond){
          if (verbose){
            message(t)
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
  } else if (!is.null(DTXCID)){
    if (!is.character(DTXCID) & !all(sapply(DTXCID, is.character))){
      stop('Please input a character list for DTXCID!')
    }
    DTXCID <- unique(DTXCID)
    if (verbose) {
      print('Using DTXCID!')
    }
    results <- lapply(DTXCID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_chemical_mol(DTXCID = t, API_key = API_key, verbose = verbose)
        },
        error = function(cond){
          if (verbose){
            message(t)
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

    names(results) <- DTXCID
    return(results)
  } else {
    stop('Please input a list of DTXSIDs or DTXCIDs!')
  }
}

#' Get image file by DTXSID or DTXCID batch
#'
#' @param DTXSID A list of chemical identifier DTXSIDs.
#' @param DTXCID A list of chemical identifier DTXCIDs.
#' @param SMILES A list of chemical identifier SMILES.
#' @param format The image type, either "png" or "svg". If left blank, will
#'   default to "png".
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of Large arrays of three dimensions representing an image. For
#'   displaying an image, one may use \code{png::writePNG()} or
#'   \code{countcolors::plotArrayAsImage()} among many such functions.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull images for multiple chemicals
#' dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
#' images <- get_chemical_image_batch(DTXSID = dtxsid)
#' if (requireNamespace("countcolors", quietly = TRUE)){
#'   countcolors::plotArrayAsImage(images[[1]])
#'   countcolors::plotArrayAsImage(images[[2]])
#' }

get_chemical_image_batch <- function(DTXSID = NULL,
                                     DTXCID = NULL,
                                     SMILES = NULL,
                                     format = "",
                                     API_key = NULL,
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
  if (!is.null(DTXSID)){
    if (!is.character(DTXSID) & !all(sapply(DTXSID, is.character))){
      stop('Please input a character list for DTXSID!')
    }
    DTXSID <- unique(DTXSID)
    if (verbose) {
      print('Using DTXSID!')
    }
    results <- purrr::map2(.x = DTXSID, .y = format, function(d, f){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_chemical_image(DTXSID = d,
                             format = f,
                             API_key = API_key,
                             verbose = verbose)
        },
        error = function(cond){
          if (verbose){
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
  } else if (!is.null(DTXCID)){
    if (!is.character(DTXCID) & !all(sapply(DTXCID, is.character))){
      stop('Please input a character list for DTXCID!')
    }
    DTXCID <- unique(DTXCID)
    if (verbose) {
      print('Using DTXCID!')
    }
    results <- purrr::map2(.x = DTXCID, .y = format, function(d, f){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_chemical_image(DTXCID = d,
                             format = f,
                             API_key = API_key,
                             verbose = verbose)
        },
        error = function(cond){
          if (verbose){
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

    names(results) <- DTXCID
    return(results)
  } else if (!is.null(SMILES)) {
    if (!is.character(SMILES) & !all(sapply(SMILES, is.character))){
      stop('Please input a character list for SMILES!')
    }
    SMILES <- unique(SMILES)
    if (verbose) {
      print('Using SMILES!')
    }
    results <- purrr::map2(.x = SMILES, .y = format, function(d, f){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_chemical_image(SMILES = d,
                             format = f,
                             API_key = API_key,
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

    names(results) <- SMILES
    return(results)
  } else {
    stop('Please input a list of DTXSIDs, DTXCIDs, or SMILEs!')
  }
}


#' Get chemical synonym batch
#'
#' @param DTXSID A list of chemical identifier DTXSIDs
#' @param API_key The user-specific API key.
#' @param rate_limit The number of seconds to wait between requests.
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of lists containing synonym information for each input
#'   DTXSID.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull synonyms for multiple chemicals
#' dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
#' batch_synonyms <- get_chemical_synonym_batch(DTXSID = dtxsid)

get_chemical_synonym_batch <- function(DTXSID = NULL,
                                       API_key = NULL,
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
  if (!is.null(DTXSID)){
    if (!is.character(DTXSID) & !all(sapply(DTXSID, is.character))){
      stop('Please input a character list for DTXSID!')
    }
    DTXSID <- unique(DTXSID)
    num_dtxsid <- length(DTXSID)
    indices <- generate_ranges(num_dtxsid)
    if (verbose) {
      print(indices)
    }

    dt <- data.table::data.table(dtxsid = character(),
                                 pcCode = character(),
                                 valid = character(),
                                 beilstein = character(),
                                 alternateCasrn = character(),
                                 good = character(),
                                 other = character(),
                                 deletedCasrn = character())

    for (i in seq_along(indices)){
      response <- httr::POST(url = paste0(chemical_api_server, '/synonym/search/by-dtxsid/'),
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
        dt <- suppressWarnings(data.table::rbindlist(list(dt,
                                                          data.table::data.table(jsonlite::fromJSON(httr::content(response,
                                                                                                                  as = 'text',
                                                                                                                  encoding = "UTF-8")))),
                                                     fill = TRUE))

      }
      Sys.sleep(rate_limit)
    }

    return(dt)



    results <- lapply(DTXSID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_chemical_synonym(DTXSID = t, API_key = API_key, verbose = verbose)
        },
        error = function(cond){
          message(t)
          message(cond$message)
          return(NA)
        }
      )
      return(attempt)
    }
    )
    names(results) <- DTXSID
    return(results)
  } else {
    stop('Please input a list of DTXSIDs!')
  }
}
