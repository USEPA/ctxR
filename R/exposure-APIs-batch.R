#' Get Product Use categories via batch
#'
#' @param DTXSID Chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between requests
#' @param Server The root address for the API endpoint.
#' @param verbose A logical indicating if some "progress report" should be
#'   given.
#'
#' @returns A list of data.frames of product use categories data corresponding
#' to the input DTXSIDs.
#' @export
#'
#' @examplesIf FALSE
#' # Retrieve product use categories for BPA and Caffeine
#' get_product_use_categories_batch(DTXSID = c('DTXSID7020182',
#'                                             'DTXSID0020232'))
get_product_use_categories_batch <- function(DTXSID = NULL,
                                             API_key = NULL,
                                             rate_limit = 0L,
                                             Server = 'https://comptox.epa.gov/ctx-api/exposure',
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
    results <- lapply(DTXSID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_product_use_category(DTXSID = t,
                                   API_key = API_key,
                                   verbose = verbose,
                                   Server = Server)
        },
        error = function(cond){
          if (verbose) {
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
  } else {
    stop('Please input a list of DTXSIDs!')
  }
}

#' Get Production Volume data via batch
#'
#' @param DTXSID Chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between requests
#' @param Server The root address for the API endpoint.
#' @param verbose A logical indicating if some "progress report" should be
#'   given.
#'
#' @returns A list of data.frames of production volume data corresponding to the
#'   input DTXSIDs.
#' @export
#'
#' @examplesIf FALSE
#' # Retrieve production volume data for BPA and Caffeine
#' get_production_volume_batch(DTXSID = c('DTXSID7020182', 'DTXSID0020232'))
#'
get_production_volume_batch <- function(DTXSID = NULL,
                                        API_key = NULL,
                                        rate_limit = 0L,
                                        Server = 'https://comptox.epa.gov/ctx-api/exposure',
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
    results <- lapply(DTXSID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_production_volume(DTXSID = t,
                                API_key = API_key,
                                verbose = verbose,
                                Server = Server)
        },
        error = function(cond){
          if (verbose) {
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
  } else {
    stop('Please input a list of DTXSIDs!')
  }
}

#' Get biomonitoring data via batch
#'
#' @param DTXSID Chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Projection Optional parameter controlling return type.
#' @param rate_limit Number of seconds to wait between requests
#' @param Server The root address for the API endpoint.
#' @param verbose A logical indicating if some "progress report" should be
#'   given.
#'
#' @returns A list of data.frames of biomonitoring data corresponding to the
#'   input DTXSIDs.
#' @export
#'
#' @examplesIf FALSE
#' # Retrieve biomonitoring data for BPA and Caffeine
#' get_biomonitoring_data_batch(DTXSID = c('DTXSID7020182', 'DTXSID0020232'))
#'
get_biomonitoring_data_batch <- function(DTXSID = NULL,
                                         API_key = NULL,
                                         Projection = '',
                                         rate_limit = 0L,
                                         Server = 'https://comptox.epa.gov/ctx-api/exposure',
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
    results <- lapply(DTXSID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_biomonitoring_data(DTXSID = t,
                                API_key = API_key,
                                Projection = Projection,
                                verbose = verbose,
                                Server = Server)
        },
        error = function(cond){
          if (verbose) {
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
  } else {
    stop('Please input a list of DTXSIDs!')
  }
}

#' Get general use keywords via batch
#'
#' @param DTXSID Chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between requests
#' @param Server The root address for the API endpoint.
#' @param verbose A logical indicating if some "progress report" should be
#'   given.
#'
#' @returns A list of data.frames of general use keywords corresponding to the
#'   input DTXSIDs.
#' @export
#'
#' @examplesIf FALSE
#' # Retrieve general use keywords for BPA and Caffeine
#' get_general_use_keywords_batch(DTXSID = c('DTXSID7020182', 'DTXSID0020232'))
#'
get_general_use_keywords_batch <- function(DTXSID = NULL,
                                           API_key = NULL,
                                           rate_limit = 0L,
                                           Server = 'https://comptox.epa.gov/ctx-api/exposure',
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
    results <- lapply(DTXSID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_general_use_keywords(DTXSID = t,
                                   API_key = API_key,
                                   verbose = verbose,
                                   Server = Server)
        },
        error = function(cond){
          if (verbose) {
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
  } else {
    stop('Please input a list of DTXSIDs!')
  }
}

#' Get reported functional use via batch
#'
#' @param DTXSID Chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between requests
#' @param Server The root address for the API endpoint.
#' @param verbose A logical indicating if some "progress report" should be
#'   given.
#'
#' @returns A list of data.frames of reported functional use corresponding to
#' the input DTXSIDs.
#' @export
#'
#' @examplesIf FALSE
#' # Retrieve reported functional use for BPA and Caffeine
#' get_reported_functional_use_batch(DTXSID = c('DTXSID7020182',
#'                                              'DTXSID0020232'))
get_reported_functional_use_batch <- function(DTXSID = NULL,
                                              API_key = NULL,
                                              rate_limit = 0L,
                                              Server = 'https://comptox.epa.gov/ctx-api/exposure',
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
    results <- lapply(DTXSID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_reported_functional_use(DTXSID = t,
                                      API_key = API_key,
                                      verbose = verbose,
                                      Server = Server)
        },
        error = function(cond){
          if (verbose) {
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
  } else {
    stop('Please input a list of DTXSIDs!')
  }
}

#' Get chemical weight fraction via batch
#'
#' @param DTXSID Chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between requests
#' @param Server The root address for the API endpoint.
#' @param verbose A logical indicating if some "progress report" should be
#'   given.
#'
#' @returns A list of data.frames of hemical weight fraction data corresponding
#' to the input DTXSIDs.
#' @export
#'
#' @examplesIf FALSE
#' # Retrieve chemical weight fraction data for BPA and Caffeine
#' get_chemical_weight_fraction_batch(DTXSID = c('DTXSID7020182',
#'                                               'DTXSID0020232'))
get_chemical_weight_fraction_batch <- function(DTXSID = NULL,
                                               API_key = NULL,
                                               rate_limit = 0L,
                                               Server = 'https://comptox.epa.gov/ctx-api/exposure',
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
    results <- lapply(DTXSID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_chemical_weight_fraction(DTXSID = t,
                                       API_key = API_key,
                                       verbose = verbose,
                                       Server = Server)
        },
        error = function(cond){
          if (verbose) {
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
  } else {
    stop('Please input a list of DTXSIDs!')
  }
}

#' Retrieve exposure related functional use data batch
#'
#' @param DTXSID Chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be
#' given.
#'
#' @return A named list of data.frames, each containing exposure functional use
#'  data for each input DTXSID.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull exposure functional use data for multiple chemicals
#' dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
#' dtxsid_func_use <- get_exposure_functional_use_batch(DTXSID = dtxsid)

get_exposure_functional_use_batch <- function(DTXSID = NULL,
                                              API_key = NULL,
                                              rate_limit = 0L,
                                              Server = exposure_api_server,
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
    results <- lapply(DTXSID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_exposure_functional_use(DTXSID = t,
                                      API_key = API_key,
                                      verbose = verbose)
        },
        error = function(cond){
          if (verbose) {
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
  } else {
    stop('Please input a list of DTXSIDs!')
  }
}




#' Retrieve exposure functional use probability data batch
#'
#' @param DTXSID Chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be
#' given.
#'
#' @return A data.table, with each row containing exposure functional use
#' probability data for each input DTXSID. NA values are filled in for
#' categories that have probability of 0
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull exposure functional use probability data for multiple chemicals
#' dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
#' dtxsid_func_use_prob <- get_exposure_functional_use_batch(DTXSID = dtxsid)
get_exposure_functional_use_probability_batch <- function(DTXSID = NULL,
                                              API_key = NULL,
                                              rate_limit = 0L,
                                              Server = exposure_api_server,
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
    results <- lapply(DTXSID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_exposure_functional_use_probability(DTXSID = t,
                                      API_key = API_key,
                                      verbose = verbose)
        },
        error = function(cond){
          if (verbose) {
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
    na_index <- which(sapply(results, function(t) {length(t) == 0}))

    results_dt <- vector("list", length = length(DTXSID))

    if (length(DTXSID) > length(na_index)){
      for (i in seq_along(DTXSID)){
        if (!(i %in% na_index)){
          temp <- results[[i]]
          temp_dt <- data.table::as.data.table(cbind(temp, DTXSID = DTXSID[[i]]))
          temp_dt <- data.table::dcast(temp_dt,
                                       formula = DTXSID ~ harmonizedFunctionalUse,
                                       value.var = "probability")
          results_dt[[i]] <- temp_dt
        } else {
          results_dt[[i]] <- data.table::data.table(DTXSID = DTXSID[[i]])
        }
      }
      names(results_dt) <- DTXSID
      final_dt <- data.table::rbindlist(results_dt, fill = TRUE)
    } else {
      final_dt <- data.table::data.table()
    }
    return(final_dt)
  } else {
    stop('Please input a list of DTXSIDs!')
  }
}

#' Retrieve httk data via batch search
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some "progress report" should be given.
#'
#' @return A named list of httk data corresponding to the input chemicals
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Retrieve information for BPA and Caffeine
#' dtxsids <- c('DTXSID7020182', 'DTXSID0020232')
#' httk_data <- get_httk_data_batch(DTXSID = dtxsids)

get_httk_data_batch <- function(DTXSID = NULL,
                                API_key = NULL,
                                rate_limit = 0L,
                                Server = exposure_api_server,
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
    results <- lapply(DTXSID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_httk_data(DTXSID = t,
                        API_key = API_key,
                        verbose = verbose,
                        Server = Server)
        },
        error = function(cond){
          if (verbose) {
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
  } else {
    stop('Please input a list of DTXSIDs!')
  }
}

#' Retrieve product data for exposure purposes batch
#'
#' @param DTXSID Chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be
#' given.
#'
#' @return A named list of data.frames, each containing exposure product
#'  data for each input DTXSID.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull exposure functional use data for multiple chemicals
#' dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
#' dtxsid_product_data <- get_exposure_product_data_batch(DTXSID = dtxsid)
get_exposure_product_data_batch <- function(DTXSID = NULL,
                                            API_key = NULL,
                                            rate_limit = 0L,
                                            Server = exposure_api_server,
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
    results <- lapply(DTXSID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_exposure_product_data(DTXSID = t,
                               API_key = API_key,
                               verbose = verbose)
        },
        error = function(cond){
          if (verbose) {
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
  } else {
    stop('Please input a list of DTXSIDs!')
  }
}

#' Retrieve document data and list presence tags for chemicals batch
#'
#' @param DTXSID Chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be
#' given.
#'
#' @return A named list of data.frames, each containing exposure list presence
#' tags use data for each input DTXSID.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull exposure functional use data for multiple chemicals
#' dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
#' exp_list_tags <- get_exposure_list_presence_tags_by_dtxsid_batch(DTXSID = dtxsid)

get_exposure_list_presence_tags_by_dtxsid_batch <- function(DTXSID = NULL,
                                                            API_key = NULL,
                                                            rate_limit = 0L,
                                                            Server = exposure_api_server,
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
    results <- lapply(DTXSID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_exposure_list_presence_tags_by_dtxsid(DTXSID = t,
                                                    API_key = API_key,
                                                    verbose = verbose)
        },
        error = function(cond){
          if (verbose) {
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
  } else {
    stop('Please input a list of DTXSIDs!')
  }
}

#' Retrieve general exposure predictions for chemicals via batch
#'
#' @param DTXSID Chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be
#' given.
#'
#' @return A named list of data.frames, each containing general exposure
#' prediction data for each input DTXSID.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull general exposure prediction data for multiple chemicals
#' dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
#' exp_pred <- get_general_exposure_prediction_batch(DTXSID = dtxsid)

get_general_exposure_prediction_batch <- function(DTXSID = NULL,
                                                  API_key = NULL,
                                                  rate_limit = 0L,
                                                  Server = exposure_api_server,
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
    results <- lapply(DTXSID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_general_exposure_prediction(DTXSID = t,
                                          API_key = API_key,
                                          verbose = verbose,
                                          Server = Server)
        },
        error = function(cond){
          if (verbose) {
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
  } else {
    stop('Please input a list of DTXSIDs!')
  }
}


#' Retrieve demographic exposure predictions for chemicals via batch
#'
#' @param DTXSID Chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be
#' given.
#'
#' @return A named list of data.frames, each containing demographic exposure
#' prediction data for each input DTXSID.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull demographic exposure prediction data for multiple chemicals
#' dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
#' exp_demo <- get_demographic_exposure_prediction_batch(DTXSID = dtxsid)

get_demographic_exposure_prediction_batch <- function(DTXSID = NULL,
                                                      API_key = NULL,
                                                      rate_limit = 0L,
                                                      Server = exposure_api_server,
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
    results <- lapply(DTXSID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_demographic_exposure_prediction(DTXSID = t,
                                          API_key = API_key,
                                          verbose = verbose,
                                          Server = Server)
        },
        error = function(cond){
          if (verbose) {
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
  } else {
    stop('Please input a list of DTXSIDs!')
  }
}


#' Get Aggregate Records by DTXSID via batch
#'
#' @param DTXSID Chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param Server The root address for the API endpoint
#' @param verbose A logicial indicating if some "progress report" should be
#'   given.
#'
#' @returns A list of data.frames containing aggregate records data for each
#'   input DTXSID.
#' @export
#'
#' @examplesIf FALSE
#' # Retrieve aggregate records data for BPA and Caffeine
#' get_aggregate_records_by_dtxsid_batch(DTXSID = c('DTXSID0020232',
#'   'DTXSID7020182'))
get_aggregate_records_by_dtxsid_batch <- function(DTXSID = NULL,
                                                  API_key = NULL,
                                                  rate_limit = 0L,
                                                  Server = 'https://comptox.epa.gov/ctx-api/exposure',
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
    results <- lapply(DTXSID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_aggregate_records_by_dtxsid(DTXSID = t,
                                          API_key = API_key,
                                          verbose = verbose,
                                          Server = Server)
        },
        error = function(cond){
          if (verbose) {
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
  } else {
    stop('Please input a list of DTXSIDs!')
  }
}

#' Get Aggregate Records by medium via batch
#'
#' @param Medium The MMDB medium of exposure
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param Server The root address for the API endpoint
#' @param verbose A logicial indicating if some "progress report" should be
#'   given.
#'
#' @returns A list of data.frames containing aggregate records data for each
#'   input medium.
#' @export
#'
#' @examplesIf FALSE
#' # Retrieve aggregate records data for 'surface water' and 'soil'
#'   get_aggregate_records_by_medium_batch(Medium = c('surface water', 'soil'))
get_aggregate_records_by_medium_batch <- function(Medium = NULL,
                                                  API_key = NULL,
                                                  rate_limit = 0L,
                                                  Server = 'https://comptox.epa.gov/ctx-api/exposure',
                                                  verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }
  if (!is.null(Medium)){
    if (!is.character(Medium) & !all(sapply(Medium, is.character))){
      stop('Please input a character list for Medium!')
    }
    Medium <- unique(Medium)
    results <- lapply(Medium, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_aggregate_records_by_medium(Medium = t,
                                          API_key = API_key,
                                          verbose = verbose,
                                          Server = Server)
        },
        error = function(cond){
          if (verbose) {
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

    names(results) <- Medium
    return(results)
  } else {
    stop('Please input a list of Media!')
  }
}

#' Get Single Sample Records by DTXSID via batch
#'
#' @param DTXSID Chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param Server The root address for the API endpoint
#' @param verbose A logicial indicating if some "progress report" should be
#'   given.
#'
#' @returns A list of data.frames containing single sample records data for each
#'   input DTXSID.
#' @export
#'
#' @examplesIf FALSE
#' # Retrieve single sample records data for BPA and Caffeine
#' get_single_sample_records_by_dtxsid_batch(DTXSID = c('DTXSID0020232',
#'   'DTXSID7020182'))
get_single_sample_records_by_dtxsid_batch <- function(DTXSID = NULL,
                                                      API_key = NULL,
                                                      rate_limit = 0L,
                                                      Server = 'https://comptox.epa.gov/ctx-api/exposure',
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
    results <- lapply(DTXSID, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_single_sample_records_by_dtxsid(DTXSID = t,
                                              API_key = API_key,
                                              verbose = verbose,
                                              Server = Server)
        },
        error = function(cond){
          if (verbose) {
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
  } else {
    stop('Please input a list of DTXSIDs!')
  }
}

#' Get Single Sample Records by medium via batch
#'
#' @param Medium The MMDB medium of exposure
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param Server The root address for the API endpoint
#' @param verbose A logicial indicating if some "progress report" should be
#'   given.
#'
#' @returns A list of data.frames containing single sample records data for each
#'   input medium.
#' @export
#'
#' @examplesIf FALSE
#' # Retrieve single sample records data for 'surface water' and 'soil'
#' get_single_sample_records_by_medium_batch(Medium = c('surface water', 'soil'))
get_single_sample_records_by_medium_batch <- function(Medium = NULL,
                                                      API_key = NULL,
                                                      rate_limit = 0L,
                                                      Server = 'https://comptox.epa.gov/ctx-api/exposure',
                                                      verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.numeric(rate_limit) | (rate_limit < 0)){
    warning('Setting rate limit to 0 seconds between requests!')
    rate_limit <- 0L
  }
  if (!is.null(Medium)){
    if (!is.character(Medium) & !all(sapply(Medium, is.character))){
      stop('Please input a character list for Medium!')
    }
    Medium <- unique(Medium)
    results <- lapply(Medium, function(t){
      Sys.sleep(rate_limit)
      attempt <- tryCatch(
        {
          get_single_sample_records_by_medium(Medium = t,
                                              API_key = API_key,
                                              verbose = verbose,
                                              Server = Server)
        },
        error = function(cond){
          if (verbose) {
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

    names(results) <- Medium
    return(results)
  } else {
    stop('Please input a list of Media!')
  }
}
