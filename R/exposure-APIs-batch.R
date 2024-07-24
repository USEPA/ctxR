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
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ctx_key()) {
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
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
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ctx_key()) {
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
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
          message(t)
          message(cond$message)
          return(NA)
        }
      )
      return(attempt)
    }
    )
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
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ctx_key()) {
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
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
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ctx_key()) {
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
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
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ctx_key()) {
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
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
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ctx_key()) {
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
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
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ctx_key()) {
      API_key <- ctx_key()
      if (verbose) {
        message('Using stored API key!')
      }
    }
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
