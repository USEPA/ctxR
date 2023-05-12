#' Get hazard data by DTXSID batch
#'
#' @param DTXSID A list of chemical identifier DTXSIDs
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between each request
#'
#' @return A named list of data.frames containing chemical (human and eco)
#'   hazard data for each input chemical.
#' @export


get_hazard_by_dtxsid_batch <- function(DTXSID = NULL,
                                       API_key = NULL,
                                       rate_limit = 0L){
  if (is.null(API_key) || !is.character(API_key)){
    stop('Please input a character string containing a valid API key!')
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
          get_hazard_by_dtxsid(DTXSID = t,
                               API_key = API_key)
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

#' Get human hazard data by DTXSID batch
#'
#' @param DTXSID A list of chemical identifier DTXSIDs.
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between each request
#'
#' @return A named lit of data.frames containing chemical human hazard data.
#' @export


get_human_hazard_by_dtxsid_batch <- function(DTXSID = NULL,
                                             API_key = NULL,
                                             rate_limit = 0L){
  if (is.null(API_key) || !is.character(API_key)){
    stop('Please input a character string containing a valid API key!')
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
          get_human_hazard_by_dtxsid(DTXSID = t,
                               API_key = API_key)
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

#' Get ecotox hazard data by DTXSID batch
#'
#' @param DTXSID A list of chemical identifier DTXSIDs.
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between each request
#'
#' @return A named lit of data.frames containing chemical ecotox hazard data.
#' @export


get_ecotox_hazard_by_dtxsid_batch <- function(DTXSID = NULL,
                                              API_key = NULL,
                                              rate_limit = 0L){
  if (is.null(API_key) || !is.character(API_key)){
    stop('Please input a character string containing a valid API key!')
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
          get_ecotox_hazard_by_dtxsid(DTXSID = t,
                                     API_key = API_key)
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


#' Get skin and eye hazard batch
#'
#' @param DTXSID The chemical identifer DTXSIDs
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between each request
#'
#' @return A named list of data.frames containing skin and eye hazard data for
#'   each input DTXSID.
#' @export


get_skin_eye_hazard_batch <- function(DTXSID = NULL,
                                      API_key = NULL,
                                      rate_limit = 0L){
  if (is.null(API_key) || !is.character(API_key)){
    stop('Please input a character string containing a valid API key!')
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
          get_skin_eye_hazard(DTXSID = t,
                              API_key = API_key)
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


#' Get cancer hazard batch
#'
#' @param DTXSID The chemical identifier DTXSIDs
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between requests
#'
#' @return A named list of data.frames, each containing cancer hazard and
#'   related data for each input DTXSID.
#' @export


get_cancer_hazard_batch <- function(DTXSID = NULL,
                                    API_key = NULL,
                                    rate_limit = 0L){
  if (is.null(API_key) || !is.character(API_key)){
    stop('Please input a character string containing a valid API key!')
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
          get_cancer_hazard(DTXSID = t,
                            API_key = API_key)
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


#' Get genetox summary batch
#'
#' @param DTXSID The chemical identifier DTXSIDs
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between requests
#'
#' @return A named list of data.frames of genetox summary data for each input
#'   DTXSID.
#' @export


get_genetox_summary_batch <- function(DTXSID = NULL,
                                      API_key = NULL,
                                      rate_limit = 0L){
  if (is.null(API_key) || !is.character(API_key)){
    stop('Please input a character string containing a valid API key!')
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
          get_genetox_summary(DTXSID = t,
                              API_key = API_key)
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


#' Get genetox details batch
#'
#' @param DTXSID The chemical identifier DTXSIDs
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between requests
#'
#' @return A named list of data.frames of genetox detail data for each input
#'   DTXSID.
#' @export


get_genetox_details_batch <- function(DTXSID = NULL,
                                      API_key = NULL,
                                      rate_limit = 0L){
  if (is.null(API_key) || !is.character(API_key)){
    stop('Please input a character string containing a valid API key!')
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
          get_genetox_details(DTXSID = t,
                              API_key = API_key)
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
