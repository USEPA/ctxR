#' Get hazard data by DTXSID batch
#'
#' @param DTXSID A list of chemical identifier DTXSIDs
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of data.frames containing chemical (human and eco)
#'   hazard data for each input chemical.


get_hazard_by_dtxsid_batch_old <- function(DTXSID = NULL,
                                           API_key = NULL,
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

#' Get hazard data by DTXSID batch
#'
#' @param DTXSID A list of chemical identifier DTXSIDs
#' @param API_key The user-specific API key
#' @param rate_limit Number of seconds to wait between each request
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.table containing chemical (human and eco) hazard data for each
#'   input chemical.
#' @export
#' @examplesIf has_ccte_key() & is.na(ccte_key() == 'FAKE_KEY')
#' # Pull hazard data for multiple chemicals
#' dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
#' batch_hazard <- get_hazard_by_dtxsid_batch(DTXSID = dtxsid)

get_hazard_by_dtxsid_batch <- function(DTXSID = NULL,
                                       API_key = NULL,
                                       rate_limit = 0L,
                                       Server = hazard_api_server,
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
  if (!is.null(DTXSID)){
    if (!is.character(DTXSID) & !all(sapply(DTXSID, is.character))){
      stop('Please input a character list for DTXSID!')
    }
    DTXSID <- unique(DTXSID)
    num_dtxsid <- length(DTXSID)
    indices <- generate_ranges(num_dtxsid)

    dt <- create_hazard_data.table()

    for (i in seq_along(indices)){

      if (verbose) {
        print(paste('The current index is i =', i, 'out of', length(indices)))
      }

      response <- httr::POST(url = paste0(Server, '/search/by-dtxsid/'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'application/json',
                               'x-api-key' = API_key
                             )),
                             body = jsonlite::toJSON(DTXSID[indices[[i]]], auto_unbox = ifelse(length(DTXSID[indices[[i]]]) > 1, 'T', 'F')))

      if (verbose) {
        print(paste('The response code is', response$status_code, 'for index i =', i))
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

create_hazard_data.table <- function(){
  dt <- data.table::data.table(id = integer(),
                               source = character(),
                               year = character(),
                               dtxsid = character(),
                               exposureRoute = character(),
                               toxvalNumeric = numeric(),
                               toxvalNumericQualifier = character(),
                               toxvalUnits = character(),
                               studyType = character(),
                               studyDurationClass = character(),
                               studyDuractionValue = numeric(),
                               studyDurationUnits = character(),
                               strain = character(),
                               sex = character(),
                               population = character(),
                               exposureMethod = character(),
                               exposureForm = character(),
                               media = character(),
                               lifestage = character(),
                               generation = character(),
                               criticalEffect = character(),
                               detailText = character(),
                               supercategory = character(),
                               speciesCommon = character(),
                               humanEcoNt = character(),
                               priorityId = integer(),
                               subsource = character(),
                               sourceUrl = character(),
                               subsourceUrl = character(),
                               riskAssessmentClass = character(),
                               toxvalType = character(),
                               toxvalSubtype = character())
  return(dt)
}

#' Get human hazard data by DTXSID batch
#'
#' @param DTXSID A list of chemical identifier DTXSIDs.
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of data.frames containing chemical human hazard data.


get_human_hazard_by_dtxsid_batch_old <- function(DTXSID = NULL,
                                                 API_key = NULL,
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

#' Get human hazard data by DTXSID batch
#'
#' @param DTXSID A list of chemical identifier DTXSIDs.
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between each request
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.table containing chemical human hazard data.
#' @export
#' @examplesIf has_ccte_key() & is.na(ccte_key() == 'FAKE_KEY')
#' # Pull human hazard data for multiples chemicals
#' dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
#' dtxsid_human_hazard <- get_human_hazard_by_dtxsid_batch(DTXSID = dtxsid)

get_human_hazard_by_dtxsid_batch <- function(DTXSID = NULL,
                                             API_key = NULL,
                                             rate_limit = 0L,
                                             Server = hazard_api_server,
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
  if (!is.null(DTXSID)){
    if (!is.character(DTXSID) & !all(sapply(DTXSID, is.character))){
      stop('Please input a character list for DTXSID!')
    }
    DTXSID <- unique(DTXSID)
    num_dtxsid <- length(DTXSID)
    indices <- generate_ranges(num_dtxsid)

    dt <- create_hazard_data.table()

    for (i in seq_along(indices)){

      if (verbose) {
        print(paste('The current index is i =', i, 'out of', length(indices)))
      }

      response <- httr::POST(url = paste0(Server, '/human/search/by-dtxsid/'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'application/json',
                               'x-api-key' = API_key
                             )),
                             body = jsonlite::toJSON(DTXSID[indices[[i]]], auto_unbox = ifelse(length(DTXSID[indices[[i]]]) > 1, 'T', 'F')))

      if (verbose) {
        print(paste('The response code is', response$status_code, 'for index i =', i))
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


#' Get ecotox hazard data by DTXSID batch
#'
#' @param DTXSID A list of chemical identifier DTXSIDs.
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of data.frames containing chemical ecotox hazard data.


get_ecotox_hazard_by_dtxsid_batch_old <- function(DTXSID = NULL,
                                                  API_key = NULL,
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

#' Get ecotox hazard data by DTXSID batch
#'
#' @param DTXSID A list of chemical identifier DTXSIDs.
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between each request
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.table containing chemical ecotox hazard data.
#' @export
#' @examplesIf has_ccte_key() & is.na(ccte_key() == 'FAKE_KEY')
#' # Pull ecotox hazard data for multiples chemicals
#' dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
#' dtxsid_ecotox_hazard <- get_ecotox_hazard_by_dtxsid_batch(DTXSID = dtxsid)

get_ecotox_hazard_by_dtxsid_batch <- function(DTXSID = NULL,
                                              API_key = NULL,
                                              rate_limit = 0L,
                                              Server = hazard_api_server,
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
  if (!is.null(DTXSID)){
    if (!is.character(DTXSID) & !all(sapply(DTXSID, is.character))){
      stop('Please input a character list for DTXSID!')
    }
    DTXSID <- unique(DTXSID)
    num_dtxsid <- length(DTXSID)
    indices <- generate_ranges(num_dtxsid)

    dt <- create_hazard_data.table()

    for (i in seq_along(indices)){

      if (verbose) {
        print(paste('The current index is i =', i, 'out of', length(indices)))
      }

      response <- httr::POST(url = paste0(Server, '/eco/search/by-dtxsid/'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'application/json',
                               'x-api-key' = API_key
                             )),
                             body = jsonlite::toJSON(DTXSID[indices[[i]]], auto_unbox = ifelse(length(DTXSID[indices[[i]]]) > 1, 'T', 'F')))

      if (verbose) {
        print(paste('The response code is', response$status_code, 'for index i =', i))
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


#' Get skin and eye hazard batch
#'
#' @param DTXSID The chemical identifier DTXSIDs
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between each request
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of data.frames containing skin and eye hazard data for
#'   each input DTXSID.


get_skin_eye_hazard_batch_old <- function(DTXSID = NULL,
                                          API_key = NULL,
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

#' Get skin and eye hazard batch
#'
#' @param DTXSID The chemical identifier DTXSIDs
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between each request
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of data.frames containing skin and eye hazard data for
#'   each input DTXSID.
#' @export
#' @examplesIf has_ccte_key() & is.na(ccte_key() == 'FAKE_KEY')
#' # Pull skin eye hazard data for multiples chemicals
#' dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
#' dtxsid_skin_eye_hazard <- get_skin_eye_hazard_batch(DTXSID = dtxsid)

get_skin_eye_hazard_batch <- function(DTXSID = NULL,
                                      API_key = NULL,
                                      rate_limit = 0L,
                                      Server = hazard_api_server,
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
  if (!is.null(DTXSID)){
    if (!is.character(DTXSID) & !all(sapply(DTXSID, is.character))){
      stop('Please input a character list for DTXSID!')
    }
    DTXSID <- unique(DTXSID)
    num_dtxsid <- length(DTXSID)
    indices <- generate_ranges(num_dtxsid)

    dt <- data.table::data.table(id = integer(),
                                 source = character(),
                                 year = integer(),
                                 endpoint = character(),
                                 dtxsid = character(),
                                 studyType = character(),
                                 strain = character(),
                                 classification = character(),
                                 guideline = character(),
                                 reliability = character(),
                                 resultText = character(),
                                 score = character(),
                                 species = character())

    for (i in seq_along(indices)){

      if (verbose) {
        print(paste('The current index is i =', i, 'out of', length(indices)))
      }

      response <- httr::POST(url = paste0(Server, '/skin-eye/search/by-dtxsid/'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'application/json',
                               'x-api-key' = API_key
                             )),
                             body = jsonlite::toJSON(DTXSID[indices[[i]]], auto_unbox = ifelse(length(DTXSID[indices[[i]]]) > 1, 'T', 'F')))

      if (verbose) {
        print(paste('The response code is', response$status_code, 'for index i =', i))
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

#' Get cancer hazard batch
#'
#' @param DTXSID The chemical identifier DTXSIDs
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between requests
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of data.frames, each containing cancer hazard and
#'   related data for each input DTXSID.


get_cancer_hazard_batch_old <- function(DTXSID = NULL,
                                        API_key = NULL,
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

#' Get cancer hazard batch
#'
#' @param DTXSID The chemical identifier DTXSIDs
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between requests
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.table containing cancer hazard and
#'   related data for each input DTXSID.
#' @export
#' @examplesIf has_ccte_key() & is.na(ccte_key() == 'FAKE_KEY')
#' # Pull cancer hazard data for multiples chemicals
#' dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
#' dtxsid_cancer_hazard <- get_cancer_hazard_batch(DTXSID = dtxsid)

get_cancer_hazard_batch <- function(DTXSID = NULL,
                                    API_key = NULL,
                                    rate_limit = 0L,
                                    Server = hazard_api_server,
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
  if (!is.null(DTXSID)){
    if (!is.character(DTXSID) & !all(sapply(DTXSID, is.character))){
      stop('Please input a character list for DTXSID!')
    }
    DTXSID <- unique(DTXSID)
    num_dtxsid <- length(DTXSID)
    indices <- generate_ranges(num_dtxsid)

    dt <- data.table::data.table(id = integer(),
                                 source = character(),
                                 url = integer(),
                                 cancerCall = character(),
                                 dtxsid = character(),
                                 exposureRoute = character())

    for (i in seq_along(indices)){

      if (verbose) {
        print(paste('The current index is i =', i, 'out of', length(indices)))
      }

      response <- httr::POST(url = paste0(Server, '/cancer-summary/search/by-dtxsid/'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'application/json',
                               'x-api-key' = API_key
                             )),
                             body = jsonlite::toJSON(DTXSID[indices[[i]]], auto_unbox = ifelse(length(DTXSID[indices[[i]]]) > 1, 'T', 'F')))

      if (verbose) {
        print(paste('The response code is', response$status_code, 'for index i =', i))
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


#' Get genetox summary batch
#'
#' @param DTXSID The chemical identifier DTXSIDs
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between requests
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of data.frames of genetox summary data for each input
#'   DTXSID.


get_genetox_summary_batch_old <- function(DTXSID = NULL,
                                          API_key = NULL,
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


#' Get genetox summary batch
#'
#' @param DTXSID The chemical identifier DTXSIDs
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between requests
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.table of genetox summary data for each input
#'   DTXSID.
#' @export
#' @examplesIf has_ccte_key() & is.na(ccte_key() == 'FAKE_KEY')
#' # Pull genetox summary data for multiples chemicals
#' dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
#' dtxsid_genetox_summary_hazard <- get_genetox_summary_batch(DTXSID = dtxsid)

get_genetox_summary_batch <- function(DTXSID = NULL,
                                      API_key = NULL,
                                      rate_limit = 0L,
                                      Server = hazard_api_server,
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
  if (!is.null(DTXSID)){
    if (!is.character(DTXSID) & !all(sapply(DTXSID, is.character))){
      stop('Please input a character list for DTXSID!')
    }
    DTXSID <- unique(DTXSID)
    num_dtxsid <- length(DTXSID)
    indices <- generate_ranges(num_dtxsid)

    dt <- data.table::data.table(id = integer(),
                                 dtxsid = character(),
                                 reportsPositive = integer(),
                                 reportsNegative = integer(),
                                 reportsOther = integer(),
                                 ames = character(),
                                 micronucleus = character())

    for (i in seq_along(indices)){

      if (verbose) {
        print(paste('The current index is i =', i, 'out of', length(indices)))
      }

      response <- httr::POST(url = paste0(Server, '/genetox/summary/search/by-dtxsid/'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'application/json',
                               'x-api-key' = API_key
                             )),
                             body = jsonlite::toJSON(DTXSID[indices[[i]]], auto_unbox = ifelse(length(DTXSID[indices[[i]]]) > 1, 'T', 'F')))

      if (verbose) {
        print(paste('The response code is', response$status_code, 'for index i =', i))
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



#' Get genetox details batch
#'
#' @param DTXSID The chemical identifier DTXSIDs
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between requests
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of data.frames of genetox detail data for each input
#'   DTXSID.


get_genetox_details_batch_old <- function(DTXSID = NULL,
                                          API_key = NULL,
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


#' Get genetox details batch
#'
#' @param DTXSID The chemical identifier DTXSIDs
#' @param API_key The user-specific API key.
#' @param rate_limit Number of seconds to wait between requests
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.table of genetox detail data for each input
#'   DTXSID.
#' @export
#' @examplesIf has_ccte_key() & is.na(ccte_key() == 'FAKE_KEY')
#' # Pull genetox details data for multiples chemicals
#' dtxsid <- c('DTXSID7020182', 'DTXSID2021315')
#' dtxsid_genetox_details_hazard <- get_genetox_details_batch(DTXSID = dtxsid)

get_genetox_details_batch <- function(DTXSID = NULL,
                                      API_key = NULL,
                                      rate_limit = 0L,
                                      Server = hazard_api_server,
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
  if (!is.null(DTXSID)){
    if (!is.character(DTXSID) & !all(sapply(DTXSID, is.character))){
      stop('Please input a character list for DTXSID!')
    }
    DTXSID <- unique(DTXSID)
    num_dtxsid <- length(DTXSID)
    indices <- generate_ranges(num_dtxsid)

    dt <- data.table::data.table(id = integer(),
                                 source = character(),
                                 year = integer(),
                                 dtxsid = character(),
                                 strain = character(),
                                 species = character(),
                                 assayCategory = character(),
                                 assayType = character(),
                                 metabolicActivation = character(),
                                 assayResult = character())

    for (i in seq_along(indices)){

      if (verbose) {
        print(paste('The current index is i =', i, 'out of', length(indices)))
      }

      response <- httr::POST(url = paste0(Server, '/genetox/details/search/by-dtxsid/'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'application/json',
                               'x-api-key' = API_key
                             )),
                             body = jsonlite::toJSON(DTXSID[indices[[i]]], auto_unbox = ifelse(length(DTXSID[indices[[i]]]) > 1, 'T', 'F')))

      if (verbose) {
        print(paste('The response code is', response$status_code, 'for index i =', i))
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
