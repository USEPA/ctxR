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
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input a character string containing a valid API key!')
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

get_hazard_by_dtxsid_batch_2 <- function(DTXSID = NULL,
                                         API_key = NULL,
                                         rate_limit = 0L,
                                         Server = hazard_api_server){
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input a character string containing a valid API key!')
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

      print(paste('The current index is i =', i, 'out of', length(indices)))

      response <- httr::POST(url = paste0(Server, '/search/by-dtxsid/'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'application/json',
                               'x-api-key' = API_key
                             )),
                             body = jsonlite::toJSON(DTXSID[indices[[i]]], auto_unbox = ifelse(length(DTXSID[indices[[i]]]) > 1, 'T', 'F')))

      print(paste('The response code is', response$status_code, 'for index i =', i))


      if (response$status_code == 200){
        dt <- suppressWarnings(data.table::rbindlist(list(dt,
                                                          data.table::data.table(jsonlite::fromJSON(httr::content(response,
                                                                                                                  as = 'text')))),
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
#'
#' @return A named lit of data.frames containing chemical human hazard data.
#' @export


get_human_hazard_by_dtxsid_batch <- function(DTXSID = NULL,
                                             API_key = NULL,
                                             rate_limit = 0L){
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input a character string containing a valid API key!')
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

get_human_hazard_by_dtxsid_batch_2 <- function(DTXSID = NULL,
                                         API_key = NULL,
                                         rate_limit = 0L,
                                         Server = hazard_api_server){
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input a character string containing a valid API key!')
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

      print(paste('The current index is i =', i, 'out of', length(indices)))

      response <- httr::POST(url = paste0(Server, '/human/search/by-dtxsid/'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'application/json',
                               'x-api-key' = API_key
                             )),
                             body = jsonlite::toJSON(DTXSID[indices[[i]]], auto_unbox = ifelse(length(DTXSID[indices[[i]]]) > 1, 'T', 'F')))

      print(paste('The response code is', response$status_code, 'for index i =', i))


      if (response$status_code == 200){
        dt <- suppressWarnings(data.table::rbindlist(list(dt,
                                                          data.table::data.table(jsonlite::fromJSON(httr::content(response,
                                                                                                                  as = 'text')))),
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
#'
#' @return A named lit of data.frames containing chemical ecotox hazard data.
#' @export


get_ecotox_hazard_by_dtxsid_batch <- function(DTXSID = NULL,
                                              API_key = NULL,
                                              rate_limit = 0L){
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input a character string containing a valid API key!')
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

get_ecotox_hazard_by_dtxsid_batch_2 <- function(DTXSID = NULL,
                                         API_key = NULL,
                                         rate_limit = 0L,
                                         Server = hazard_api_server){
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input a character string containing a valid API key!')
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

      print(paste('The current index is i =', i, 'out of', length(indices)))

      response <- httr::POST(url = paste0(Server, '/eco/search/by-dtxsid/'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'application/json',
                               'x-api-key' = API_key
                             )),
                             body = jsonlite::toJSON(DTXSID[indices[[i]]], auto_unbox = ifelse(length(DTXSID[indices[[i]]]) > 1, 'T', 'F')))

      print(paste('The response code is', response$status_code, 'for index i =', i))


      if (response$status_code == 200){
        dt <- suppressWarnings(data.table::rbindlist(list(dt,
                                                          data.table::data.table(jsonlite::fromJSON(httr::content(response,
                                                                                                                  as = 'text')))),
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
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input a character string containing a valid API key!')
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

get_skin_eye_hazard_batch_2 <- function(DTXSID = NULL,
                                      API_key = NULL,
                                      rate_limit = 0L,
                                      Server = hazard_api_server){
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input a character string containing a valid API key!')
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

      print(paste('The current index is i =', i, 'out of', length(indices)))

      response <- httr::POST(url = paste0(Server, '/skin-eye/search/by-dtxsid/'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'application/json',
                               'x-api-key' = API_key
                             )),
                             body = jsonlite::toJSON(DTXSID[indices[[i]]], auto_unbox = ifelse(length(DTXSID[indices[[i]]]) > 1, 'T', 'F')))

      print(paste('The response code is', response$status_code, 'for index i =', i))


      if (response$status_code == 200){
        dt <- suppressWarnings(data.table::rbindlist(list(dt,
                                                          data.table::data.table(jsonlite::fromJSON(httr::content(response,
                                                                                                                  as = 'text')))),
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
#'
#' @return A named list of data.frames, each containing cancer hazard and
#'   related data for each input DTXSID.
#' @export


get_cancer_hazard_batch <- function(DTXSID = NULL,
                                    API_key = NULL,
                                    rate_limit = 0L){
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input a character string containing a valid API key!')
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


get_cancer_hazard_batch_2 <- function(DTXSID = NULL,
                                    API_key = NULL,
                                    rate_limit = 0L,
                                    Server = hazard_api_server){
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input a character string containing a valid API key!')
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

      print(paste('The current index is i =', i, 'out of', length(indices)))

      response <- httr::POST(url = paste0(Server, '/cancer-summary/search/by-dtxsid/'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'application/json',
                               'x-api-key' = API_key
                             )),
                             body = jsonlite::toJSON(DTXSID[indices[[i]]], auto_unbox = ifelse(length(DTXSID[indices[[i]]]) > 1, 'T', 'F')))

      print(paste('The response code is', response$status_code, 'for index i =', i))


      if (response$status_code == 200){
        dt <- suppressWarnings(data.table::rbindlist(list(dt,
                                                          data.table::data.table(jsonlite::fromJSON(httr::content(response,
                                                                                                                  as = 'text')))),
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
#'
#' @return A named list of data.frames of genetox summary data for each input
#'   DTXSID.
#' @export


get_genetox_summary_batch <- function(DTXSID = NULL,
                                      API_key = NULL,
                                      rate_limit = 0L){
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input a character string containing a valid API key!')
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


get_genetox_summary_batch_2 <- function(DTXSID = NULL,
                                      API_key = NULL,
                                      rate_limit = 0L,
                                      Server = hazard_api_server){
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input a character string containing a valid API key!')
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

      print(paste('The current index is i =', i, 'out of', length(indices)))

      response <- httr::POST(url = paste0(Server, '/genetox/summary/search/by-dtxsid/'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'application/json',
                               'x-api-key' = API_key
                             )),
                             body = jsonlite::toJSON(DTXSID[indices[[i]]], auto_unbox = ifelse(length(DTXSID[indices[[i]]]) > 1, 'T', 'F')))

      print(paste('The response code is', response$status_code, 'for index i =', i))


      if (response$status_code == 200){
        dt <- suppressWarnings(data.table::rbindlist(list(dt,
                                                          data.table::data.table(jsonlite::fromJSON(httr::content(response,
                                                                                                                  as = 'text')))),
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
#'
#' @return A named list of data.frames of genetox detail data for each input
#'   DTXSID.


get_genetox_details_batch_old <- function(DTXSID = NULL,
                                      API_key = NULL,
                                      rate_limit = 0L){

  if (is.null(API_key) || !is.character(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input a character string containing a valid API key!')
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
#' @param Server The root address for the API endpoint
#'
#' @return A named list of data.frames of genetox detail data for each input
#'   DTXSID.
#' @export


get_genetox_details_batch <- function(DTXSID = NULL,
                                      API_key = NULL,
                                      rate_limit = 0L,
                                      Server = hazard_api_server){
  if (is.null(API_key) || !is.character(API_key)){
    if (has_ccte_key()) {
      API_key <- ccte_key()
      message('Using stored API key!')
    } else {
      stop('Please input a character string containing a valid API key!')
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

      print(paste('The current index is i =', i, 'out of', length(indices)))

      response <- httr::POST(url = paste0(Server, '/genetox/details/search/by-dtxsid/'),
                             httr::add_headers(.headers = c(
                               'Accept' = 'application/json',
                               'Content-Type' = 'application/json',
                               'x-api-key' = API_key
                             )),
                             body = jsonlite::toJSON(DTXSID[indices[[i]]], auto_unbox = ifelse(length(DTXSID[indices[[i]]]) > 1, 'T', 'F')))

      print(paste('The response code is', response$status_code, 'for index i =', i))


      if (response$status_code == 200){
        dt <- suppressWarnings(data.table::rbindlist(list(dt,
                                                          data.table::data.table(jsonlite::fromJSON(httr::content(response,
                                                                                                                  as = 'text')))),
                                                     fill = TRUE))
      }
      Sys.sleep(rate_limit)
    }



    return(dt)
  } else {
    stop('Please input a list of DTXSIDs!')
  }
}
