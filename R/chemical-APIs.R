#' Retrieve chemical details from DTXSID of DTXCID
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param DTXCID The chemical identifier DTXCID
#' @param Projection The format and chemical detail data returned. Allowed
#'   values are 'chemicaldetailall', 'chemicaldetailstandard',
#'   'chemicalidentifier', 'chemicalstructure', 'ntatoolkit',
#'   'ccdchemicaldetails'. If left empty or there is a mismatch, the default
#'   format will be 'chemicaldetailstandard'.
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be
#'   given.
#'
#' @return A data.table containing chemical information for the chemical with
#'   DTXSID matching the input parameter.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemical details for BPA
#' bpa <- get_chemical_details(DTXSID = 'DTXSID7020182')

get_chemical_details <- function(DTXSID = NULL,
                                 DTXCID = NULL,
                                 Projection = 'chemicaldetailstandard',
                                 API_key = NULL,
                                 Server = chemical_api_server,
                                 verbose = FALSE){
  if (is.null(DTXSID) & is.null(DTXCID))
    stop('Please input a DTXSID or DTXCID!')
  else if (!is.null(DTXSID) & !is.null(DTXCID))
    stop('Please input either a DTXSID or DTXCID, but not both!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  projection_entries <- c('chemicaldetailall',
                          'chemicaldetailstandard',
                          'chemicalidentifier',
                          'chemicalstructure',
                          'ntatoolkit',
                          'ccdchemicaldetails')
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
        message(paste0('Using `Projection` = ', projection_entries[index], '!'))
      }
      Projection <- projection_entries[index]
    }
  }

  projection_url <- paste0('?projection=', Projection)

  if (!is.null(DTXSID)){
    response <- httr::GET(url = paste0(Server, '/detail/search/by-dtxsid/', DTXSID, projection_url),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  } else {
    response <- httr::GET(url = paste0(Server, '/detail/search/by-dtxcid/', DTXCID, projection_url),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  }

  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    empty_table <- create_data.table_chemical_details(index = index)
    data_list <- jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")) #Parse to list
    missing_names <- which(sapply(data_list, is.null))
    if (length(missing_names) > 0){
    df <- t(data.frame(unlist(data_list), row.names = names(data_list)[-missing_names]))
    } else {
      df <- t(data.frame(unlist(data_list), row.names = names(data_list)))
    }
    #return(data_list)
    dt <- suppressWarnings(data.table::rbindlist(list(empty_table,
                                                      data.table::data.table(df)),
                                                 fill = TRUE))
    return(dt)
  } else {
    if (verbose){
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()
}

#' Create chemical details data.table helper function
#'
#' @param index Determine which format should be used.
#'
#' @return An empty data.table with columns matching the expected format of the
#'   get_chemical_details API call.
#' @keywords internal


create_data.table_chemical_details <- function(index = -1){
  if (index %in% 2:6 ){
    if (index == 2){
      data <- data.table::data.table(id = character(),
                                     cpdataCount = integer(),
                                     inchikey = character(),
                                     wikipediaArticle = character(),
                                     monoisotopicMass = numeric(),
                                     percentAssays = numeric(),
                                     pubchemCount = integer(),
                                     pubmedCount = numeric(),
                                     sourcesCount = integer(),
                                     qcLevel = integer(),
                                     qcLevelDesc = character(),
                                     isotope = integer(),
                                     multicomponent = integer(),
                                     totalAssays = integer(),
                                     pubchemCid = integer(),
                                     relatedSubstanceCount = integer(),
                                     relatedStructureCount = integer(),
                                     casrn = character(),
                                     compoundId = integer(),
                                     genericSubstanceId = integer(),
                                     preferredName = character(),
                                     activeAssays = integer(),
                                     molFormula = character(),
                                     hasStructureImage = integer(),
                                     iupacName = character(),
                                     smiles = character(),
                                     inchiString = character(),
                                     qcNotes = character(),
                                     qsarReadySmiles = character(),
                                     msReadySmiles = character(),
                                     irisLink = character(),
                                     pprtvLink = character(),
                                     descriptorStringTsv = character(),
                                     isMarkush = integer(),
                                     dtxsid = character(),
                                     dtxcid = character(),
                                     toxcastSelect = character())
    } else if (index == 3){
      data <- data.table::data.table(inchikey = character(),
                                     casrn = character(),
                                     preferredName = character(),
                                     iupacName = character(),
                                     dtxsid = character(),
                                     dtxcid = character())
    } else if (index == 4){
      data <- data.table::data.table(id = character(),
                                     inchikey = character(),
                                     casrn = character(),
                                     preferredName = character(),
                                     hasStructureImage = integer(),
                                     smiles = character(),
                                     inchiString = character(),
                                     qsarReadySmiles = character(),
                                     msReadySmiles = character(),
                                     dtxsid = character(),
                                     dtxcid = character())
    } else if (index == 5) {
      data <- data.table::data.table(preferredName = character(),
                                     inchikey = character(),
                                     msReadySmiles = character(),
                                     dtxsid = character(),
                                     dtxcid = character(),
                                     casrn = character(),
                                     sourcesCount = integer(),
                                     totalAssays = integer(),
                                     smiles = character(),
                                     activeAssays = integer(),
                                     cpdataCount = integer(),
                                     molFormula = character(),
                                     monoisotopicMass = numeric(),
                                     percentAssays = numeric(),
                                     expocatMedianPrediction = character(),
                                     expocat = character(),
                                     nhanes = character())
    } else {
      data <- data.table::data.table(id = character(),
                                     dtxsid = character(),
                                     dtxcid = character(),
                                     casrn = character(),
                                     compoundId = integer(),
                                     genericSubstanceId = integer(),
                                     preferredName = character(),
                                     activeAssays = integer(),
                                     molFormula = character(),
                                     monoisotopicMass = numeric(),
                                     percentAssays = integer(),
                                     pubchemCount = integer(),
                                     pubmedCount = integer(),
                                     sourcesCount = integer(),
                                     qcLevel = integer(),
                                     qcLevelDesc = character(),
                                     isotope = integer(),
                                     multicomponent = integer(),
                                     totalAssays = integer(),
                                     toxcastSelect = character(),
                                     pubchemCid = integer(),
                                     relatedSubstanceCount = integer(),
                                     relatedStructureCount = integer(),
                                     hasStructureImage = integer(),
                                     iupacName = character(),
                                     smiles = character(),
                                     inchiString = character(),
                                     averageMass = numeric(),
                                     qcNotes = character(),
                                     qsarReadySmiles = character(),
                                     msReadySmiles = character(),
                                     irisLink = character(),
                                     pprtvLink = character(),
                                     isMarkush = integer(),
                                     inchikey = character(),
                                     wikipediaArticle = character(),
                                     cpdataCount = integer())
    }
    return(data)
  }
  data <- data.table::data.table(id = integer(),
                                 dtxsid = character(),
                                 dtxcid = character(),
                                 casrn = character(),
                                 compoundId = integer(),
                                 genericSubstanceId = integer(),
                                 preferredName = character(),
                                 activeAssays = integer(),
                                 cpdataCount = integer(),
                                 molFormula = character(),
                                 monoisotopicMass = double(),
                                 percentAssays = double(),
                                 pubchemCount = integer(),
                                 pubmedCount = double(),
                                 sourcesCount = integer(),
                                 qcLevel = integer(),
                                 qcLevelDesc = character(),
                                 stereo = character(),
                                 isotope = integer(),
                                 multicomponent = integer(),
                                 totalAssays = integer(),
                                 toxcastSelect = character(),
                                 pubchemCid = integer(),
                                 relatedSubstanceCount = integer(),
                                 relatedStructureCount = integer(),
                                 hasStructureImage = integer(),
                                 iupacName = character(),
                                 smiles = character(),
                                 inchiString = character(),
                                 averageMass = double(),
                                 inchikey = character(),
                                 qcNotes = character(),
                                 qsarReadySmiles = character(),
                                 msReadySmiles = character(),
                                 irisLink = character(),
                                 pprtvLink = character(),
                                 wikipediaArticle = character(),
                                 descriptorStringTsv = character(),
                                 isMarkush = integer(),
                                 dateLoaded = character(),
                                 hchemHashKey = character())
  return(data)
}

#' Check existence by DTXSID
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating whether some "progress report" should be
#' given.
#'
#' @return A data.table with information on whether the input DTXSID is valid,
#' and where to find more information on the chemical when the DTXSID is valid.
#' @export
#'
#' @examplesIf FALSE
#' # DTXSID for bpa
#' bpa <- check_existence_by_dtxsid('DTXSID7020182')
#' # False DTXSID
#' false_res <- check_existence_by_Dtxsid('DTXSID7020182f')

check_existence_by_dtxsid <- function(DTXSID = NULL,
                                      API_key = NULL,
                                      Server = chemical_api_server,
                                      verbose = FALSE){
  if (is.null(DTXSID) | !is.character(DTXSID)){
    stop('Please input a DTXSID!')
  }

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/ghslink/to-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    res_content <- jsonlite::fromJSON(httr::content(response,
                                                    as = 'text',
                                                    encoding = "UTF-8"))
    if (is.null(res_content$safetyUrl)){
      res_content$safetyUrl <- NA_character_
    }
    res <- data.table::rbindlist(list(res_content))
    return(res)
  } else {
    if (verbose){
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()


}

get_chemical_details_by_listname <- function(listname = NULL,
                                             API_key = NULL,
                                             Server = chemical_api_server,
                                             verbose = FALSE){
  if (is.null(listname) | !is.character(listname)){
    stop('Please input a character string for listname!')
  }

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/detail/search/by-listname/', listname),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 401){
    stop('Please input an API_key!')
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose){
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()


}

#' Get Smiles
#'
#' @param name Chemical name
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be
#'   given.
#'
#' @return A string giving a SMILES string for the input chemical.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' bpa_smiles <- get_smiles(name = "Bisphenol A")

get_smiles <- function(name = NULL,
                       API_key = NULL,
                       Server = chemical_api_server,
                       verbose = FALSE){
  if (is.null(name) | !is.character(name)){
    stop('Please input a character string for name!')
  }

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/opsin/to-smiles/', prepare_word(name)),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    return(httr::content(response, as = 'text', encoding = "UTF-8"))
  } else {
    if (verbose){
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()

}

#' Get InChIKey
#'
#' @param name Chemical name
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be
#'   given.
#' @return A string giving the associated InChIKey.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' bpa_inchikey <- get_inchikey(name = "Bisphenol A")

get_inchikey <- function(name = NULL,
                         API_key = NULL,
                         Server = chemical_api_server,
                         verbose = FALSE){
  if (is.null(name) | !is.character(name)){
    stop('Please input a character string for name!')
  }

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/opsin/to-inchikey/', prepare_word(name)),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    return(httr::content(response, as = 'text', encoding = "UTF-8"))
  } else {
    if (verbose){
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()

}

#' Get InChI
#'
#' @param name Chemical name
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be
#'   given.
#' @return A string giving the associated inchi string.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' bpa_inchi <- get_inchi(name = "Bisphenol A")

get_inchi <- function(name = NULL,
                      API_key = NULL,
                      Server = chemical_api_server,
                      verbose = FALSE){
  if (is.null(name) | !is.character(name)){
    stop('Please input a character string for name!')
  }

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/opsin/to-inchi/', prepare_word(name)),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    return(httr::content(response, as = 'text', encoding = "UTF-8"))
  } else {
    if (verbose){
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()

}

#' Get chemicals by property and its value range
#'
#' @param start A numeric value, the beginning of the range
#' @param end A numeric value, the end of the range
#' @param property A string, the property in question
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be
#'   given.
#'
#' @return A data.frame containing chemical information for chemicals matching
#'   the search criteria.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemicals with a given property in a set range
#' density <- get_chemical_by_property_range(start = 1.311, end = 1.313,
#'                                           property = 'Density')

get_chemical_by_property_range <- function(start = NULL,
                                           end = NULL,
                                           property = NULL,
                                           API_key = NULL,
                                           Server = chemical_api_server,
                                           verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (is.null(start) || is.null(end) || !is.numeric(start) || !is.numeric(end)){
    stop('Please input a numeric value for both start and end!')
  }

  if (start > end){
    warning('Swapping values for start and end!', immediate. = TRUE)
    temp <- end
    end <- start
    start <- temp
  }

  if (is.null(property)){
    stop('Please input a value for property!')
  }

  response <- httr::GET(url = paste0(Server, '/property/search/by-range/',
                                         prepare_word(property),'/', start, '/', end),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                          )
                        )

  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
    } else {
      if (verbose){
        print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
      }
    }
    return()
}


#' Retrieve chemical information
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param type This specifies whether to only grab predicted or experimental
#'   results. If not specified, it will grab all details. The allowable input
#'   values are "predicted" or "experimental".
#' @param API_key The user-specific API Key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame containing chemical information for the chemical with
#'   DTXSID matching the input parameter.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemical information for BPA
#' bpa <- get_chem_info(DTXSID = 'DTXSID7020182')

get_chem_info <- function(DTXSID = NULL,
                          type = "",
                          API_key = NULL,
                          Server = chemical_api_server,
                          verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  types <- c("", "predicted", "experimental")
  type <- which(types %in% type)
  if (length(type) == 0){
    stop('Please input a correct choice for type!')
  } else if (length(type) > 1){
    warning('Setting type to ""!')
    type <- ''
  } else {
    type <- types[type]
  }

  if (type == '') {
    response <- httr::GET(url = paste0(Server, '/property/search/by-dtxsid/', DTXSID),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  } else {
    response <- httr::GET(url = paste0(Server, '/property/search/by-dtxsid/', DTXSID,'?type=', type),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  }


 # response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/property/search/by-dtxsid/', DTXSID),
#                        httr::add_headers(.headers = c(
#                          'Content-Type' =  'application/json',
#                          'x-api-key' = API_key)
#                          )
#                        )


  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text',encoding = "UTF-8")))
  } else {
    if (verbose){
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
 return()
}

#' Get fate by DTXSID
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame containing chemical information for the chemical with
#'   DTXSID matching the input parameter.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemical fate data for BPA
#' bpa <- get_fate_by_dtxsid(DTXSID = 'DTXSID7020182')

get_fate_by_dtxsid <- function(DTXSID = NULL,
                               API_key = NULL,
                               Server = chemical_api_server,
                               verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/fate/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()

}


#' Chemical starts with
#'
#' @param word A character string of a chemical identifier or portion of a chemical
#'   identifier. Identifiers can be a chemical name, dtxsid, dtxcid, casrn, or inchikey.
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be
#'   given.
#' @param top The number of results to return if there are multiple results
#'   available
#'
#' @return A data.frame of chemicals and related values matching the query
#'   parameters
#' @author Paul Kruse, Kristin Issacs
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemicals that start with a fragment DTXSID
#' dtxsid_fragment <- chemical_starts_with(word = 'DTXSID702018')

chemical_starts_with <- function(word = NULL,
                           API_key = NULL,
                           Server = chemical_api_server,
                           verbose = FALSE,
                           top = NULL){
  if (is.null(word) || !is.character(word)){
    stop('Please input a character value for word!')
  }

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
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

  word <- prepare_word(word)

  response <- httr::GET(url = paste0(Server, '/search/start-with/', word, ifelse(is.null(top), '', paste0("?top=", top))),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if (response$status == 401){
    stop(httr::content(response)$detail)
  }

  if (response$status == 400) {
    parsed_response <- jsonlite::fromJSON(httr::content(response, as = 'text', encoding = 'UTF-8'))
    if ('suggestions' %in% names(parsed_response)){
      frame <- data.frame(Chemical = urltools::url_decode(word))
      frame$Suggestion <- list(parsed_response$suggestions)
      return(frame)
    }
  } else if (response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()



}



#' Chemical equal
#'
#' @param word A character string of a chemical identifier or portion of a chemical
#'   identifier. Identifiers can be a chemical name, dtxsid, dtxcid, casrn, or inchikey.
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame of chemicals and related values matching the query
#'   parameters
#' @author Paul Kruse, Kristin Issacs
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemicals with matching DTXSID
#' bpa_dtxsid <- chemical_equal(word = 'DTXSID7020182')

chemical_equal <- function(word = NULL,
                           API_key = NULL,
                           Server = chemical_api_server,
                           verbose = FALSE){
  if (is.null(word) || !is.character(word)){
    stop('Please input a character value for word!')
  }

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  word <- prepare_word(word)

  response <- httr::GET(url = paste0(Server, '/search/equal/', word),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  } else if (response$status == 400) {
    parsed_response <- jsonlite::fromJSON(httr::content(response, as = 'text', encoding = 'UTF-8'))
    if ('suggestions' %in% names(parsed_response)){
      frame <- data.frame(Chemical = urltools::url_decode(word))
      frame$Suggestion <- list(parsed_response$suggestions)
      return(frame)
    }
  } else if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()



}





#' Chemical contains
#'
#' @param word A character string of a chemical identifier or portion of a chemical
#'   identifier. Identifiers can be a chemical name, dtxsid, dtxcid, casrn, or inchikey.
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#' @param top The number of results to return if there are multiple results
#'   available
#'
#' @return A data.frame of chemicals and related values matching the query
#'   parameters
#' @author Paul Kruse, Kristin Issacs
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemicals that contain substring
#' substring_chemicals <- chemical_contains(word = 'TXSID702018')

chemical_contains <- function(word = NULL,
                              API_key = NULL,
                              Server = chemical_api_server,
                              verbose = FALSE,
                              top = NULL){
  if (is.null(word) || !is.character(word)){
    stop('Please input a character value for word!')
  }

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
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

  word <- prepare_word(word)

  response <- httr::GET(url = paste0(Server, '/search/contain/', word, ifelse(is.null(top), '', paste0("?top=", top))),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  } else if (response$status == 400) {
    parsed_response <- jsonlite::fromJSON(httr::content(response, as = 'text', encoding = 'UTF-8'))
    if ('suggestions' %in% names(parsed_response)){
      frame <- data.frame(Chemical = urltools::url_decode(word))
      frame$Suggestion <- list(parsed_response$suggestions)
      return(frame)
    }
  } else if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()



}

#' Prepare url helper function
#'
#' @param word A character string
#' @return A character string that is ready for use in http request
#' @keywords internal


prepare_word <- function(word){
  # Handle question marks
  split_words <- stringr::str_split(string = word,
                                    pattern = '\\?',
                                    n = 2)[[1]]
  if (length(split_words) == 1){
    temp_word <- urltools::url_encode(split_words[[1]])
  } else {
    if (nchar(split_words[[2]]) == 0){
      temp_word <- urltools::url_encode(split_words[[1]])
    } else {
      temp_word <- paste0(urltools::url_encode(split_words[[1]]),
                          '?',
                          urltools::url_encode(split_words[[2]]),
                          '=')
    }
  }

  # Handle other non-alpha-numeric characters
  temp_word <- gsub("%26", "&", temp_word)
  temp_word <- gsub("%23", "#", temp_word)
  return(temp_word)
}


#' Get msready by mass
#'
#' @param start The starting value for mass range
#' @param end The ending value for mass range
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A list of DTXSIDs with msready mass falling within the given range.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemicals with msready mass in given range
#' mass_range <- get_msready_by_mass(start = 200.9, end = 200.95)

get_msready_by_mass <- function(start = NULL,
                                end = NULL,
                                API_key = NULL,
                                Server = chemical_api_server,
                                verbose = FALSE){
  if(is.null(start) || is.null(end) || !is.numeric(start) || !is.numeric(end)){
    stop('Please input a numeric value for both start and end!')
  }

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (start < 0 || end < 0){
    stop('Both start and end must be non-negative!')
  }

  if (start > end){
    warning('Swapping values for start and end!', immediate. = TRUE)
    temp <- end
    end <- start
    start <- temp
  }

  response <- httr::GET(url = paste0(Server, '/msready/search/by-mass/', start, '/', end),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()

}



#' Get msready by formula
#'
#' @param formula A string denoting the input chemical formula
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A character list of DTXSIDs with chemical formulas matching the
#'   search criteria
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemicals that match input formula
#' mass_formula <- get_msready_by_formula(formula = 'C16H24N2O5S')

get_msready_by_formula <- function(formula = NULL,
                                   API_key = NULL,
                                   Server = chemical_api_server,
                                   verbose = FALSE){
  if(is.null(formula)){
    stop("Please input a non-null value for formula!")
  } else if (!is.character(formula)){
    stop("Please input a character string for the formula parameter!")
  }

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/msready/search/by-formula/', formula),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()


}


#' Get msready by DTXCID
#'
#' @param DTXCID The chemical identifier DTXCID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A character list of DTXSIDs with DTXCIDs matching the
#'   search criteria
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemicals with matching DTXCID
#' dtxcid_msready <- get_msready_by_dtxcid(DTXSID = 'DTXCID30182')

get_msready_by_dtxcid <- function(DTXCID = NULL,
                                   API_key = NULL,
                                  Server = chemical_api_server,
                                  verbose = FALSE){
  if(is.null(DTXCID)){
    stop("Please input a non-null value for DTXCID!")
  } else if (!is.character(DTXCID)){
    stop("Please input a character string for the DTXCID parameter!")
  }

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/msready/search/by-dtxcid/', DTXCID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()


}

#' Get all list types
#'
#' @param API_key The user-specific API key.
#' @param Server The root address for the API endpoint.
#' @param verbose A logical indicating if some "progress report" should be given.
#'
#' @return A character list of types of lists.
#' @export
#'
#' @examplesIf FALSE
#' get_all_list_types()
get_all_list_types <- function(API_key = NULL,
                               Server = chemical_api_server,
                               verbose = FALSE){

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(chemical_api_server, "/list/type"),
                        httr::add_headers(.headers = c('Accept' = 'application/json',
                                                       'Content' = 'application/json',
                                                       'x-api-key' = API_key)))
  return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = 'UTF-8')))


}

#' Get chemical lists by type
#'
#' @param type The type of list. This is a case sensitive parameter and returns
#'   lists only for values of "federal", "international", "state", and "other".
#' @param Projection Optional parameter controlling return type. It takes values
#'   'chemicallistall' and 'chemicallistname' with the former as the default
#'   value.
#' @param API_key The user-specified API key.
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame containing information about lists that meet the search
#'   criteria.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemical lists by type
#' federal <- get_chemical_lists_by_type(type = 'Federal')

get_chemical_lists_by_type <- function(type = NULL,
                                       Projection = '',
                                       API_key = NULL,
                                       Server = chemical_api_server,
                                       verbose = FALSE){
  if (is.null(type) | !is.character(type))
    stop('Please input a value for parameter type from the list `federal`, `international`, `state`, and `other`!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }


  projection_entries <- c('chemicallistall',
                          'chemicallistname',
                          '')
  index <- -1
  if (!is.character(Projection)){
    warning('Setting `Projection` to empty string!')
    Projection <- ''
  } else {
    Projection <- tolower(Projection)
    index <- which(projection_entries %in% Projection)
    if (length(index) != 1){
      warning('Setting `Projection` to empty string!')
      Projection <- ''
      index <- -1
    }
  }

  projection_url <- ifelse(index %in% c(-1,3), '', paste0('?projection=', projection_entries[index]))

  response <- httr::GET(url = paste0(Server, '/list/search/by-type/', type, projection_url),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()


}


#' Get chemical list by name
#'
#' @param list_name The name of the list of chemicals
#' @param Projection Optional parameter controlling return type. It takes values
#'   chemicallistall' and 'chemicallistname' with the former as the default
#'   value.
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame containing information about the chemical list. Note,
#'   this is not the chemical list itself. To access the chemicals in the list,
#'   use \code{\link{get_chemicals_in_list}}.
#' @seealso \code{\link{get_chemicals_in_list}}
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemical list by list name
#' ccl4 <- get_public_chemical_list_by_name(list_name = 'CCL4')

get_public_chemical_list_by_name <- function(list_name = NULL,
                                             Projection = '',
                                             API_key = NULL,
                                             Server = chemical_api_server,
                                             verbose = FALSE){
  if (is.null(list_name))
    stop('Please input list_name!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  projection_entries <- c('chemicallistall',
                          'chemicallistname',
                          'chemicallistwithdtxsids',
                          '')
  index <- -1
  if (!is.character(Projection)){
    warning('Setting `Projection` to empty string!')
    Projection <- ''
  } else {
    Projection <- tolower(Projection)
    index <- which(projection_entries %in% Projection)
    if (length(index) != 1){
      warning('Setting `Projection` to empty string!')
      Projection <- ''
      index <- -1
    }
  }

  projection_url <- ifelse(index %in% c(-1, 4), '', paste0('?projection=', projection_entries[index]))



  response <- httr::GET(url = paste0(Server, '/list/search/by-name/', list_name, projection_url),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    return(data.frame(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8"))))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()

}

#' Get chemical lists containing given chemical
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A list of names of chemical lists that contain the given chemical
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemical lists containing BPA
#' bpa_lists <- get_lists_containing_chemical(DTXSID = 'DTXSID7020182')

get_lists_containing_chemical <- function(DTXSID = NULL,
                                          API_key = NULL,
                                          Server = chemical_api_server,
                                          verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input a non-null value for DTXSID!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/list/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()

}

#' Get chemicals in a given chemical list
#'
#' @param list_name The name of the list of chemicals
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame of the chemical list
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Retrieve chemicals contained in chemical list 'CCL4'
#' ccl4_chemicals <- get_chemicals_in_list(list_name = 'CCL4')

get_chemicals_in_list <- function(list_name = NULL,
                                  API_key = NULL,
                                  Server = chemical_api_server,
                                  verbose = FALSE){






  if (is.null(list_name) | !is.character(list_name))
    stop('Please input a character value for list_name!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  new_response <- get_public_chemical_list_by_name(list_name = list_name,
                                                   Projection = 'chemicallistwithdtxsids',
                                                   API_key = API_key,
                                                   Server = Server,
                                                   verbose = verbose)

  if (is.null(new_response)){
    return()
  }

  if ('dtxsids' %in% names(new_response)){
    return(get_chemical_details_batch(DTXSID = strsplit(x = new_response$dtxsids, split = ',')[[1]]))
  }

  response <- httr::GET(url = paste0(Server, '/list/chemicals/search/by-listname/', list_name),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()


}


#' Get chemicals in a list specified by starting characters
#'
#' @param list_name The name of the list to search
#' @param word The starting characters to match chemicals in the given list
#' @param API_key The user-specific api key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some "progress report" should be given.
#'
#' @return A list of DTXSIDs matching the list and search word criteria
#' @export
#'
#' @examplesIf FALSE
#' bis_biosolids_2021 <- get_chemicals_in_list_start(list_name = 'BIOSOLIDS2021',
#'                                                   word = 'Bi')

get_chemicals_in_list_start <- function(list_name = NULL,
                                        word = NULL,
                                        API_key = NULL,
                                        Server = chemical_api_server,
                                        verbose = FALSE){
  if (is.null(list_name) | !is.character(list_name))
    stop('Please input a character value for list_name!')
  else if (is.null(word) | !is.character(word))
    stop('Please input a character value for word!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }


  response <- httr::GET(url = paste0(Server, '/list/chemicals/search/start-with/',
                                     prepare_word(list_name),'/',
                                     prepare_word(word)),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()
}

#' Get chemicals in a list specified by exact characters
#'
#' @param list_name The name of the list to search
#' @param word The exact characters to match chemicals in the given list
#' @param API_key The user-specific api key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some "progress report" should be given.
#'
#' @return A list of DTXSIDs matching the list and search word criteria
#' @export
#'
#' @examplesIf FALSE
#' bis_biosolids_2021 <- get_chemicals_in_list_exact(list_name = 'BIOSOLIDS2021',
#'                                                   word = 'Bisphenol A')

get_chemicals_in_list_exact <- function(list_name = NULL,
                                        word = NULL,
                                        API_key = NULL,
                                        Server = chemical_api_server,
                                        verbose = FALSE){
  if (is.null(list_name) | !is.character(list_name))
    stop('Please input a character value for list_name!')
  else if (is.null(word) | !is.character(word))
    stop('Please input a character value for word!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }


  response <- httr::GET(url = paste0(Server, '/list/chemicals/search/equal/',
                                     prepare_word(list_name),'/',
                                     prepare_word(word)),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()
}

#' Get chemicals in a list specified by contained characters
#'
#' @param list_name The name of the list to search
#' @param word The contained characters to match chemicals in the given list
#' @param API_key The user-specific api key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some "progress report" should be given.
#'
#' @return A list of DTXSIDs matching the list and search word criteria
#' @export
#'
#' @examplesIf FALSE
#' bis_biosolids_2021 <- get_chemicals_in_list_contain(list_name = 'BIOSOLIDS2021',
#'                                                     word = 'Bis')

get_chemicals_in_list_contain <- function(list_name = NULL,
                                          word = NULL,
                                          API_key = NULL,
                                          Server = chemical_api_server,
                                          verbose = FALSE){
  if (is.null(list_name) | !is.character(list_name))
    stop('Please input a character value for list_name!')
  else if (is.null(word) | !is.character(word))
    stop('Please input a character value for word!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }


  response <- httr::GET(url = paste0(Server, '/list/chemicals/search/contain/',
                                     prepare_word(list_name),'/',
                                     prepare_word(word)),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()
}



#' Get all public chemical lists
#'
#' @param Projection Optional parameter controlling return type. It takes values
#'   chemicallistall' and 'chemicallistname' with the former as the default
#'   value.
#' @param API_key The user-specific api key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A data.frame containing information on all public chemical lists
#'   available from the CTX chemical api.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull all chemical lists
#' all_lists <- get_all_public_chemical_lists()

get_all_public_chemical_lists <- function(Projection = '',
                                          API_key = NULL,
                                          Server = chemical_api_server,
                                          verbose = FALSE){
  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  projection_entries <- c('chemicallistall',
                          'chemicallistname',
                          '')
  index <- -1
  if (!is.character(Projection)){
    warning('Setting `Projection` to empty string!')
    Projection <- ''
  } else {
    Projection <- tolower(Projection)
    index <- which(projection_entries %in% Projection)
    if (length(index) != 1){
      warning('Setting `Projection` to empty string!')
      Projection <- ''
      index <- -1
    }
  }

  projection_url <- ifelse(index %in% c(-1, 3), '', paste0('?projection=', projection_entries[index]))


  response <- httr::GET(url = paste0(Server, '/list/', projection_url),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8")))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()

}


#' Get mrv file by DTXSID or DTXCID
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param DTXCID The chemical identifier DTXCID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return XML file format for representing a mrv file.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull mrv file for BPA by dtxsid
#' bpa_mrv <- get_chemical_mrv(DTXSID = 'DTXSID7020182')
#' # Pull mrv file for BPA by dtxcid
#' bpa_mrv <- getchemical_mrv(DTXCID = 'DTXCID30182')

get_chemical_mrv <- function(DTXSID = NULL,
                             DTXCID = NULL,
                             API_key = NULL,
                             Server = chemical_api_server,
                             verbose = FALSE){
  if (is.null(DTXSID) & is.null(DTXCID))
    stop('Please input a DTXSID or DTXCID!')
  else if (!is.null(DTXSID) & !is.null(DTXCID))
    stop('Please input either a DTXSID or DTXCID, but not both!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.null(DTXSID)){
    response <- httr::GET(url = paste0(Server, '/file/mrv/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
  } else {
    response <- httr::GET(url = paste0(Server, '/file/mrv/search/by-dtxcid/', DTXCID),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  }
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    return(httr::content(response, encoding = "UTF-8"))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()

}

#' Get mol file by DTXSID or DTXCID
#'
#' @param DTXSID Chemical identifier DTXSID
#' @param DTXCID Chemical identifier DTXCID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A character string giving a mol file representation
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull mol file for BPA by dtxsid
#' bpa_mol <- get_chemical_mol(DTXSID = 'DTXSID7020182')
#' # Pull mol file for BPA by dtxcid
#' bpa_mol <- get_chemical_mol(DTXCID = 'DTXCID30182')

get_chemical_mol <- function(DTXSID = NULL,
                             DTXCID = NULL,
                             API_key = NULL,
                             Server = chemical_api_server,
                             verbose = FALSE){
  if (is.null(DTXSID) & is.null(DTXCID))
    stop('Please input a DTXSID or DTXCID!')
  else if (!is.null(DTXSID) & !is.null(DTXCID))
    stop('Please input either a DTXSID or DTXCID, but not both!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (!is.null(DTXSID)){
    response <- httr::GET(url = paste0(Server, '/file/mol/search/by-dtxsid/', DTXSID),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  } else {
    response <- httr::GET(url = paste0(Server, '/file/mol/search/by-dtxcid/', DTXCID),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  }
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    return(httr::content(response, encoding = "UTF-8"))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()

}


#' Get image file by DTXSID or DTXCID
#'
#' @param DTXSID Chemical identifier DTXSID
#' @param DTXCID Chemical identifier DTXCID
#' @param gsid DSSTox Generic Substance Identifier
#' @param SMILES Chemical identifier SMILES
#' @param format The image type, either "png" or "svg". If left blank, will
#'   default to "png".
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A Large array of three dimensions representing an image. For
#'   displaying this, one may use \code{png::writePNG()} or
#'   \code{countcolors::plotArrayAsImage()} among many such functions.
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull chemical image for BPA by dtxsid
#' bpa_image_matrix <- get_chemical_image(DTXSID = 'DTXSID7020182')
#' if (requireNamespace("countcolors", quietly = TRUE)){
#'   countcolors::plotArrayAsImage(bpa_image_matrix)
#' }
#' # Pull chemical image for BPA by dtxcid
#' bpa_image_matrix <- get_chemical_image(DTXCID = 'DTXCID30182')
#' if (requireNamespace("countcolors", quietly = TRUE)){
#'   countcolors::plotArrayAsImage(bpa_image_matrix)
#' }

get_chemical_image <- function(DTXSID = NULL,
                               DTXCID = NULL,
                               gsid = NULL,
                               SMILES = NULL,
                               format = "",
                               API_key = NULL,
                               Server = chemical_api_server,
                               verbose = FALSE){
  if (is.null(DTXSID) & is.null(DTXCID) & is.null(gsid) & is.null(SMILES))
    stop('Please input a DTXSID, DTXCID, gsid, or SMILES!')
  else if (length(which(!sapply(list(DTXSID, DTXCID, gsid, SMILES), is.null))) > 1)
    stop('Please input only one DTXSID, DTXCID, or SMILES, and not multiple!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  if (format == 'png'){
    image_type = "Image+Format=png"
  } else if (format == 'svg'){
    image_type = "Image+Format=svg"
  } else {
    image_type = ""
  }

  if (!is.null(DTXSID)){
    response <- httr::GET(url = paste0(Server, '/file/image/search/by-dtxsid/', DTXSID, '?', image_type),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  } else if (!is.null(DTXCID)) {
    response <- httr::GET(url = paste0(Server, '/file/image/search/by-dtxcid/', DTXCID, '?', image_type),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  } else if (!is.null(gsid)) {
    response <- httr::GET(url = paste0(Server, '/file/image/search/by-gsid/', gsid, '?', image_type),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  } else {
    response <- httr::GET(url = paste0(Server, '/file/image/generate?smiles=', prepare_word(SMILES), '&', image_type),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  }
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    return(httr::content(response, encoding = "UTF-8"))
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()

}

#' Get chemical synonym
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be given.
#'
#' @return A named list of synonym information for the input DTXSID
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Pull synonyms for BPA
#' bpa_synonym <- get_chemical_synonym(DTXSID = 'DTXSID7020182')

get_chemical_synonym <- function(DTXSID = NULL,
                                 API_key = NULL,
                                 Server = chemical_api_server,
                                 verbose = FALSE){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }


  response <- httr::GET(url = paste0(Server, '/synonym/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    parse_list <- jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8"))
    parse_length <- length(parse_list)
    if (parse_length){
      parse_indices <- which(sapply(1:parse_length, function(t){is.null(parse_list[[t]])}))
      if (length(parse_indices)){
        parse_list[parse_indices] <- NA_character_
      }
      for (i in 1:parse_length){
        if (length(parse_list[[i]]) > 1){
          parse_list[[i]] <- list(parse_list[[i]])
        }
      }
      }

    parse_dt <- data.table::data.table(tibble::as_tibble_row(parse_list))
    data.table::setcolorder(parse_dt, c('dtxsid', 'pcCode', 'valid', 'beilstein',
                                        'alternateCasrn', 'good', 'other',
                                        'deletedCasrn'))

    return(parse_dt)
  } else {
    if (verbose) {
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }

  return()
}

#' Chemical API Endpoint status
#'
#' @return Status of Chemical API Endpoints
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' status <- get_chemical_endpoint_status()
#' print(status)

get_chemical_endpoint_status <- function(){
  request <- httr::GET(url = paste0(chemical_api_server, "/health"))
  return(request$status_code)
}
