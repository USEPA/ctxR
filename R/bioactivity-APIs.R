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
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
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

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
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
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    res <- tryCatch({jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8"))},
                    error = function(cond){
                      data.table::data.table()
                    }
    )

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
      columns <- which(names(res) %in% param_cols[col_index])
      # Handle case in which potentially nested columns are NA
      # If the column is NA, it will be a logical. Otherwise, the column will be
      # a different class.
      non_na <- sapply(columns, function(t){
        return(!is.logical((res[[t]])))
      })
      non_na <- which(non_na)
      # Only unnest columns that are not NA
      col_index <- intersect(col_index, non_na)
    }
    if (length(col_index) > 0){
     res <- tidyr::unnest_wider(data = res, col = param_cols[col_index])
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
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
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

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/data/summary/search/by-aeid/', AEID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
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

#' Get single concentration data
#'
#' @param AEID The assay endpoint identifier AEID
#' @param Projection The format and concentration data returned. Allowed values
#'   are 'single-conc' and 'ccd-single-conc'. The default format is
#'   'single-conc'.
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some "progress report" should be
#'   given.
#'
#' @returns A data.frame of single concentration screening data for requested
#'   ToxCast assay component endpoint ID (AEID).
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Get single conc data for AEID 3032 in different projections
#' aeid_3032 <- get_single_concentration(AEID = 3032)
#' aeid_ccd_3032 <- get_single_concentration(AEID = 3032,
#'                                           Projection = 'ccd-single-conc')
get_single_concentration <- function(AEID = NULL,
                                     Projection = 'single-conc',
                                     API_key = NULL,
                                     Server = bioactivity_api_server,
                                     verbose = FALSE){
  if (is.null(AEID))
    stop('Please input an AEID!')


  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  projection_entries <- c('single-conc',
                          'ccd-single-conc')
  index <- 1
  if (!is.character(Projection)){
    warning('Setting `Projection` to `single-conc`')
    Projection <- 'single-conc'
  } else {
    Projection <- tolower(Projection)
    index <- which(projection_entries %in% Projection)
    if (length(index) == 0){
      stop('Please input a correct value for `Projection`!')
    } else if (length(index) > 1){
      warning('Setting `Projection` to `single-conc`')
      Projection <- 'single-conc'
      index <- 1
    } else {
      if (length(Projection) > 1){
        message(paste0('Using `Projection` = ', projection_entries[index], '!'))
      }
      Projection <- projection_entries[index]
    }
  }

  projection_url <- paste0('?projection=', Projection)

  response <-  httr::GET(url = paste0(Server, '/assay/single-conc/search/by-aeid/', AEID, projection_url),
                         httr::add_headers(.headers = c(
                           'Content-Type' =  'application/json',
                           'x-api-key' = API_key)
                         )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    res <- jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8"))
    df_col_names <- names(res)[which(lapply(res, typeof) == 'list')]
    if (length(df_col_names)) {
      for (i in 1:length(df_col_names)){
        res <- res |> tidyr::unnest(df_col_names[[i]], keep_empty = TRUE)
      }
     }

    return(res)
  } else {
    if (verbose){
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()
}

#' Get assay summary data by gene symbol
#'
#' @param geneSymbol The gene symbol.
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some "progress report" should be given.
#'
#' @returns A data.frame of assay summary data for applicable assays for
#' requested offical gene symbol.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Retrieve summary data for gene symbol TUBA1A
#' summary_tuba1a <- get_assay_summary_by_gene(geneSymbol = 'TUBA1A')
#' summary_tuba1a
get_assay_summary_by_gene <- function(geneSymbol = NULL,
                                      API_key = NULL,
                                      Server = bioactivity_api_server,
                                      verbose = FALSE){
  if (is.null(geneSymbol))
    stop('Please input an geneSymbol!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <-  httr::GET(url = paste0(Server, '/assay/search/by-gene/', geneSymbol),
                         httr::add_headers(.headers = c(
                           'Content-Type' =  'application/json',
                           'x-api-key' = API_key)
                         )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    res <- jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8"))
    return(res)
  } else {
    if (verbose){
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()
}

#' Get AEID by assay component endpoint name
#'
#' @param endpoint The assay component endpoint name.
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some "progress report" should be given.
#'
#' @returns An integer corresponding to the AEID of the given assay endpoint.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Retrieve AEID for end point 'APR_HepG2_MicrotubuleCSK_1hr'
#' aeid <- get_aeid_by_endpoint(endpoint = 'APR_HepG2_MicrotubuleCSK_1hr')
#' aeid
get_aeid_by_endpoint <- function(endpoint = NULL,
                                      API_key = NULL,
                                      Server = bioactivity_api_server,
                                      verbose = FALSE){
  if (is.null(endpoint))
    stop('Please input an endpoint!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <-  httr::GET(url = paste0(Server, '/assay/search/by-endpoint/?endpoint=', endpoint),
                         httr::add_headers(.headers = c(
                           'Content-Type' =  'application/json',
                           'x-api-key' = API_key)
                         )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    res <- jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8"))
    return(res)
  } else {
    if (verbose){
      print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
    }
  }
  return()
}


#' Retrieve all assays
#'
#' @param Projection The format and assay data returned. Allowed values are
#'   'assay-all' and 'ccd-assay-list'. The default format is 'assay-all'.
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be
#'   given.
#'
#' @return A data.frame containing all the assays and associated information
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Retrieve all assays
#' assays <- get_all_assays()
get_all_assays <- function(Projection = 'assay-all',
                           API_key = NULL,
                           Server = bioactivity_api_server,
                           verbose = FALSE){

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  projection_entries <- c('assay-all',
                          'ccd-assay-list')
  index <- 1
  if (!is.character(Projection)){
    warning('Setting `Projection` to `assay-all`')
    Projection <- 'assay-all'
  } else {
    Projection <- tolower(Projection)
    index <- which(projection_entries %in% Projection)
    if (length(index) == 0){
      stop('Please input a correct value for `Projection`!')
    } else if (length(index) > 1){
      warning('Setting `Projection` to `assay-all`')
      Projection <- 'assay-all'
      index <- 1
    } else {
      if (length(Projection) > 1){
        message(paste0('Using `Projection` = ', projection_entries[index], '!'))
      }
      Projection <- projection_entries[index]
    }
  }

  projection_url <- paste0('?projection=', Projection)


  response <-  httr::GET(url = paste0(Server, '/assay/', projection_url),
                         httr::add_headers(.headers = c(
                           'Content-Type' =  'application/json',
                           'x-api-key' = API_key)
                         )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    res <- jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8"))
    #df_col_names <- intersect(c('gene', 'assayList', 'citations'), names(res))
    df_col_names <- names(res)[which(lapply(res, typeof) == 'list')]
    if (length(df_col_names)) {
      for (i in 1:length(df_col_names)){
        res <- res |> tidyr::unnest(df_col_names[[i]], keep_empty = TRUE, names_sep = '_')
      }

    #   res[df_col_names] <- lapply(res[df_col_names],
    #                                                      function(df) do.call('mapply', c(list, df,
    #                                                                                       SIMPLIFY = FALSE,
    #                                                                                       USE.NAMES = FALSE)))
    }

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
#' @param Projection The format and assay data returned. Allowed values are
#'   'ccd-assay-annotation', 'ccd-assay-gene', 'ccd-assay-citations',
#'   'ccd-assay-tcpl', 'ccd-assay-reagents', and 'assay-all'. The default format
#'   is 'assay-all'.
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be
#'   given.
#'
#' @return A data.table containing the annotated assays corresponding to the
#'   input AEID parameter
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Retrieve annotation for an assay
#' annotation <- get_annotation_by_aeid(AEID = 159)
get_annotation_by_aeid <- function(AEID = NULL,
                                   Projection = 'assay-all',
                                   API_key = NULL,
                                   Server = bioactivity_api_server,
                                   verbose = FALSE){
  if (is.null(AEID))
    stop('Please input an AEID!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  projection_entries <- c('assay-all',
                          'ccd-assay-annotation',
                          'ccd-assay-gene',
                          'ccd-assay-citations',
                          'ccd-assay-tcpl',
                          'ccd-assay-reagents')
  index <- 1
  if (!is.character(Projection)){
    warning('Setting `Projection` to `assay-all`')
    Projection <- 'assay-all'
  } else {
    Projection <- tolower(Projection)
    index <- which(projection_entries %in% Projection)
    if (length(index) == 0){
      stop('Please input a correct value for `Projection`!')
    } else if (length(index) > 1){
      warning('Setting `Projection` to `assay-all`')
      Projection <- 'assay-all'
      index <- 1
    } else {
      if (length(Projection) > 1){
        message(paste0('Using `Projection` = ', projection_entries[index], '!'))
      }
      Projection <- projection_entries[index]
    }
  }

  projection_url <- ifelse(index == 1, '', paste0('?projection=', Projection))

  response <- httr::GET(url = paste0(Server, '/assay/search/by-aeid/', AEID, projection_url),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    if (length(response$content) > 0){
      res <- jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8"))
      if (length(res) == 0){
        return(res)
      }

      # Note which columns have lists of data.frames
      df_col_names <- names(res)[which(lapply(res, typeof) == 'list')]

      # for (i in 1:length(res)){
      #   if (is.null(res[[i]])) res[[i]] <- NA # set any nulls to NA
      #   if (length(res[[i]]) > 1) {
      #     res[[i]] <- list(res[[i]]) # put lengths > 1 into a list to be just length 1, will unnest after
      #   }
      # }

      # Fix nested data.frames
      #df_col_names <- names(res)[which(lapply(res, typeof) == 'list')]
      if (length(df_col_names) & index != 4) {
        for (i in 1:length(df_col_names)){
          res <- res |> tidyr::unnest(df_col_names[[i]], keep_empty = TRUE, names_sep = '_')
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

#' Get total assay count
#'
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be
#'   given.
#'
#' @returns An integer indicating the total number of assays.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' assay_count <- get_total_assay_count()
#' print(assay_count)
get_total_assay_count <- function(API_key = NULL,
                                  Server = bioactivity_api_server,
                                  verbose = FALSE){

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  response <- httr::GET(url = paste0(Server, '/assay/count'),
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
      print('The request was successful but there is no information to return...')
    }
  }
  return()
}

#' Get list of chemical DTXSIDs for a given assay
#'
#' @param AEID The assay endpoint identifier AEID
#' @param Projection The format and DTXSID data returned. Allowed values are
#'   'dtxsidonly' and 'ccdassaydetails'. The default format
#'   is 'dtxsidonly'.
#' @param API_key The user-specific API key
#' @param Server The root address for the API endpoint
#' @param verbose A logical indicating if some “progress report” should be
#'   given.
#'
#' @returns A list of DTXSIDs or data.frame of assay information.
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' dtxsid_list <- get_chemicals_by_assay(AEID = 3032)
get_chemicals_by_assay <- function(AEID = NULL,
                                   Projection = 'dtxsidonly',
                                   API_key = NULL,
                                   Server = bioactivity_api_server,
                                   verbose = FALSE){
  if (is.null(AEID))
    stop('Please input an AEID!')

  API_key <- check_api_key(API_key = API_key, verbose = verbose)
  if (is.null(API_key) & verbose){
    warning('Missing API key. Please supply during function call or save using `register_ctx_api_key()`!')
  }

  projection_entries <- c('dtxsidonly',
                          'ccdassaydetails')
  index <- 1
  if (!is.character(Projection)){
    warning('Setting `Projection` to `dtxsidonly`')
    Projection <- 'dtxsidonly'
  } else {
    Projection <- tolower(Projection)
    index <- which(projection_entries %in% Projection)
    if (length(index) == 0){
      stop('Please input a correct value for `Projection`!')
    } else if (length(index) > 1){
      warning('Setting `Projection` to `dtxsidonly`')
      Projection <- 'dtxsidonly'
      index <- 1
    } else {
      if (length(Projection) > 1){
        message(paste0('Using `Projection` = ', projection_entries[index], '!'))
      }
      Projection <- projection_entries[index]
    }
  }

  projection_url <- paste0('?projection=', Projection)

  response <- httr::GET(url = paste0(Server, '/assay/chemicals/search/by-aeid/', AEID, projection_url),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
  if(response$status_code == 401){
    stop(httr::content(response)$detail)
  }
  if(response$status_code == 200){
    res <- jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8"))
    return(res)
  } else {
    if (verbose){
      print('The request was successful but there is no information to return...')
    }
  }
  return()
}



#' Bioactivity API Endpoint status
#'
#' @return Status of Bioactivity API Endpoints
#' @export
#'
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' status <- get_bioactivity_endpoint_status()
#' print(status)

get_bioactivity_endpoint_status <- function(){
  request <- httr::GET(url = paste0(bioactivity_api_server, "/health"))
  return(request$status_code)
}
