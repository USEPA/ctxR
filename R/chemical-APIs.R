#' Retrieve chemical details from DTXSID of DTXCID
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param DTXCID The chemical identifier DTXCID
#' @param API_key The user-specific API key
#'
#' @return A data.frame containing chemical information for the chemical with
#'   DTXSID matching the input parameter.
#' @export
#'
get_chemical_details <- function(DTXSID = NULL,
                                        DTXCID = NULL,
                                        API_key = NULL){
  if (is.null(DTXSID) & is.null(DTXCID))
    stop('Please input a DTXSID or DTXCID!')
  else if (!is.null(DTXSID) & !is.null(DTXCID))
    stop('Please input either a DTXSID or DTXCID, but not both!')
  else if (is.null(API_key))
    stop('Please input an API_key')

  if (!is.null(DTXSID)){
    response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/detail/search/by-dtxsid/', DTXSID),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  } else {
    response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/detail/search/by-dtxcid/', DTXCID),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  }


  if(response$status_code == 200){
    data_list <- jsonlite::fromJSON(httr::content(response, as = 'text')) #Parse to list
    missing_names <- which(sapply(data_list, is.null)) #Determine missing values
    df <- t(data.frame(unlist(data_list), row.names = names(data_list)[-missing_names])) #Convert present values to data.frame with one row
    rownames(df) <- NULL #Delete row name
    missing_cols <- t(data.frame(rep(NA_real_, length(missing_names)), row.names = names(data_list)[missing_names])) #Generate column of missing values
    df2 <- cbind(df, missing_cols) #Combine both sets of columns
    dt <- data.table::setcolorder(data.table::data.table(df2), match(names(data_list), dimnames(df2)[[2]])) #Reorder columns
    return(dt)
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
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
#' @return A data.frame containing chemical information for the chemical with
#'   DTXSID matching the input parameter.
#' @export


get_chem_info <- function(DTXSID = NULL,
                          type = c("", "predicted", "experimental"),
                          API_key = NULL){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key))
    stop('Please input an API_key')

  type <- match.arg(type)

  if (type == '') {
    response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/property/search/by-dtxsid/', DTXSID),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  } else {
    response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/property/search/by-dtxsid/', DTXSID,'?type=', type),
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



  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
 return()
}

#' Get fate by DTXSID
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#'
#' @return @return A data.frame containing chemical information for the chemical with
#'   DTXSID matching the input parameter.
#' @export


get_fate_by_dtxsid <- function(DTXSID = NULL,
                               API_key = NULL){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key))
    stop('Please input an API_key')

  response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/fate/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()

}


#' Chemical starts with
#'
#' @param word A character string of a chemical name or portion of a chemical
#'   name
#' @param API_key The user-specific API key
#'
#' @return A data.frame of chemicals and related values matching the query
#'   parameters
#' @export


chemical_starts_with <- function(word = NULL,
                           API_key = NULL){
  if (is.null(word) || !is.character(word)){
    stop('Please input a character value for word!')
  } else if (is.null(API_key)){
    stop('Please input an API_key!')
  }

  word <- prepare_word(word)

  response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/search/start-with/', word),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()



}



#' Chemical equal
#'
#' @param word A character string of a chemical name or portion of a chemical
#'   name
#' @param API_key The user-specific API key
#'
#' @return A data.frame of chemicals and related values matching the query
#'   parameters
#' @export


chemical_equal <- function(word = NULL,
                           API_key = NULL){
  if (is.null(word) || !is.character(word)){
    stop('Please input a character value for word!')
  } else if (is.null(API_key)){
    stop('Please input an API_key!')
  }

  word <- prepare_word(word)

  response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/search/equal/', word),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()



}





#' Chemical contains
#'
#' @param word A character string of a chemical name or portion of a chemical
#'   name
#' @param API_key The user-specific API key
#'
#' @return A data.frame of chemicals and related values matching the query
#'   parameters
#' @export


chemical_contains <- function(word = NULL,
                              API_key = NULL){
  if (is.null(word) || !is.character(word)){
    stop('Please input a character value for word!')
  } else if (is.null(API_key)){
    stop('Please input an API_key!')
  }

  word <- prepare_word(word)

  response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/search/contain/', word),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()



}

#' Prepare url helper function
#'
#' @param word A character string
#' @return A character string that is ready for use in http request


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
#'
#' @return A list of DTXSIDs with msready mass falling within the given range.
#' @export


get_msready_by_mass <- function(start = NULL,
                                end = NULL,
                                API_key = NULL){
  if(is.null(start) || is.null(end) || !is.numeric(start) || !is.numeric(end)){
    stop('Please input a numeric value for both start and end!')
  } else if (is.null(API_key)){
    stop('Please input an API_key!')
  }

  if (start < 0 || end < 0){
    stop('Both start and end must be non-negative!')
  }

  if (start > end){
    warning('Swapping values for start and end!')
    temp <- end
    end <- start
    start <- temp
  }

  response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/msready/search/by-mass/', start, '/', end),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()

}



#' Get msready by formula
#'
#' @param formula A string denoting the input chemical formula
#' @param API_key The user-specific API key
#'
#' @return A character list of DTXSIDs with chemical formulas matching the
#'   search criteria
#' @export


get_msready_by_formula <- function(formula = NULL,
                                   API_key = NULL){
  if(is.null(formula)){
    stop("Please input a non-null value for formula!")
  } else if (!is.character(formula)){
    stop("Please input a character string for the formula parameter!")
  } else if (is.null(API_key)){
    stop('Please input an API_key!')
  }

  response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/msready/search/by-formula/', formula),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()


}


#' Get msready by DTXCID
#'
#' @param DTXCID The chemical identifier DTXCID
#' @param API_key The user-specific API key
#'
#' @return A character list of DTXSIDs with DTXCIDs matching the
#'   search criteria
#' @export


get_msready_by_dtxcid <- function(DTXCID = NULL,
                                   API_key = NULL){
  if(is.null(DTXCID)){
    stop("Please input a non-null value for DTXCID!")
  } else if (!is.character(DTXCID)){
    stop("Please input a character string for the DTXCID parameter!")
  } else if (is.null(API_key)){
    stop('Please input an API_key!')
  }

  response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/msready/search/by-dtxcid/', DTXCID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()


}



#' Get chemical lists by type
#'
#' @param type The type of list. This is a case sensitive parameter and returns
#'   lists only for values of "federal", "international", "state", and "other".
#' @param API_key The user-specified API key.
#'
#' @return A data.frame containing information about lists that meet the search
#'   criteria.
#' @export


get_chemical_lists_by_type <- function(type = NULL,
                                      API_key = NULL){
  if (is.null(type))
    stop('Please input list_name!')
  else if (is.null(API_key))
    stop('Please input an API_key!')

  response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/list/search/by-type/', type),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()


}


#' Get chemical list by name
#'
#' @param list_name The name of the list of chemicals
#' @param API_key The user-specific API key
#'
#' @return A data.frame containing information about the chemical list. Note,
#'   this is not the chemical list itself. To access the chemicals in the list,
#'   use \code{\link{get_chemicals_in_list}}.
#' @seealso \code{\link{get_chemicals_in_list}}
#' @export


get_public_chemical_list_by_name <- function(list_name = NULL,
                                             API_key = NULL){
  if (is.null(list_name))
    stop('Please input list_name!')
  else if (is.null(API_key))
    stop('Please input an API_key!')



  response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/list/search/by-name/', list_name),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 200){
    return(data.frame(jsonlite::fromJSON(httr::content(response, as = 'text'))))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()

}

#' Get chemical lists containing given chemical
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param API_key The user-specific API key
#'
#' @return A list of names of chemical lists that contain the given chemical
#' @export


get_lists_containing_chemical <- function(DTXSID = NULL,
                                          API_key = NULL){
  if (is.null(DTXSID))
    stop('Please input list_name!')
  else if (is.null(API_key))
    stop('Please input an API_key!')

  response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/list/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()

}

#' Get chemicals in a given chemical list
#'
#' @param list_name The name of the list of chemicals
#' @param API_key The user-specific API key
#'
#' @return A data.frame of the chemical list
#' @export


get_chemicals_in_list <- function(list_name = NULL,
                                  API_key = NULL){
  if (is.null(list_name))
    stop('Please input list_name!')
  else if (is.null(API_key))
    stop('Please input an API_key!')



  response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/list/chemicals/search/by-listname/', list_name),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()


}

#' Get all public chemical lists
#'
#' @param API_key The user-specific api key
#'
#' @return A data.frame containing information on all public chemical lists
#'   available from the CCTE chemical api.
#' @export


get_all_public_chemical_lists <- function(API_key = NULL){
  if (is.null(API_key)){
    stop('Please input an API_key!')
  }

  response <- httr::GET(url = 'https://api-ccte.epa.gov/chemical/list/',
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 200){
    return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()

}


#' Get mrv file by DTXSID or DTXCID
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param DTXCID The chemical identifier DTXCID
#' @param API_key The user-specific API key
#'
#' @return XML file format for representing a mrv file.
#' @export


get_chemical_mrv <- function(DTXSID = NULL,
                             DTXCID = NULL,
                             API_key = NULL){
  if (is.null(DTXSID) & is.null(DTXCID))
    stop('Please input a DTXSID or DTXCID!')
  else if (!is.null(DTXSID) & !is.null(DTXCID))
    stop('Please input either a DTXSID or DTXCID, but not both!')
  else if (is.null(API_key))
    stop('Please input an API_key!')

  if (!is.null(DTXSID)){
    response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/file/mrv/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )
  } else {
    response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/file/mrv/search/by-dtxcid/', DTXCID),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  }

  if(response$status_code == 200){
    return(httr::content(response))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()

}

#' Get mol file by DTXSID or DTXCID
#'
#' @param DTXSID Chemical identifier DTXSID
#' @param DTXCID Chemical identifier DTXCID
#' @param API_key The user-specific API key
#'
#' @return A character string giving a mol file representation
#' @export


get_chemical_mol <- function(DTXSID = NULL,
                             DTXCID = NULL,
                             API_key = NULL){
  if (is.null(DTXSID) & is.null(DTXCID))
    stop('Please input a DTXSID or DTXCID!')
  else if (!is.null(DTXSID) & !is.null(DTXCID))
    stop('Please input either a DTXSID or DTXCID, but not both!')
  else if (is.null(API_key))
    stop('Please input an API_key!')

  if (!is.null(DTXSID)){
    response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/file/mol/search/by-dtxsid/', DTXSID),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  } else {
    response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/file/mol/search/by-dtxcid', DTXCID),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  }

  if(response$status_code == 200){
    return(httr::content(response))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()

}


#' Get image file by DTXSID or DTXCID
#'
#' @param DTXSID Chemical identifier DTXSID
#' @param DTXCID Chemical identifier DTXCID
#' @param format The image type, either "png" or "svg". If left blank, will
#'   default to "png".
#' @param API_key The user-specific API key
#'
#' @return A Large array of three dimensions representing an image. For
#'   displaying this, one may use \code{png::writePNG()} or
#'   \code{countcolors::plotArrayAsImage()} among many such functions.
#' @export


get_chemical_image <- function(DTXSID = NULL,
                             DTXCID = NULL,
                             format = "",
                             API_key = NULL){
  if (is.null(DTXSID) & is.null(DTXCID))
    stop('Please input a DTXSID or DTXCID!')
  else if (!is.null(DTXSID) & !is.null(DTXCID))
    stop('Please input either a DTXSID or DTXCID, but not both!')
  else if (is.null(API_key))
    stop('Please input an API_key!')
  if (format == 'png'){
    image_type = "?format=png"
  } else if (format == 'svg'){
    image_type = "?format=svg"
  } else {
    image_type = ""
  }

  if (!is.null(DTXSID)){
    response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/file/image/search/by-dtxsid/', DTXSID, image_type),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  } else {
    response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/file/image/search/by-dtxcid/', DTXCID, image_type),
                          httr::add_headers(.headers = c(
                            'Content-Type' =  'application/json',
                            'x-api-key' = API_key)
                          )
    )
  }

  if(response$status_code == 200){
    return(httr::content(response))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()

}



