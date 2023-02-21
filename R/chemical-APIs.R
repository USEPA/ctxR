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


#' Get mrv file by DTXSID
#'
#' @param DTXSID The chemical identifer DTXSID
#' @param API_key The user-specific API key
#'
#' @return XML file format for representing a mrv file.
#' @export


get_chemical_mrv_by_dtxsid <- function(DTXSID = NULL,
                                       API_key = NULL){
  if (is.null(DTXSID))
    stop('Please input list_name!')
  else if (is.null(API_key))
    stop('Please input an API_key!')

  response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/file/mrv/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

  if(response$status_code == 200){
    return(httr::content(response))
    #return(jsonlite::fromJSON(httr::content(response, as = 'text')))
  } else {
    print(paste0('The request was unsuccessful, returning an error of ', response$status_code, '!'))
  }
  return()







}
