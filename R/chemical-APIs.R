#' Retrieve chemical details from DTXSID
#'
#' @param DTXSID The chemical identifer DTXSID
#' @param API_key The user-specific API key
#'
#' @return A data.frame containing chemical information for the chemical with
#'   DTXSID matching the input parameter.
#' @export
#'
get_chemical_details_dtxsid <- function(DTXSID = NULL, API_key = NULL){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key))
    stop('Please input an API_key')

  response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/detail/search/by-dtxsid/', DTXSID),
                        httr::add_headers(.headers = c(
                          'Content-Type' =  'application/json',
                          'x-api-key' = API_key)
                        )
  )

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
#' @param API_key The user-specific API Key
#' @return A data.frame containing chemical information for the chemical with
#'   DTXSID matching the input parameter.
#' @export


get_chem_info <- function(DTXSID = NULL, API_key = NULL){
  if (is.null(DTXSID))
    stop('Please input a DTXSID!')
  else if (is.null(API_key))
    stop('Please input an API_key')

  response <- httr::GET(url = paste0('https://api-ccte.epa.gov/chemical/property/search/by-dtxsid/', DTXSID),
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
