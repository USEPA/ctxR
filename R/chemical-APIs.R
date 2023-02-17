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
    return(jsonlite::fromJSON(content(response, as = 'text')))
  } else {
    print(response$status_code)
  }
 return()
}
