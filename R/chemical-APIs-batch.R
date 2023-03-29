

#' Retrieve chemical details from DTXSID of DTXCID in batch search
#'
#' @param DTXSID The chemical identifier DTXSID
#' @param DTXCID The chemical identifier DTXCID
#' @param API_key The user-specific API key
#'
#' @return A named list of data.tables containing chemical information for the
#'   chemicals with DTXSID or DTXCID matching the input parameter.
#' @export


get_chemical_details_batch <- function(DTXSID = NULL,
                                       DTXCID = NULL,
                                       API_key = NULL){
  if (!is.null(DTXSID)){
    DTXSID <- unique(DTXSID)
    print('Using DTXSID!')
    results <- lapply(DTXSID, function(t){
      attempt <- tryCatch(
        {
          get_chemical_details(DTXSID = t, API_key = API_key)
        },
        error = function(cond){
          message(t)
          message(cond)
          return()
        }
      )
      return(attempt)
    }
    )
    names(results) <- DTXSID
    }

  if (!is.null(DTXCID)){
    DTXCID <- unique(DTXCID)
    print('Using DTCSID!')
    results <- lapply(DTXCID, function(t){
      attempt <- tryCatch(
        {
          get_chemical_details(DTXCID = t, API_key = API_key)
        },
        error = function(cond){
          message(t)
          message(cond)
          return()
        }
      )
      return(attempt)
    }
    )
    names(results) <- DTXCID
  }
  return(results)
}
