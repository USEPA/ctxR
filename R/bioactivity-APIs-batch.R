#' Retrieve bioactivity data from DTXSID or AEID batch
#'
#' @param DTXSID A list of chemical identifier DTXSIDs.
#' @param AEID A list of assay endpoint identifiers AEIDs.
#' @param API_key The user-specific API key.
#'
#' @return A named list of data.frames containing bioactivity information for the chemicals with
#'   DTXSID or assays with AEID matching the input parameter.
#' @export


get_bioactivity_details_batch <- function(DTXSID = NULL,
                                          AEID = NULL,
                                          API_key = NULL){
  if (is.null(API_key) || !is.character(API_key)){
    stop('Please input a character string containing a valid API key!')
  }
  if (!is.null(DTXSID)){
    DTXSID <- unique(DTXSID)
    print('Using DTXSID!')
    results <- lapply(DTXSID, function(d){
      attempt <- tryCatch(
        {
          get_bioactivity_details(DTXSID = d,
                             API_key = API_key)
        },
        error = function(cond){
          message(d)
          message(cond$message)
          return(NA)
        }
      )
      return(attempt)
    }
    )
    names(results) <- DTXSID
    return(results)
  } else if (!is.null(AEID)){
    AEID <- unique(AEID)
    print('Using AEID!')
    results <- lapply(AEID, function(a){
      attempt <- tryCatch(
        {
          get_bioactivity_details(AEID = a,
                                  API_key = API_key)
        },
        error = function(cond){
          message(a)
          message(cond$message)
          return(NA)
        }
      )
      return(attempt)
    }
    )
    names(results) <- AEID
    return(results)
  } else {
    stop('Please input a list of DTXSIDs or AEIDs!')
  }
}
