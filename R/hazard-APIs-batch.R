#' Get hazard data by DTXSID batch
#'
#' @param DTXSID A list of chemical identifier DTXSIDs
#' @param API_key The user-specific API key
#'
#' @return A named list of data.frames containing chemical (human and eco)
#'   hazard data for each input chemical.
#' @export


get_hazard_by_dtxsid_batch <- function(DTXSID = NULL,
                                       API_key = NULL){
  if (!is.null(DTXSID)){
    DTXSID <- unique(DTXSID)
    results <- lapply(DTXSID, function(t){
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

#' Get human hazard data by DTXSID batch
#'
#' @param DTXSID A list of chemical identifier DTXSIDs.
#' @param API_key The user-specific API key.
#'
#' @return A named lit of data.frames containing chemical human hazard data.
#' @export


get_human_hazard_by_dtxsid_batch <- function(DTXSID = NULL,
                                             API_key = NULL){
  if (!is.null(DTXSID)){
    DTXSID <- unique(DTXSID)
    results <- lapply(DTXSID, function(t){
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
