

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
          message(cond$message)
          return(NA)
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
          message(cond$message)
          return(NA)
        }
      )
      return(attempt)
    }
    )
    names(results) <- DTXCID
  }
  return(results)
}


#' Retrieve chemical information in batch search
#'
#' @param DTXSID A vector of chemical identifier DTXSIDs
#' @param type A vector of type used in get_chem_info(). This specifies whether
#'   to only grab predicted or experimental results. If not specified, it will
#'   grab all details. The allowable input values are "", predicted", or
#'   "experimental".
#' @param API_key The user-specific API key.
#'
#' @return A named list of data.frames containing chemical information for the
#'   chemicals with DTXSID matching the input parameter.
#' @export


get_chem_info_batch <- function(DTXSID = NULL,
                                type = '',
                                API_key = NULL){
  if (!is.null(DTXSID)){
    DTXSID <- unique(DTXSID)
    if (length(type) > 1){
      if(length(type) < length(DTXSID)){
        stop('Length of type must equal length of DTXSID!')
        type <- c(type, rep('', (length(DTXSID) - length(type))))
      } else if (length(type) > length(DTXSID)){
        warning('Truncating type to match length of DTXSID!',
                immediate. = TRUE)
        type <- type[1:(length(DTXSID))]
      }
    }

    results <- purrr::map2(.x = DTXSID, .y = type, function(d, t){
      attempt <- tryCatch(
        {
          get_chem_info(DTXSID = d,
                        type = t,
                        API_key = API_key)
        },
        error = function(cond){
          message('There was an error!')
          message(paste('DTXSID:', d))
          message(paste('type:', t))
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
    stop('Please input a value for DTXSID!')
  }
}
