

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
    } else if (!is.null(DTXCID)){
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
    } else {
    stop('Please input a list of DTXSIDs or DTXCIDs!')
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
        warning('Length of type must equal length of DTXSID!')
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

#' Retrieve chemical fate data in batch search
#'
#' @param DTXSID A vector of chemicals identifier DTXSIDs
#' @param API_key The user-specific API key
#'
#' @return A named list of data.frames containing chemical fate information for
#'   the chemicals with DTXSID matching the input parameter.
#' @export


get_fate_by_dtxsid_batch <- function(DTXSID = NULL,
                                     API_key = NULL){
  if (!is.null(DTXSID)){
    DTXSID <- unique(DTXSID)
    results <- lapply(DTXSID, function(t){
      attempt <- tryCatch(
        {
          get_fate_by_dtxsid(DTXSID = t, API_key = API_key)
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

#' Chemical starts with batch search
#'
#' @param word_list A list of character strings of chemical names or portion of
#'   chemical names
#' @param API_key User-specific API key
#'
#' @return A named list of data.frames of chemicals and related values matching
#'   the query parameters
#' @export


chemical_starts_with_batch <- function(word_list = NULL,
                                       API_key = NULL){
  if (!is.null(word_list)){
    word_list <- unique(word_list)
    results <- lapply(word_list, function(t){
      attempt <- tryCatch(
        {
          chemical_starts_with(word = t, API_key = API_key)
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
    names(results) <- word_list
    return(results)
  } else {
    stop('Please input a list of chemical names!')
  }
}

#' Chemical equal batch search
#'
#' @param word_list A list of character strings of chemical names or portion of
#'   chemical names
#' @param API_key User-specific API key
#'
#' @return A named list of data.frames of chemicals and related values matching
#'   the query parameters
#' @export


chemical_equal_batch <- function(word_list = NULL,
                                 API_key = NULL){
  if (!is.null(word_list)){
    word_list <- unique(word_list)
    results <- lapply(word_list, function(t){
      attempt <- tryCatch(
        {
          chemical_equal(word = t, API_key = API_key)
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
    names(results) <- word_list
    return(results)
  } else {
    stop('Please input a list of chemical names!')
  }
}

#' Chemical contains batch search
#'
#' @param wordlist A list of character strings of chemical names or portion of
#'   chemical names
#' @param API_key User-specific API key
#'
#' @return A named list of data.frames of chemicals and related values matching
#'   the query parameters
#' @export


chemical_contains_batch <- function(wordlist = NULL,
                                    API_key = NULL){
  if (!is.null(word_list)){
    word_list <- unique(word_list)
    results <- lapply(word_list, function(t){
      attempt <- tryCatch(
        {
          chemical_equal(word = t, API_key = API_key)
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
    names(results) <- word_list
    return(results)
  } else {
    stop('Please input a list of chemical names!')
  }
}

#' Get ms ready by mass batch search
#'
#' @param start_list A numeric list of starting values for mass range
#' @param end_list A numeric list of ending values for mass range
#' @param API_key The user-specific API key
#'
#' @return A named list of character lists with DTXSIDs with msready masses
#'   falling within the given ranges.
#' @export
#'
#'
get_msready_by_mass_batch <- function(start_list = NULL,
                                       end_list = NULL,
                                       API_key = NULL){
  if(is.null(start_list) || is.null(end_list)){
    stop('Please input a list for both start_list and end_list!')
  } else if (length(start_list) != length(end_list)) {
    stop('Mismatch in length of list!')
  } else if (!all(sapply(c(start_list, end_list), is.numeric))) {
    stop('Only numeric values allowed in each list!')
  }

  # Sort min/max from lists to avoid errors
  start_list_ <- pmin(start_list, end_list)
  end_list_ <- pmax(start_list, end_list)

  results <- purrr::map2(.x = start_list_, .y = end_list_, function(d, t){
    attempt <- tryCatch(
      {
        get_msready_by_mass(start = d,
                            end = t,
                            API_key = API_key)
      },
      error = function(cond){
        message('There was an error!')
        message(paste('Start:', d))
        message(paste('End:', t))
        message(cond$message)
        return(NA)
      }
    )
    return(attempt)
  }
  )
  names(results) <- paste0('(Start, End) = (', start_list_, ', ', end_list_, ')')
  return(results)
}

#' Get msready by formula batch search
#'
#' @param formula_list A list of strings denoting the input chemicals formulas
#' @param API_key The user-specific API key
#'
#' @return A named list of character lists of DTXSIDs with chemical formulas
#'   matching the search criteria
#' @export


get_msready_by_formula_batch <- function(formula_list = NULL,
                                          API_key = NULL){
  if (!is.null(formula_list)){
    word_list <- unique(word_list)
    results <- lapply(word_list, function(t){
      attempt <- tryCatch(
        {
          get_msready_by_formula(formula = t, API_key = API_key)
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
    names(results) <- word_list
    return(results)
  } else {
    stop('Please input a list of chemical formulas!')
  }
}

#' Get msready by DTXCID batch search
#'
#' @param DXTCID A list of chemical identifer DTXCIDs
#' @param API_key A user-specific API key
#'
#' @return A named list of character lists of DTXSIDs with DTXCIDs matching the
#'   search criteria
#' @export


get_msready_by_dtxcid_batch <- function(DXTCID = NULL,
                                        API_key = NULL){
  if(!is.null(DTXCID)){
    DTXCID <- unique(DTXCID)
    results <- lapply(DTXCID, function(t){
      attempt <- tryCatch(
        {
          get_msready_by_dtxcid(DTXCID = t, API_key = API_key)
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
    return(results)
  } else {
    stop('Please input a list of DTXCIDs!')
  }
}

#' Get chemical lists by type batch search
#'
#' @param type_list A list of list types. This is a case sensitive parameter and returns
#'   lists only for values of "federal", "international", "state", and "other".
#' @param API_key The user-specified API key.
#'
#' @return A named list of data.frames containing information about lists that meet the search
#'   criteria.
#' @export


get_chemical_lists_by_type_batch <- function(type_list = NULL,
                                             API_key = NULL){
  if (!is.null(type_list)){
    type_list <- unique(type_list)
    results <- lapply(type_list, function(t){
      attempt <- tryCatch(
        {
          get_chemical_lists_by_type(type = t, API_key = API_key)
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
    names(results) <- type_list
    return(results)
  } else {
    stop('Please input a list of list types!')
  }
}

#' Get chemical list by name batch
#'
#' @param name_list A list of chemical list names.
#' @param API_key The user-specific API key.
#'
#' @return A named list of data.frames containing information about the chemical lists. Note,
#'   these are not the chemical lists themselves. To access the chemicals in a given list,
#'   use \code{\link{get_chemicals_in_list}}.
#' @seealso \code{\link{get_chemicals_in_list}}
#' @export
#'
#'
get_public_chemical_list_by_name_batch <- function(name_list = NULL,
                                                   API_key = NULL){
  if (!is.null(name_list)){
    name_list <- unique(name_list)
    results <- lapply(name_list, function(t){
      attempt <- tryCatch(
        {
          get_public_chemical_list_by_name(list_name = t,
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
    names(results) <- name_list
    return(results)
  } else {
    stop('Please input a list of list names!')
  }
}

#' Get chemical lists containing given chemical batch
#'
#' @param chemical_list A list of the chemical identifier DTXSIDs.
#' @param API_key The user-specific API key.
#'
#' @return A named list of chemical lists that contain the given chemicals.
#' @export


get_lists_containing_chemical_batch <- function(chemical_list = NULL,
                                                API_key = NULL){
  if (!is.null(chemical_list)){
    chemical_list <- unique(chemical_list)
    results <- lapply(chemical_list, function(t){
      attempt <- tryCatch(
        {
          get_lists_containing_chemical(DTXSID = t,
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
    names(results) <- chemical_list
    return(results)
  } else {
    stop('Please input a list of DTXSIDs!')
  }
}

#' Get chemicals in a given chemical list batch
#'
#' @param list_names A list of names of chemical lists.
#' @param API_key The user-specific API key.
#'
#' @return A named list of data.frames each containing chemicals in the
#'   corresponding chemical lists.
#' @export


get_chemicals_in_list_batch <- function(list_names = NULL,
                                        API_key = NULL){
  if (!is.null(list_names)){
    list_names <- unique(list_names)
    results <- lapply(list_names, function(t){
      attempt <- tryCatch(
        {
          get_chemicals_in_list(list_name = t,
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
    names(results) <- list_names
    return(results)
  } else {
    stop('Please input a list of names of chemical lists!')
  }
}

#' Ger mrv file by DTXSID or DTXCID batch
#'
#' @param DTXSID A list of the chemical identifier DTXSIDs.
#' @param DTXCID A list of the chemical identifier DTXCIDs.
#' @param API_key The user-specific API key.
#'
#' @return A named list of XML file format for representing a mrv file for each
#'   chemicals.
#' @export


get_chemical_mrv_batch <- function(DTXSID = NULL,
                                   DTXCID = NULL,
                                   API_key = NULL){
  if (!is.null(DTXSID)){
    DTXSID <- unique(DTXSID)
    print('Using DTXSID!')
    results <- lapply(DTXSID, function(t){
      attempt <- tryCatch(
        {
          get_chemical_mrv(DTXSID = t, API_key = API_key)
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
  } else if (!is.null(DTXCID)){
    DTXCID <- unique(DTXCID)
    print('Using DTXCID!')
    results <- lapply(DTXCID, function(t){
      attempt <- tryCatch(
        {
          get_chemical_mrv(DTXCID = t, API_key = API_key)
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
    return(results)
  } else {
    stop('Please input a list of DTXSIDs or DTXCIDs!')
  }
}
