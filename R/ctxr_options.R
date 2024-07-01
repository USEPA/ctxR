#' ctxR Options
#'
#' ctxR stores options as a named list in R's global options, i.e.
#' `getOption('ctxR')`. It currently stores two such options, one for CCTE
#' credentialing and one to suppress private API information in the URLs printed
#' to the screen when web queries are placed. For both of those, see
#' [register_ctx_api_key()].
#'
#' @param ... a named listing of options to set
#' @param option a specific option to query, e.g. `display_api_key`
#' @name ctxR_options
#' @returns
#'  * `set_ctxR_option()` does not have a return value but has the side effect
#'  of setting options used by other functions.
#'
#'  * `has_ctxR_option()` returns a Boolean.
#'
#'  * `has_ctxR_options()` returns a Boolean.
#' @seealso [register_ctx_api_key()]
#'
#'
#'






#' @rdname ctxR_options
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Set ctxR options
#' set_ctxR_option('display_api_key' = FALSE)
set_ctxR_option <- function(...) {

  # if there is no ctxR option create the list with the arguments and return
  if (!has_ctxR_options()) {
    options('ctxR' = list(...))
    return(invisible())
  }

  # otherwise, go through arguments sequentially and add/update
  # them in the list ctxR options
  ctxR <- getOption('ctxR')
  arg_list <- lapply(as.list(match.call())[-1], eval, envir = parent.frame())
  for (k in seq_along(arg_list)) {
    if (names(arg_list)[k] %in% names(ctxR)) {
      ctxR[names(arg_list)[k]] <- arg_list[k]
    } else {
      ctxR <- c(ctxR, arg_list[k])
    }
  }

  # set new ctxR
  options('ctxR' = ctxR)

  # return
  invisible()
}



#' @rdname ctxR_options
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Check if there are options registered to 'ctxR'
#' has_ctxR_options()

has_ctxR_options <- function() {
  !is.null(getOption('ctxR'))
}

#' @rdname ctxR_options
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Check if a specific option is registered for 'ctxR'
#' has_ctxR_option('display_api_key')

has_ctxR_option <- function(option) {

  if (has_ctxR_options()){
    option %in% names(getOption('ctxR'))
  } else {
    FALSE
  }
}
