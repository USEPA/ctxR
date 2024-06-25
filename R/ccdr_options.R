#' ccdr Options
#'
#' ccdr stores options as a named list in R's global options, i.e.
#' `getOption('ccdr')`. It currently stores two such options, one for CCTE
#' credentialing and one to supress private API information in the URLs printed
#' to the screen when web queries are placed. For both of those, see
#' [register_ccdr()].
#'
#' @param ... a named listing of options to set
#' @param option a specific option to query, e.g. `display_api_key`
#' @name ccdr_options
#' @returns
#'  * `set_ccdr_option()` does not have a return value but has the side effect
#'  of setting options used by other functions.
#'
#'  * `has_ccdr_option()` returns a Boolean.
#'
#'  * `has_ccdr_options()` returns a Boolean.
#' @seealso [register_ccdr()]
#'
#'
#'






#' @rdname ccdr_options
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Set ccdr options
#' set_ccdr_option('display_api_key' = FALSE)
set_ccdr_option <- function(...) {

  # if there is no ccdr option create the list with the arguments and return
  if (!has_ccdr_options()) {
    options('ccdr' = list(...))
    return(invisible())
  }

  # otherwise, go through arguments sequentially and add/update
  # them in the list ccdr options
  ccdr <- getOption('ccdr')
  arg_list <- lapply(as.list(match.call())[-1], eval, envir = parent.frame())
  for (k in seq_along(arg_list)) {
    if (names(arg_list)[k] %in% names(ccdr)) {
      ccdr[names(arg_list)[k]] <- arg_list[k]
    } else {
      ccdr <- c(ccdr, arg_list[k])
    }
  }

  # set new ccdr
  options('ccdr' = ccdr)

  # return
  invisible()
}



#' @rdname ccdr_options
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Check if there are options registered to 'ccdr'
#' has_ccdr_options()

has_ccdr_options <- function() {
  !is.null(getOption('ccdr'))
}

#' @rdname ccdr_options
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Check if a specific option is registered for 'ccdr'
#' has_ccdr_option('display_api_key')

has_ccdr_option <- function(option) {

  if (has_ccdr_options()){
    option %in% names(getOption('ccdr'))
  } else {
    FALSE
  }
}
