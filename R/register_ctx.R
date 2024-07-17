#' Register CTX API Key for ctxR
#'
#' This page contains documentation tools related to enabling CTX API services
#' in R.
#'
#' To obtain an API key and enable services, go to
#' \url{https://www.epa.gov/comptox-tools/computational-toxicology-and-exposure-apis}.
#' This documentation shows you how to obtain an API key to allow access to the
#' CTX APIs.
#'

#' To tell ctxR about your API key, use [register_ctx_api_key()], e.g.
#' `register_ctx_api_key(key = 'grbwigbwoginrowgbwibgdibdvinrginiwgo')` (that's a fake
#' key). This will set your API key for the current session, but if you restart
#' R, you'll need to do it again. You can set it permanently by setting `write =
#' TRUE`m see the examples. If you set it permanently it will be stored in a
#' local file, and that will be accessed by ctxR persistently across
#' sessions.
#'
#' Users should be aware that the API key, a string of garbled
#' characters/numbers/symbols, is a PRIVATE key - it uniquely identifies and
#' authenticates you to CTX's services. If anyone gets your API key, they can
#' use it to masquerade as you to CTX. To mitigate against users inadvertently
#' sharing their keys, by default ctxR never displays a user's key in messages
#' displayed to the console.
#'
#' Users should be aware that ctxR has no mechanism with which to safeguard the
#' private key once registered with R. That is to say, once you register your
#' API key, any R function will have access to it. As a consequence, ctxR will
#' not know if another function, potentially from a compromised package,
#' accesses the key and uploads it to a third party. For this reason, when using
#' ctxR we recommend a heightened sense of security and self-awareness: only use
#' trusted packages, do not save the API keys in script files, etc.
#'
#' @param key an API key
#' @param write if TRUE, stores the secrets provided in the .Renviron file
#' @param ... a dumped formal argument to the generic print method
#' @returns
#'  * `showing_key` returns a Boolean.
#'
#'  * `ctxR_show_api_key()` has no return value but has the side effect of
#'  changing the display settings of the API key.
#'
#'  * `ctxR_hide_api_key()` has no return value but has the side effect of
#'  changing the display settings of the API key.
#'
#'  * `register_ctx_api_key()` has no return value but has the side effect of
#'  storing the API key.
#'
#'  * `print.ctx_credentials()` has no return value and is an S3 method for
#'  printing the `ctx_credentials` class.
#'
#'  * `ctx_key()` returns a string, either the stored API key or
#'   \code{NA_character_}.
#'
#'  * `has_ctx_key()` returns a Boolean.
#' @name register_ctx_api_key



#' @rdname register_ctx_api_key
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Check if API key is showing
#' showing_key()

showing_key <- function() {

  if (has_ctxR_option('display_api_key')) {
    getOption('ctxR')$display_api_key
  } else {
    FALSE
  }
}



#' @rdname register_ctx_api_key
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Toggle API key to display
#' ctxR_show_api_key()

ctxR_show_api_key <- function() {
  set_ctxR_option('display_api_key' = TRUE)
  cli::cli_alert_warning('ctxR will now display PRIVATE API keys in the console.')
  invisible()
}



#' @rdname register_ctx_api_key
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Toggle API key to be hidden
#' ctxR_hide_api_key()

ctxR_hide_api_key <- function() {
  set_ctxR_option('display_api_key' = FALSE)
  cli::cli_alert_info('ctxR will now suppress private API keys in the console.')
  invisible()
}



#' @rdname register_ctx_api_key
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Register key for this session
#' register_ctx_api_key(key = 'YOUR API KEY')
#' # Register key over sessions
#' register_ctx_api_key(key = 'YOUR API KEY', write = TRUE)

register_ctx_api_key <- function(key, write = FALSE) {

  # allow register_ctx_api_key to work when ctxR not loaded
  #
  # FILL in details
  #

  # get current options
  options <- getOption('ctxR')


  # deal with API key
  if (!missing(key) && write) {

    if (R.version.string < "4.0.0"){
      warning("This function relies on R version 4.0.0 or later. Cannot store API key over multiple sessions.")
    } else {
      ctxRdir <- tools::R_user_dir("ctxR")
      if (!dir.exists(ctxRdir)){
        dir.create(ctxRdir, recursive = TRUE)
      }
      fname <- file.path(ctxRdir, "api.dcf")
      if (file.exists(fname)) {
        warning("Existing file found, so overwriting")
      }
      con <- file(fname)
      cat("key:", key, file = con)
      close(con)
    }
#
#     # grab .Renviron file path
#     environ_file <- file.path(Sys.getenv('HOME'), '.Renviron')
#
#     # create .Renviron file it does not exist
#     if (!file.exists(file.path(Sys.getenv('HOME'), '.Renviron'))) {
#       cli::cli_alert_info('Creating file {environ_file}')
#       file.create(environ_file)
#     }
#
#     # read in lines
#     environ_lines <- readLines(environ_file)
#
#     # if no key present, add; otherwise replace old one
#     if (!any(stringr::str_detect(environ_lines, 'CCTE_API_KEY='))) {
#
#       cli::cli_alert_info('Adding key to {environ_file}')
#       environ_lines <- c(environ_lines, glue::glue('CCTE_API_KEY={key}'))
#       writeLines(environ_lines, environ_file)
#
#     } else {
#
#       key_line_index <- which(stringr::str_detect(environ_lines, 'CCTE_API_KEY='))
#       old_key <- stringr::str_extract(environ_lines[key_line_index], '(?<=CCTE_API_KEY=)\\w+')
#       cli::cli_alert_info('Replacing old key ({old_key}) with new key in {environ_file}')
#       environ_lines[key_line_index] <- glue::glue('CCTE_API_KEY={key}')
#       writeLines(environ_lines, environ_file)
#
#     }
#
    # set key in current session
    .pkgenv[["api"]] <- key
    #Sys.setenv('CTX_API_KEY' = key)

  } else if (!missing(key) && !write) {

    # set key in current session
    .pkgenv[["api"]] <- key
    #Sys.setenv('CTX_API_KEY' = key)

  }

  # class
  class(options) <- 'ctxR_credentials'

  # set new options
  options(ctxR = options)

  # return
  invisible(NULL)
}





#' @rdname register_ctx_api_key
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Print function for ctx_credentials class
#' print.ctx_credentials()

print.ctx_credentials <- function(...) {
  cat('Key -',     if (!has_ctx_key()) '' else {if(showing_key()) ctx_key() else 'xxx' }, '\n')
}



#' @rdname register_ctx_api_key
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Display ctx API key
#' ctx_key()

ctx_key <- function() {
  key <- .pkgenv[["api"]] #Sys.getenv('CTX_API_KEY')

  if (key == '') {
    return(NA_character_)
  } else {
    return(key)
  }
}


#' @rdname register_ctx_api_key
#' @export
#' @examplesIf has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')
#' # Check whether API key is registered
#' has_ctx_key()

has_ctx_key <- function () !is.na(ctx_key())




