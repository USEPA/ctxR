#' Register a CCTE API
#'
#' This page contains documentation tools related to enabling CCTE API services
#' in R.
#'
#' To obtain an API key and enable services, go to
#' \url{https://api-ccte.epa.gov/docs/}. This documentation shows you how to
#' obtain an API key to allow access to the CCTE APIs.
#'
#' To tell ccdr about your API key, use [register_ccte()], e.g.
#' `register_ccte(key = 'grbwigbwoginrowgbwibgdibdvinrginiwgo')` (that's a fake
#' key). This will set your API key for the current session, but if you restart
#' R, you'll need to do it again. You can set it permanently by setting `write =
#' TRUE`m see the examples. If you set it permanently it will be stored in your
#' .Renviron file, and that will be accessed by ccdr persistently across
#' sessions.
#'
#' Users should be aware that the API key, a string of garbled
#' characters/numbers/symbols, is a PRIVATE key - it uniquely identifies and
#' authenticates you to CCTE's services. If anyone gets your API key, they can
#' use it to masquerade as you to CCTE. To mitigate against users inadvertently
#' sharing their keys, by default ccdr never displays a user's key in messages
#' displayed to the console.
#'
#' Users should be aware that ccdr has no mechanism with which to safeguard the
#' private key once registered with R. That is to say, once you register your
#' API key, any R function will have access to it. As a consequence, ccdr will
#' not know if another function, potentially from a compromised package,
#' accesses the key and uploads it to a third party. For this reason, when using
#' ccdr we recommend a heightened sense of security and self-awareness: only use
#' trusted packages, do not save the API keys in script files, etc.
#'
#' @param key an API key
#' @param write if TRUE, stores the secrets provided in the .Renviron file
#' @param ... a dumped formal argument to the generic print method
#' @return NULL
#' @name register_ccte



#' @rdname register_ccte
#' @export
#' @examplesIf has_ccte_key() & is.na(ccte_key() == 'FAKE_KEY')
#' # Check if API key is showing
#' showing_key()

showing_key <- function() {

  if (has_ccdr_option('display_api_key')) {
    getOption('ccdr')$display_api_key
  } else {
    FALSE
  }
}



#' @rdname register_ccte
#' @export
#' @examplesIf has_ccte_key() & is.na(ccte_key() == 'FAKE_KEY')
#' # Toggle API key to display
#' ccdr_show_api_key()

ccdr_show_api_key <- function() {
  set_ccdr_option('display_api_key' = TRUE)
  cli::cli_alert_warning('ccdr will now display PRIVATE API keys in the console.')
}



#' @rdname register_ccte
#' @export
#' @examplesIf has_ccte_key() & is.na(ccte_key() == 'FAKE_KEY')
#' # Toggle API key to be hidden
#' ccdr_hide_api_key()

ccdr_hide_api_key <- function() {
  set_ccdr_option('display_api_key' = FALSE)
  cli::cli_alert_info('ccdr will now suppress private API keys in the console.')
}



#' @rdname register_ccte
#' @export
#' @examplesIf has_ccte_key() & is.na(ccte_key() == 'FAKE_KEY')
#' # Register key for this session
#' register_ccdr(key = 'YOUR API KEY')
#' # Register key over sessions
#' register_ccdr(key = 'YOUR API KEY', write = TRUE)

register_ccdr <- function(key, write = FALSE) {

  # allow register_ccdr to work when ccdr not loaded
  #
  # FILL in details
  #

  # get current options
  options <- getOption('ccdr')


  # deal with API key
  if (!missing(key) && write) {

    if (R.version.string < "4.0.0"){
      warning("This function relies on R version 4.0.0 or later. Cannot store API key over multiple sessions.")
    } else {
      ccdrdir <- tools::R_user_dir("ccdR")
      if (!dir.exists(ccdrdir)){
        dir.create(ccdrdir, recursive = TRUE)
      }
      fname <- file.path(ccdrdir, "api.dcf")
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
    Sys.setenv('CCTE_API_KEY' = key)

  } else if (!missing(key) && !write) {

    # set key in current session
    Sys.setenv('CCTE_API_KEY' = key)

  }

  # class
  class(options) <- 'ccdr_credentials'

  # set new options
  options(ccdr = options)

  # return
  invisible(NULL)
}





#' @rdname register_ccte
#' @export
#' @examplesIf has_ccte_key() & is.na(ccte_key() == 'FAKE_KEY')
#' # Print function for ccte_credentials class
#' print.ccte_credentials()

print.ccte_credentials <- function(...) {
  cat('Key -',     if (!has_ccte_key()) '' else {if(showing_key()) ccte_key() else 'xxx' }, '\n')
}



#' @rdname register_ccte
#' @export
#' @examplesIf has_ccte_key() & is.na(ccte_key() == 'FAKE_KEY')
#' # Display ccte API key
#' ccte_key()

ccte_key <- function() {
  key <- Sys.getenv('CCTE_API_KEY')

  if (key == '') {
    return(NA_character_)
  } else {
    return(key)
  }
}


#' @rdname register_ccte
#' @export
#' @examplesIf has_ccte_key() & is.na(ccte_key() == 'FAKE_KEY')
#' # Check whether API key is registered
#' has_ccte_key()

has_ccte_key <- function () !is.na(ccte_key())




