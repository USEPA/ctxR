.onAttach <- function(...) {

  tos <- paste0(
    cli::col_green(cli::symbol$info),
    ' ',
    "CCTE's Terms of Service: ",
    cli::col_blue(cli::style_italic(
      cli::style_hyperlink('<https://api-ccte.epa.gov/docs/>', 'https://api-ccte.epa.gov/docs/')
    ))
  )
  cite <- paste0(
    cli::col_green(cli::symbol$info),
    ' ',
    'Please cite ', cli::col_blue('ccdr'), ' if you use it! Use `citation(\'ccdr\')` for details.'
  )

  rlang::inform(
    paste0(tos, '\n', cite),
    class = 'packageStartupMessage'
  )

  bootstrap_ccdr()

}


bootstrap_ccdr <- function() {
  set_ccdr_option(
    'ccte' = structure(
      list(

    ),
    class = 'ccte_credentials'
  ),
  'display_api_key' = FALSE
  )
}
