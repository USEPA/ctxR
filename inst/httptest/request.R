function (request) {
  request |>
    gsub_request("https://api-ccte.epa.gov/", "", fixed=TRUE)
}
