function (request) {
  request |>
    gsub_request("https://api-ccte.epa.gov/", "", fixed=TRUE) |>
    gsub_request("exposure-batch", "exp-batch", fixed = TRUE) |>
    gsub_request("functional-use", "fun-use", fixed = TRUE)
}
