function (request) {
  request |>
    gsub_request("https://api-ccte.epa.gov/", "", fixed=TRUE) |>
    gsub_request("exposure-batch", "exp-batch", fixed = TRUE) |>
    gsub_request("functional-use", "fun-use", fixed = TRUE) |>
    gsub_request("exposure/seem/demographic", "expo-demo", fixed = TRUE) |>
    gsub_request("exposure/seem/general", "expo-gen", fixed = TRUE) |>
    gsub_request("api-ccte-stg.epa.gov", "stg", fixed = TRUE)
}
