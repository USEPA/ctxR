function (request) {
  request |>
    gsub_request("https://api-ccte.epa.gov/", "", fixed=TRUE) |>
    gsub_request("https://comptox.epa.gov/ctx-api/", "ctx/", fixed = TRUE) |>
    gsub_request("exposure-batch", "exp-batch", fixed = TRUE) |>
    gsub_request("functional-use", "fun-use", fixed = TRUE) |>
    gsub_request("exposure/seem/demographic", "expo-demo", fixed = TRUE) |>
    gsub_request("exposure/seem/general", "expo-gen", fixed = TRUE) |>
    gsub_request("exposure/ccd", "ccd", fixed = TRUE) |>
    gsub_request("probability/search", "prob-s") |>
    gsub_request("chemical/list/chemicals/search", "clcs", fixed = TRUE) |>
    gsub_request("chemical/list/search", "cls", fixed = TRUE) |>
    gsub_request("property/experimental/search", "prop-exp-s", fixed = TRUE) |>
    gsub_request("api-ccte-stg.epa.gov", "stg", fixed = TRUE)
  }
