
sepa_base_url <- function(...) {
  ## https://timeseriesdoc.sepa.org.uk/api-documentation/api-function-reference/
  root <- "https://timeseries.sepa.org.uk/KiWIS/KiWIS?"
  backend_config <- "service=kisters&type=queryServices&datasource=0"
  base_url <- paste0(root, backend_config)
  base_url
}

#' Get sites from SEPA
#'
#' This function gets the table of hydrological stations
#'
#' @param ... Additional arguments. None implemented.
#'
#' @return A tibble.
#' @export
sepa_get_sites <- function(...) {
  query <- "&request=getStationList"
  format <- "&format=objson"
  url <- paste0(sepa_base_url(), query, format)
  response <- GET(url)
  x <- fromJSON(rawToChar(response$content))
  x <- as_tibble(x)
  ## sites <- new_tibble(
  ##   sites, server = server,
  ##   class = "whos_sites"
  ## )
  ## sites
  x
}
