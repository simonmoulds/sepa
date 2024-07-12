#' Get available groups
#'
#' Returns information about the available measurement stations groups. 
#' Group names include "StationsWithFlow", "StationsWithRainfall", amongst 
#' others. Group IDs can be used to filter other queries such as 
#' [sepa_station_list()].
#'
#' @return A tibble.
#' @export
#' @examples
#' \dontrun{
#' grps <- sepa_group_list()
#' }
sepa_group_list <- function() {

  api_url <- sepa_base_url()

  api_query <- list(
    service = "kisters",
    datasource = 0,
    type = "queryServices",
    request = "getGroupList",
    format = "json",
    kvp = "true"
  )

  raw <- request(api_url) |>
    req_url_query(!!!api_query) |>
    req_timeout(15) |>
    req_user_agent("sepa (https://github.com/simonmoulds/sepa)") |>
    req_perform()

  json_content <- resp_body_json(raw)
  nms <- json_content[[1]] |> as.character()
  rows <- lapply(
    json_content[-1], FUN = function(x) as.data.frame(setNames(x, nms))
  )
  content_dat <- do.call("bind_rows", rows) |> as_tibble()
  return(content_dat)
}