#' Get station metadata
#'
#' Returns metadata about measurement stations in the SEPA network.
#' 
#' @section Return fields: 
#' 
#' The argument `return_fields` allows you to specify the return fields. 
#' The default return fields are as follows: 
#' 
#' * `station_name`
#' * `station_no`
#' * `station_id`
#' * `station_latitude`
#' * `station_longitude` 
#' 
#' There are too many options to document here. Instead, take a look at 
#' the online documentation for [getStationList](https://timeseries.sepa.org.uk/KiWIS/KiWIS?datasource=0&service=kisters&type=queryServices&request=getrequestinfo)
#' to see the possible values that can be supplied to the API parameter 
#' \strong{returnfields}. 
#' 
#' @param station_search_term Character. A string pattern to search
#'   station names. This argument supports wildcard matching using
#'   the asterisk (*) symbol. The search is case insensitive.
#' @param bounding_box Numeric. A bounding box within which
#'   to search for stations. This argument should be a vector
#'   with exactly four values formatted as follows:
#'   (`min_x`, `min_y`, `max_x`, `max_y`).
#' @param group_id Character or numeric. A station group id. See 
#'   [sepa_group_list()].
#' @param return_fields Character. A vector of return fields. In 
#'   most cases the default return fields will suffice. See details.
#' @param ... Additional arguments. None implemented.
#'
#' @return A tibble.
#' @export
#' @examples
#' \dontrun{
#' # Filter by name
#' stns <- sepa_station_list(station_search_term="Auch*")
#'
#' # Filter by region
#' stns <- sepa_station_list(bounding_box = c(-2, 48, 2, 60))
#'
#' # Filter by group
#' q_grp <- grps |>
#'   dplyr::filter(group_name %in% "StationsWithFlow") |>
#'   dplyr::pull(group_id)
#' stns <- sepa_station_list(group_id = q_grp)
#' 
#' # Change the return fields
#' fields <- c("station_name", "station_no", "station_id", "object_type")
#' stns <- sepa_station_list(group_id = q_grp, return_fields = fields)
#' }
sepa_station_list <- function(station_search_term,
                              bounding_box,
                              group_id,
                              return_fields, ...) {

  ## Account for user-provided return fields
  if (missing(return_fields)) {
    return_fields <- c(
      "station_name", "station_no", "station_id", 
      "station_latitude", "station_longitude"
    )
    return_fields <- paste0(return_fields, collapse = ",")
  } else {
    if (!inherits(return_fields, "character")) {
      stop(
        strwrap(
          "User supplied return_fields must be comma separated string or 
          vector of strings", prefix = " ", initial = ""
        )
      )
    }
  }

  api_url <- sepa_base_url()

  ## Base query
  api_query <- list(
    service = "kisters",
    datasource = 0,
    type = "queryServices",
    request = "getStationList",
    format = "json",
    kvp = "true",
    returnfields = paste(
      return_fields,
      collapse = ","
    )
  )

  ## Check for search term
  if (!missing(station_search_term)) {
    station_search_term <- paste(
      station_search_term,
      toupper(station_search_term),
      tolower(station_search_term),
      sep = ","
    )
    api_query[["station_name"]] <- station_search_term
  }

  ## Check for bounding box
  if (!missing(bounding_box)) {
    bounding_box <- paste(bounding_box, collapse = ",")
    api_query[["bbox"]] <- bounding_box
  }

  ## Check for group_id
  if (!missing(group_id)) {
    api_query[["stationgroup_id"]] <- group_id
  }

  raw <- request(api_url) |>
    req_url_query(!!!api_query) |>
    req_timeout(15) |>
    req_perform()
  
  json_content <- resp_body_json(raw)
  nms <- json_content[[1]] |> as.character()
  rows <- lapply(
    json_content[-1],
    FUN = function(x) as.data.frame(setNames(x, nms))
  )
  content_dat <- do.call("bind_rows", rows) |> as_tibble()

  ## Cast lat/lon columns if they exist
  content_dat <- content_dat |>
    mutate(
      across(any_of(c("station_latitude", "station_longitude")), as.double)
    ) |>
    as_tibble()

  # Remove any bogus stations 
  if ("station_name" %in% names(content_dat)) { 
    # Common strings for culling bogus stations
    garbage <- c(
      "^#", "^--", "testing", "^Template\\s", 
      "\\sTEST$", "\\sTEMP$", "\\stest\\s"
    )
    garbage <- paste(garbage, collapse = "|")
    content_dat <- content_dat |> 
      filter(!grepl(garbage, .data$station_name))
  }

  return(content_dat)
}