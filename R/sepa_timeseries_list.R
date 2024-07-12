#' Get list of available time series for one or more stations
#'
#' @details
#' This function allows you to retrieve timeseries IDs given `station_id`, 
#' `ts_name` or `group_id`. Note that will the first two parameters can be 
#' used in tandem, e.g. to search for a timeseries at a particular station, 
#' `group_id` must be used on its own. 
#' 
#' @inheritParams sepa_station_list
#' @param station_id Character or numeric. Either a single station ID or a
#'   vector of station IDs.
#' @param ts_name Character. A time series short name to search for.  This
#'   argument supports wildcard matching using the asterisk (*) symbol.
#' @param coverage Logical. Whether or not to return the start and end dates
#'   of the time series. Default is TRUE, but setting to FALSE will result
#'   in faster queries.
#' @param ... Additional arguments. None implemented.
#'
#' @return A tibble.
#' @export
#' @examples  
#' \dontrun{
#' # Get all discharge timeseries
#' q_grp <- sepa_group_list() |> 
#'   filter(group_name == "StationsWithFlow") |> 
#'   pull(group_id)
#' ts_list <- sepa_timeseries_list(group_id = q_grp)
#' # Search for all stations with 15 minute instantaneous flow 
#' } 
sepa_timeseries_list <- function(station_id,
                                 ts_name,
                                 coverage = TRUE,
                                 group_id,
                                 return_fields, ...) {

  ## Check for no input
  if (missing(station_id) && missing(ts_name) && missing(group_id)) {
    stop("No station_id, ts_name or group_id provided.")
  }

  ## Account for user-provided return fields
  if (missing(return_fields)) {
    return_fields <- c(
      "station_name", "station_id", "stationparameter_name",
      "ts_id", "ts_name"
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

    ## Account for user listing coverage in return_fields
    if (length(grepl("coverage", return_fields))) {
      return_fields <- gsub(
        ",coverage|coverage,",
        "",
        return_fields
      )
    }
  }

  api_url <- sepa_base_url()

  api_query <- list(
    service = "kisters",
    datasource = 0,
    type = "queryServices",
    request = "getTimeseriesList",
    format = "json",
    kvp = "true",
    returnfields = paste(
      return_fields,
      collapse = ","
    )
  )

  if (!missing(station_id)) {
    ## Account for multiple station_ids
    station_id <- paste(station_id, collapse = ",")
    api_query[["station_id"]] <- station_id
  }

  ## Check for ts_name search
  if (!missing(ts_name)) {
    api_query[["ts_name"]] <- ts_name
  }

  ## Check for group_id
  if (!missing(group_id)){
    if (!missing(ts_name) || !missing(station_id)) { 
      stop(
        strwrap(
          "`group_id` cannot be used in conjunction with `station_id` or 
          `ts_name`", prefix = " ", initial = ""
        )
      )
    }
    api_query[["stationgroup_id"]] <- group_id
  }

  if (coverage == TRUE){
    ## Turn coverage columns on
    api_query[["returnfields"]] <- paste0(
      api_query[["returnfields"]],
      ",coverage"
    )
  }

  raw <- request(api_url) |>
    req_url_query(!!!api_query) |>
    req_timeout(30) |>
    req_perform()
  
  json_content <- resp_body_json(raw)
  nms <- json_content[[1]] |> as.character()
  rows <- lapply(
    json_content[-1], FUN = function(x) as.data.frame(setNames(x, nms))
  )
  content_dat <- do.call("bind_rows", rows) |> as_tibble()
  
  ## Cast lat/lon columns if they exist
  content_dat <- content_dat |>
    mutate(
      across(any_of(c("station_latitude", "station_longitude")), as.double)
    )

  ## Cast coverage columns if the exist
  content_dat <- content_dat |>
    mutate(across(any_of(c("from", "to")), lubridate::ymd_hms)) |>
    as_tibble()

  return(content_dat)
}
