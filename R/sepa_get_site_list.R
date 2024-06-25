
#' Get tibble with available groups
#'
#' Returns all available groups. This can be used to filter
#' other queries such as `sepa_station_list`.
#'
#' @return A tibble.
#' @export
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
  rows <- lapply(json_content[-1], FUN = function(x) as.data.frame(setNames(x, nms)))
  content_dat <- do.call("rbind", rows) |> as_tibble()

  return(content_dat)
}


#' Get tibble with station information
#'
#' Returns all available stations in the SEPA network
#'
#' @param station_search_term Character. A string pattern to search
#'   station names. This argument supports wildcard matching
#'   using the asterisk (*) symbol. The search is case
#'   insensitive.
#' @param bounding_box Numeric. A bounding box within which
#'   to search for stations. This argument should be a vector
#'   with exactly four values formatted as follows:
#'   (min_x, min_y, max_x, max_y).
#' @param group_id Character or numeric. A station group id (see ki_group_list)
#' @param return_fields Character. A vector of return fields.
#' @param ... Additional arguments. None implemented.
#'
#' @return A tibble.
#' @export
sepa_station_list <- function(station_search_term, bounding_box, group_id, return_fields, ...) {

  ## Common strings for culling bogus stations
  garbage <- c(
    "^#", "^--", "testing",
    "^Template\\s", "\\sTEST$",
    "\\sTEMP$", "\\stest\\s"
  )

  ## Account for user-provided return fields
  if (missing(return_fields)) {
    return_fields <- "station_name,station_no,station_id,station_latitude,station_longitude"
  } else {
    if (!inherits(return_fields, "character")) {
      stop(
        "User supplied return_fields must be comma separated string or vector of strings"
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
    station_search_term <- paste(station_search_term,
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
  rows <- lapply(json_content[-1], FUN = function(x) as.data.frame(setNames(x, nms)))
  content_dat <- do.call("rbind", rows) |> as_tibble()

  ## Cast lat/lon columns if they exist
  content_dat <- content_dat |>
    mutate(across(any_of(c("station_latitude", "station_longitude")), as.double))

  return(content_dat)
}


#' Get list of available time series for one or more stations
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
sepa_timeseries_list <- function(station_id, ts_name, coverage = TRUE, group_id, return_fields, ...) {

  ## Check for no input
  if (missing(station_id) & missing(ts_name) & missing(group_id)) {
    stop("No station_id, ts_name or group_id provided.")
  }

  ## Account for user-provided return fields
  if (missing(return_fields)) {
    ## Default
    return_fields <- "station_name,station_id,stationparameter_name,ts_id,ts_name"
  } else {
    if (!inherits(return_fields, "character")) {
      stop(
        "User supplied return_fields must be comma separated string or vector of strings"
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

  if (coverage == TRUE){
    ## Turn coverage columns on
    api_query[['returnfields']] <- paste0(
      api_query[['returnfields']],
      ",coverage"
    )
  }

  ## Check for ts_name search
  if (!missing(ts_name)) {
    api_query[["ts_name"]] <- ts_name
  }

  ## Check for group_id
  if (!missing(group_id)){
    api_query[["group_id"]] <- group_id
  }

  raw <- request(api_url) |> 
    req_url_query(!!!api_query) |> 
    req_timeout(15) |>
    req_perform()
  
  json_content <- resp_body_json(raw)
  nms <- json_content[[1]] |> as.character()
  rows <- lapply(json_content[-1], FUN = function(x) as.data.frame(setNames(x, nms)))
  content_dat <- do.call("rbind", rows) |> as_tibble()
  
  ## Cast lat/lon columns if they exist
  content_dat <- content_dat |>
    mutate(across(any_of(c("station_latitude", "station_longitude")), as.double))

  ## Cast coverage columns if the exist
  content_dat <- content_dat |>
    mutate(across(any_of(c("from", "to")), lubridate::ymd_hms))

  return(content_dat)
}


#' Get time series data for one or more stations
#'
#' @inheritParams sepa_station_list
#' @param ts_id Character or numeric. Either a single station ID or a
#'   vector of station IDs. Time series IDs can be found using the
#'   `sepa_timeseries_list` function.
#' @param start_date Date or character formatted "YYYY-MM-DD". Defaults to yesterday.
#' @param end_date Date or character formatted "YYYY-MM-DD". Defaults to today.
#' @param ... Additional arguments. None implemented.
#'
#' @return A tibble.
#' @export
#' @examples
#'
#' # Get groups
#' grps <- sepa_group_list()
#' q_grp <- grps |> dplyr::filter(group_name %in% "StationsWithFlow") |> dplyr::pull(group_id)
#' stns <- sepa_station_list(group_id = q_grp)
#'
sepa_timeseries_values <- function(ts_id, start_date, end_date, return_fields, ...) {

  # Default to past 24 hours
  if (missing(start_date) || missing(end_date)) {
    message("No start or end date provided, trying to return data for past 24 hours")
    start_date <- Sys.Date() - 1
    end_date <- Sys.Date()
  } else {
    check_date(start_date, end_date)
  }

  # Account for user-provided return fields
  if (missing(return_fields)) {
    return_fields <- "Timestamp,Value"
  } else {
    if(!inherits(return_fields, "character")){
      stop(
        "User supplied return_fields must be comma separated string or vector of strings"
      )
    }
    return_fields <- c("Timestamp", "Value", return_fields)
  }

  # Identify hub
  api_url <- sepa_base_url()

  if (missing(ts_id)) {
    stop("Please enter a valid ts_id.")
  } else {
    # Account for multiple ts_ids
    ts_id_string <- paste(ts_id, collapse = ",")
  }

  # Metadata to return
  ts_meta <- paste(c(
    "ts_unitname",
    "ts_unitsymbol",
    "ts_name",
    "ts_id",
    "stationparameter_name",
    "station_name",
    "station_id"
  ), collapse = ",")

  api_query <- list(
    service = "kisters",
    datasource = 0,
    type = "queryServices",
    request = "getTimeseriesValues",
    format = "json",
    kvp = "true",
    ts_id = ts_id_string,
    from = start_date,
    to = end_date,
    metadata = "true",
    md_returnfields = ts_meta,
    returnfields = paste(
      return_fields,
      collapse = ","
    )
  )

  raw <- request(api_url) |> 
    req_url_query(!!!api_query) |> 
    req_timeout(60) |>
    req_perform()
  
  json_content <- resp_body_json(raw)[[1]] 

  if (length(names(json_content)) == 3) {
    stop(json_content$message)
  }

  if ("rows" %in% names(json_content)) {
    num_rows <- sum(as.numeric(json_content$rows))
    if (num_rows == 0) {
      stop("No data available for selected ts_id(s).")
    }
  }

  ts_cols <- unlist(strsplit(json_content$columns[[1]], ","))
  ts_data <- lapply(json_content$data, FUN = function(x) setNames(x, ts_cols))

  ts_data <- do.call("rbind", ts_data)
  content_dat <- ts_data |> 
    mutate(
      Timestamp = lubridate::ymd_hms(ts_data$Timestamp),
      Value = as.numeric(ts_data$Value),
      ts_name = json_content$ts_name,
      ts_id = json_content$ts_id,
      Units = json_content$ts_unitsymbol,
      stationparameter_name = json_content$stationparameter_name,
      station_name = json_content$station_name,
      station_id = json_content$station_id
    )

  return(content_dat)
}
