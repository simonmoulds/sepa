#' Get time series data for one or more stations
#'
#' @inheritParams sepa_station_list
#' @param ts_id Character or numeric. Either a single station ID or a
#'   vector of station IDs. Time series IDs can be found using the
#'   [sepa_timeseries_list()] function.
#' @param start_date Date or character formatted "YYYY-MM-DD". Defaults to
#'   yesterday.
#' @param end_date Date or character formatted "YYYY-MM-DD". Defaults to today.
#' @param ... Additional arguments. None implemented.
#'
#' @return A tibble.
#' @export
sepa_timeseries_values <- function(ts_id,
                                   start_date,
                                   end_date,
                                   return_fields, ...) {

  # Default to past 24 hours
  if (missing(start_date) || missing(end_date)) {
    message(
      strwrap(
        "No start or end date provided, attempting to retrieve data for past 
        24 hours", prefix = " ", initial = ""
      )
    )
    start_date <- Sys.Date() - 1
    end_date <- Sys.Date()
  } else {
    check_date(start_date, end_date)
  }

  # Account for user-provided return fields
  if (missing(return_fields)) {
    return_fields <- "Timestamp,Value"
  } else {
    if (!inherits(return_fields, "character")) {
      stop(
        strwrap(
          "User supplied return_fields must be comma separated string or
          vector of strings", prefix = " ", initial = ""
        )
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

  ts_data <- do.call("bind_rows", ts_data) |> as.data.frame()
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
    ) |>
    as_tibble()

  return(content_dat)
}
