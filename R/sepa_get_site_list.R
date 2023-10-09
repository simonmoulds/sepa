
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
#' @param param Character. If provided, only sites measuring this
#'   parameter will be returned. Otherwise all sites will be return.
#' @param ... Additional arguments. None implemented.
#'
#' @return A tibble.
#' @export
sepa_get_sites <- function(param = NULL, ...) {
  query <- "&request=getStationList"
  if (!is.null(param)) {
    query <- paste0(query, "&parametertype_name=", param)
  }
  format <- "&format=objson"
  url <- paste0(sepa_base_url(), query, format)
  response <- GET(url)
  x <- fromJSON(rawToChar(response$content))
  x <- as_tibble(x)
  x
}

#' @rdname sepa_get_sites
#' @export
sepa_get_flow_sites <- function(...) {
  sepa_get_sites(param = "Q")
}

#' @rdname sepa_get_sites
#' @export
sepa_get_level_sites <- function(...) {
  sepa_get_sites(param = "S")
}

#' @rdname sepa_get_sites
#' @export
sepa_get_precip_sites <- function(...) {
  sepa_get_sites(param = "Precip")
}

#' Get available parameters for a set of SEPA sites
#'
#' @param id Character. If provided, only information for this
#'   site will be returned. Otherwise information for all sites
#'   is returned.
#' @param ... Additional arguments. None implemented.
#'
#' @return A tibble containing one row for each station ID and
#'   parameter combination.
#' @export
sepa_get_parameters <- function(id = NULL, ...) {
  query <- "&request=getParameterList"
  if (!is.null(id)) {
    query <- paste0(query, "&station_id=", id)
  }
  format <- "&format=objson"
  url <- paste0(sepa_base_url(), query, format)
  response <- GET(url)
  x <- fromJSON(rawToChar(response$content))
  x <- as_tibble(x) %>% arrange(x, .data[["station_name"]], .data[["parametertype_name"]])
  x
}

#' Get available time series data for a parameter and site
#'
#' @param id Character. Station ID.
#' @param param Character. Parameter name.
#' @param ... Additional arguments. None implemented.
#'
#' @return A tibble.
#' @export
sepa_get_ts <- function(id, param, ...) {
  query <- "&request=getTimeseriesList"
  if (length(id) > 1) {
    query <- paste0(query, "&station_id=", paste0(id, collapse = ", "))
  } else {
    query <- paste0(query, "&station_id=", id)
  }
  query <- paste0(query, "&parametertype_name=", param)
  format <- "&format=objson"
  url <- paste0(sepa_base_url(), query, format)
  response <- GET(URLencode(url))
  x <- fromJSON(rawToChar(response$content))
  x <- as_tibble(x)
  x
}

check_datetime <- function(x, arg = "argname", call = caller_env(2)) {
  if (!is.Date(x) | is.POSIXt(x)) {
    cli_abort(c(
      "{.arg {arg}} must be a Date or POSIXt object.",
      "x" = "You've supplied a {.cls {class(x)}} object."
    ), call = call)
  }
}

format_datetime <- function(x, ...) {
  format(x, "%Y-%m-%dT%H:%M:%S")
}

param_not_available_error <- function(station_id, param, call = caller_env(2)) {
  cli_abort("Parameter {.arg {param}} not available for station {.arg {station_id}}", call = call)
}

ts_not_available_error <- function(station_id, param, ts, call = caller_env(2)) {
  cli_abort("Timeseries {.arg {ts}} for parameter {.arg {param}} unavailable for station {.arg {station_id}}", call = call)
}

get_ts_id <- function(station_id, param, ts) {
  ts_list <- sepa_get_ts(station_id, param)
  if (nrow(ts_list) == 0) {
    param_not_available_error(station_id, param)
  }
  ts_id <- ts_list$ts_id[ts_list$ts_name %in% ts]
  if (length(ts_id) == 0) {
    ts_not_available_error(station_id, param, ts)
  }
  ts_id
}

#' Get SEPA timeseries data
#'
#' @param ts_id Character.
#' @param station_id Character.
#' @param from Date or POSIXt.
#' @param to Date or POSIXt.
#' @param ... Additional arguments. None implemented.
#'
#' @return A tibble.
#' @export
sepa_get_data <- function(ts_id, from, to, ...) {
  query <- "&request=getTimeseriesValues"
  if (length(ts_id) > 1) {
    query <- paste0(query, "&ts_id=", paste0(ts_id, collapse = ", "))
  } else {
    query <- paste0(query, "&ts_id=", ts_id)
  }
  if (!is.null(from)) {
    check_datetime(from, "from")
    query <- paste0(query, "&from=", format_datetime(from))
  }
  if (!is.null(to)) {
    check_datetime(to)
    query <- paste0(query, "&to=", format_datetime(to))
  }
  returnfields <- c("Timestamp", "Value", "Quality Code")
  returnfields <- paste0("&returnfields=", paste0(returnfields, collapse=", "))
  format <- "&format=dajson"
  url <- paste0(sepa_base_url(), query, format, returnfields)
  response <- GET(URLencode(url))
  x <- fromJSON(rawToChar(response$content))
  data_list <- list()
  for (i in 1:nrow(x)) {
    x_row <- x[i, ]
    n_rows <- x_row$rows
    if (n_rows == 0) {
      next
    }
    colnames <- trimws(strsplit(x_row$columns, ",")[[1]])
    data <- x_row$data[[1]]
    colnames(data) <- colnames
    data <- as_tibble(data) %>%
      mutate(Timestamp = as_datetime(.data[["Timestamp"]])) %>%
      mutate(across(-all_of("Timestamp"), as.numeric))
    data_list[[i]] <- data
  }
  x <- do.call("rbind", data_list)
  x
}

#' @rdname sepa_get_data
#' @export
sepa_daily_flow <- function(station_id, from = NULL, to = NULL, ...) {
  ts_id <- get_ts_id(station_id, "Q", "Day.Mean")
  data <- sepa_get_data(ts_id, from, to, ...)
  data
}

#' @rdname sepa_get_data
#' @export
sepa_daily_level <- function(station_id, from = NULL, to = NULL, ...) {
  ts_id <- get_ts_id(station_id, "S", "Day.Mean")
  data <- sepa_get_data(ts_id, from, to, ...)
  data
}

#' @rdname sepa_get_data
#' @export
sepa_daily_precip <- function(station_id, from = NULL, to = NULL, ...) {
  ts_id <- get_ts_id(station_id, "Precip", "Day.Total")
  data <- sepa_get_data(ts_id, from, to, ...)
  data
}

#' @rdname sepa_get_data
#' @export
sepa_15min_flow <- function(station_id, from = NULL, to = NULL, ...) {
  ts_id <- get_ts_id(station_id, "Q", "15minute")
  data <- sepa_get_data(ts_id, from, to, ...)
  data
}

#' @rdname sepa_get_data
#' @export
sepa_15min_level <- function(station_id, from = NULL, to = NULL, ...) {
  ts_id <- get_ts_id(station_id, "S", "15minute")
  data <- sepa_get_data(ts_id, from, to, ...)
  data
}

#' @rdname sepa_get_data
#' @export
sepa_15min_precip <- function(station_id, from = NULL, to = NULL, ...) {
  ts_id <- get_ts_id(station_id, "Precip", "15minute.Total")
  data <- sepa_get_data(ts_id, from, to, ...)
  data
}

#' @rdname sepa_get_data
#' @export
sepa_15min_flow_realtime <- function(station_id = NULL, ...) {
  if (is.null(station_id)) {
    station_id <- sepa_get_flow_sites()$station_id
  }
  ts_id <- get_ts_id(station_id, "Q", "15minute")
  data <- sepa_get_data(ts_id, from = NULL, to = NULL)
  data
}
