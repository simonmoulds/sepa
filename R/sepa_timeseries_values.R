#' Get timeseries data for one or more stations
#'
#' @section Return fields: 
#' 
#' The argument `return_fields` allows you to specify the return fields. 
#' The default return fields are as follows: 
#' 
#' * `Timestamp`
#' * `Value`
#' 
#' It is also possible to return some metadata fields. You can do this by 
#' setting `metadata=TRUE` and supplying `md_return_fields`. If the latter 
#' is not given then the following default fields will be requested:
#' 
#' * `ts_unitname`
#' * `ts_unitsymbol`
#' * `ts_name`
#' * `ts_id`
#' * `stationparameter_name`
#' * `station_name`
#' * `station_id`
#'
#' Lastly, there are some additional optional return fields that can be 
#' returned by setting `custom_attribute_fields=TRUE` and supplying 
#' `ca_sta_returnfields`. If the latter is not set then the following 
#' fields will be requested: 
#' 
#' * `CATCHMENT_SIZE`
#' * `GAUGE_DATUM`
#' 
#' In all cases, note that some fields are not available for some timeseries. 
#'  
#' There are too many options to document here. Instead, take a look at 
#' the online documentation for [getTimeseriesValues](https://timeseries.sepa.org.uk/KiWIS/KiWIS?datasource=0&service=kisters&type=queryServices&request=getrequestinfo)
#' to see the possible values that can be supplied to the API parameters
#' \strong{returnfields}, \strong{md_returnfields}. Unfortunately the 
#' possible arguments to \strong{ca_sta_returnfields} are currently 
#' undocumented. 
#' 
#' @inheritParams sepa_station_list
#' @param ts_id Character or numeric. Either a single station ID or a
#'   vector of station IDs. Time series IDs can be found using the
#'   [sepa_timeseries_list()] function.
#' @param start_date Date or character formatted "YYYY-MM-DD". Defaults to
#'   yesterday.
#' @param end_date Date or character formatted "YYYY-MM-DD". Defaults to today.
#' @param metadata Logical. Whether or not to return metadata alongside the 
#'   timeseries values. 
#' @param md_return_fields Character. A vector of metadata return fields. Only 
#'   used if `metadata` is TRUE. See details.
#' @param ca_sta Logical. Whether or not to return custom 
#'   attribute fields. Default is FALSE. 
#' @param ca_sta_return_fields Character. 
#' @param ... Additional arguments. None implemented.
#'
#' @return A tibble.
#' @export
sepa_timeseries_values <- function(ts_id,
                                   start_date,
                                   end_date,
                                   return_fields,
                                   metadata = FALSE,
                                   md_return_fields,
                                   ca_sta = FALSE,
                                   ca_sta_return_fields,
                                   ...) {

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
  if (missing(md_return_fields)) {
    if (!metadata) { 
      md_return_fields <- c()
    } else { 
      md_return_fields <- c(
        "ts_unitname",
        "ts_unitsymbol",
        "ts_name",
        "ts_id",
        "stationparameter_name",
        "station_name",
        "station_id"
      )
    }
  } else { 
    # Just in case this has been supplied
    md_return_fields[!md_return_fields == "ca_sta"]
  }
  
  # Custom attributes
  if (ca_sta) { 
    metadata <- TRUE
    md_return_fields <- c(md_return_fields, "ca_sta")
  }
  md_return_fields <- paste(md_return_fields, collapse = ",")

  if (missing(ca_sta_return_fields)) { 
    ca_sta_return_fields <- paste(c("CATCHMENT_SIZE", "GAUGE_DATUM"), collapse = ",")
  } else { 
    ca_sta_return_fields <- paste(ca_sta_return_fields, collapse = ",")
  }

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
    metadata = metadata,
    md_returnfields = md_return_fields,
    ca_returnfields = ca_sta_return_fields,
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
      Value = as.numeric(ts_data$Value)
    ) |>
    as_tibble()

  # Add metadata fields
  if (metadata) { 
    md_return_fields_sep <- str_trim(strsplit(md_return_fields, ",")[[1]])
    md_return_fields_sep <- md_return_fields_sep[!md_return_fields_sep == "ca_sta"]
    for (field in md_return_fields_sep) { 
      content_dat <- content_dat |> mutate(!!field := json_content[[field]])
    }
  }

  if (ca_sta) { 
    ca_sta_return_fields_sep <- str_trim(strsplit(ca_sta_return_fields, ",")[[1]])
    for (field in ca_sta_return_fields_sep) { 
      content_dat <- content_dat |> mutate(!!field := json_content[[field]])
    }
  }

  return(content_dat)
}
