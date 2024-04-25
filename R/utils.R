

sepa_base_url <- function(...) {
  ## https://timeseriesdoc.sepa.org.uk/api-documentation/api-function-reference/
  root <- "https://timeseries.sepa.org.uk/KiWIS/KiWIS?"
  return(root)
}

get_query <- function(...) { NULL }


check_date <- function(start_date, end_date){

  start_status <- tryCatch({
    lubridate::ymd(start_date)
  }, warning = function(w){
    stop("start_date must be in YYYY-MM-DD format", call. = FALSE)
  })

  end_status <- tryCatch({
    lubridate::ymd(end_date)
  }, warning = function(w){
    stop("end_date must be in YYYY-MM-DD format", call. = FALSE)
  })

  if(lubridate::ymd(start_date) > lubridate::ymd(end_date)){
    stop("start_date is greater than end_date")
  }

}


format_datetime <- function(x, ...) {
  format(x, "%Y-%m-%dT%H:%M:%S")
}


has_internet <- function(...) {
  z <- try(suppressWarnings(
    readLines('https://www.google.com', n = 1)
    ), silent = TRUE)
  !inherits(z, "try-error")
}


check_ki_response <- function(response) {
  # Check for query error
  if(inherits(response, "error")){
    stop("Query returned error: ", raw$message)
  }

  # Check for timeout / 404
  if(!inherits(response, "response")){
    stop("Check that KiWIS hub is accessible via a web browser.")
  }
}
