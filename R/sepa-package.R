#' sepa: Retrieve data from the Scottish Environmental Protection Agency
#'
#' @aliases NULL sepa-package
#' @import checkmate
#' @import httr
#' @import methods
#' @importFrom cli cli_abort
#' @importFrom lubridate is.Date is.POSIXt as_datetime
#' @importFrom dplyr across all_of arrange .data filter mutate
#' @importFrom rlang caller_env
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr %>%
#' @importFrom tibble tibble as_tibble new_tibble
#' @importFrom utils URLencode
#'
#' @examples
#' \dontrun{
#' print("Hello, world")
#' }
"_PACKAGE"
