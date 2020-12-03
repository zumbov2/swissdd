#' Get national results and counting status in real time or for selected dates or a time range in the past
#'
#' \code{get_nationalvotes} is one of the two main functions of swissvote package. It allows to retrieve the results and the counting status for national ballots.
#'
#' get_nationalvotes - retrieve vote results for national ballots at district- or municipality level for selected dates or a given date range.
#'
#' @param geolevel geographical level for which the results should be loaded. Options: "national", "canton", "district", "municipality" or "zh_counting_districts".#' @param votedates dates of the ballots to be selected. Default: most recent ballot available. Format: "YYYY-MM-DD".
#' @param votedates dates of the ballots to be selected. Default: most recent ballot available. Format: "YYYY-MM-DD".
#' @param from_date starting point in time from which vote results should be retrieved. Format: "YYYY-MM-DD".
#' @param to_date end point in time to which vote results should be retrieved. Format: "YYYY-MM-DD".
#' 
#' @importFrom purrr map_dfr
#' @importFrom lubridate ymd
#' 
#' @export
#' 
#' @return a tibble containing the results
#' 
#' @examples
#' 
#' # Selection by range
#' results <- get_nationalvotes(
#'     geolevel = "district", 
#'     from_date = "2018-01-01",
#'     to_date = "2018-12-31"
#'     )
#' 
#' # Selection by end date only
#' get_nationalvotes(to_date = "1983-12-04")
#'  
#' # Selection of a specific vote date
#' get_nationalvotes(votedates = "2014-02-09")
#'
get_nationalvotes <- function(geolevel = "municipality", votedates = NULL, from_date = NULL, to_date = NULL) {
  
  # Check inputs
  if (!geolevel %in% c("national", "canton", "district", "municipality", "zh_counting_districts")) stop("Please select valid 'geolevel'.")
  if (!is.null(votedates) & (!is.null(from_date) | !is.null(to_date))) stop("Please select the vote dates either with 'votedates' OR via a range ('from_date' / 'to_date').")
  
  # Parse dates
  if (!is.null(votedates)) votedates <- lubridate::ymd(votedates)
  if (!is.null(from_date)) from_date <- lubridate::ymd(from_date)
  if (!is.null(to_date)) to_date <- lubridate::ymd(to_date)
  
  # Call base api
  call_res <- call_base_api(geolevel = "national")
  available_dates <- available_votedates(geolevel = "national", call_res)
  
  # Define vote dates to be fetched
  if (is.null(votedates) & is.null(from_date) & is.null(to_date)) votedates <- max(available_dates)
  if (!is.null(from_date) | !is.null(to_date)) {
    
    if (is.null(from_date)) from_date <- lubridate::ymd(min(available_dates))
    if (is.null(to_date)) to_date <- lubridate::ymd(max(available_dates))
    votedates <- available_dates[available_dates >= from_date & available_dates <= to_date]
    
  }
  
  # Check votedates
  if (sum(!votedates %in% available_dates) > 0) stop("One or more 'votedates' not found, please call available_votedates() to check which dates are available. Also check if the format is correct (YYYY-MM-DD).")
  
  # Iterate over dates and create dataframe
  votedata <- purrr::map_dfr(
    votedates, 
    swiss_json_to_dfr,
    geolevel = geolevel,
    call_res = call_res
  )
  
  # Return
  return(votedata)
  
}