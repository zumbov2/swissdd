#' Plot Cantonal Votes
#'
#' \code{plot_cantonalvotes} plots the results of cantonal votes in a choropleth map using ggplot2.
#'
#' @param votedate date of the ballot. Default: most recent ballot available.
#' @param vote_id id of the vote. Default: first id mentioned in the data set.
#' @param geolevel geographical level. Options: district", "municipality" or "zh_counting_districts".
#' @param measure measure to color the administrative units. Options: "result" for the yes vote share or "turnout"
#'     for the voter turnout of a given vote.
#' @param standardize if \code{TRUE}, the scale of the measure ranges from 0 to 100 percent. Recommended for comparisons 
#'     between votes. 
#' @param theme defines basic appearance of the map. Five options are available: "srf" for a theme inspired by the
#'     plots of Swiss Radio and Television, and "A" to "E" for the viridis color scales magma, inferno, plasma, viridis and cividis.
#' 
#' @return a ggplot object
#' 
#' @importFrom lubridate ymd
#' @importFrom dplyr left_join rename bind_rows
#' @importFrom tibble tibble
#' 
#' @examples
#' 
#' # Plot the most recent cantonal vote
#' plot_cantonalvotes()
#' 
#' # Plot a specific cantonal vote
#' plot_cantonalvotes(votedate = "2020-02-09", vote_id = 104945)
#' 
#' @export
plot_cantonalvotes <- function(votedate = NULL, vote_id = NULL, geolevel = "municipality", measure = "result", 
                               standardize = T, theme = "srf") {
  
  # Check inputs
  if (!geolevel %in% c("district", "municipality", "zh_counting_districts")) stop("Please select valid 'geolevel'.")
  if (!measure %in% c("result", "turnout")) stop("Please select valid 'measure'.")
  if (!theme %in% c("srf", "A", "B", "C", "D", "E")) stop("Please select valid 'theme'.")
  
  # API calls
  call_res_geodata <- call_geodata_api()
  call_res_base <- call_base_api(geolevel = "canton")
  available_dates <- available_votedates(geolevel = "canton", call_res_base)
  
  # Vote date
  if (!is.null(votedate)) votedate <- lubridate::ymd(votedate)
  if (is.null(votedate)) votedate <- max(available_dates)
  if (sum(!votedate %in% available_dates) > 0) stop("'votedate' not found, please call available_votedates() to check which dates are available. Also check if the format is correct (YYYY-MM-DD).")
  
  # Fetch vote data
  vote_data <- get_cantonalvotes(geolevel = geolevel, votedates = votedate)
  if (is.null(vote_id)) vote_id <- unique(vote_data[["id"]])[1]
  vote_data <- vote_data[vote_data[["id"]] == vote_id,]
  if (nrow(vote_data) == 0) stop ("No data found for the specified 'vote_id'")
  
  # Join geo with vote data 
  if (geolevel == "municipality") {
    
    pd <- dplyr::left_join(
      get_geodata(geolevel = geolevel, call_res = call_res_geodata),
      vote_data,
      by = "mun_id"
    )
    
  }
  if (geolevel == "district") {
    
    pd <- dplyr::left_join(
      get_geodata(geolevel = geolevel, call_res = call_res_geodata),
      vote_data,
      by = "district_id"
    )
    
  }
  if (geolevel == "zh_counting_districts") {
    
    pd <- dplyr::left_join(
      get_geodata(geolevel = geolevel, call_res = call_res_geodata),
      vote_data,
      by = "mun_id"
    )
    
  }
  
  # Filter data by canton
  pd <- pd[!is.na(pd[["jaStimmenInProzent"]]),]
  
  # Measure
  if (measure == "result") {
    
    pd <- pd %>% dplyr::rename(measure = jaStimmenInProzent)
    legend_title <- "jaStimmenInProzent"
    
  }
  if (measure == "turnout") {
    
    pd <- pd %>% dplyr::rename(measure = stimmbeteiligungInProzent)
    legend_title <- "stimmbeteiligungInProzent"
    
  }
  
  # Pseudo-standardization
  if (standardize) {
    
    pd2 <- tibble::tibble(
      id = vote_id,
      measure = c(0, 100)
    )
    
    pd <- pd %>% dplyr::bind_rows(pd2)
    
  }
  
  # Base plot
  plot_map_cantonal(pd, legend_title = legend_title, theme = theme)
  
}
