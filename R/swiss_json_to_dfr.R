#' Transform an opendata.swiss national results json into a tibble
#'
#' \code{swiss_json_to_dfr} transforms the json containing the results of a selected federal votedate into a tibble.
#'
#' @param votedate date of the ballot. Default: most recent ballot available. To select multiple ballots use the 'get_swissvotes'-function. Format = YYYYMMDD
#' @param geolevel geographical level for which the results should be loaded. Options: "national", "canton", "district" or "municipality".
#' @param dataurl url of the dataset on opendata.swiss
#' @param index selection by index of the resource (last published = 1).
#' @param call_res result of a previous call to the base API. Optional argument.
#' 
#' @importFrom httr GET http_error content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom purrr map_chr map
#' @importFrom dplyr "%>%" bind_cols rename filter bind_rows
#' @importFrom tidyr unnest unpack
#' @importFrom lubridate ymd
#' 
#' @return a tibble containing the results
#' 
#' @export
#' 
#' @examples
#' 
#' # Transform the json of the most recent vote
#' results <- swiss_json_to_dfr()
#'
#' # Transform the json of a selected votedate
#' swiss_json_to_dfr(votedate = "2019-02-10")
#'
swiss_json_to_dfr <- function(votedate = NULL, geolevel = "municipality", dataurl = NULL, index = NULL, call_res) {
  
  # Get urls
  if (is.null(dataurl)) {
    
    urls <- get_urls(geolevel = "national", call_res = call_res)
    if (is.null(votedate)) votedate <- max(urls[["date"]])
    dataurl <- urls[urls[["date"]] == votedate,][["download_url"]]
    
  }
  
  # Index
  if (!is.null(index)) dataurl <- dataurl[index]
  if (length(dataurl) > 1) stop("This is not a vectorised function. Only one URL can be queried at a time.")
  
  # Download data
  res <- httr::GET(dataurl)
  if (httr::http_error(res)) message("The API does not respond properly. Do you have an internet connection and an open proxy?")
  res_data <- suppressWarnings(jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8")))
  
  # Simplification
  data_national <- res_data[["schweiz"]][["vorlagen"]]
  data_cantons <- res_data[["schweiz"]][["vorlagen"]][["kantone"]]
  
  # Geolevel specific extraction
  if (geolevel == "national") {
    
    findata <- tibble::tibble(
      id = data_national[["vorlagenId"]],
      name = purrr::map_chr(data_national[["vorlagenTitel"]], c(2,1))) %>%
      dplyr::bind_cols(data_national[["resultat"]])
    
  }
  if (geolevel == "canton") {
    
    findata <- tibble::tibble(
      canton_id = purrr::map(data_cantons, 1),
      canton_name = purrr::map(data_cantons, 2),
      name = purrr::map_chr(data_national[["vorlagenTitel"]], c(2,1)),
      id = data_national[["vorlagenId"]],
      res = purrr::map(data_cantons, 3)
    ) %>% 
      tidyr::unnest(c(canton_id, canton_name, res))
    
  }
  if (geolevel == "district") {
    
    findata <- tibble::tibble(
      name = purrr::map_chr(data_national[["vorlagenTitel"]], c(2,1)),
      id = data_national[["vorlagenId"]],
      canton_id = purrr::map(data_cantons, 1),
      canton_name = purrr::map(data_cantons, 2),
      res = purrr::map(res_data[["schweiz"]][["vorlagen"]][["kantone"]], "bezirke")
      ) %>%
      tidyr::unnest(c(res, canton_id, canton_name)) %>% 
      tidyr::unnest(res) %>% 
      dplyr::rename(
        district_id = geoLevelnummer, 
        district_name = geoLevelname
      ) %>%
      tidyr::unpack(resultat) 
    
  }
  if (geolevel == "zh_counting_districts" & is.list(data_cantons[[1]]$zaehlkreise)) {
    
    zaehlkreise <- tibble::tibble(
      name = purrr::map_chr(data_national[["vorlagenTitel"]], c(2,1)),
      id = data_national[["vorlagenId"]],
      canton_id = "1",
      canton_name = data_cantons[[1]][["geoLevelname"]][[1]],
      res = purrr::map(data_cantons, "zaehlkreise")
    ) %>% 
      tidyr::unnest(res) %>% 
      tidyr::unnest(res) %>% 
      tidyr::unpack(resultat) %>% 
      dplyr::rename(
        mun_id = geoLevelnummer, 
        mun_name = geoLevelname
      )
    
  }  
  if (geolevel %in% c("municipality", "zh_counting_districts")){   
    
    findata <- tibble::tibble(
      name = purrr::map_chr(data_national[["vorlagenTitel"]], c(2,1)),
      id = data_national[["vorlagenId"]],
      canton_id = purrr::map(data_cantons, 1),
      canton_name = purrr::map(data_cantons, 2),
      res = purrr::map(res_data[["schweiz"]][["vorlagen"]][["kantone"]], "gemeinden")
    ) %>%
      tidyr::unnest(c(res, canton_id, canton_name)) %>% 
      tidyr::unnest(res) %>% 
      tidyr::unpack(resultat) %>% 
      dplyr::rename(
        mun_id = geoLevelnummer, 
        mun_name = geoLevelname
      )
    
    # Add results for counting districts 
    if(geolevel == "zh_counting_districts" & is.list(data_cantons[[1]]$zaehlkreise)){
      
      findata <- findata %>%
        dplyr::filter(!mun_id %in% c(261, 230)) %>%
        dplyr::bind_rows(zaehlkreise)
      
    }
    
    
  }
  
  # Add votedate
  if (is.null(votedate)) {
    
    urls <- get_urls(geolevel = "national", call_res = call_res)
    votedate <- urls[urls[["download_url"]] == dataurl,][["date"]]
    
  }
  findata$votedate <- lubridate::ymd(votedate)
  
  # Return
  return(findata)
  
}