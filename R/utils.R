#' @importFrom httr GET http_error
#'
#' @noRd
call_base_api <- function(geolevel = "national") {
  
  if (geolevel == "national") res <- httr::GET("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-eidgenoessischen-abstimmungsvorlagen")
  if (geolevel == "canton") res <- httr::GET("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-kantonalen-abstimmungsvorlagen")
  if (httr::http_error(res)) message("The API does not respond properly. Do you have an internet connection and an open proxy?")
  return(res)
  
}

#' @importFrom httr content 
#' @importFrom tibble tibble 
#' @importFrom purrr map 
#'
#' @noRd
get_urls <- function(geolevel = "national", call_res) {
  
  if (missing(call_res)) call_res <- call_base_api(geolevel = geolevel)
  cnt <- httr::content(call_res)
  resources <- cnt[["result"]][["resources"]]
  
  urls <- tibble::tibble(
    date =  unlist(purrr::map(resources, "coverage")),
    download_url = unlist(purrr::map(resources, "download_url"))
    )
  
  return(urls)
  
}

#' @importFrom httr GET http_error
#'
#' @noRd
call_geodata_api <- function() {
  
  res <- httr::GET("https://opendata.swiss/api/3/action/package_show?id=geodaten-zu-den-eidgenoessischen-abstimmungsvorlagen")
  if (httr::http_error(res)) message("The API does not respond properly. Do you have an internet connection and an open proxy?")
  return(res)
  
}

#' @importFrom dplyr mutate case_when
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_manual guide_legend 
#'     unit labs theme element_rect element_blank 
#'     element_text scale_fill_viridis_c ggtitle 
#'
#' @noRd
plot_map_national <- function(dt, lakes, legend_title, theme) {
  
  # Base plot
  if (theme == "srf") {
    
    # Prepare data
    dt <- dt %>% 
      dplyr::mutate(
        measure = factor(dplyr::case_when(
          measure < 35 ~ "",
          measure >= 35 & measure < 40 ~ "35", 
          measure >= 40 & measure < 45 ~ "40",
          measure >= 45 & measure < 50 ~ "45",
          measure >= 50 & measure < 55 ~ "50",
          measure >= 55 & measure < 60 ~ "55",
          measure >= 60 & measure < 65 ~ "60",
          measure >= 65 ~ "65"
          ), levels = c("", "35", "40", "45", "50", "55", "60", "65")
        )
      )
    
    # Plot
    p1 <- ggplot2::ggplot(dt) +
      ggplot2::geom_sf(ggplot2::aes(fill = measure), color = "white") +
      ggplot2::scale_fill_manual(
        values = c(
          "#8d0613", "#c91022", "#f1434a", "#ff9193",
          "#91cdff", "#42a2f1", "#1a7bc5", "#105182"
          ),
        drop = F,
        name = "JA-Anteil in %",
        guide = ggplot2::guide_legend(
          direction = "horizontal",
          keyheight = ggplot2::unit(2, units = "mm"),
          keywidth = ggplot2::unit(c(25, rep(7, 6), 25), units = "mm"),
          title.position = "top",
          title.hjust = 0,
          label.hjust = 1,
          nrow = 1,
          byrow = T,
          reverse = T,
          label.position = "bottom",
        )
      ) +
      ggplot2::labs(
        title = unique(dt[["name"]]),
        caption = paste0(
          "Stimmbeteiligung ", 
          round(100 * sum(dt$eingelegteStimmzettel, na.rm = T) / sum(dt$anzahlStimmberechtigte, na.rm = T), 0),
          "%"
          )
      ) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
        legend.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
        panel.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
        panel.grid = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.position = "bottom",
        legend.justification = "right",
        legend.text = ggplot2::element_text(color = "#6b6960"),
        plot.title = ggplot2::element_text(face = "bold"),
        plot.caption = ggplot2::element_text(hjust = 0, colour = "#6b6960")
      )
    
    } else {
    
    p1 <- ggplot2::ggplot(dt) +
      ggplot2::geom_sf(ggplot2::aes(fill = measure), color = "white") +
      ggplot2::scale_fill_viridis_c(option = theme, direction = -1, name = legend_title) +
      ggplot2::ggtitle(unique(dt[["name"]])) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        legend.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.grid = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
      )
    
    }
  
  # Add lakes
  if (!is.null(lakes)) {
    
    if (theme == "srf") p1 <- p1 + ggplot2::geom_sf(data = lakes, fill = "white", color = "white")
    if (!theme == "srf") p1 <- p1 + ggplot2::geom_sf(data = lakes, fill = "#ceefff", color = "#4889c5")
    
  }

  # Display
  p1
      
}

#' @importFrom dplyr mutate case_when
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_manual guide_legend 
#'     unit labs theme element_rect element_blank 
#'     element_text scale_fill_viridis_c ggtitle 
#'
#' @noRd
plot_map_cantonal <- function(dt, legend_title, theme) {
  
  # Base plot
  if (theme == "srf") {
    
    # Prepare data
    dt <- dt %>% 
      dplyr::mutate(
        measure = factor(dplyr::case_when(
          measure < 35 ~ "",
          measure >= 35 & measure < 40 ~ "35", 
          measure >= 40 & measure < 45 ~ "40",
          measure >= 45 & measure < 50 ~ "45",
          measure >= 50 & measure < 55 ~ "50",
          measure >= 55 & measure < 60 ~ "55",
          measure >= 60 & measure < 65 ~ "60",
          measure >= 65 ~ "65"
        ), levels = c("", "35", "40", "45", "50", "55", "60", "65")
        )
      )
    
    # Plot
    ggplot2::ggplot(dt) +
      ggplot2::geom_sf(ggplot2::aes(fill = measure), color = "white") +
      ggplot2::scale_fill_manual(
        values = c(
          "#8d0613", "#c91022", "#f1434a", "#ff9193",
          "#91cdff", "#42a2f1", "#1a7bc5", "#105182"
        ),
        drop = F,
        name = "JA-Anteil in %",
        guide = ggplot2::guide_legend(
          direction = "horizontal",
          keyheight = ggplot2::unit(2, units = "mm"),
          keywidth = ggplot2::unit(c(25, rep(7, 6), 25), units = "mm"),
          title.position = "top",
          title.hjust = 0,
          label.hjust = 1,
          nrow = 1,
          byrow = T,
          reverse = T,
          label.position = "bottom",
        )
      ) +
      ggplot2::labs(
        title = paste0(unique(dt[["de"]]), " (", unique(dt[["canton_name"]]), ")"),
        caption = paste0(
          "Stimmbeteiligung ", 
          round(100 * sum(dt$eingelegteStimmzettel, na.rm = T) / sum(dt$anzahlStimmberechtigte, na.rm = T), 0),
          "%"
        )
      ) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
        legend.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
        panel.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
        panel.grid = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.position = "bottom",
        legend.justification = "right",
        legend.text = ggplot2::element_text(color = "#6b6960"),
        plot.title = ggplot2::element_text(face = "bold"),
        plot.caption = ggplot2::element_text(hjust = 0, colour = "#6b6960")
      )
    
  } else {
    
    ggplot2::ggplot(dt) +
      ggplot2::geom_sf(ggplot2::aes(fill = measure), color = "white") +
      ggplot2::scale_fill_viridis_c(option = theme, direction = -1, name = legend_title) +
      ggplot2::ggtitle(paste0(unique(dt[["de"]]), " (", unique(dt[["canton_name"]]), ")")) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        legend.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.grid = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
      )
    
  }
  
}