plot_fishing_gear <- function(.data, data_source, state) {
  .data <- .data %>%
    dplyr::filter(gear_type != "") %>%
    dplyr::group_by(country, yearmonth, gear_type) %>%
    dplyr::summarise(gear_count = dplyr::n()) %>%
    dplyr::arrange(gear_type) %>%
    dplyr::mutate(
      gear_type = factor(gear_type, levels = unique(gear_type))
    )

  if(nrow(.data) <= MIN_DATA_ROWS) return(list(p = NO_PLOT_ATTEMP, trend = NO_TREND_ATTEMP))
  
  maxcount <- .data$gear_count

  gear_types <- c(
    "Beach seine",
    "Freediving",
    "Gillnet",
    "Handline",
    "Harpoon",
    "Longline",
    "Net",
    "Scuba diving",
    "Spear gun"
  )

  
  # levels(.data$gear_type) <- list(
  #   tr(state, "Beach seine") := "Beach seine",
  #   tr(state, "Freediving") := "Freediving",
  #   tr(state, "Gillnet") := "Gillnet",
  #   tr(state, "Handline") := "Handline",
  #   tr(state, "Harpoon") := "Harpoon",
  #   tr(state, "Longline") := "Longline",
  #   tr(state, "Net") := "Net",
  #   tr(state, "Scuba diving") := "Scuba diving",
  #   tr(state, "Spear gun") := "Spear gun"
  # )
  # ggsci::pal_npg(palette = c("nrc"), alpha = 1)(9)

  gear_colors <- c(
    "#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF",
    "#F39B7FFF", "#8491B4FF", "#91D1C2FF", "#DC0000FF",
    "#7E6148FF"
  )
  
  if (data_source == "historical") return(list(p = HISTORICAL_WARNING, trend = NO_TREND_ATTEMP))
  
  p <- try(.data %>%
    ggplot(aes(
      x = yearmonth,
      y = gear_count,
      group = gear_type,
      fill = gear_type
    )) +
    geom_col(aes(fill = gear_type)) +
    scale_fill_manual(name = "", breaks = gear_types, values = gear_colors) +
    # scale_color_npg(alpha=0.9)+
    scale_y_continuous(
      breaks = integer_breaks(),
      limits = c(0, max(maxcount)),
      oob = scales::squish
    ) +
    labs(
      x = "",
      y = tr(state, "Number gear type reported each month"),
      title = tr(state, "Type of fishing gear reported"),
      fill = tr(state, "Gear type")
    ) +
    theme_rare(), silent = TRUE)


  trend <- ifelse("Destructive" %in% .data$gear_type, "Destructive", "No destructive")
  
  list(plot = p, trend = trend, data = .data)
}
