plot_fishing_gear <- function(.data) {
  
  .data <- .data %>%
    dplyr::filter(gear_type != "") %>%
    dplyr::group_by(country, yearmonth, gear_type) %>%
    dplyr::summarise(gear_count = dplyr::n()) %>%
    dplyr::arrange(gear_type) %>%
    dplyr::mutate(
      gear_type = factor(gear_type, levels = unique(gear_type))
    )


  maxcount <- .data$gear_count

  gear_types <- c(
    "Beach seine", "Freediving", "Gillnet", "Handline", "Harpoon",
    "Longline", "Net", "Scuba diving", "Spear gun"
  )

  #ggsci::pal_npg(palette = c("nrc"), alpha = 1)(9)
  gear_colors <- c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF",
  "#F39B7FFF", "#8491B4FF", "#91D1C2FF", "#DC0000FF",
  "#7E6148FF")

  .data %>%
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
      y = "Number gear type reported each month",
      title = "Type of fishing gear reported",
      fill = "Gear type"
    ) +
    theme_rare()
}
