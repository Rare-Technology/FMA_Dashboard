plot_average_length <- function(.data, loess_span = 0.5) {
  .data <- .data %>%
    dplyr::group_by(country, yearmonth) %>%
    dplyr::summarise(avg_length = mean(length,
      na.rm = TRUE
    ))
  # trend
  # glm
  mod <- glm(avg_length ~ yearmonth,
    family = gaussian,
    data = .data
  )

  indicator_trend <- trend_indicator(mod)
  indicator_color <- trend_color(mod)

  # Number of species recorded
  .data %>%
    ggplot(aes(yearmonth,
      avg_length,
      colour = ma_name
    )) +
    geom_point(aes(col = species),
      pch = 16,
      color = indicator_color,
      cex = 3,
      alpha = 0.7
    ) +
    geom_smooth(
      method = "loess",
      # formula = "y ~ x",
      span = loess_span,
      col = indicator_color,
      fill = indicator_color,
      alpha = 0.1
    ) +
    labs(
      x = "",
      y = "Average Length (cm)",
      title = "Average length",
      subtitle = indicator_trend
    ) +
    scale_y_continuous(
      breaks = integer_breaks(),
      limits = c(
        0,
        max(.data$avg_length)
      ),
      oob = scales::squish
    ) +
    theme_rare(subtitle_color = indicator_color)
}
