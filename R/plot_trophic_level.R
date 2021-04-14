plot_trophic_level <- function(.data, loess_span) {
  .data <- .data %>%
    dplyr::group_by(country, yearmonth) %>%
    dplyr::summarise(avg_troph = mean(trophic_level,
      na.rm = TRUE
    ))

  # glm
  mod <- glm(avg_troph ~ yearmonth,
    family = gaussian,
    data = .data
  )

  indicator_trend <- trend_indicator(mod)
  indicator_color <- trend_color(mod)

  .data %>%
    ggplot(aes(
      x = yearmonth,
      y = avg_troph
    )) +
    geom_point(
      col = indicator_color,
      pch = 16,
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
      xlab = "",
      ylab = "Average trophic level",
      title = "Average trophic level",
      subtitle = indicator_trend
    ) +
    scale_y_continuous(oob = scales::squish) +
    theme_rare(subtitle_color = indicator_color)
}
