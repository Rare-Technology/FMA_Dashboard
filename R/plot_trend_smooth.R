# plot_trend_smooth(rarefma::init_data_filtered, species, f = count_unique)

plot_trend_smooth <- function(.data, var, f,
                              title = "", y_title = "",
                              loess_span = 0.5, ymin = NA, ymax = NA) {
  

  .data <- .data %>%
    dplyr::group_by(country, yearmonth) %>%
    dplyr::summarise(result = f({{ var }}, na.rm = TRUE))

  if(nrow(.data) <= MIN_DATA_ROWS) return(list(p = NO_PLOT_ATTEMP, trend = NO_TREND_ATTEMP))
  
  mod <- glm(result ~ yearmonth,
    family = gaussian,
    data = .data
  )

  indicator_trend <- trend_indicator(mod)
  indicator_color <- trend_color(mod)
  
  # Number of species recorded
  p <- try(.data %>%
    ggplot(aes(yearmonth, result)) +
    geom_point(
      col = indicator_color,
      pch = 16,
      cex = 3,
      alpha = 0.7
    ) +
    geom_smooth(
      method = "loess",
      span = loess_span,
      col = indicator_color,
      fill = indicator_color,
      alpha = 0.1
    ) +
    labs(
      x = "",
      y = y_title,
      title = title,
      subtitle = indicator_trend
    ) +
    scale_y_continuous( # breaks = integer_breaks(),
      limits = c(ymin, ymax),
      oob = scales::squish
    ) +
    theme_rare(subtitle_color = indicator_color),
    silent = TRUE)

  list(plot = p, trend = indicator_trend)
}
