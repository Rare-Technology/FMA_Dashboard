plot_cpue <- function(.data, loess_span, ymin = NA, ymax = NA) {

  # Estimate sum catch per fisher per day
  catch_cpue <- .data %>%
    dplyr::group_by(yearmonth, fisher_id) %>%
    dplyr::summarise(sum_weight_kg = sum(weight_kg,
      na.rm = TRUE
    )) %>%
    dplyr::group_by(yearmonth) %>%
    dplyr::summarise(cpue_kg = round(mean(sum_weight_kg,
      na.rm = TRUE
    ), 2))

  if(nrow(.data) <= MIN_DATA_ROWS) return(list(p = NO_PLOT_ATTEMP, trend = NO_TREND_ATTEMP))
  
  
  mod <- glm(cpue_kg ~ yearmonth,
    family = gaussian,
    data = catch_cpue
  )


    indicator_trend <- trend_indicator(mod)
    indicator_color <- trend_color(mod)
  


  p <- try(catch_cpue %>%
    ggplot(aes(yearmonth, cpue_kg)) +
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
      title = "CPUE (catch in kg/trip)",
      subtitle = indicator_trend,
      x = "",
      y = "CPUE (kg/trip)"
    ) +
    scale_y_continuous(limits = c(ymin, ymax), oob = scales::squish) +
    theme_rare(subtitle_color = indicator_color), silent = TRUE)

  list(plot = p, trend = indicator_trend, data = catch_cpue)
}
