# plot_trend_smooth(rarefma::init_data_filtered, species, f = count_unique)

plot_trend_smooth <- function(.data, var, f,
                              title = "", y_title = "",
                              loess_span = 0.5, ymin = NA, ymax = NA,
                              use_MA_facet = FALSE, use_family_facet = FALSE) {
  if (use_MA_facet) {
    .data <- .data %>% 
      dplyr::group_by(country, maa, yearmonth) %>% 
      dplyr::summarise(result = f({{ var }}, na.rm = TRUE)) %>% 
      dplyr::ungroup()
  } else {
    .data <- .data %>%
      dplyr::group_by(country, yearmonth) %>%
      dplyr::summarise(result = f({{ var }}, na.rm = TRUE)) #%>% 
      # dplyr::ungroup()
  }
    
  if(nrow(.data) <= MIN_DATA_ROWS) return(list(p = NO_PLOT_ATTEMP, trend = NO_TREND_ATTEMP))
  
  if (use_MA_facet) {
    for (ma_name in unique(.data$maa)) { # regression for each maa
      mod <- glm(result ~ yearmonth,
                 family = gaussian,
                 data = .data %>% dplyr::filter(maa == ma_name))
      .data$indicator_trend[.data$maa == ma_name] <- trend_indicator(mod)
      .data$indicator_color[.data$maa == ma_name] <- trend_color(mod)
      .data <- .data %>% tidyr::unite("maa_trend", c('maa', 'indicator_trend'), sep = "\n", remove = FALSE)
    }
    
    mod <- glm(result ~ yearmonth,
               family = gaussian,
               data = .data
    )
    
    .data_all <- .data %>% dplyr::select(-maa_trend)
    
    p <- try(
      ggplot(.data, aes(yearmonth, result)) +
        geom_smooth(
          data = .data_all,
          aes(
            x = yearmonth,
            y = result
          ),
          color = 'blue',
          fill = 'blue',
          method = "loess",
          span = loess_span,
          alpha = 0.1
        ) +
        geom_point(
          aes(color = indicator_trend),
          pch = 16,
          cex = 1,
          alpha = 0.7,
          show.legend = FALSE
        ) +
        geom_smooth(
          aes(color = indicator_trend, fill = indicator_trend),
          method = "loess",
          span = loess_span,
          alpha = 0.1,
          show.legend = FALSE
        ) +
        labs(
          x = "",
          y = y_title,
          title = title
        ) +
        scale_y_continuous( # breaks = integer_breaks(),
          limits = c(ymin, ymax),
          oob = scales::squish
        ) +
        facet_wrap(~ maa_trend) +
        scale_fill_manual(values = c(
          "No change" = "grey20",
          "Increasing" = "darkgreen",
          "Decreasing" = "darkred"
        )) +
        scale_color_manual(values = c(
          "No change" = "grey20",
          "Increasing" = "darkgreen",
          "Decreasing" = "darkred"
        )) +
        theme_rare(),
      silent = TRUE)
        
      indicator_trend <- NULL # to show warning message on interpretation tab
  } else {
    mod <- glm(result ~ yearmonth,
      family = gaussian,
      data = .data
    )
    indicator_trend <- trend_indicator(mod)
    indicator_color <- trend_color(mod)
    
    p <- try(
      ggplot(data = .data, aes(yearmonth, result)) +
        geom_point(
          color = indicator_color,
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
    }
  
  list(plot = p, trend = indicator_trend, data = .data)
}
