plot_reporting_effort <- function(.data, loess_span = 0.5) {
  .data <-.data %>%
    dplyr::group_by(country, week, transaction_date) %>%
    dplyr::summarise(
      fisher_count = count_unique(fisher_id),
      buyer_count = count_unique(buyer_id)
    )
  
  
  if(nrow(.data) <= MIN_DATA_ROWS) return(list(p = NO_PLOT_ATTEMP, trend = NO_TREND_ATTEMP))
  # ---- glm
  mod_fisher <- glm(fisher_count ~ transaction_date,
    family = gaussian,
    data = .data
  )

  mod_buyer <- glm(buyer_count ~ transaction_date,
    family = gaussian,
    data = .data
  )
  
  indicator_trend_fisher_out <- trend_indicator(mod_fisher)
  indicator_trend_fisher <- indicator_trend_fisher_out$res
  pval_fisher <- indicator_trend_fisher_out$pval
  pval_fisher <- ifelse(!is.na(pval_fisher) & pval_fisher < 0.05,
                 ifelse(pval_fisher < 0.001,
                        "(p < 0.001)",
                        paste0("(", paste("p =", round(pval_fisher, 3)), ")")
                 ),
                 ""
  )
  indicator_color_fisher <- trend_color(mod_fisher)
  indicator_trend_buyer_out <- trend_indicator(mod_buyer)
  indicator_trend_buyer <- indicator_trend_buyer_out$res
  pval_buyer <- indicator_trend_buyer_out$pval
  pval_buyer <- ifelse(!is.na(pval_buyer) & pval_buyer < 0.05,
                 ifelse(pval_buyer < 0.001,
                        "(p < 0.001)",
                        paste0("(", paste("p =", round(pval_buyer, 3)), ")")
                 ),
                 ""
  )
  indicator_color_buyer <- trend_color(mod_buyer)

  # Number of fishers reporting
  fishers_plot <- try(.data %>%
    ggplot(aes(transaction_date, fisher_count)) +
    geom_col(
      fill = "grey50",
      alpha = 0.3
    ) +
    geom_smooth(
      method = "loess",
      formula = "y ~ x",
      span = loess_span,
      col = indicator_color_fisher,
      fill = indicator_color_fisher,
      alpha = 0.1
    ) +
    labs(
      title = "Fishers reporting",
      subtitle = paste(indicator_trend_fisher, pval_fisher),
      x = "",
      y = "Total number of fishers reporting per day"
    ) +
    scale_y_continuous(
      breaks = integer_breaks(),
      limits = c(
        0,
        max(.data$fisher_count)
      ),
      oob = scales::squish
    ) +
    theme_rare(rotate_x = TRUE, subtitle_color = indicator_color_buyer),
    silent = TRUE)

  # Numbber of buyers reporting
  buyers_plot <- try(.data %>%
    ggplot(aes(
      transaction_date,
      buyer_count
    )) +
    geom_col(
      fill = "grey50",
      alpha = 0.3
    ) +
    geom_smooth(
      method = "loess",
      span = loess_span,
      col = indicator_color_buyer,
      fill = indicator_color_buyer,
      alpha = 0.1
    ) +
    labs(
      title = "Buyers reporting",
      subtitle = paste(indicator_trend_buyer, pval_buyer),
      x = "",
      y = "Total number of buyers reporting per day"
    ) +
    scale_y_continuous(
      breaks = integer_breaks(),
      limits = c(
        0,
        max(.data$buyer_count)
      ),
      oob = scales::squish
    ) +
    theme_rare(rotate_x = TRUE, subtitle_color = indicator_color_buyer), silent = TRUE)

  if("try-error" %in% c(class(fishers_plot), class(buyers_plot))) 
    return(plot = fishers_plot, trend = NO_TREND_ATTEMP)
  
  p <- suppressWarnings(cowplot::plot_grid(fishers_plot, buyers_plot, ncol = 2))
  
  list(plot = p, trend = unique(indicator_trend_fisher, indicator_trend_buyer),
       subplots = list(fishers_plot, buyers_plot), data = .data)
}
