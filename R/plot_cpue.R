plot_cpue <- function(.data, loess_span, ymin = NA, ymax = NA, use_MA_facet = FALSE) {

  # Estimate sum catch per fisher per day
  if (use_MA_facet) {
    catch_cpue <- .data %>%
      dplyr::group_by(yearmonth, maa, fisher_id) %>%
      dplyr::summarise(sum_weight_kg = sum(weight_kg,
                                           na.rm = TRUE
      )) %>%
      dplyr::group_by(yearmonth, maa) %>%
      dplyr::summarise(cpue_kg = round(mean(sum_weight_kg,
                                            na.rm = TRUE
      ), 2))
  } else {
    catch_cpue <- .data %>%
      dplyr::group_by(yearmonth, fisher_id) %>%
      dplyr::summarise(sum_weight_kg = sum(weight_kg,
        na.rm = TRUE
      )) %>%
      dplyr::group_by(yearmonth) %>%
      dplyr::summarise(cpue_kg = round(mean(sum_weight_kg,
        na.rm = TRUE
      ), 2))
  }
  if(nrow(.data) <= MIN_DATA_ROWS) return(list(p = NO_PLOT_ATTEMP, trend = NO_TREND_ATTEMP))
  
  if (use_MA_facet) {
    for (ma_name in unique(.data$maa)) {
      mod <- glm(cpue_kg ~ yearmonth,
        family = gaussian,
        data = catch_cpue %>% dplyr::filter(maa == ma_name)
      )
      indicator_trend_out <- trend_indicator(mod)
      catch_cpue$indicator_trend[catch_cpue$maa == ma_name] <- indicator_trend_out$res
      catch_cpue$indicator_color[catch_cpue$maa == ma_name] <- trend_color(mod)
      pval <- indicator_trend_out$pval
      catch_cpue$pval[catch_cpue$maa == ma_name] <- ifelse(!is.na(pval) & pval < 0.05,
        ifelse(pval < 0.001,
          "(p < 0.001)",
          paste0("(", paste("p =", round(pval, 3)), ")")
        ),
        ""
      )
      catch_cpue <- catch_cpue %>%
        tidyr::unite("trend_subtitle", c('indicator_trend', 'pval'), sep=" ", remove = FALSE) %>% 
        tidyr::unite("maa_trend", c('maa', 'trend_subtitle'), sep = "\n", remove = FALSE)
    }
    mod <- glm(cpue_kg ~ yearmonth,
               family = gaussian,
               data = catch_cpue
    )
    
    catch_cpue_all <- catch_cpue %>% dplyr::select(-maa_trend)
    catch_cpue_max <- catch_cpue %>% dplyr::filter(cpue_kg == max(cpue_kg, na.rm=TRUE)) %>% head(1)
    
    p <- try(
      ggplot(catch_cpue, aes(yearmonth, cpue_kg)) +
        geom_smooth(
          data = catch_cpue_all,
          aes(
            x = yearmonth,
            y = cpue_kg,
            linestype = "Combined MA's"
          ),
          color = 'blue',
          fill = 'blue',
          method = "loess",
          span = loess_span,
          alpha = 0.1,
          show.legend=TRUE
        ) +
        geom_point(
          aes(col = indicator_trend),
          pch = 16,
          cex = 3,
          alpha = 0.7,
          show.legend = FALSE
        ) +
        geom_smooth(
          aes(color = indicator_trend, fill = indicator_trend, linetype = "Individual MA"),
          method = "loess",
          span = loess_span,
          alpha = 0.1,
          show.legend = FALSE
        ) +
        labs(
          title = "CPUE (catch in kg/trip)",
          x = "",
          y = "CPUE (kg/trip)"
        ) +
        scale_y_continuous(limits = c(ymin, ymax), oob = scales::squish) +
        facet_wrap(~ maa_trend, ncol = 2) +
        scale_fill_manual(values = c(
          "No change" = "grey20",
          "Increasing" = "darkgreen",
          "Decreasing" = "darkred"
        ),
        guide = FALSE) +
        scale_color_manual(values = c(
          "No change" = "grey20",
          "Increasing" = "darkgreen",
          "Decreasing" = "darkred",
          "Combined MA's" = "blue"
        ),
        guide = FALSE) +
        scale_linetype_manual(name = "",
          values = c("Combined MA's" = "dashed", "Individual MA" = "solid")) +
        theme_rare(), silent = TRUE)
    
    indicator_trend <- NULL
  } else {
    mod <- glm(cpue_kg ~ yearmonth,
      family = gaussian,
      data = catch_cpue
    )
    indicator_trend_out <- trend_indicator(mod)
    indicator_trend <- indicator_trend_out$res
    indicator_color <- trend_color(mod)
    pval <- indicator_trend_out$pval
    pval <- ifelse(!is.na(pval) & pval < 0.05,
     ifelse(pval < 0.001,
        "(p < 0.001)",
        paste0("(", paste("p =", round(pval, 3)), ")")
     ),
     ""
    )

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
        subtitle = paste(indicator_trend, pval),
        x = "",
        y = "CPUE (kg/trip)"
      ) +
      scale_y_continuous(limits = c(ymin, ymax), oob = scales::squish) +
      theme_rare(subtitle_color = indicator_color), silent = TRUE)
  }

  list(plot = p, trend = indicator_trend, data = catch_cpue)
}
