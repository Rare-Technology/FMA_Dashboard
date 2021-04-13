plot_species_composition <- function(.data, loess_span){
  
  .data <-.data %>%
    dplyr::group_by(country, yearmonth) %>%
    dplyr::summarise (species_count = count_unique(species))
  
  #glm
  mod <- glm(species_count ~ yearmonth, 
             family = gaussian,
             data = .data)
  
  indicator_trend <- trend_indicator(mod)
  indicator_color <- trend_color(mod)
  
  #Number of species recorded
  .data %>%
    ggplot(aes(yearmonth, 
               species_count)) +
    geom_point(col=indicator_color, 
               pch=16, 
               cex=3,
               alpha = 0.7) + 
    geom_smooth(method="loess", 
                span = loess_span,
                col= indicator_color, 
                fill = indicator_color, 
                alpha = 0.1) +
    labs(
      x = "",
      y = "Total number of species recorded in the catch",
      title = "Number of species in the catch",
      subtitle = indicator_trend
    ) +
    scale_y_continuous(breaks = integer_breaks(),
                       limits = c(0, max(.data$species_count)),
                       oob = scales::squish) +
      theme_rare(subtitle_color = indicator_color)

}