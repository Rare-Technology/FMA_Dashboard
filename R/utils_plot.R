trend_indicator <- function(mod) {
  ifelse (coef(mod)[2] > 0 & summary(mod)$coef[,"Pr(>|t|)"] < 0.05, "Increasing",
          ifelse(coef(mod)[2] < 0 & summary(mod)$coef[,"Pr(>|t|)"] < 0.05, "Decreasing", "No change"))[[1]]
  
}

trend_color <- function(mod) {
  ifelse(coef(mod)[2] > 0 & summary(mod)$coef[,"Pr(>|t|)"] < 0.05, "darkgreen",
         ifelse(coef(mod)[2] < 0 & summary(mod)$coef[,"Pr(>|t|)"] < 0.05, "darkred", "grey20"))[[1]]
}

theme_rare <- function(rotate_x = FALSE, subtitle_color = "black") {

  # https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r
  theme <- hrbrthemes::theme_ipsum(
    axis_title_size = 14,
    axis_title_just = "cc",
    plot_margin = margin(5, 5, 5, 5)
    ) + 
    theme(
      legend.text = element_text(size=14),
      plot.title = element_text(size=22),
      plot.subtitle = element_text(size=15, colour = subtitle_color)
    )

  if(rotate_x){
    theme <- theme + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  theme
}