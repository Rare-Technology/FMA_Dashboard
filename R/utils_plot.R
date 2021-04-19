trend_indicator <- function(mod) {
  
  coefval <- coef(mod)[2]
  p <- summary(mod)$coef[, "Pr(>|t|)"][2]
  res <- "No change"
  if(is.na(coefval) | is.na(p)) return(res)
  
  if(coefval > 0 & p < 0.05) res <- "Increasing"
  if(coefval < 0 & p < 0.05) res <- "Decreasing"
  
  res

}

trend_color <- function(mod) {
  coefval <- coef(mod)[2]
  p <- summary(mod)$coef[, "Pr(>|t|)"][2]
  col <- "grey20"
  if(is.na(coefval) | is.na(p)) return(col)
  
  if(coefval > 0 & p < 0.05) col <- "darkgreen"
  if(coefval < 0 & p < 0.05) col <- "darkred"
  
  col
}

color_froese <- function(indicator, reference) {
  ifelse(indicator > max(reference), "darkgreen",
    ifelse(indicator < min(reference), "red", "darkorange")
  )[[1]]
}


color_froese_Pmat <- function(indicator) { ifelse(indicator > 90, "darkgreen", ifelse(indicator < 50, "red", "darkorange"))[[1]] }

color_froese_Popt <- function(indicator) { ifelse(indicator > 80, "darkgreen", ifelse(indicator < 50, "red", "darkorange"))[[1]] }

color_froese_Pmega <- function(indicator) { ifelse(indicator > 40, "red", ifelse (indicator < 20 ,"darkorange", "darkgreen"))[[1]] }

theme_rare <- function(rotate_x = FALSE, subtitle_color = "black") {

  # https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r
  theme <- hrbrthemes::theme_ipsum_rc(
    axis_title_size = 14,
    axis_title_just = "cc",
    plot_margin = margin(5, 5, 5, 5),
    axis_text_size = 13
  ) +
    theme(
      legend.text = element_text(size = 14),
      plot.title = element_text(size = 22),
      plot.subtitle = element_text(size = 15, colour = subtitle_color)
    )

  if (rotate_x) {
    theme <- theme + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }

  theme
}
