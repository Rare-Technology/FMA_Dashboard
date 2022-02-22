trend_indicator <- function(mod) {
  
  coefval <- coef(mod)[2]
  p <- summary(mod)$coef[, "Pr(>|t|)"][2]
  res <- "No change"
  if(is.na(coefval) | is.na(p)) return(list(res=res, pval=p))
  
  if(coefval > 0 & p < 0.05) res <- "Increasing"
  if(coefval < 0 & p < 0.05) res <- "Decreasing"
  
  list(res=res, pval=p)

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

label_percent <- function(pct) {
  if (pct >= 0.1) {
    paste(round(pct, 1), "%")
  } else if (pct > 0) {
    "< 0.1%"
  } else {
    "0%"
  }
}

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

display_filters <- function(state, html=TRUE) {
  if (html == TRUE) {
  out <- c('<div class="column">
           <strong>', tr(state, "Country"), '</strong><br>&emsp;&emsp;',
      paste(state$country$selected, collapse=', '),
    '<br>',
    '<strong>', tr(state, "Subnational unit"), '</strong><br>&emsp;&emsp;',
      paste(state$subnational$selected, collapse=', '),
    '<br>',
    '<strong>', tr(state, "Local government unit"), '</strong><br>&emsp;&emsp;',
      paste(state$local$selected, collapse=', '),
    '<br>',
    '<strong>', tr(state, "Managed access area"), '</strong><br>&emsp;&emsp;',
      paste(state$maa$selected, collapse=', '),
    '</div>
    <div class="column">
    <strong>', tr(state, "Date"), '</strong><br>&emsp;&emsp;',
      paste(state$current_date_range$valmin, state$current_date_range$valmax, sep=' to '),
    '<br>')
  if (length(state$family$selected) == length(state$family$choices)) {
    out <- c(out,
             '<strong>', tr(state, "Family"), '</strong>
             <details>
                  <summary>&emsp;&emsp;', tr(state, "All selected."),
                  ' <u>', tr(state, "Click to show."), '</u></summary>',
                  paste(state$family$selected, collapse=', '),
             '</details>')
  } else {
    out <- c(out,
             '<strong>', tr(state, "Family"), '</strong>
             <details>
                <summary&emsp;&emsp;><u>', tr(state, "Click to show."), '</u></summary>',
                paste(state$family$selected, collapse = ', '),
             '</details>')
  }
  if (length(state$species$selected) == length(state$species$choices)) {
    out <- c(out,
             '<strong>', tr(state, "Species"), '</strong><details>
                <summary>&emsp;&emsp;', tr(state, "All selected."),
                ' <u>', tr(state, "Click to show."), '</u></summary>',
                paste(state$species$selected, collapse=', '),
             '</details>')
  } else {
    out <- c(out,
             '<strong>', tr(state, "Species"), '</strong>
             <details>
                <summary>&emsp;&emsp;<u>', tr(state, "Click to show."), '</u></summary>',
                paste(state$species$selected, collapse=', '),
             '</details>')
  }
  if (!state$current_indicator %in% c(tr(state, "Fishing Gear"),
                                      tr(state, "Size Structure"),
                                      tr(state, "Size Proportions"))) {
    out <- c(out,
             '<strong>', tr(state, "Curve smoothing"), '</strong><br>&emsp;&emsp;',
                paste(state$loess_span),
             '<br>')
  }
  reference_idx <- which(fma_reference_points$`Performance Indicator` == state$current_indicator)
  out <- c(out,
           '<strong>', tr(state, "Indicator description"), '</strong><br>&emsp;&emsp;',
              paste(fma_reference_points$Description[reference_idx]),
            '</div>')
  } else {
    out <- c(tr(state, "Country"), paste(state$country$selected), '\n',
             tr(state, "Subnational unit"), paste(state$subnational$selected, collapse=', '), '\n',
             tr(state, "Local government unit"), paste(state$local$selected, collapse=', '), '\n',
             tr(state, "Managed access area"), paste(state$maa$selected, collapse=', '), '\n',
             tr(state, "Date"), paste(state$current_date_range$valmin, state$current_date_range$valmax, sep=' to '), '\n',
             tr(state, "Family"), paste(state$family$selected), '\n',
             tr(state, "Species"), paste(state$species$selected), '\n')
    out <- paste(out, collapse='\n')
  }
  out
}