# TODO improve the display for percents. currently, plotting the box and labels separately gives mostly
# good, but inconsistent results
plot_size_structure <- function(.data, plot_title, y_title, sel_species, Pmat = -Inf, Pmega = -Inf, Popt = -Inf) {
  .data <- .data %>%
    dplyr::filter(species == sel_species[1], !is.na(length), !is.na(count), !is.na(lmax)) %>%
    dplyr::filter(sum(count) > 100) %>% 
    dplyr::ungroup() %>% 
    droplevels()

  if(nrow(.data) <= MIN_DATA_ROWS) return(list(p = NO_PLOT_ATTEMP, trend = NO_TREND_ATTEMP))
  
  length_data <- data.frame(length_cm = rep(
    .data$length,
    .data$count
  ))
  
  of_lmax <- unique(.data$lmax)
  
  # calculate Length-based indicators base on Froese and Binohlan formulas
  froeseTemp <- froese_binohlan(of_lmax, length_data$length_cm)

  # build fma metrics list
  fma_metrics_df <- list(
    country = unique(.data$country),
    maa = unique(.data$maa),
    species = unique(.data$species),
    avg_length = mean(length_data$length_cm, na.rm = TRUE),
    counts = sum(.data$count, na.rm = TRUE),
    froeseTemp
  )
  
  # labels for legend
  Pmat_label <-  label_percent(froeseTemp$percentMature)
  Popt_label <- label_percent(froeseTemp$percentOpt)
  Pmega_label <- label_percent(froeseTemp$percentMega)
  plot_range <- max(length_data$length_cm, na.rm = TRUE) - min(length_data$length_cm, na.rm = TRUE)
  x_annot_box <- min(length_data$length_cm, na.rm = TRUE) + 0.78 * plot_range
  x_annot_text <- min(length_data$length_cm, na.rm = TRUE) + 0.8 * plot_range
  
  # Plot LF
  p <- ggplot(length_data, aes(x = length_cm)) +
    geom_histogram(
      position = "identity",
      binwidth = 1,
      fill = "grey80",
      colour = "black"
    ) +
    labs(
      title = plot_title,
      subtitle = paste("Species: ", fma_metrics_df$species),
      x = "Length (cm)",
      y = y_title
    ) +
    geom_vline(
      xintercept = fma_metrics_df$avg_length,
      color = "grey50"
    ) +
    annotate("label",
             x = fma_metrics_df$avg_length,
             y = 0,
             label = paste0(
               "Lavg\n",
               signif(fma_metrics_df$avg_length, 3), " cm"
             ),
             angle = 90
    ) +
    geom_vline(
      xintercept = froeseTemp$Lmat,
      color = "red",
      show.legend = TRUE
    ) +
    annotate("label",
             x = froeseTemp$Lmat,
             y = 0,
             label = paste0(
               "Lmat\n",
               signif(froeseTemp$Lmat, 3), " cm"
             ),
             angle = 90
    ) +
    geom_vline(
      xintercept = froeseTemp$Lopt,
      color = "darkgreen",
      show.legend = TRUE
    ) +
    annotate("label",
             x = froeseTemp$Lopt,
             y = 0,
             label = paste0(
               "Lopt\n",
               signif(froeseTemp$Lopt, 3), " cm"
             ),
             angle = 90
    ) +
    geom_vline(
      xintercept = froeseTemp$Lmega,
      color = "darkblue",
      show.legend = TRUE
    ) +
    annotate("label",
             x = froeseTemp$Lmega,
             y = 0,
             label = paste0(
               "Lmega\n",
               signif(froeseTemp$Lmega, 3), " cm"
             ),
             angle = 0
    ) +
    annotate(
      geom = "label",
      x = x_annot_box, y = Inf, hjust = 0, vjust = 1.1,
      label = "    ",
      size = 33
    ) +
    annotate(
      geom = "text",
      x = x_annot_text, y = Inf, hjust = 0, vjust = 2,
      size = 5,
      label = deparse(bquote(
        P[mat]:~~.(Pmat_label)
      )),
      parse = TRUE
    ) +
    annotate(
      geom = "text",
      x = x_annot_text, y = Inf, hjust = 0, vjust = 3.5,
      size = 5,
      label = deparse(bquote(
        P[opt]:~~.(Popt_label)
      )),
      parse = TRUE
    ) +
    annotate(
      geom = "text",
      x = x_annot_text, y = Inf, hjust = 0, vjust = 5,
      size = 5,
      label = deparse(bquote(
        P[mega]:~~.(Pmega_label)
      )),
      parse = TRUE
    ) +
    theme_rare()
  
  list(plot = p, trend = NO_TREND_ATTEMP)
}
