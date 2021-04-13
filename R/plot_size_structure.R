plot_size_structure <- function(.data, sel_species, Pmat = -Inf, Pmega = -Inf, Popt = -Inf){
  

  .data <- .data %>%
    dplyr::filter(species %in% sel_species) %>%
    droplevels()
  
  length_data <- data.frame(length_cm = rep(.data$length, 
                                            .data$count))
  
  #Use the highest available Lmax, (from the literature or from the data)
  of_lmax <- ifelse (unique(.data$lmax) > max(length_data$length_cm),
           unique(.data$lmax),
           max(length_data$length_cm))
  
  #calculate Length-based indicators base on Froese and Binohlan formulas
  froeseTemp <- froese_binohlan(of_lmax, length_data$length_cm)
  
  #build fma metrics list
  fma_metrics_df <- list(country = unique(.data$country),
                         maa = unique(.data$maa), 
                         species = unique(.data$species),
                         avg_length = mean(length_data$length_cm, na.rm = TRUE),
                         counts = sum(.data$count, na.rm = TRUE),
                         froeseTemp)
  #Plot LF
  ggplot(length_data, aes(x=length_cm)) +
    geom_histogram(position="identity", 
                   binwidth = 1,
                   fill="grey80",
                   colour="black") +
    labs(
      title = "Size structure",
      subtitle = paste("Species: ", fma_metrics_df$species),
      x = "Length (cm)",
      y = "Frequency"
    ) +
    geom_vline(xintercept = fma_metrics_df$avg_length, 
               color = "grey50")+
    annotate("label", 
             x= fma_metrics_df$avg_length, 
             y = 0, 
             label = paste0("Lavg\n", 
                            signif(fma_metrics_df$avg_length,3), " cm"),
             angle = 90) +
    geom_vline(xintercept = froeseTemp$Lmat, 
               color="red") + 
    annotate("label", 
             x = froeseTemp$Lmat, 
             y = 0 , 
             label = paste0("Lmat\n", 
                            signif(froeseTemp$Lmat,3)," cm"),
             angle = 90)+
    geom_vline(xintercept = froeseTemp$Lopt, 
               color="darkgreen") +
    annotate("label", 
             x = froeseTemp$Lopt, 
             y = 0 , 
             label = paste0("Lopt\n", 
                            signif(froeseTemp$Lopt,3)," cm"),
             angle = 90)+
    geom_vline(xintercept = froeseTemp$Lmega, 
               color="blue") +
    annotate("label", 
             x = froeseTemp$Lmega, 
             y = 0 , 
             label = paste0("Lmega\n", 
                            signif(froeseTemp$Lmega,3)," cm"),
             angle = 0)+
    geom_label(x = Inf, 
               y = Inf,
               hjust = 1.1, 
               vjust = 1.1,
               label = "      ", 
               size = 30)+
    geom_text(x = Inf, 
              y = Inf, 
              hjust = 1.5, 
              vjust = 2, 
              size = 5,
              colour = color_froese(froeseTemp$percentMature, Pmat),
              label = paste("Pmat: ", signif(froeseTemp$percentMature,3),"%")
    ) +
    geom_text(x = Inf, y = Inf, hjust = 1.5, vjust = 4, 
              size = 5,
              colour = color_froese(froeseTemp$percentOpt, Popt),
              label = paste("Popt: ", signif(froeseTemp$percentOpt,2),"%")
    ) +
    geom_text(x = Inf, y = Inf, hjust = 1.5, vjust = 6, 
              size = 5,
              colour = color_froese_Pmega(froeseTemp$percentMega, Pmega),
              label = paste("Pmega: ", signif(froeseTemp$percentMega,2),"%")
    ) +
    theme_rare()
}