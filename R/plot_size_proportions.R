plot_size_proportions <- function(.data, sel_species){
  
  .data <- .data %>%
    dplyr::filter(species %in% sel_species) %>%
    droplevels()
  
  ### Calculate Froese indicators per MA and species per month
  fma_df <- data.frame (country = NA,
                        ma_name = NA, 
                        species = NA,
                        yearmonth = NA,
                        avg_length = 0,
                        uni_length = 0,
                        counts = 0, 
                        percentMature = 0,
                        percentOpt = 0,
                        percentMega = 0,
                        Lopt = 0,
                        Lopt_lower = 0,
                        Lopt_upper = 0,
                        Lmega = 0,
                        Lmat = 0,
                        selectivity = NA,
                        status = NA)[-1,]
  
  ## loop for calculations
  
  for (k in unique(.data$yearmonth)) {
    
    ourfish_ctry_sub <- .data %>%
      dplyr::filter(yearmonth == k) %>%
      droplevels()
    
    #calculate lenghts
    length_data <- data.frame(length_cm = rep(ourfish_ctry_sub$length, 
                                              ourfish_ctry_sub$count))
    
    #Use the highest available Lmax, (from the literature or from the data)
    of_lmax <- ifelse (unique(ourfish_ctry_sub$lmax) > max(length_data$length_cm),
                unique(ourfish_ctry_sub$lmax),
              max(length_data$length_cm))
    
    #calculate Length-based indicators base on Froese and Binohlan formulas
    froeseTemp <- froese_binohlan(of_lmax, length_data$length_cm)
    
    fma_metrics_df <- data.frame(country = unique(ourfish_ctry_sub$country),
                                 maa = unique(ourfish_ctry_sub$maa), 
                                 species = unique(ourfish_ctry_sub$species[1]),
                                 yearmonth = unique(ourfish_ctry_sub$yearmonth),
                                 avg_length = mean(length_data$length_cm, na.rm = TRUE),
                                 uni_length = length(unique(length_data)),
                                 counts = sum(ourfish_ctry_sub$count, na.rm = TRUE),
                                 froeseTemp)
    
    fma_df <- rbind(fma_df, fma_metrics_df)
    
  }
  
  #plot  
  fma_df %>%
    ggplot(aes(yearmonth)) + 
    geom_line(aes(y=percentMature), 
              col = "red")+
    geom_line(aes(y=percentOpt), 
              col= "darkgreen")+
    geom_line(aes(y=percentMega), 
              col= "blue")+
    labs(
      title = "Size proportions",
      subtitle = paste("Species: ", fma_metrics_df$species),
      x = "",
      y = "Proportion (%)"
    ) +
    annotate("label", x=min(fma_df$yearmonth, na.rm = TRUE), 
             y= c(subset(fma_df, yearmonth == min(yearmonth, na.rm = TRUE))$percentMature,
                  subset(fma_df, yearmonth == min(yearmonth, na.rm = TRUE))$percentOpt,
                  subset(fma_df, yearmonth == min(yearmonth, na.rm = TRUE))$percentMega),
             label = c("Pmat", "Popt", "Pmega"), 
             col=c(2,"darkgreen",4))+
    theme_rare() 
}