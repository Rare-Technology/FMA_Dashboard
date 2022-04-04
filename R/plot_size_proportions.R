plot_size_proportions <- function(.data, sel_species, use_species_facet = FALSE) {

  if (use_species_facet) {
    if(length(sel_species) > 10) return(list(p = FACET_WARNING, trend = NO_TREND_ATTEMP))
    
    .data <- .data %>%
      dplyr::filter(species %in% sel_species, !is.na(length), !is.na(count), !is.na(lmax)) %>%
      dplyr::group_by(yearmonth, species) %>% 
      dplyr::filter(sum(count) > 100) %>% 
      dplyr::ungroup() %>% 
      droplevels()
  } else {
    .data <- .data %>%
      dplyr::filter(species %in% sel_species, !is.na(length), !is.na(count), !is.na(lmax)) %>%
      dplyr::group_by(yearmonth) %>% 
      dplyr::filter(sum(count) > 100) %>% 
      dplyr::ungroup() %>% 
      droplevels()
  }
  
  if(nrow(.data) <= MIN_DATA_ROWS) return(list(p = NO_PLOT_ATTEMP, trend = NO_TREND_ATTEMP))
    
    
  ### Calculate Froese indicators per MA and species per month
  fma_df <- data.frame(
    country = NA,
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
    status = NA
  )[-1, ]

  if (use_species_facet) {
    for (s in unique(.data$species)) {
      for (k in unique(.data$yearmonth)) {
        ourfish_ctry_sub <- .data %>% 
          dplyr::filter(yearmonth == k, species == s) %>% 
          droplevels()
        
        if (nrow(ourfish_ctry_sub) > 0) {
        
          # calculate lengths
          length_data <- data.frame(length_cm = rep(
            ourfish_ctry_sub$length,
            ourfish_ctry_sub$count
          ))
          
          of_lmax <- unique(ourfish_ctry_sub$lmax)
          
          # calculate Length-based indicators base on Froese and Binohlan formulas
          froeseTemp <- froese_binohlan(of_lmax, length_data$length_cm)

          fma_metrics_df <- data.frame(
            country = unique(ourfish_ctry_sub$country),
            maa = unique(ourfish_ctry_sub$maa),
            species = unique(ourfish_ctry_sub$species),
            yearmonth = unique(ourfish_ctry_sub$yearmonth),
            avg_length = mean(length_data$length_cm, na.rm = TRUE),
            uni_length = length(unique(length_data)),
            counts = sum(ourfish_ctry_sub$count, na.rm = TRUE),
            froeseTemp
          )
          
          fma_df <- rbind(fma_df, fma_metrics_df)
        }
      }
    }
    
    # plot
    p <- try(
      ggplot(fma_df, aes(yearmonth)) +
      geom_line(aes(y = percentMature, color = "Mature"), size = 2) +
      geom_line(aes(y = percentOpt, color = "Optimal"), size = 2) +
      geom_line(aes(y = percentMega, color = "Megaspawner"), size = 2) +
      facet_wrap(~ species) +
      labs(
        title = "Size proportions",
        # subtitle = paste("Species: ", sel_species),
        x = "",
        y = "Proportion (%)"
      ) +
      theme_rare() +
      scale_color_manual(
        name = "Proportion",
        values = c(
          "Mature" = "red",
          "Optimal" = "darkgreen",
          "Megaspawner" = "lightblue")
      ) +
      scale_x_date(date_labels = "%b-%y"), silent = TRUE)
  } else {
    ## loop for calculations
    for (k in unique(.data$yearmonth)) {
      for (s in sel_species) {
        ourfish_ctry_sub <- .data %>%
          dplyr::filter(yearmonth == k, species == s) %>%
          droplevels()
      
        # We've filtered out only to yearmonths where the species has more than 100 records,
        # but since we're aggregating different species, not everyone will appear on every
        # yearmonth, and ourfish_ctry_sub will be blank. In this case, cut to the chase
        # and append a row with empty length data to fma_df
        if (nrow(ourfish_ctry_sub) == 0) { next }
        
        # calculate lengths
        length_data <- data.frame(length_cm = rep(
          ourfish_ctry_sub$length,
          ourfish_ctry_sub$count
        ))
    
        of_lmax <- unique(ourfish_ctry_sub$lmax)
    
        # calculate Length-based indicators base on Froese and Binohlan formulas
        froeseTemp <- froese_binohlan(of_lmax, length_data$length_cm)
        
        fma_metrics_df <- data.frame(
          country = unique(ourfish_ctry_sub$country),
          maa = unique(ourfish_ctry_sub$maa),
          species = unique(ourfish_ctry_sub$species),
          yearmonth = unique(ourfish_ctry_sub$yearmonth),
          avg_length = mean(length_data$length_cm, na.rm = TRUE),
          uni_length = length(unique(length_data)),
          counts = sum(ourfish_ctry_sub$count, na.rm = TRUE),
          froeseTemp
        )
    
        fma_df <- rbind(fma_df, fma_metrics_df)
      }
    }
    
    fma_df <- fma_df %>% 
      dplyr::mutate(
        weightedPercentMature = counts * percentMature,
        weightedPercentOpt = counts * percentOpt,
        weightedPercentMega = counts * percentMega
      ) %>% 
      dplyr::group_by(yearmonth) %>% 
      dplyr::summarize(
        counts = sum(counts),
        percentMature = sum(weightedPercentMature) / counts,
        percentOpt = sum(weightedPercentOpt) / counts,
        percentMega = sum(weightedPercentMega) / counts
      )
  
    # plot
    p <- try(
      ggplot(fma_df, aes(yearmonth)) +
      geom_line(aes(y = percentMature, color = "Mature"), size = 2
      ) +
      geom_line(aes(y = percentOpt, color = "Optimal"), size = 2
      ) +
      geom_line(aes(y = percentMega, color = "Megaspawner"), size = 2
      ) +
      labs(
        title = "Size proportions",
        # subtitle = paste("Species: ", sel_species),
        x = "",
        y = "Proportion (%)"
      ) +
      theme_rare() +
      scale_color_manual(
        name = "Proportion",
        values = c(
          "Mature" = "red",
          "Optimal" = "darkgreen",
          "Megaspawner" = "lightblue")
      ) +
      scale_x_date(date_labels = "%b-%y"), silent = TRUE)
    
    if (length(sel_species) == 1) {
      p <- p + labs(subtitle = paste("Species: ", sel_species))
    }
  }
  
  if (length(unique(.data$maa)) == 1) {
    fma_df$maa <- unique(.data$maa)
  }
  fma_df <- fma_df %>% 
    dplyr::mutate(
      year = lubridate::year(yearmonth),
      month = lubridate::month(yearmonth)
    )
  
  list(plot = p, trend = NO_TREND_ATTEMP, data = fma_df)
}
