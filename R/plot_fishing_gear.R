plot_fishing_gear <- function(.data){
  

  .data <-.data %>%
    dplyr::filter(gear_type != "") %>%
    dplyr::group_by(country, yearmonth, gear_type) %>%
    dplyr::summarise (gear_count = dplyr::n())
  
  maxcount <- .data$gear_count
  #Number of species recorded
.data %>%
    ggplot(aes(x= yearmonth, 
               y = gear_count,
               group = gear_type,
               fill = gear_type)) +
    geom_col(aes(fill = gear_type)) + 
    scale_fill_npg(name = "", alpha=0.9)+
    scale_color_npg(alpha=0.9)+
    scale_y_continuous(breaks = integer_breaks(),
                       limits = c(0, max(maxcount)),
                       oob = scales::squish)+
    labs(
      x = "", 
      y = "Number gear type reported each month",
      title = "Type of fishing gear reported",
      fill = "Gear type") +
  theme_rare()



}

