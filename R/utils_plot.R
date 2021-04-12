theme_rare <- function() {

  # theme_bw() + 
  #   theme(legend.position = "right",
  #         legend.text = element_text(size=14),
  #         text = element_text(size=14),
  #         plot.title = element_text(size=14, 
  #                                   hjust = 0.5, 
  #                                   face = "bold"),
  #         plot.subtitle = element_text(size=15, 
  #                                      hjust = 0.5),
  #         axis.title = element_text(size=14),
  #         axis.text = element_text(size=14),
  #         strip.background = element_rect(fill="#EA883A"),
  #         strip.text = element_text(size = 14),
  #         panel.grid = element_blank()) 
  # https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r
  hrbrthemes::theme_ipsum(
    axis_title_size = 14,
    axis_title_just = "cc"
    ) + 
    theme(
      legend.text = element_text(size=14)
    )
}