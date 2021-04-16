tippy_class_alt <- function(class, txt, fontsize = 12, ...){
  tippy_class(
    class, 
    content = glue::glue("<span style='font-size:12px;'>{txt}<span>"), 
    allowHTML = TRUE, 
    placement = "right",
    animation = "shift-away",
    ...
  )
}

tooltip_label <- function(class, txt){
  HTML(glue::glue("{txt}&nbsp<i style='color:#8d8989' class='fas fa-info-circle {class}'></i>"))
}

