tr <- function(state, txt) {
  if (state$language == "English") {
    txt
  } else {
    out <- script %>% 
      dplyr::filter(English == txt) %>% 
      dplyr::pull(state$language)
    if (length(out) == 0) {
      return(txt)
    } else {
      out
    }
  }
}

tr_v <- Vectorize(tr)