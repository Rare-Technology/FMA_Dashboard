count_unique <- function(x) { length(unique(x)) }

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

trend_indicator <- function(mod) {
  ifelse (coef(mod)[2] > 0 & summary(mod)$coef[,"Pr(>|t|)"] < 0.05, "Increasing",
          ifelse(coef(mod)[2] < 0 & summary(mod)$coef[,"Pr(>|t|)"] < 0.05, "Decreasing", "No change"))[[1]]
  
}

trend_color <- function(mod) {
  ifelse(coef(mod)[2] > 0 & summary(mod)$coef[,"Pr(>|t|)"] < 0.05, "darkgreen",
         ifelse(coef(mod)[2] < 0 & summary(mod)$coef[,"Pr(>|t|)"] < 0.05, "darkred", "grey20"))[[1]]
}