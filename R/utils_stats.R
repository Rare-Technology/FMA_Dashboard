count_unique <- function(x, na.rm = TRUE) {
  length(unique(x))
}

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

## Froese function

froese_binohlan <- function(lmax, lengthData) {

  # Linf
  Linf <- 10^(0.044 + 0.9841 * log10(lmax))
  # Lmat
  Lmat <- 10^(0.8979 * log10(Linf) - 0.0782)
  # Lopt
  Lopt <- 10^(1.0421 * log10(Linf) - 0.2742)
  Lopt_lower <- Lopt * 0.9
  Lopt_upper <- Lopt * 1.1
  # Lmega
  Lmega <- Lopt * 1.1

  percentMature <- length(which(lengthData > Lmat)) / length(lengthData) * 100
  percentOpt <- length(which(lengthData > Lopt_lower & lengthData < Lopt_upper)) / length(lengthData) * 100
  percentMega <- length(which(lengthData > Lmega)) / length(lengthData) * 100
  p_obj <- percentMature / 100 + percentOpt / 100 + percentMega / 100
  selectivity <- dplyr::case_when(
    p_obj < 1 & percentOpt / 100 + percentMega / 100 == 0 & percentMature / 100 > 0.25 ~ "Fish small and immature",
    p_obj < 1 & percentOpt / 100 + percentMega / 100 == 0 & percentMature / 100 <= 0.25 ~ "Fish small and immature",
    p_obj < 1 & percentOpt / 100 + percentMega / 100 > 0 & Lmat <= 0.75 * Lopt & percentMature / 100 > 0.4 ~ "Fish small and optimally-sized or all but biggest",
    p_obj < 1 & percentOpt / 100 + percentMega / 100 > 0 & Lmat == 0.9 * Lopt & percentMature / 100 > 0.25 ~ "Fish small and optimally-sized or all but biggest",
    p_obj < 1 & percentOpt / 100 + percentMega / 100 > 0 & (Lmat != 0.9 * Lopt | percentMature / 100 <= 0.25) ~ "Fish small and optimally-sized or all but biggest",
    p_obj < 1 & percentOpt / 100 + percentMega / 100 > 0 & (Lmat > 0.75 * Lopt | percentMature / 100 <= 0.4) ~ "Fish small and optimally-sized or all but biggest",
    1 < p_obj & p_obj < 2 & Lmat <= 0.75 * Lopt & percentMature / 100 > 0.95 ~ "Fish maturity ogive",
    1 < p_obj & p_obj < 2 & Lmat == 0.9 * Lopt & percentMature / 100 > 0.9 ~ "Fish maturity ogive",
    1 < p_obj & p_obj < 2 & (Lmat > 0.75 * Lopt | percentMature / 100 <= 0.95) ~ "Fish maturity ogive",
    1 < p_obj & p_obj < 2 & (Lmat != 0.9 * Lopt | percentMature / 100 <= 0.9) ~ "Fish maturity ogive",
    p_obj == 2 & percentOpt / 100 < 1 & percentOpt / 100 < 0.65 ~ "Fish optimally-sized and bigger",
    p_obj == 2 & percentOpt / 100 < 1 & percentOpt / 100 >= 0.65 ~ "Fish optimally-sized and bigger",
    p_obj == 2 & percentOpt / 100 == 1 ~ "Fish optimally-sized"
  )

  status <- dplyr::case_when(
    p_obj < 1 & percentOpt / 100 + percentMega / 100 == 0 & percentMature / 100 > 0.25 ~ "Spawning biomass above reference point",
    p_obj < 1 & percentOpt / 100 + percentMega / 100 == 0 & percentMature / 100 <= 0.25 ~ "Spawning biomass below reference point",
    p_obj < 1 & percentOpt / 100 + percentMega / 100 > 0 & Lmat <= 0.75 * Lopt & percentMature / 100 > 0.4 ~ "Spawning biomass above reference point",
    p_obj < 1 & percentOpt / 100 + percentMega / 100 > 0 & Lmat == 0.9 * Lopt & percentMature / 100 > 0.25 ~ "Spawning biomass above reference point",
    p_obj < 1 & percentOpt / 100 + percentMega / 100 > 0 & (Lmat != 0.9 * Lopt | percentMature / 100 <= 0.25) ~ "Spawning biomass below reference point",
    p_obj < 1 & percentOpt / 100 + percentMega / 100 > 0 & (Lmat > 0.75 * Lopt | percentMature / 100 <= 0.4) ~ "Spawning biomass below reference point",
    1 < p_obj & p_obj < 2 & Lmat <= 0.75 * Lopt & percentMature / 100 > 0.95 ~ "Spawning biomass above reference point",
    1 < p_obj & p_obj < 2 & Lmat == 0.9 * Lopt & percentMature / 100 > 0.9 ~ "Spawning biomass above reference point",
    1 < p_obj & p_obj < 2 & (Lmat > 0.75 * Lopt | percentMature / 100 <= 0.95) ~ "Spawning biomass below reference point",
    1 < p_obj & p_obj < 2 & (Lmat != 0.9 * Lopt | percentMature / 100 <= 0.9) ~ "Spawning biomass below reference point",
    p_obj == 2 & percentOpt / 100 < 1 & percentOpt / 100 < 0.65 ~ "Spawning biomass above reference point",
    p_obj == 2 & percentOpt / 100 < 1 & percentOpt / 100 >= 0.65 ~ "Spawning biomass below reference point",
    p_obj == 2 & percentOpt / 100 == 1 ~ "No status information"
  )

  return(list(
    percentMature = percentMature,
    percentOpt = percentOpt,
    percentMega = percentMega,
    Lopt = Lopt,
    Lopt_lower = Lopt_lower,
    Lopt_upper = Lopt_upper,
    Lmega = Lmega,
    Lmat = Lmat,
    selectivity = selectivity,
    status = status
  ))
}
