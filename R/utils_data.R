get_dates <- function(.data) {
  .data %>%
    dplyr::select(transaction_date) %>%
    dplyr::distinct() %>%
    dplyr::arrange() %>%
    dplyr::pull()
}

create_data_summary <- function(.data) {
  .data %>%
    dplyr::group_by(
      country,
      subnational,
      local,
      maa,
      family,
      species
    ) %>%
    dplyr::summarise( # `Villages Reporting` = as.numeric(count_unique(community)),
      `Start Date` = min(transaction_date, na.rm = TRUE),
      `End Date` = max(transaction_date, na.rm = TRUE),
      `No. Months` = as.numeric(count_unique(yearmonth)),
      `No. Years` = as.numeric(count_unique(year)),
      `Total Counts` = sum(count,
        na.rm = TRUE
      ),
      `Total Weight (kg)` = round(
        sum(weight_kg,
          na.rm = TRUE
        ),
        2
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      country,
      subnational,
      local,
      maa,
      family,
      species,
      `Start Date`,
      `End Date`
    )
}