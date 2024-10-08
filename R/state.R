initialize_state <- function() {
  reactiveValues(
    data_source = fma_init_data_source,
    data_full = fma_data_raw[[fma_init_data_source]]$data,
    data_filtered = fma_init_data_filtered,
    data_summary = fma_data_summary[[fma_init_data_source]],
    data_summary_filtered = fma_init_data_summary_filtered,
    data_geo_family_species = fma_data_geo_family_species[[fma_init_data_source]],
    selections_geo = fma_init_geo_selections,
    country = list(
      choices = fma_init_geo_selections$country$choices,
      selected = fma_init_geo_selections$country$selected
    ),
    subnational = list(
      choices = fma_init_geo_selections$subnational$choices,
      selected = fma_init_geo_selections$subnational$selected
    ),
    local = list(
      choices = fma_init_geo_selections$local$choices,
      selected = fma_init_geo_selections$local$choices
    ),
    maa = list(
      choices = fma_init_geo_selections$maa$choices,
      selected = fma_init_geo_selections$maa$choices
    ),
    family = list(
      choices = fma_init_family_species_selections$family,
      selected = fma_init_family_species_selections$family
    ),
    species = list(
      choices = fma_init_family_species_selections$species,
      selected = fma_init_family_species_selections$species
    ),
    current_date_range = fma_init_date_range,
    current_tab = "Start",
    current_indicator = "Species Composition",
    current_min_records = list(min = 0, max = 300, value = 0),
    current_trend = NA,
    ma_facet = FALSE,
    family_facet = FALSE,
    species_facet = FALSE,
    fixed_yscale = TRUE,
    loess_span = 0.5,
    current_plot = NULL,
    language = "English",
    resetFilters = 0
  )
}
