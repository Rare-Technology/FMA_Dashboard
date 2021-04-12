state <- reactiveValues(
  data_source = rarefma::init_data_source,
  data_full = rarefma::data_raw[[rarefma::init_data_source]]$data,
  data_filtered = rarefma::init_data_filtered,
  data_summary = rarefma::data_summary[[rarefma::init_data_source]],
  data_summary_filtered = rarefma::init_data_summary_filtered,
  data_geo_family_species = rarefma::data_geo_family_species[[rarefma::init_data_source]],
  dates = rarefma::init_dates,
  selections_geo = rarefma::init_geo_selections,
  country = list(
    choices = rarefma::init_geo_selections$country$choices,
    selected = rarefma::init_geo_selections$country$selected
  ),
  subnational = list(
    choices = rarefma::init_geo_selections$subnational$choices,
    selected = rarefma::init_geo_selections$subnational$choices
  ),
  local = list(
    choices = rarefma::init_geo_selections$local$choices,
    selected = rarefma::init_geo_selections$local$choices
  ),
  maa = list(
    choices = rarefma::init_geo_selections$maa$choices,
    selected = rarefma::init_geo_selections$maa$choices
  ),
  family = list(
    choices = rarefma::init_family_species_selections$family,
    selected = rarefma::init_family_species_selections$family
  ),
  species = list(
    choices = rarefma::init_family_species_selections$species,
    selected = rarefma::init_family_species_selections$species
  ),
  current_tab = 'Data',
  performance_indicators = 'Reporting Effort'

)