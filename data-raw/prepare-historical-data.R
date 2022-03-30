library(googlesheets4)
library(rfishbase)
library(stringr)

googlesheets4::gs4_deauth()

# Historical data from Tinambac, Philippines
### March 23 2022
# NOTICE: MANY records are missing scientific taxonomy info! Currently nearly 24k records
# lack a value in species_scientific, out of about 60k records. This may be because
# translating local species into scientific species is still in progress.
# Also, some records may use a name that is not valid on the rfishbase::fishbase table, but
# will otherwise give you a match on the website (for example, Pseudosciaena anea)
# Need to figure out how to match these...
historical <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1EsnbgSm3nEfDiqG4POdpNa8c5KAu3vqZiZ-AOpWcuk4/")
historical <- historical %>% 
  # Some values for species_scientific look like this: Platax pinnatus(Linnaeus,1758)
  # To make sure fishbase will get correct results, we fix this in the following mutate
  # Then extract the genus and species from the scientific name
  dplyr::mutate(
    species_scientific = sapply(
      species_scientific,
      function (x) {
        stringr::str_split(x, pattern = '\\(', simplify = TRUE)[1] %>% 
        stringr::str_trim() %>% 
        stringr::str_to_sentence()
      }
    )
  ) %>% 
  tidyr::separate(species_scientific, c('Genus', 'Species'), sep = ' ', remove = FALSE)

### Find/compute Lmax's for species. If anything is on fishbase for a given species, use the 
# largest lmax on record, otherwise use the aggregated mean by genus.
fishbase_genus_filter <- rfishbase::load_taxa() %>% 
  dplyr::filter(Genus %in% unique(historical$Genus)) %>% 
  dplyr::select(species_scientific = Species, Genus)

lengthlength_species <- rfishbase::length_length(fishbase_genus_filter$species_scientific) %>% 
  dplyr::select(species_scientific = Species, LengthMax) %>% 
  dplyr::mutate(lmax = as.numeric(LengthMax)) %>% 
  dplyr::group_by(species_scientific) %>% 
  dplyr::summarize(lmax_species = max(lmax, na.rm = TRUE)) %>% 
  dplyr::mutate(lmax_species = ifelse(is.infinite(lmax_species), NA, lmax_species)) %>% 
  tidyr::separate(species_scientific, c('Genus', 'Species'), sep = ' ', remove = FALSE)

lengthlength_genus <- lengthlength_species %>% 
  dplyr::group_by(Genus) %>% 
  dplyr::summarize(lmax_genus = mean(lmax_species, na.rm = TRUE))

lengthlength_species <- lengthlength_species %>% 
  dplyr::select(species_scientific, lmax_species)

historical <- historical %>% 
  dplyr::left_join(., lengthlength_species, by = 'species_scientific') %>% 
  dplyr::left_join(., lengthlength_genus, by = 'Genus')

historical <- historical %>% 
  dplyr::mutate(
    lmax = ifelse(is.na(lmax_species),
      lmax_genus,
      lmax_species
    )
  ) %>%
  dplyr::select(-c(lmax_species, lmax_genus))


### Calculate lengths using a/b coefficients plus weight_kg. Then limit values by lmax
# As of Mar 22, 2022, only ONE species from Phils historical catch data has a/b coefficients recorded on fishbase!
poplw_filter <- rfishbase::poplw() %>% 
  dplyr::select(species_scientific = Species, a, b) %>% 
  dplyr::filter(species_scientific %in% unique(historical$species_scientific)) %>% 
  dplyr::group_by(species_scientific) %>% 
  dplyr::summarize(
    a = mean(a, na.rm = TRUE),
    b = mean(b, na.rm = TRUE)
  )

historical <- dplyr::left_join(historical, poplw_filter, by = 'species_scientific')

# Mar 23 2022
# The date column is a little weird. About 99% of it is in POSIXct form, where
# they are the number of seconds since Jan 1, 1970. But the last fraction is in
# a string form like this: "2//16/2016". As a result, as.numeric() turns
# these into NA's.
# Since very few rows (~700 out of 60k) look like this, I will leave this to be fixed later
historical$date <- as.numeric(historical$date)
historical <- historical %>% dplyr::filter(!is.na(date))
historical$date <- as.Date.POSIXct(historical$date)

historical <- historical %>% 
  dplyr::mutate(
    length = ifelse(is.na(a) | is.na(b) | is.na(weight_kg),
      NA,
      exp(log(1000*weight_kg/count/a) / b)
    )
  ) %>% 
  dplyr::mutate(
    length = ifelse(is.infinite(length), NA, length) # some Inf's come from previous mutate
  ) %>% 
  dplyr::mutate(
    length = ifelse(!is.na(length) & !is.na(lmax), # to avoid e,g, NA > lmax
      ifelse(length > lmax,
        lmax,
        length
      ),
      length
    )
  ) %>% 
  dplyr::mutate(
    country = "Philippines",
    subnational = "Camarines Sur",
    subnational_id = 148,
    local = "Tinambac",
    local_id = 279,
    maa = "Tinambac",
    maa_id = 172,
    year = lubridate::year(date),
    month = lubridate::month(date),
    week = lubridate::week(date)
  )

historical$yearmonth <- as.Date(paste0(historical$year, "-", historical$month,"-", "01"),
                             "%Y-%m-%d")

fishbase_filter <- rfishbase::load_taxa() %>% 
  dplyr::filter(Species %in% unique(historical$species_scientific)) %>% 
  dplyr::select(species_scientific = Species, Family)
  
### Mar 23 2022
# At some point, sort out Family values for catches with a species_scientific given by
# the genus (Genus sp. or Genus spp.)
historical <- dplyr::left_join(historical, fishbase_filter, by = 'species_scientific')

# Get trophic level data. Similar to length, we will extract and use any available data on fishbase.
# Missing trophic levels will be imputed first on an aggregated genus level, and then, if necessary,
# an aggregated family level
fishbase_family_filter <- rfishbase::load_taxa() %>% 
  dplyr::filter(Family %in% unique(historical$Family)) %>% 
  dplyr::select(species_scientific = Species, Family)

family_troph <- rfishbase::ecology(fishbase_family_filter$species_scientific) %>% 
  dplyr::select(species_scientific = Species, DietTroph)

family_troph <- dplyr::left_join(family_troph, fishbase_family_filter, by = 'species_scientific')

family_troph <- family_troph %>% 
  dplyr::group_by(Family) %>% 
  dplyr::summarize(family_trophic_level = mean(DietTroph, na.rm = TRUE))

genus_troph <- rfishbase::ecology(fishbase_genus_filter$species_scientific) %>% 
  dplyr::select(species_scientific = Species, DietTroph) %>% 
  dplyr::left_join(., fishbase_genus_filter, by = 'species_scientific') %>% 
  dplyr::group_by(Genus) %>% 
  dplyr::summarize(genus_trophic_level = mean(DietTroph, na.rm = TRUE))

species_troph <- rfishbase::ecology(unique(historical$species_scientific)) %>% 
  dplyr::select(species_scientific = Species, DietTroph)
  # need to aggregate within same species?

historical <- dplyr::left_join(historical, family_troph, by = 'Family') %>% 
  dplyr::left_join(., genus_troph, by = 'Genus') %>% 
  dplyr::left_join(., species_troph, by = 'species_scientific') %>% 
  dplyr::mutate(
    trophic_level = ifelse(is.na(DietTroph),
      ifelse(is.na(genus_trophic_level),
        family_trophic_level,
        genus_trophic_level
      ),
      DietTroph
    )
  )

historical <- historical %>% 
  dplyr::rename(
    family = Family,
    species = species_scientific,
    community = community_name,
    transaction_date = date,
    label = species_local
  ) %>% 
  dplyr::select(
    -c(
      Genus,
      genus_trophic_level,
      family_trophic_level,
      DietTroph,
      country_id,
      community_id,
      fishbase_id, # only 16 non-NA as of Mar 23 2022
      fisher_id,
      price_per_kg,
      total_price,
      month
    )
  ) %>% 
  dplyr::rename(fisher_id = fisher_name) # the fisher_id col dropped in the prev select has no data
  
detach(package:rfishbase)
detach(package:googlesheets4)



