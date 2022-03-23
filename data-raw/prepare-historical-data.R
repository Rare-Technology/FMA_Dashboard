library(googlesheets4)
library(rfishbase)
library(stringr)

googlesheets4::gs4_deauth()

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
fishbase_filter <- rfishbase::fishbase %>% 
  dplyr::filter(Genus %in% unique(historical$Genus)) %>% 
  tidyr::unite(species_scientific, c('Genus', 'Species'), sep = ' ', remove = FALSE) %>% 
  dplyr::select(species_scientific, Genus, Species)

lengthlength_species <- rfishbase::length_length(fishbase_filter$species_scientific) %>% 
  dplyr::select(species_scientific = Species, LengthMax) %>% 
  dplyr::mutate(lmax = as.numeric(LengthMax)) %>% 
  dplyr::filter(!is.na(lmax)) %>% 
  dplyr::group_by(species_scientific) %>% 
  dplyr::summarize(lmax_species = max(lmax)) %>% 
  tidyr::separate(species_scientific, c('Genus', 'Species'), sep = ' ', remove = FALSE)

lengthlength_genus <- lengthlength_species %>% 
  dplyr::filter(!is.infinite(lmax_species)) %>% 
  dplyr::group_by(Genus) %>% 
  dplyr::summarize(lmax_genus = mean(lmax_species))

lengthlength_species <- lengthlength_species %>% 
  dplyr::select(species_scientific, lmax_species)

historical <- dplyr::left_join(historical, lengthlength_species, by = 'species_scientific')
historical <- dplyr::left_join(historical, lengthlength_genus, by = 'Genus')

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

historical <- historical %>% 
  dplyr::mutate(
    length = ifelse(is.na(a) | is.na(b) | is.na(weight_kg),
                    NA,
                    (1000*weight_kg / a)**(1/b)
    )
  ) %>% 
  dplyr::mutate(
    length = ifelse(!is.na(length) & !is.na(lmax), # to avoid e,g, NA > lmax
                    ifelse(length > lmax,
                      lmax,
                      length
                    ),
                    length
    )
  )

detach(package:rfishbase)
detach(package:googlesheets4)



