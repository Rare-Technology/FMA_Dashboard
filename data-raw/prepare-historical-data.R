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
historical_gear_types <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/12vauVdTESJEr1E-1yo0YtdrmZnwupb7DL4Z8lMgLYOY/")

historical <- dplyr::left_join(historical, historical_gear_types, by = 'gear_type') %>% 
  dplyr::select(-`gear_type 2`, -gear_type) %>% 
  dplyr::rename(gear_type = gear_type_new)

historical <- historical %>% 
  # Some values for species_scientific look like this: Platax pinnatus(Linnaeus,1758)
  # To make sure we can properly join with fishbase data from bio_table, we will
  # clean these to look more like "Platax pinnatus"
  dplyr::mutate(
    species_scientific = sapply(
      species_scientific,
      function (x) {
        stringr::str_split(x, pattern = '\\(', simplify = TRUE)[1] %>% 
        stringr::str_trim() %>% 
        stringr::str_to_sentence()
      }
    )
  )

# Add fishbase data
bio_table <- readr::read_csv("https://query.data.world/s/33b6xpfgoufkomz5d6diclqzhwh5mv") %>% 
  dplyr::select(species_scientific = Species, family = Family, lmax, a, b, trophic_level)
historical <- dplyr::left_join(historical, bio_table, by = "species_scientific")

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

historical <- historical %>% 
  dplyr::select(
    country, subnational, subnational_id, local, local_id, maa, maa_id, community = community_name,
    year, yearmonth, week, transaction_date = date,
    gear_type, n_fishers, fisher_id = fisher_name,
    species = species_scientific, label = species_local, family,
    length, weight_kg, count, lmax, a, b, trophic_level
  )

detach(package:rfishbase)
detach(package:googlesheets4)