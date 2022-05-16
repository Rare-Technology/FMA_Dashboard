library(dplyr)
library(tidyr)
library(lubridate)
library(rfishbase)

# ---- Read in raw data
# join_ourfish_footprint_fishbase from Fisheries Dashboard
# https://data.world/rare/fisheries-dashboard/workspace/
ourfish <- read.csv("https://query.data.world/s/ted7ge2eirwtuww5mnwnbxe2lb2t6f",
               header=TRUE, stringsAsFactors=FALSE);


ourfish$date <- as.Date(ourfish$date, "%Y-%m-%d")
ourfish$year <- lubridate::year(ourfish$date)
ourfish$month <- lubridate::month(ourfish$date)
ourfish$week <- lubridate::week(ourfish$date)


ourfish <- ourfish %>% mutate(weight_kg=weight_mt*1000)

weight_length <- function (weight, .count, a, b) {
  exp(log(weight*1000/.count/a)/b)
}

ourfish$Length <- weight_length(ourfish$weight_kg, 
                                ourfish$count, 
                                ourfish$a, 
                                ourfish$b)

lhiData <- readr::read_csv('data-raw/LHI_Database.csv')

## Select country, managed access, and species 
ourfish_ctry_raw <- ourfish %>%
  dplyr::filter (country != "",
            ma_name != "",
            species_scientific != "",
            family_scientific != "", 
            !is.na(date)) %>%
  droplevels()

##Add trophic level data by species
ourfish <- merge (ourfish_ctry_raw, lhiData[,c("Species", "Troph")],
                      by.x= 'species_scientific', by.y="Species")
#set month and year as Date
ourfish$yearmonth <- as.Date(paste0(ourfish$year, "-", ourfish$month,"-", "01"),
                             "%Y-%m-%d")

fma_reference_points <- readr::read_csv("data-raw/reference_points.csv")

# ---- Convert factors to character
clean_string <- function(x) {
  stringr::str_replace_all(as.character(x), "\\r|\\n", "")
}
ourfish <- ourfish %>%
  dplyr::mutate_if(
    is.factor,
    clean_string
  )


# ---- Select only relevant fields
ourfish <- ourfish %>%
  dplyr::select(
    country,
    subnational = snu_name,
    subnational_id = snu_id,
    local = lgu_name,
    local_id = lgu_id,
    maa = ma_name,
    maa_id = ma_id,
    family = family_scientific,
    species = species_scientific,
    community = community_name,
    year,
    yearmonth,
    week,
    transaction_date = date,
    label,
    length = Length,
    count,
    weight_kg,
    gear_type,
    fisher_id,
    buyer_id,
    trophic_level = Troph,
    lmax
  ) %>%
  dplyr::arrange(
    country,
    subnational,
    local,
    maa,
    family,
    species
  )


# ---- Fixes

# uncomment this when implementing gear type data
ourfish$gear_type[ourfish$gear_type == "Spear Gun"] <- "Spear gun"
ourfish$gear_type[ourfish$gear_type == "Beach Seine"] <- "Beach seine"
ourfish$gear_type[ourfish$gear_type == "Línea y anzuelo"] <- "Handline"
ourfish$length[is.infinite(ourfish$length)] <- NA
ourfish$count[ourfish$count == 0] <- NA

# ----- Length data
# Fill in gaps within Lmax data by taking genus means
# Put an upper bound of Lmax on catch length data to make data more consistent with literature
# Unfortunately, as of Mar 16 2022, this process reduced the number of NA lmax's from
# 14144 to 13513, only a 4% decrease
# One difficulty is that there are a lot of Genuses that don't have enough literature data
# to compute aggregate means.

# Get genus info to be able to impute on
ourfish <- ourfish %>% 
  tidyr::separate(species, into = c('Genus', 'Species'), sep = ' ', remove = FALSE)

# Get the fishbase Genus-Species info for all fish in the same Genus as fish from our data
fishbase_filter <- rfishbase::load_taxa() %>% 
  dplyr::filter(Genus %in% unique(ourfish$Genus)) %>% 
  dplyr::select(Gensp = Species, Genus)

# Using the filtered fishbase info, get Lmax for all fish
fishbase_lmax <- rfishbase::length_weight(fishbase_filter$Gensp) %>% 
  dplyr::select(Gensp = Species, LengthMax, Type) %>% 
  dplyr::filter(Type == "TL") %>% 
  dplyr::group_by(Gensp) %>% 
  dplyr::summarize(Lmax = median(LengthMax, na.rm = TRUE))

# Aggregate means of Lmax across genus
genus_Lmax_means = fishbase_lmax %>% 
  dplyr::group_by(Genus) %>% 
  dplyr::summarize(genus_Lmax = mean(Lmax, na.rm = TRUE))

# Impute lmax data
ourfish <- ourfish %>% 
  dplyr::left_join(., genus_Lmax_means, by = 'Genus') %>% 
  dplyr::mutate(
    lmax = ifelse(is.na(lmax), genus_Lmax, lmax)
  ) %>% 
  dplyr::select(-c(Genus, Species, genus_Lmax))

# collect a/b coefficients to use to calculate lengths
poplw_filter <- rfishbase::poplw() %>%
  dplyr::select(species = Species, a, b) %>%
  dplyr::filter(species %in% unique(ourfish$species)) %>% 
  dplyr::group_by(species) %>%
  dplyr::summarize(
    a = mean(a, na.rm = TRUE),
    b = mean(b, na.rm = TRUE)
  )

ourfish <- left_join(ourfish, poplw_filter, by = 'species')

# Impute length data
ourfish <- ourfish %>% 
  dplyr::mutate(
    length = ifelse(is.na(length),
      ifelse(!is.na(weight_kg) & !is.na(a) & !is.na(b),
        exp(log(1000*weight_kg/count/a) / b),
        length
      ),
      length
    )
  )

# All lengths longer than Lmax are set equal to Lmax. if either length or lmax is NA return whatever the length is
ourfish <- ourfish %>% 
  dplyr::mutate(
    length = ifelse(is.na(length) | is.na(lmax),
      length,
      ifelse(length > lmax,
        lmax,
        length
      )
    )
  )

# ---- A function to get the unique geo combination

create_geo_table <- function(.data) {
  dplyr::distinct(
    .data,
    country,
    subnational,
    subnational_id,
    local,
    local_id,
    maa,
    maa_id
  ) %>%
    dplyr::arrange(country, subnational, local, maa)
}

source("data-raw/prepare-historical-data.R") #create tibble `historical`

# ---- List of two raw datasets

fma_data_raw <- list(
  ourfish = list(
    label = "OurFish Data",
    data = ourfish
  ),
  historical = list(
    label = "Historical/Enumerator Data",
    data = historical
  )
)

fma_data_geo <- purrr::map(fma_data_raw, function(x) {
  create_geo_table(x$data)
})


fma_data_summary <- purrr::map(fma_data_raw, function(x) {
  rarefma::create_data_summary(x$data)
})

# ---- Choose default data set
fma_init_data_source <- "ourfish"

# ---- Set the initial app selections
fma_init_geo_selections <- rarefma::get_geo_selections(fma_data_geo[[fma_init_data_source]],
  country_selected = "Indonesia"
)

# ---- create a list of data sources for the app
fma_data_sources <- names(fma_data_raw)
names(fma_data_sources) <- purrr::map_chr(fma_data_raw, ~ .$label)

fma_init_data_filtered <- fma_data_raw[[fma_init_data_source]]$data %>%
  dplyr::filter(
    country %in% fma_init_geo_selections$country$selected,
    subnational %in% fma_init_geo_selections$subnational$selected,
    local %in% fma_init_geo_selections$local$selected,
    maa %in% fma_init_geo_selections$maa$choices
  )

fma_init_data_summary_filtered <- fma_data_summary[[fma_init_data_source]] %>%
  dplyr::filter(
    country %in% fma_init_geo_selections$country$selected,
    subnational %in% fma_init_geo_selections$subnational$selected,
    local %in% fma_init_geo_selections$local$selected,
    maa %in% fma_init_geo_selections$maa$choices
  )


fma_init_dates <- fma_init_data_filtered %>%
  dplyr::select(transaction_date) %>%
  dplyr::distinct() %>%
  dplyr::arrange() %>%
  dplyr::pull()

fma_init_date_range <- list(
  min = min(fma_init_dates, na.rm = TRUE),
  max = max(fma_init_dates, na.rm = TRUE),
  valmin = min(fma_init_dates, na.rm = TRUE),
  valmax = max(fma_init_dates, na.rm = TRUE)
)
  

fma_init_family_species_selections <-
  rarefma::get_family_species_selections(
    .data = fma_data_raw[[fma_init_data_source]]$data,
    country = fma_init_geo_selections$country$selected,
    subnational = fma_init_geo_selections$subnational$selected,
    local = fma_init_geo_selections$local$selected,
    maa = fma_init_geo_selections$maa$choices#,
    #dates = c(fma_init_date_range$min, fma_init_date_range$max)
  )


# I did benchmarking to see if we should have a
# data_geo and a data_geo_family_species and use
# data_geo in most cases but the speed differences
# were tiny -- 1.42 ms vs 1.43 ms.
fma_data_geo_family_species <- purrr::map(
  fma_data_raw,
  ~ dplyr::distinct(
    .$data,
    country,
    subnational,
    subnational_id,
    local,
    local_id,
    maa,
    maa_id,
    family,
    species
  ) %>%
    dplyr::arrange(country, subnational, local, maa, family, species)
)

fma_init_performance_indicators <- "Species Composition"

usethis::use_data(
  fma_data_raw,
  fma_data_sources,
  fma_data_geo_family_species,
  fma_data_summary,
  fma_init_data_source,
  fma_init_geo_selections,
  fma_init_data_filtered,
  fma_init_family_species_selections,
  fma_init_data_summary_filtered,
  fma_init_date_range,
  fma_init_performance_indicators,
  overwrite = TRUE
)
