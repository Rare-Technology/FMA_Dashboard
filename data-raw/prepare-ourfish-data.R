library(dplyr)
library(lubridate)

# ---- Read in raw data
# ourfish <- readr::read_rds("data-raw/ourfish.rds")

# join_ourfish_footprint_fishbase from Fisheries Dashboard
# https://data.world/rare/fisheries-dashboard/workspace/
ourfish <- read.csv("https://query.data.world/s/lxjnrn7fhkv6cbivbt2uqrqwmskyv2",
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
    # iso3 = country_code,
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
    # gear_type,
    # fisher_id,
    # buyer_id,
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
# ourfish$gear_type[ourfish$gear_type == "spear gun"] <- "Spear gun"
# ourfish$gear_type[ourfish$gear_type == "Beach Seine"] <- "Beach seine"
ourfish$length[is.infinite(ourfish$length)] <- NA
ourfish$count[ourfish$count == 0] <- NA

# ---- A function to get the unique geo combination

create_geo_table <- function(.data) {
  dplyr::distinct(
    .data,
    country,
    # iso3,
    subnational,
    subnational_id,
    local,
    local_id,
    maa,
    maa_id
  ) %>%
    dplyr::arrange(country, subnational, local, maa)
}

# ---- List of two raw datasets

fma_data_raw <- list(
  ourfish = list(
    label = "OurFish Data",
    data = ourfish
  )#,
  # other = list(
  #   label = "Test other data source",
  #   data = fake_data
  # )
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
    maa %in% fma_init_geo_selections$maa$selected
  )

fma_init_data_summary_filtered <- fma_data_summary[[fma_init_data_source]] %>%
  dplyr::filter(
    country %in% fma_init_geo_selections$country$selected,
    subnational %in% fma_init_geo_selections$subnational$selected,
    local %in% fma_init_geo_selections$local$selected,
    maa %in% fma_init_geo_selections$maa$selected
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
    maa = fma_init_geo_selections$maa$selected#,
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
    # iso3,
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



usethis::use_data(
  fma_data_raw,
  # fma_reference_points, #see other script
  fma_data_sources,
  fma_data_geo_family_species,
  fma_data_summary,
  fma_init_data_source,
  fma_init_geo_selections,
  fma_init_data_filtered,
  fma_init_family_species_selections,
  fma_init_data_summary_filtered,
  fma_init_date_range,
  #fma_table_names,
  overwrite = TRUE
)
